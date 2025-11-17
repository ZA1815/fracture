use std::collections::HashMap;
use std::io::{self, Error, ErrorKind, Result, SeekFrom};
use std::path::{Path, PathBuf};
use std::pin::Pin;
use std::sync::Arc;
use std::task::{Context, Poll};
use std::time::SystemTime;

use parking_lot::lock_api::Mutex;
use parking_lot::RwLock;
use tokio::io::{AsyncRead, AsyncReadExt, AsyncSeek, AsyncWrite, AsyncWriteExt, ReadBuf};

use crate::chaos::{self, ChaosOperation};
use crate::time::deterministic_now;
use crate::task::yield_now;

#[derive(Clone, Debug)]
pub struct FsConfig {
    pub chunk_size: usize,
    pub yield_after_bytes: usize,
    pub enable_torn_writes: bool,
    pub enable_corruption: bool,
    pub max_file_size: usize
}

impl Default for FsConfig {
    fn default() -> Self {
        Self {
            chunk_size: 256,
            yield_after_bytes: 128,
            enable_torn_writes: true,
            enable_corruption: true,
            max_file_size: 100 * 1024 * 1024
        }
    }
}

#[derive(Clone, Debug)]
pub struct Metadata {
    pub len: u64,
    pub is_dir: bool,
    pub is_file: bool,
    pub modified: SystemTime,
    pub accessed: SystemTime,
    pub created: SystemTime,
    pub permissions: Permissions
}

impl Metadata {
    fn new_file() -> Self {
        let now = deterministic_now();
        Self {
            len: 0,
            is_dir: false,
            is_file: true,
            modified: now,
            accessed: now,
            created: now,
            permissions: Permissions::default()
        }
    }

    fn new_dir() -> Self {
        let now = deterministic_now();
        Self {
            len: 0,
            is_dir: true,
            is_file: false,
            modified: now,
            accessed: now,
            created: now,
            permissions: Permissions::default()
        }
    }

    pub fn len(&self) -> u64 {
        self.len
    }

    pub fn is_dir(&self) -> bool {
        self.is_dir
    }

    pub fn is_file(&self) -> bool {
        self.is_file
    }

    pub fn modified(&self) -> Result<SystemTime> {
        Ok(self.modified)
    }

    pub fn accessed(&self) -> Result<SystemTime> {
        Ok(self.accessed)
    }

    pub fn created(&self) -> Result<SystemTime> {
        Ok(self.created)
    }

    pub fn permissions(&self) -> Permissions {
        self.permissions.clone()
    }
}

#[derive(Clone, Debug)]
pub struct Permissions {
    readonly: bool
}

impl Default for Permissions {
    fn default() -> Self {
        Self { readonly: false }
    }
}

impl Permissions {
    pub fn readonly(&self) -> bool {
        self.readonly
    }

    pub fn set_readonly(&mut self, readonly: bool) {
        self.readonly = readonly;
    }
}

struct FileEntry {
    data: Vec<u8>,
    metadata: Metadata,
    open_count: usize
}

impl FileEntry {
    fn new() -> Self {
        Self {
            data: Vec::new(),
            metadata: Metadata::new_file(),
            open_count: 0
        }
    }

    fn update_modified(&mut self) {
        self.metadata.modified = deterministic_now();
        self.metadata.len = self.data.len() as u64;
    }

    fn update_acessed(&mut self) {
        self.metadata.accessed = deterministic_now();
    }
}

#[derive(Clone)]
enum Entry {
    File(Arc<RwLock<HashMap<String, Entry>>>),
    Directory(Arc<RwLock<HashMap<String, Entry>>>)
}

struct FsRegistry {
    root: RwLock<HashMap<String, Entry>>,
    config: FsConfig
}

impl FsRegistry {
    fn new() -> Self {
        Self {
            root: RwLock::new(HashMap::new()),
            config: FsConfig::default()
        }
    }

    fn get_or_create_entry(&self, path: &Path, create: bool) -> Result<Entry> {
        let path_str = path.to_string_lossy();
        let components: Vec<&str> = path_str.trim_start_matches('/').split('/').filter(|s| !s.is_empty()).collect();

        if components.is_empty() {
            return Err(Error::new(ErrorKind::InvalidInput, "fracture: Invalid path"))
        }

        let mut root = self.root.write();
        let mut current_map = &mut *root;

        for (i, component) in components.iter().enumerate() {
            let is_last = i == components.len() - 1;

            if is_last && create {
                if !current_map.contains_key(*component) {
                    let entry = Entry::File(Arc::new(RwLock::new(FileEntry::new())));
                    current_map.insert(component.to_string(), entry.clone());
                    return Ok(entry);
                }
                else {
                    return match current_map.get(*component).unwrap() {
                        Entry::File(f) => Ok(Entry::File(Arc::clone(f))),
                        Entry::Directory(_) => Err(Error::new(ErrorKind::IsADirectory, "fracture: Path is a directory"))
                    };
                }
            }

            if !current_map.contains_key(*component) {
                if !create {
                    return Err(Error::new(ErrorKind::NotFound, "fracture: Path not found"));
                }
                let dir = Entry::Directory(Arc::new(RwLock::new(HashMap::new())));
                current_map.insert(component.to_string(), dir);
            }

            match current_map.get_mut(*component).unwrap() {
                Entry::Directory(dir) => {
                    // Placeholder
                    return Err(Error::new(ErrorKind::Other, "fracture: Path navigation not fully implemented"));
                }
                Entry::File(_) if !is_last => {
                    return Err(Error::new(ErrorKind::NotADirectory, "fracture: Path component is a file"));
                }
                Entry::File(f) => {
                    return Ok(Entry::File(Arc::clone(f)));
                }
            }
        }

        Err(Error::new(ErrorKind::Other, "fracture: Failed to resolve path"))
    }

    fn get_entry_simple(&self, path: &Path) -> Result<Entry> {
        let path_str = path.to_string_lossy().to_string();
        let root = self.root.read();

        root.get(&path_str).cloned().ok_or_else(|| Error::new(ErrorKind::NotFound, "fracture: File not found"))
    }

    fn create_entry(&self, path: &Path) -> Result<Entry> {
        let path_str = path.to_string_lossy().to_string();
        let mut root = self.root.write();

        if root.contains_key(&path_str) {
            return Err(Error::new(ErrorKind::AlreadyExists, "fracture: File already exists"));
        }

        let entry = Entry::File(Arc::new(RwLock::new(FileEntry::new())));
        root.insert(path_str, entry.clone());
        Ok(entry)
    }

    fn remove_entry(&self, path: &Path) -> Result<()> {
        let path_str = path.to_string_lossy().to_string();
        let mut root = self.root.write();

        root.remove(&path_str).ok_or_else(|| Error::new(ErrorKind::NotFound, "fracture: File not found"));

        Ok(())
    }

    fn rename_entry(&self, from: &Path, to: &Path) -> Result<()> {
        let from_str = from.to_string_lossy().to_string();
        let to_str = to.to_string_lossy().to_string();

        let mut root = self.root.write();

        let entry = root.remove(&from_str).ok_or_else(|| Error::new(ErrorKind::NotFound, "fracture: Source file not found"))?;

        root.insert(to_str, entry);
        Ok(())
    }

    fn create_dir(&self, path: &Path) -> Result<()> {
        let path_str = path.to_string_lossy().to_string();
        let mut root = self.root.write();

        if root.contains_key(&path_str) {
            return Err(Error::new(ErrorKind::AlreadyExists, "fracture: Directory already exists"));
        }

        let entry = Entry::Directory(Arc::new(RwLock::new(HashMap::new())));
        root.insert(path_str, entry);

        Ok(())
    }

    fn read_dir(&self, path: &Path) -> Result<Vec<DirEntry>> {
        let path_str = path.to_string_lossy().to_string();
        let root = self.root.read();

        match root.get(&path_str) {
            Some(Entry::Directory(dir)) => {
                let dir_map = dir.read();
                let entries = dir_map.keys()
                    .map(|name| DirEntry {
                        path: PathBuf::from(format!("{}/{}", path_str, name)),
                        file_name: name.clone()
                    })
                    .collect();

                Ok(entries)
            }
            Some(Entry::File(_)) => Err(Error::new(ErrorKind::NotADirectory, "fracture: Not a directory")),
            None => Err(Error::new(ErrorKind::NotFound, "fracture: Directory not found"))
        }
    }
}

static FS: std::sync::LazyLock<FsRegistry> = std::sync::LazyLock::new(FsRegistry::new);

pub struct File {
    entry: Arc<RwLock<FileEntry>>,
    cursor: Arc<Mutex<u64>>,
    path: PathBuf,
    readable: bool,
    writable: bool,
    append: bool
}

impl File {
    pub async fn open<P: AsRef<Path>>(path: P) -> Result<Self> {
        if chaos::should_fail(ChaosOperation::FsOpen) {
            return Err(Error::new(ErrorKind::PermissionDenied, "fracture: Open failed (chaos)"));
        }

        let path = path.as_ref();
        let entry = match FS.get_entry_simple(path) {
            Ok(Entry::File(f)) => f,
            Ok(Entry::Directory(_)) => return Err(Error::new(ErrorKind::IsADirectory, "fracture: Is a directory")),
            Err(e) => return Err(e)
        };

        let mut file_entry = entry.write();
        file_entry.open_count += 1;
        file_entry.update_accessed();

        yield_now().await;

        Ok(Self {
            entry,
            cursor: Arc::new(Mutex::new(0)),
            path: path.to_path_buf(),
            readable: true,
            writable: false,
            append: false
        })
    }

    pub async fn create<P: AsRef<Path>>(path: P) -> Result<Self> {
        if chaos::should_fail(ChaosOperation::FsCreate) {
            return Err(Error::new(ErrorKind::PermissionDenied, "fracture: Create failed (chaos)"));
        }

        if chaos::should_fail(ChaosOperation::FsAlreadyExists) {
            return Err(Error::new(ErrorKind::AlreadyExists, "fracture: File already exists (chaos)"));
        }

        let path = path.as_ref();
        let entry = FS.create_entry(path)
            .or_else(|_| FS.get_entry_simple(path))
            .and_then(|e| match e {
                Entry::File(f) => Ok(f),
                Entry::Directory(_) => Err(Error::new(ErrorKind::IsADirectory, "fracture: Is a directory"))
            })?;

        let mut file_entry = entry.write();
        file_entry.data.clear();
        file_entry.open_count += 1;
        file_entry.update_modified();

        yield_now().await;

        Ok(Self {
            entry,
            cursor: Arc::new(Mutex::new(0)),
            path: path.to_path_buf(),
            readable: true,
            writable: true,
            append: false
        })
    }

    pub async fn options() -> OpenOptions {
        OpenOptions::new()
    }

    pub async fn sync_all(&self) -> Result<()> {
        if chaos::should_fail(ChaosOperation::FsSyncAll) {
            return Err(Error::new(ErrorKind::Other, "fracture: Sync failed (chaos)"));
        }

        yield_now().await;

        Ok(())
    }

    pub async fn sync_data(&self) -> Result<()> {
        if chaos::should_fail(ChaosOperation::FsSyncData) {
            return Err(Error::new(ErrorKind::Other, "fracture: Sync data failed (chaos)"));
        }

        yield_now().await;

        Ok(())
    }

    pub async fn set_len(&self, size: u64) -> Result<()> {
        if chaos::should_fail(ChaosOperation::FsTruncate) {
            return Err(Error::new(ErrorKind::Other, "fracture: Truncate failed (chaos)"));
        }

        let mut entry = self.entry.write();
        entry.data.resize(size as usize, 0);
        entry.update_modified();

        yield_now().await;

        Ok(())
    }

    pub async fn metadata(&self) -> Result<Metadata> {
        if chaos::should_fail(ChaosOperation::FsMetadata) {
            return Err(Error::new(ErrorKind::Other, "fracture: Metadata failed (chaos)"));
        }

        let entry = self.entry.read();

        Ok(entry.metadata.clone())
    }

    pub async fn try_clone(&self) -> Result<Self> {
        let mut entry = self.entry.write();
        entry.open_count += 1;
        drop(entry);

        Ok(Self {
            entry: Arc::clone(&self.entry),
            cursor: Arc::new(Mutex::new(*self.cursor.lock())),
            path: self.path.clone(),
            readable: self.readable,
            writable: self.writable,
            append: self.append
        })
    }

    pub async fn set_permissions(&self, perm: Permissions) -> Result<()> {
        if chaos::should_fail(ChaosOperation::FsSetPermissions) {
            return Err(Error::new(ErrorKind::PermissionDenied, "fracture: Set permissions failed (chaos)"));
        }

        let mut entry = self.entry.write();
        entry.metadata.permissions = perm;
        yield_now().await;

        Ok(())
    }
}

impl Drop for File {
    fn drop(&mut self) {
        let mut entry = self.entry.write();
        entry.open_count = entry.open_count.saturating_sub(1);
    }
}

impl AsyncRead for File {
    fn poll_read(
            self: Pin<&mut Self>,
            cx: &mut Context<'_>,
            buf: &mut ReadBuf<'_>,
        ) -> Poll<io::Result<()>> {
        if !self.readable {
            return Poll::Ready(Err(Error::new(ErrorKind::PermissionDenied, "fracture: File not opened for reading")));
        }

        if chaos::should_fail(ChaosOperation::FsRead) {
            return Poll::Ready(Err(Error::new(ErrorKind::BrokenPipe, "fracture: Read failed (chaos)")));
        }

        if chaos::should_fail(ChaosOperation::FsCorruption) {
            let available = buf.remaining().min(4);
            if available > 0 {
                let unfilled = buf.initialize_unfilled_to(available);
                for byte in unfilled {
                    *byte = 0xFF;
                }
                buf.advance(available);
            }
            return Poll::Ready(Ok(()))
        }

        let config = &FS.config;
        let cursor_pos = *self.cursor.lock();

        let entry = self.entry.read();
        let data_len = entry.data.len() as u64;

        if cursor_pos >= data_len {
            return Poll::Ready(Ok(()))
        }

        let chunk_size = config.chunk_size.min(buf.remaining());
        let read_size = chunk_size.min((data_len - cursor_pos) as usize);

        if read_size > 0 {
            let start = cursor_pos as usize;
            let end = start + read_size;

            buf.put_slice(&entry.data[start..end]);
            drop(entry);

            *self.cursor.lock() = cursor_pos + read_size as u64;

            if read_size >= config.yield_after_bytes {
                cx.waker().wake_by_ref();
                return Poll::Pending;
            }
        }

        Poll::Ready(Ok(()))
    }
}

impl AsyncWrite for File {
    fn poll_write(
            self: Pin<&mut Self>,
            cx: &mut Context<'_>,
            buf: &[u8],
        ) -> Poll<std::result::Result<usize, io::Error>> {
        if !self.writable {
            return Poll::Ready(Err(Error::new(ErrorKind::PermissionDenied, "fracture: File not opened for writing")));
        }

        if chaos::should_fail(ChaosOperation::FsWrite) {
            return Poll::Ready(Err(Error::new(ErrorKind::BrokenPipe, "fracture: Write failed (chaos)")));
        }

        if chaos::should_fail(ChaosOperation::FsQuotaExceeded) {
            return Poll::Ready(Err(Error::new(ErrorKind::Other, "fracture: Disk full (chaos)")));
        }

        let config = &FS.config;
        let mut cursor_pos = *self.cursor.lock();

        if self.append {
            let entry = self.entry.read();
            cursor_pos = entry.data.len() as u64;
            drop(entry);
        }

        let chunk_size = config.chunk_size.min(buf.len());
        let write_size = if chaos::should_fail(ChaosOperation::IoPartialWrite) && config.enable_torn_writes {
            chunk_size / 2
        }
        else {
            chunk_size
        };

        if write_size == 0 {
            return Poll::Ready(Ok(()))
        }

        let mut entry = self.entry.write();

        let new_size = cursor_pos as usize + write_size;
        if new_size > config.max_file_size {
            return Poll::Ready(Err(Error::new(ErrorKind::Other, "fracture: File too large")));
        }

        if new_size > entry.data.len() {
            entry.data.resize(new_size, 0);
        }

        let start = cursor_pos as usize;
        let end = start + write_size;

        if chaos::should_fail(ChaosOperation::FsCorruption) && config.enable_corruption {
            for (i, byte) in buf[..write_size].iter().enumerate() {
                entry.data[start + i] = byte ^ 0xAA;
            }
        }
        else {
            entry.data[start..end].copy_from_slice(&buf[..write_size]);
        }

        entry.update_modified();
        drop(entry);

        *self.cursor.lock() = cursor_pos + write_size as u64;

        if write_size >= config.yield_after_bytes {
            cx.waker().wake_by_ref();
            return Poll::Pending;
        }

        Poll::Ready(Ok(write_size))
    }

    fn poll_flush(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<std::result::Result<(), io::Error>> {
        if chaos::should_fail(ChaosOperation::FsSync) {
            return Poll::Ready(Err(Error::new(ErrorKind::Other, "fracture: Flush failed (chaos)")));
        }

        cx.waker().wake_by_ref();
        Poll::Ready(Ok(()))
    }

    fn poll_shutdown(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<std::result::Result<(), io::Error>> {
        Poll::Ready(Ok(()))
    }
}

impl AsyncSeek for File {
    fn start_seek(self: Pin<&mut Self>, position: SeekFrom) -> io::Result<()> {
        if chaos::should_fail(ChaosOperation::FsSeek) {
            return Err(Error::new(ErrorKind::Other, "fracture: Seek failed (chaos)"));
        }

        let entry = self.entry.read();
        let file_len = entry.data.len() as u64;
        drop(entry);

        let mut cursor = self.cursor.lock();
        let new_pos = match position {
            SeekFrom::Start(pos) => pos as i64,
            SeekFrom::Current(offset) => *cursor as i64 + offset,
            SeekFrom::End(offset) => file_len as i64 + offset
        };

        if new_pos < 0 {
            return Err(Error::new(ErrorKind::InvalidInput, "fracture: Invalid seek position"));
        }

        *cursor = new_pos as u64;

        Ok(())
    }

    fn poll_complete(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<io::Result<u64>> {
        Poll::Ready(Ok(*self.cursor.lock()))
    }
}

#[derive(Clone, Debug)]
pub struct OpenOptions {
    read: bool,
    write: bool,
    append: bool,
    truncate: bool,
    create: bool,
    create_new: bool
}

impl OpenOptions {
    pub fn new() -> Self {
        Self {
            read: false,
            write: false,
            append: false,
            truncate: false,
            create: false,
            create_new: false,
        }
    }

    pub fn read(&mut self, read: bool) -> &mut Self {
        self.read = read;
        self
    }

    pub fn write(&mut self, write: bool) -> &mut Self {
        self.write = write;
        self
    }

    pub fn append(&mut self, append: bool) -> &mut Self {
        self.append = append;
        self
    }

    pub fn truncate(&mut self, truncate: bool) -> &mut Self {
        self.truncate = truncate;
        self
    }

    pub fn create(&mut self, create: bool) -> &mut Self {
        self.create = create;
        self
    }

    pub fn create_new(&mut self, create_new: bool) -> &mut Self {
        self.create_new = create_new;
        self
    }

    pub async fn open<P: AsRef<Path>>(&self, path: P) -> Result<File> {
        if chaos::should_fail(ChaosOperation::FsOpen) {
            return Err(Error::new(ErrorKind::PermissionDenied, "fracture: Open failed (chaos)"));
        }

        let path = path.as_ref();

        if self.create_new {
            let entry = FS.create_entry(path)?;
            match entry {
                Entry::File(f) => {
                    let mut file_entry = f.write();
                    file_entry.open_count += 1;
                    drop(file_entry);

                    yield_now().await;

                    return Ok(File {
                        entry: f,
                        cursor: Arc::new(Mutex::new(0)),
                        path: path.to_path_buf(),
                        readable: self.read,
                        writable: self.write || self.append,
                        append: self.append
                    });
                }
                Entry::Directory(_) => return Err(Error::new(ErrorKind::IsADirectory, "fracture: Is a directory"))
            }
        }
        let entry = if self.create {
            FS.create_entry(path).or_else(|_| FS.get_entry_simple(path))
        }
        else {
            FS.get_entry_simple(path)
        };

        let entry = match entry? {
            Entry::File(f) => f,
            Entry::Directory(_) => return Err(Error::new(ErrorKind::IsADirectory, "fracture: Is a directory")),
        }

        let mut file_entry = entry.write();

        if self.truncate {
            file_entry.data.clear();
        }

        file_entry.open_count += 1;
        file_entry.update_accessed();

        yield_now().await;

        Ok(File {
            entry,
            cursor: Arc::new(Mutex::new(0)),
            path: path.to_path_buf(),
            readable: self.read,
            writable: self.write || self.append,
            append: self.append
        })
    }
}

impl Default for OpenOptions {
    fn default() -> Self {
        Self::new()
    }
}

pub struct DirEntry {
    path: PathBuf,
    file_name: String
}

impl DirEntry {
    pub fn path(&self) -> PathBuf {
        self.path.clone()
    }

    pub fn file_name(&self) -> std::ffi::OsString {
        self.file_name.clone().into()
    }

    pub async fn metadata(&self) -> Result<Metadata> {
        metadata(&self.path).await;
    }

    pub async fn file_type(&self) -> Result<FileType> {
        let meta = self.metadata().await?;
        
        Ok(FileType {
            is_dir: meta.is_dir,
            is_file: meta.is_file
        })
    }
}

pub struct ReadDir {
    entries: Vec<DirEntry>,
    index: usize
}

impl ReadDir {
    pub async fn next_entry(&mut self) -> Result<Option<DirEntry>> {
        if chaos::should_fail(ChaosOperation::FsReadDir) {
            return Err(Error::new(ErrorKind::Other, "fracture: Read dir failed (chaos)"));
        }

        if self.index >= self.entries.len() {
            return Ok(None);
        }

        let entry = self.entries[self.index].clone();
        self.index += 1;

        yield_now().await;

        Ok(Some(entry))
    }
}

pub struct FileType {
    is_dir: bool,
    is_file: bool
}

impl FileType {
    pub fn is_dir(&self) -> bool {
        self.is_dir
    }

    pub fn is_file(&self) -> bool {
        self.is_file
    }

    pub fn is_symlink(&self) -> bool {
        false
    }
}

pub async fn read<P: AsRef<Path>>(path: P) -> Result<Vec<u8>> {
    let mut file = File::open(path).await?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).await?;

    Ok(buffer)
}

pub async fn read_to_string<P: AsRef<Path>>(path: P) -> Result<String> {
    let bytes = read(path).await?;
    String::from_utf8(bytes).map_err(|e| Error::new(ErrorKind::InvalidData, format!("fracture: {}", e)))
}

pub async fn write<P: AsRef<Path>, C: AsRef<[u8]>>(path: P, contents: C) -> Result<()> {
    let mut file = File::create(path).await?;
    file.write_all(contents.as_ref()).await?;
    file.sync_all().await?;

    Ok(())
}

pub async fn remove_file<P: AsRef<Path>>(path: P) -> Result<()> {
    if chaos::should_fail(ChaosOperation::FsRemoveFile) {
        return Err(Error::new(ErrorKind::PermissionDenied, "fracture: Remove failed (chaos)"));
    }

    FS.remove_entry(path.as_ref())?;
    yield_now().await;

    Ok(())
}

pub async fn rename<P: AsRef<Path>, Q: AsRef<Path>>(from: P, to: Q) -> Result<()> {
    if chaos::should_fail(ChaosOperation::FsRename) {
        return Err(Error::new(ErrorKind::PermissionDenied, "fracture: Rename failed (chaos)"));
    }

    if chaos::should_fail(ChaosOperation::RaceCondition) {
        yield_now().await;
    }

    FS.rename_entry(from.as_ref(), to.as_ref())?;
    yield_now().await;

    Ok(())
}

pub async fn copy<P: AsRef<Path>, Q: AsRef<Path>>(from: P, to: Q) -> Result<u64> {
    if chaos::should_fail(ChaosOperation::FsCopy) {
        return Err(Error::new(ErrorKind::PermissionDenied, "fracture: Copy failed (chaos)"));
    }

    let data = read(from).await?;
    let len = data.len() as u64;
    write(to, data).await?;

    Ok(len)
}

pub async fn metadata<P: AsRef<Path>>(path: P) -> Result<Metadata> {
    if chaos::should_fail(ChaosOperation::FsMetadata) {
        return Err(Error::new(ErrorKind::PermissionDenied, "fracture: Metadata failed (chaos)"));
    }

    let entry = FS.get_entry_simple(path.as_ref())?;
    match entry {
        Entry::File(f) => {
            let file_entry = f.read();
            Ok(file_entry.metadata.clone())
        }
        Entry::Directory(_) => Ok(Metadata::new_dir())
    }
}

pub async fn canonicalize<P: AsRef<Path>>(path: P) -> Result<PathBuf> {
    if chaos::should_fail(ChaosOperation::FsCanonical) {
        return Err(Error::new(ErrorKind::Other, "fracture: Canonicalize failed (chaos)"));
    }

    Ok(path.as_ref().to_path_buf())
}

pub async fn create_dir<P: AsRef<Path>>(path: P) -> Result<()> {
    if chaos::should_fail(ChaosOperation::FsCreateDir) {
        return Err(Error::new(ErrorKind::PermissionDenied, "fracture: Create dir failed (chaos)"));
    }

    FS.create_dir(path.as_ref())?;
    yield_now().await;

    Ok(())
}

pub async fn create_dir_all<P: AsRef<Path>>(path: P) -> Result<()> {
    create_dir(path).await
}

pub async fn remove_dir<P: AsRef<Path>>(path: P) -> Result<()> {
    if chaos::should_fail(ChaosOperation::FsRemoveDir) {
        return Err(Error::new(ErrorKind::PermissionDenied, "fracture: Remove dir failed (chaos)"));
    }

    FS.remove_entry(path.as_ref())?;
    yield_now().await;

    Ok(())
}

pub async fn remove_dir_all<P: AsRef<Path>>(path: P) -> Result<()> {
    remove_dir(path).await
}

pub async fn read_dir<P: AsRef<Path>>(path: P) -> Result<ReadDir> {
    if chaos::should_fail(ChaosOperation::FsReadDir) {
        return Err(Error::new(ErrorKind::PermissionDenied, "fracture: Read dir failed (chaos)"));
    }

    let entries = FS.read_dir(path.as_ref())?;
    yield_now().await;

    Ok(ReadDir {
        entries,
        index: 0
    })
}

pub async fn set_permissions<P: AsRef<Path>>(path: P, perm: Permissions) -> Result<()> {
    if chaos::should_fail(ChaosOperation::FsSetPermissions) {
        return Err(Error::new(ErrorKind::PermissionDenied, "fracture: Set permissions failed (chaos)"));
    }

    let entry = FS.get_entry_simple(path.as_ref())?;
    match entry {
        Entry::File(f) => {
            let mut file_entry = f.write();
            file_entry.metadata.permissions = perm;
        }
        Entry::Directory(_) => {}
    }

    yield_now().await;

    Ok(())
}

pub async fn symlink_metadata<P: AsRef<Path>>(path: P) -> Result<Metadata> {
    metadata(path).await
}

pub async fn read_link<P: AsRef<Path>>(path: P) -> Result<PathBuf> {
    // Placeholder
    Err(Error::new(ErrorKind::Other, "fracture: Symlinks not supported"))
}

pub async fn hard_link<P: AsRef<Path>, Q: AsRef<Path>>(src: P, dst: Q) -> Result<()> {
    if chaos::should_fail(ChaosOperation::FsHardLink) {
        return Err(Error::new(ErrorKind::PermissionDenied, "fracture: Hard link failed (chaos)"));
    }

    // Placeholder
    Err(Error::new(ErrorKind::Other, "Hard links not supported"))
}

pub fn set_chunk_size(size: usize) {
    // Placeholder
    // Currently, config set at init
}

pub fn set_corruption_enabled(enabled: bool) {
    // Placeholder
    // Currently, config set at init
}

pub fn get_config() -> FsConfig {
    FS.config.clone()
}