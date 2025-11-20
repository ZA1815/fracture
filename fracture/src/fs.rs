use std::path::{Path, PathBuf};
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use std::io::{self, Result, Error, ErrorKind, SeekFrom};
use std::pin::Pin;
use std::task::{Context, Poll};
use std::time::SystemTime;
use std::time::Duration;

use crate::io::{AsyncRead, AsyncWrite, AsyncSeek, ReadBuf};
use crate::runtime::Handle;
use crate::chaos::{self, ChaosOperation};
use crate::time::sleep;

async fn simulate_io_latency() {
    if let Some(delay) = chaos::get_delay("disk_io") {
        sleep(delay).await;
    } else {
        sleep(Duration::from_micros(10)).await;
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct FileType {
    is_dir: bool,
    is_file: bool,
    is_symlink: bool,
}

impl FileType {
    pub fn is_dir(&self) -> bool {
        self.is_dir
    }
    pub fn is_file(&self) -> bool {
        self.is_file
    }
    pub fn is_symlink(&self) -> bool {
        self.is_symlink
    }
}

#[derive(Clone, Debug)]
pub struct Metadata {
    len: u64,
    file_type: FileType,
    readonly: bool,
    created: SystemTime,
    modified: SystemTime,
    accessed: SystemTime,
}

impl Metadata {
    pub fn file_type(&self) -> FileType {
        self.file_type
    }

    pub fn is_dir(&self) -> bool {
        self.file_type.is_dir()
    }

    pub fn is_file(&self) -> bool {
        self.file_type.is_file()
    }

    pub fn is_symlink(&self) -> bool {
        self.file_type.is_symlink()
    }

    pub fn len(&self) -> u64 {
        self.len
    }

    pub fn permissions(&self) -> Permissions {
        Permissions {
            readonly: self.readonly
        }
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
}

#[derive(Debug, Clone)]
pub struct Permissions {
    readonly: bool
}

impl Permissions {
    pub fn readonly(&self) -> bool {
        self.readonly
    }

    pub fn set_readonly(&mut self, readonly: bool) {
        self.readonly = readonly;
    }
}

#[derive(Clone)]
struct FileEntry {
    content: Vec<u8>,
    file_type: FileType,
    readonly: bool,
    created: SystemTime,
    modified: SystemTime,
    accessed: SystemTime,
    inode: u64
}

impl FileEntry {
    fn new_file(now: SystemTime, inode: u64) -> Self {
        Self {
            content: Vec::new(),
            file_type: FileType { is_dir: false, is_file: true, is_symlink: false },
            readonly: false,
            created: now,
            modified: now,
            accessed: now,
            inode

        }
    }

    fn new_dir(now: SystemTime, inode: u64) -> Self {
        Self {
            content: Vec::new(),
            file_type: FileType { is_dir: true, is_file: false, is_symlink: false },
            readonly: false,
            created: now,
            modified: now,
            accessed: now,
            inode
        }
    }
}

pub struct FileSystemState {
    files: HashMap<PathBuf, Arc<Mutex<FileEntry>>>,
    next_inode: u64
}

impl FileSystemState {
    pub fn new() -> Self {
        Self {
            files: HashMap::new(),
            next_inode: 1
        }
    }

    fn assign_inode(&mut self) -> u64 {
        let id = self.next_inode;
        self.next_inode += 1;
        id
    }
}

pub struct File {
    path: PathBuf,
    entry: Arc<Mutex<FileEntry>>,
    cursor: u64,
    can_read: bool,
    can_write: bool
}

impl File {
    pub async fn open<P: AsRef<Path>>(path: P) -> Result<File> {
        OpenOptions::new().read(true).open(path).await
    }

    pub async fn create<P: AsRef<Path>>(path: P) -> Result<File> {
        OpenOptions::new().write(true).create(true).truncate(true).open(path).await
    }

    pub async fn sync_all(&self) -> Result<()> {
        if chaos::should_fail(ChaosOperation::FsSyncAll) {
             return Err(Error::new(ErrorKind::Other, "fracture: SyncAll failed (chaos)"));
        }
        simulate_io_latency().await;

        Ok(()) 
    }

    pub async fn sync_data(&self) -> Result<()> {
        if chaos::should_fail(ChaosOperation::FsSyncData) {
             return Err(Error::new(ErrorKind::Other, "fracture: SyncData failed (chaos)"));
        }
        simulate_io_latency().await;

        Ok(()) 
    }

    pub async fn set_len(&self, size: u64) -> Result<()> {
        if chaos::should_fail(ChaosOperation::FsTruncate) {
            return Err(Error::new(ErrorKind::Other, "fracture: Truncate failed (chaos)"));
        }

        if !self.can_write {
            return Err(Error::new(ErrorKind::PermissionDenied, "fracture: Not opened for write"));
        }

        simulate_io_latency().await;

        let mut locked = self.entry.lock().unwrap();
        if locked.readonly {
            return Err(Error::new(ErrorKind::PermissionDenied, "fracture: Read-only file"))
        }

        if size > locked.content.len() as u64 && chaos::should_fail(ChaosOperation::FsQuotaExceeded) {
            return Err(Error::new(ErrorKind::Other, "fracture: No space left on device (chaos)"));
        }

        locked.content.resize(size as usize, 0);

        let handle = Handle::current();
        if let Some(core_rc) = handle.core.upgrade() {
            let core = core_rc.borrow();
            locked.modified = core.sys_now();
        }

        Ok(())
    }

    pub async fn metadata(&self) -> Result<Metadata> {
        if chaos::should_fail(ChaosOperation::FsMetadata) {
            return Err(Error::new(ErrorKind::Other, "fracture: Metadata failed (chaos)"));
        }
        simulate_io_latency().await;

        let locked = self.entry.lock().unwrap();

        Ok(Metadata {
            len: locked.content.len() as u64,
            file_type: locked.file_type,
            readonly: locked.readonly,
            created: locked.created,
            modified: locked.modified,
            accessed: locked.accessed,
        })
    }

    pub async fn try_clone(&self) -> Result<File> {
        Ok(File {
            path: self.path.clone(),
            entry: self.entry.clone(),
            cursor: self.cursor,
            can_read: self.can_read,
            can_write: self.can_write,
        })
    }

    pub async fn set_permissions(&self, perm: Permissions) -> Result<()> {
        if chaos::should_fail(ChaosOperation::FsSetPermissions) {
             return Err(Error::new(ErrorKind::PermissionDenied, "fracture: SetPermissions failed (chaos)"));
        }
        simulate_io_latency().await;

        let mut locked = self.entry.lock().unwrap();
        locked.readonly = perm.readonly();

        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct OpenOptions {
    read: bool,
    write: bool,
    append: bool,
    truncate: bool,
    create: bool,
    create_new: bool,
}

impl OpenOptions {
    pub fn new() -> Self {
        Self {
            read: false,
            write: false,
            append: false,
            truncate: false,
            create: false,
            create_new: false
        }
    }

    pub async fn open<P: AsRef<Path>>(&self, path: P) -> Result<File> {
        if chaos::should_fail(ChaosOperation::FsOpen) {
            return Err(Error::new(ErrorKind::Other, "fracture: Open failed (chaos)"));
        }

        let path = path.as_ref().to_path_buf();
        simulate_io_latency().await;

        let handle = Handle::current();
        let core_rc = handle.core.upgrade().ok_or_else(|| Error::new(ErrorKind::Other, "fracture: Runtime dropped"))?;
        let mut core = core_rc.borrow_mut();

        let now = core.sys_now();
        let exists = core.fs.files.contains_key(&path);

        if self.create_new && exists {
            if chaos::should_fail(ChaosOperation::FsAlreadyExists) {
                return Err(Error::new(ErrorKind::AlreadyExists, "fracture: File exists (chaos)"));
            }
            return Err(Error::new(ErrorKind::AlreadyExists, "fracture: File exists"));
        }

        if !exists && !self.create && !self.create_new {
            if chaos::should_fail(ChaosOperation::FsNotFound) {
                 return Err(Error::new(ErrorKind::NotFound, "fracture: File not found (chaos)"));
            }
            return Err(Error::new(ErrorKind::NotFound, "fracture: File not found"));
        }

        if chaos::should_fail(ChaosOperation::FsPermissionDenied) {
            return Err(Error::new(ErrorKind::PermissionDenied, "fracture: Permission denied (chaos)"));
        }

        let entry_ref = if exists {
            let entry = core.fs.files.get(&path).unwrap().clone();

            if entry.lock().unwrap().is_dir {
                return Err(Error::new(ErrorKind::IsADirectory, "fracture: Is a directory"));
            }

            if self.truncate && self.write {
                let mut locked = entry.lock().unwrap();

                if locked.readonly {
                    return Err(Error::new(ErrorKind::PermissionDenied, "fracture: Read-only file"));
                }

                locked.content.clear();
                locked.modified = now;
            }

            entry
        }
        else {
            if let Some(parent) = path.parent() {
                if !parent.as_os_str().is_empty() && !core.fs.files.contains_key(parent) {
                    return Err(Error::new(ErrorKind::NotFound, "fracture: Parent directory not found"));
                }
                
                let inode = core.fs.assign_inode();
                let entry = Arc::new(Mutex::new(FileEntry::new_file(now, inode)));
                core.fs.files.insert(path.clone(), entry.clone());

                entry
            }
            else {
                let inode = core.fs.assign_inode();
                let entry = Arc::new(Mutex::new(FileEntry::new_file(now, inode)));
                core.fs.files.insert(path.clone(), entry.clone());

                entry
            }
        };

        {
            let mut locked = entry_ref.lock().unwrap();
            locked.accessed = now;
        }

        let cursor = if self.append {
            entry_ref.lock().unwrap().content.len() as u64
        }
        else {
            0
        };

        Ok(File {
            path,
            entry: entry_ref,
            cursor,
            can_read: self.read,
            can_write: self.write || self.append,
        })
    }

    pub fn read(&mut self, mode: bool) -> &mut Self {
        self.read = mode;
        self
    }
    pub fn write(&mut self, mode: bool) -> &mut Self {
        self.write = mode;
        self
    }
    pub fn append(&mut self, mode: bool) -> &mut Self {
        self.append = mode;
        self
    }
    pub fn truncate(&mut self, mode: bool) -> &mut Self {
        self.truncate = mode;
        self
    }
    pub fn create(&mut self, mode: bool) -> &mut Self {
        self.create = mode;
        self
    }
    pub fn create_new(&mut self, mode: bool) -> &mut Self {
        self.create_new = mode;
        self
    }
}

impl AsyncRead for File {
    fn poll_read(self: Pin<&mut Self>, _cx: &mut Context<'_>, buf: &mut ReadBuf<'_>) -> Poll<Result<()>> {
        if chaos::should_fail(ChaosOperation::FsRead) {
             return Poll::Ready(Err(Error::new(ErrorKind::Other, "fracture: Read failed (chaos)")));
        }

        if !self.can_read {
            return Poll::Ready(Err(Error::new(ErrorKind::PermissionDenied, "fracture: Not opened for reading")));
        }

        let mut locked = self.entry.lock().unwrap();

        let handle = Handle::current();
        if let Some(core_rc) = handle.core.upgrade() {
            let core = core_rc.borrow();
            locked.accessed = core.sys_now();
        }

        let pos = self.cursor as usize;

        if pos >= locked.content.len() {
            return Poll::Ready(Ok(()))
        }

        let max_read = if chaos::should_fail(ChaosOperation::IoPartialRead) {
            1.max(buf.remaining() / 2)
        }
        else {
            buf.remaining()
        };

        let available = &locked.content[pos..];
        let to_copy = std::cmp::min(available.len(), buf.remaining());

        if chaos::should_fail(ChaosOperation::FsCorruption) {
            let mut garbage = vec![0u8; to_copy];
            
            if let Some(core_rc) = handle.core.upgrade() {
                let mut core = core_rc.borrow_mut();
                core.rng.fill_bytes(&mut garbage);
            }
            else {
                garbage.fill(0xAA); 
            }
            
            buf.put_slice(&garbage);
        }
        else {
            buf.put_slice(&available[..to_copy]);
        }

        self.cursor += to_copy as u64;

        Poll::Ready(Ok(()))
    }
}

impl AsyncWrite for File {
    fn poll_write(mut self: Pin<&mut Self>, _cx: &mut Context<'_>, buf: &[u8]) -> Poll<Result<usize>> {
        if chaos::should_fail(ChaosOperation::FsWrite) {
             return Poll::Ready(Err(Error::new(ErrorKind::Other, "fracture: Write failed (chaos)")));
        }

        if chaos::should_fail(ChaosOperation::FsQuotaExceeded) {
             return Poll::Ready(Err(Error::new(ErrorKind::Other, "fracture: Disk quota exceeded (chaos)")));
        }

        if !self.can_write {
            return Poll::Ready(Err(Error::new(ErrorKind::PermissionDenied, "fracture: Not opened for writing")));
        }

        let mut locked = self.entry.lock().unwrap();

        if locked.readonly {
            return Poll::Ready(Err(Error::new(ErrorKind::PermissionDenied, "fracture: Read-only file")));
        }

        let pos = self.cursor as usize;

        if pos > locked.content.len() {
            locked.content.resize(pos, 0);
        }

        let to_write_len = if chaos::should_fail(ChaosOperation::IoPartialWrite) && buf.len() > 1 {
             1.max(buf.len() / 2)
        } else {
             buf.len()
        };

        let end = pos + buf.len();
        if end > locked.content.len() {
            locked.content.resize(end, 0);
        }

        locked.content[pos..end].copy_from_slice(buf);
        self.cursor += buf.len() as u64;

        Poll::Ready(Ok(buf.len()))
    }

    fn poll_flush(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Result<()>> {
        if chaos::should_fail(ChaosOperation::FsSync) {
             return Poll::Ready(Err(Error::new(ErrorKind::Other, "fracture: Flush failed (chaos)")));
        }

        Poll::Ready(Ok(()))
    }

    fn poll_shutdown(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Result<()>> {
        Poll::Ready(Ok(()))
    }
}

impl AsyncSeek for File {
    fn start_seek(mut self: Pin<&mut Self>, position: io::SeekFrom) -> Result<()> {
        if chaos::should_fail(ChaosOperation::FsSeek) {
             return Err(Error::new(ErrorKind::Other, "fracture: Seek failed (chaos)"));
        }

        let len = self.entry.lock().unwrap().content.len() as u64;
        let new_pos = match position {
            SeekFrom::Start(n) => n,
            SeekFrom::End(n) => (len as i64 + n) as u64,
            SeekFrom::Current(n) => (self.cursor as i64 + n) as u64,
        };

        self.cursor = new_pos;

        Ok(())
    }

    fn poll_complete(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Result<u64>> {
        Poll::Ready(Ok(self.cursor))
    }
}

#[derive(Debug)]
pub struct DirEntry {
    pub path: PathBuf,
    pub metadata: Metadata
}

impl DirEntry {
    pub fn path(&self) -> PathBuf {
        self.path.clone()
    }

    pub fn file_name(&self) -> std::ffi::OsString { 
        self.path.file_name().unwrap_or_default().to_os_string() 
    }

    pub async fn metadata(&self) -> Result<Metadata> { 
        Ok(self.metadata.clone()) 
    }

    pub async fn file_type(&self) -> Result<FileType> {
        Ok(self.metadata.file_type)
    }
}

pub struct ReadDir {
    entries: std::vec::IntoIter<DirEntry>,
}

impl ReadDir {
    pub async fn next_entry(&mut self) -> Result<Option<DirEntry>> {
        Ok(self.entries.next())
    }
}

pub async fn read_dir<P: AsRef<Path>>(path: P) -> Result<ReadDir> {
    if chaos::should_fail(ChaosOperation::FsReadDir) {
         return Err(Error::new(ErrorKind::Other, "fracture: ReadDir failed (chaos)"));
    }
    simulate_io_latency().await;

    let path = path.as_ref().to_path_buf();
    let handle = Handle::current();
    let core = handle.core.upgrade().ok_or(Error::new(ErrorKind::Other, "fracture: Runtime dropped"))?;
    let core = core.borrow();

    if let Some(entry) = core.fs.files.get(&path) {
        if !entry.lock().unwrap().file_type.is_dir() {
            return Err(Error::new(ErrorKind::Other, "fracture: Not a directory"));
        }
    } else {
        return Err(Error::new(ErrorKind::NotFound, "fracture: Directory not found"));
    }

    let mut entries = Vec::new();
    for (key, val) in core.fs.files.iter() {
        if key.parent() == Some(&path) {
            let locked = val.lock().unwrap();
            entries.push(DirEntry {
                path: key.clone(),
                metadata: Metadata {
                    len: locked.content.len() as u64,
                    file_type: locked.file_type,
                    readonly: locked.readonly,
                    created: locked.created,
                    modified: locked.modified,
                    accessed: locked.accessed,
                }
            });
        }
    }

    Ok(ReadDir { entries: entries.into_iter() })
}

pub async fn create_dir<P: AsRef<Path>>(path: P) -> Result<()> {
    if chaos::should_fail(ChaosOperation::FsCreateDir) {
         return Err(Error::new(ErrorKind::Other, "fracture: CreateDir failed (chaos)"));
    }
    simulate_io_latency().await;

    let path = path.as_ref().to_path_buf();
    let handle = Handle::current();
    let core_rc = handle.core.upgrade().unwrap();
    let mut core = core_rc.borrow_mut();

    if chaos::should_fail(ChaosOperation::FsPermissionDenied) {
        return Err(Error::new(ErrorKind::PermissionDenied, "fracture: Permission denied (chaos)"));
    }
    
    if core.fs.files.contains_key(&path) {
        return Err(Error::new(ErrorKind::AlreadyExists, "fracture: File exists"));
    }
    if let Some(parent) = path.parent() {
        if !parent.as_os_str().is_empty() && !core.fs.files.contains_key(parent) {
             return Err(Error::new(ErrorKind::NotFound, "fracture: Parent directory not found"));
        }
    }

    let now = core.sys_now();
    let inode = core.fs.assign_inode();
    let entry = Arc::new(Mutex::new(FileEntry::new_dir(now, inode)));
    core.fs.files.insert(path, entry);
    Ok(())
}

pub async fn create_dir_all<P: AsRef<Path>>(path: P) -> Result<()> {
    let path = path.as_ref();
    if let Some(parent) = path.parent() {
        if !parent.as_os_str().is_empty() {
            let components: Vec<_> = path.components().collect();
            let mut current = PathBuf::new();
            for component in components {
                current.push(component);
                create_dir(&current).await;
            }
            return Ok(());
        }
    }

    match create_dir(path).await {
        Ok(_) => Ok(()),
        Err(e) if e.kind() == ErrorKind::AlreadyExists => Ok(()),
        Err(e) => Err(e),
    }
}

pub async fn remove_file<P: AsRef<Path>>(path: P) -> Result<()> {
    if chaos::should_fail(ChaosOperation::FsRemoveFile) {
         return Err(Error::new(ErrorKind::Other, "fracture: RemoveFile failed (chaos)"));
    }
    simulate_io_latency().await;

    let handle = Handle::current();
    let mut core = handle.core.upgrade().unwrap().borrow_mut();
    let path = path.as_ref();
    if let Some(entry) = core.fs.files.get(path) {
        if entry.lock().unwrap().file_type.is_dir() {
            return Err(Error::new(ErrorKind::PermissionDenied, "fracture: Is a directory"));
        }
    } else {
        return Err(Error::new(ErrorKind::NotFound, "fracture: File not found"));
    }
    core.fs.files.remove(path);

    Ok(())
}

pub async fn remove_dir<P: AsRef<Path>>(path: P) -> Result<()> {
    if chaos::should_fail(ChaosOperation::FsRemoveDir) {
         return Err(Error::new(ErrorKind::Other, "fracture: RemoveDir failed (chaos)"));
    }
    simulate_io_latency().await;

    let handle = Handle::current();
    let mut core = handle.core.upgrade().unwrap().borrow_mut();
    let path = path.as_ref();
    match core.fs.files.get(path) {
        Some(entry) => {
            if !entry.lock().unwrap().file_type.is_dir() {
                return Err(Error::new(ErrorKind::Other, "fracture: Not a directory"));
            }
        },
        None => return Err(Error::new(ErrorKind::NotFound, "fracture: Directory not found")),
    }
    for key in core.fs.files.keys() {
        if key.parent() == Some(path) {
            return Err(Error::new(ErrorKind::Other, "fracture: Directory not empty"));
        }
    }
    core.fs.files.remove(path);

    Ok(())
}

pub async fn rename<P: AsRef<Path>, Q: AsRef<Path>>(from: P, to: Q) -> Result<()> {
    if chaos::should_fail(ChaosOperation::FsRename) {
         return Err(Error::new(ErrorKind::Other, "fracture: Rename failed (chaos)"));
    }
    simulate_io_latency().await;

    let handle = Handle::current();
    let mut core = handle.core.upgrade().unwrap().borrow_mut();
    let entry = core.fs.files.remove(from.as_ref()).ok_or(Error::new(ErrorKind::NotFound, "fracture: File not found"))?;
    core.fs.files.insert(to.as_ref().to_path_buf(), entry);

    Ok(())
}

pub async fn copy<P: AsRef<Path>, Q: AsRef<Path>>(from: P, to: Q) -> Result<u64> {
    if chaos::should_fail(ChaosOperation::FsCopy) {
         return Err(Error::new(ErrorKind::Other, "fracture: Copy failed (chaos)"));
    }
    let content = read(&from).await?;
    simulate_io_latency().await;

    write(&to, &content).await?;

    Ok(content.len() as u64)
}

pub async fn hard_link<P: AsRef<Path>, Q: AsRef<Path>>(original: P, link: Q) -> Result<()> {
    if chaos::should_fail(ChaosOperation::FsHardLink) {
         return Err(Error::new(ErrorKind::Other, "fracture: HardLink failed (chaos)"));
    }
    simulate_io_latency().await;

    let handle = Handle::current();
    let mut core = handle.core.upgrade().unwrap().borrow_mut();
    let entry = core.fs.files.get(original.as_ref())
        .ok_or(Error::new(ErrorKind::NotFound, "fracture: Original file not found"))?
        .clone();
    if core.fs.files.contains_key(link.as_ref()) {
        return Err(Error::new(ErrorKind::AlreadyExists, "fracture: Link path exists"));
    }
    core.fs.files.insert(link.as_ref().to_path_buf(), entry);

    Ok(())
}

pub async fn metadata<P: AsRef<Path>>(path: P) -> Result<Metadata> {
    if chaos::should_fail(ChaosOperation::FsMetadata) {
         return Err(Error::new(ErrorKind::Other, "fracture: Metadata failed (chaos)"));
    }
    simulate_io_latency().await;

    let path = path.as_ref().to_path_buf();
    let handle = Handle::current();
    let core_rc = handle.core.upgrade().ok_or(Error::new(ErrorKind::Other, "fracture: Runtime dropped"))?;
    let core = core_rc.borrow();
    let entry = core.fs.files.get(&path).ok_or(Error::new(ErrorKind::NotFound, "fracture: File not found"))?;
    let locked = entry.lock().unwrap();

    Ok(Metadata {
        len: locked.content.len() as u64,
        file_type: locked.file_type,
        readonly: locked.readonly,
        created: locked.created,
        modified: locked.modified,
        accessed: locked.accessed,
    })
}

pub async fn set_permissions<P: AsRef<Path>>(path: P, perm: Permissions) -> Result<()> {
    if chaos::should_fail(ChaosOperation::FsSetPermissions) {
         return Err(Error::new(ErrorKind::Other, "fracture: SetPermissions failed (chaos)"));
    }
    simulate_io_latency().await;

    let path = path.as_ref().to_path_buf();
    let handle = Handle::current();
    let core_rc = handle.core.upgrade().ok_or(Error::new(ErrorKind::Other, "Runtime dropped"))?;
    let core = core_rc.borrow();
    let entry = core.fs.files.get(&path).ok_or(Error::new(ErrorKind::NotFound, "File not found"))?;
    let mut locked = entry.lock().unwrap();
    locked.readonly = perm.readonly();

    Ok(())
}

pub async fn canonicalize<P: AsRef<Path>>(path: P) -> Result<PathBuf> {
    if chaos::should_fail(ChaosOperation::FsCanonical) {
         return Err(Error::new(ErrorKind::Other, "fracture: Canonicalize failed (chaos)"));
    }

    let path = path.as_ref();

    if path.is_absolute() {
        Ok(path.to_path_buf())
    }
    else {
        Ok(Path::new("/").join(path))
    }
}

pub async fn symlink_metadata<P: AsRef<Path>>(path: P) -> Result<Metadata> {
    metadata(path).await // Placeholder, need to fix after implementing symlinks
}

pub async fn read_link<P: AsRef<Path>>(_path: P) -> Result<PathBuf> {
    Err(Error::new(ErrorKind::InvalidInput, "Not a symlink"))
}

pub async fn symlink<P: AsRef<Path>, Q: AsRef<Path>>(_original: P, _link: Q) -> Result<()> {
    if chaos::should_fail(ChaosOperation::FsSymlink) {
         return Err(Error::new(ErrorKind::Other, "fracture: Symlink failed (chaos)"));
    }
    
    Err(Error::new(ErrorKind::Unsupported, "Symlinks not supported in simulation")) // Placeholder
}

pub async fn read<P: AsRef<Path>>(path: P) -> Result<Vec<u8>> {
    let mut file = File::open(path).await?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).await?;

    Ok(buffer)
}

pub async fn write<P: AsRef<Path>, C: AsRef<[u8]>>(path: P, contents: C) -> Result<()> {
    let mut file = File::create(path).await?;

    file.write_all(contents.as_ref()).await
}

pub async fn read_to_string<P: AsRef<Path>>(path: P) -> Result<String> {
    let bytes = read(path).await?;

    String::from_utf8(bytes).map_err(|e| Error::new(ErrorKind::InvalidData, e))
}