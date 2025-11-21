use std::collections::HashMap;
use std::ffi::{OsStr, OsString};
use std::io::{self, Result};
use std::path::{Path, PathBuf};
use std::pin::Pin;
use std::process::{ExitStatus as StdExitStatus, Stdio as StdStdio};
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::{Arc, Mutex as StdMutex};
use std::task::{Context, Poll};
use std::time::Duration;
use crate::io::{AsyncRead, AsyncWrite, ReadBuf};
use parking_lot::RwLock;

use crate::chaos::{self, ChaosOperation};
use crate::time::sleep;

static PROCESS_REGISTRY: std::sync::LazyLock<ProcessRegistry> = std::sync::LazyLock::new(ProcessRegistry::new);

static NEXT_PID: AtomicU32 = AtomicU32::new(1000);

struct ProcessRegistry {
    processes: Arc<RwLock<HashMap<u32, Arc<StdMutex<ProcessState>>>>>
}

impl ProcessRegistry {
    fn new() -> Self {
        Self {
            processes: Arc::new(RwLock::new(HashMap::new()))
        }
    }

    fn register(&self, state: ProcessState) -> u32 {
        let pid = NEXT_PID.fetch_add(1, Ordering::SeqCst);
        let mut processes = self.processes.write();
        processes.insert(pid, Arc::new(StdMutex::new(state)));

        pid
    }

    fn get(&self, pid: u32) -> Option<Arc<StdMutex<ProcessState>>> {
        let processes = self.processes.read();
        processes.get(&pid).cloned()
    }

    fn remove(&self, pid: u32) {
        let mut processes = self.processes.write();
        processes.remove(&pid);
    }

    fn list(&self) -> Vec<u32> {
        let processes = self.processes.read();
        processes.keys().copied().collect()
    }

    fn count(&self) -> usize {
        self.processes.read().len()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ProcessStatus {
    Running,
    Sleeping,
    Stopped,
    Zombie,
    Dead
}

struct ProcessState {
    pid: u32,
    status: ProcessStatus,
    exit_code: Option<i32>,
    start_time: crate::time::ChaosInstant,
    command: String,
    args: Vec<String>,
    env: HashMap<String, String>,
    cwd: PathBuf,
    stdout_data: Vec<u8>,
    stderr_data: Vec<u8>,
    stdin_data: Vec<u8>,
    config: ProcessConfig
}

#[derive(Clone)]
struct ProcessConfig {
    stdin_piped: bool,
    stdout_piped: bool,
    stderr_piped: bool,
}

impl ProcessState {
    fn new(command: String, args: Vec<String>, env: HashMap<String, String>, cwd: PathBuf, config: ProcessConfig) -> Self {
        Self {
            pid: 0,
            status: ProcessStatus::Running,
            exit_code: None,
            start_time: crate::time::ChaosInstant::now(),
            command,
            args,
            env,
            cwd,
            stdout_data: Vec::new(),
            stderr_data: Vec::new(),
            stdin_data: Vec::new(),
            config
        }
    }

    fn simulate_execution(&mut self) -> ExitStatus {
        let command_hash = self.command.bytes()
            .fold(0u32, |acc, b| acc.wrapping_mul(31).wrapping_add(b as u32));

        if self.command.contains("fail") {
            self.exit_code = Some(1);
            self.status = ProcessStatus::Dead;
            ExitStatus::from_code(1)
        }
        else if self.command.contains("timeout") {
            self.status = ProcessStatus::Sleeping;
            ExitStatus::from_code(124)
        }
        else if self.command.contains("segfault") {
            self.status = ProcessStatus::Dead;
            ExitStatus::from_signal(11)
        }
        else {
            self.exit_code = Some(0);
            self.status = ProcessStatus::Dead;

            let output = format!("Output from {}\n", self.command);
            self.stdout_data.extend_from_slice(output.as_bytes());

            ExitStatus::from_code(0)
        }
    }

    fn runtime(&self) -> Duration {
        self.start_time.elapsed()
    }
}

pub struct Command {
    program: OsString,
    args: Vec<OsString>,
    env: HashMap<OsString, OsString>,
    env_clear: bool,
    cwd: Option<PathBuf>,
    stdin: Stdio,
    stdout: Stdio,
    stderr: Stdio,
    uid: Option<u32>,
    gid: Option<u32>,
    kill_on_drop: bool,
}

impl Command {
    pub fn new<S: AsRef<OsStr>>(program: S) -> Self {
        Self {
            program: program.as_ref().to_owned(),
            args: Vec::new(),
            env: HashMap::new(),
            env_clear: false,
            cwd: None,
            stdin: Stdio::inherit(),
            stdout: Stdio::inherit(),
            stderr: Stdio::inherit(),
            uid: None,
            gid: None,
            kill_on_drop: false
        }
    }

    pub fn arg<S: AsRef<OsStr>>(&mut self, arg: S) -> &mut Self {
        self.args.push(arg.as_ref().to_owned());
        self
    }

    pub fn args<I, S>(&mut self, args: I) -> &mut Self
    where I: IntoIterator<Item = S>, S: AsRef<OsStr> {
        self.args.extend(args.into_iter().map(|s| s.as_ref().to_owned()));
        self
    }

    pub fn env<K, V>(&mut self, key: K, val: V) -> &mut Self
    where K: AsRef<OsStr>, V: AsRef<OsStr> {
        self.env.insert(key.as_ref().to_owned(), val.as_ref().to_owned());
        self
    }

    pub fn envs<I, K, V>(&mut self, vars: I) -> &mut Self
    where I: IntoIterator<Item = (K, V)>, K: AsRef<OsStr>, V: AsRef<OsStr> {
        self.env.extend(vars.into_iter().map(|(k, v)| (k.as_ref().to_owned(), v.as_ref().to_owned())));
        self
    }

    pub fn env_remove<K: AsRef<OsStr>>(&mut self, key: K) -> &mut Self {
        self.env.remove(key.as_ref());
        self
    }

    pub fn env_clear(&mut self) -> &mut Self {
        self.env_clear = true;
        self.env.clear();
        self
    }

    pub fn current_dir<P: AsRef<Path>>(&mut self, dir: P) -> &mut Self {
        self.cwd = Some(dir.as_ref().to_owned());
        self
    }

    pub fn stdin<T: Into<Stdio>>(&mut self, cfg: T) -> &mut Self {
        self.stdin = cfg.into();
        self
    }

    pub fn stdout<T: Into<Stdio>>(&mut self, cfg: T) -> &mut Self {
        self.stdout = cfg.into();
        self
    }

    pub fn stderr<T: Into<Stdio>>(&mut self, cfg: T) -> &mut Self {
        self.stderr = cfg.into();
        self
    }

    pub fn uid(&mut self, id: u32) -> &mut Self {
        self.uid = Some(id);
        self
    }

    pub fn gid(&mut self, id: u32) -> &mut Self {
        self.gid = Some(id);
        self
    }

    pub fn kill_on_drop(&mut self, kill: bool) -> &mut Self {
        self.kill_on_drop = kill;
        self
    }

    pub fn spawn(&mut self) -> Result<Child> {
        if chaos::should_fail(ChaosOperation::ProcessSpawn) {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "fracture: Process spawn failed (chaos)",
            ));
        }

        let mut env = if self.env_clear {
            HashMap::new()
        }
        else {
            std::env::vars().map(|(k, v)| (k, v)).collect()
        };

        for (k, v) in &self.env {
            env.insert(k.to_string_lossy().to_string(), v.to_string_lossy().to_string());
        }

        let config = ProcessConfig {
            stdin_piped: matches!(self.stdin, Stdio::Piped),
            stdout_piped: matches!(self.stdout, Stdio::Piped),
            stderr_piped: matches!(self.stderr, Stdio::Piped),
        };

        let state = ProcessState::new(
            self.program.to_string_lossy().to_string(),
            self.args.iter().map(|s| s.to_string_lossy().to_string()).collect(),
            env,
            self.cwd.clone().unwrap_or_else(|| std::env::current_dir().unwrap()),
            config
        );

        let pid = PROCESS_REGISTRY.register(state);

        Ok(Child {
            pid,
            stdin: if matches!(self.stdin, Stdio::Piped) {
                Some(ChildStdin::new(pid))
            }
            else {
                None
            },
            stdout: if matches!(self.stdout, Stdio::Piped) {
                Some(ChildStdout::new(pid))
            }
            else {
                None
            },
            stderr: if matches!(self.stderr, Stdio::Piped) {
                Some(ChildStderr::new(pid))
            }
            else {
                None
            },
            kill_on_drop: self.kill_on_drop
        })
    }

    pub async fn status(&mut self) -> Result<ExitStatus> {
        let child = self.spawn()?;
        child.wait_with_output().await.map(|output| output.status)
    }

    pub async fn output(&mut self) -> Result<Output> {
        let child = self.spawn()?;
        child.wait_with_output().await
    }
}

pub struct Child {
    pid: u32,
    pub stdin: Option<ChildStdin>,
    pub stdout: Option<ChildStdout>,
    pub stderr: Option<ChildStderr>,
    kill_on_drop: bool,
}

impl Child {
    pub fn id(&self) -> Option<u32> {
        Some(self.pid)
    }

    pub async fn wait(&mut self) -> Result<ExitStatus> {
        if chaos::should_fail(ChaosOperation::ProcessWait) {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "fracture: Wait failed (chaos)",
            ));
        }

        if chaos::should_fail(ChaosOperation::ProcessTimeout) {
            crate::time::sleep(Duration::from_secs(60)).await;
        }

        sleep(Duration::from_millis(10)).await;

        let process = PROCESS_REGISTRY.get(self.pid)
            .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "fracture: Process not found"))?;

        let mut state = process.lock().unwrap();
        let exit_status = state.simulate_execution();

        if chaos::should_fail(ChaosOperation::ProcessZombie) {
            state.status = ProcessStatus::Zombie;
        }

        Ok(exit_status)
    }

    pub async fn wait_with_output(mut self) -> Result<Output> {
        let status = self.wait().await?;

        let process = PROCESS_REGISTRY.get(self.pid)
            .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "fracture: Process not found"))?;

        let state = process.lock().unwrap();

        Ok(Output {
            status,
            stdout: state.stdout_data.clone(),
            stderr: state.stderr_data.clone()
        })
    }

    pub fn try_wait(&mut self) -> Result<Option<ExitStatus>> {
        let process = PROCESS_REGISTRY.get(self.pid)
            .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "fracture: Process not found"))?;

        let state = process.lock().unwrap();

        if state.status == ProcessStatus::Dead {
            Ok(Some(ExitStatus::from_code(state.exit_code.unwrap_or(0))))
        }
        else {
            Ok(None)
        }
    }

    pub fn start_kill(&mut self) -> Result<()> {
        if chaos::should_fail(ChaosOperation::ProcessKill) {
            return Err(io::Error::new(
                io::ErrorKind::PermissionDenied,
                "fracture: Kill failed (chaos)",
            ));
        }

        let process = PROCESS_REGISTRY.get(self.pid)
            .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "fracture: Process not found"))?;

        let mut state = process.lock().unwrap();
        state.status = ProcessStatus::Dead;
        state.exit_code = Some(-9);

        Ok(())
    }

    pub async fn kill(&mut self) -> Result<()> {
        self.start_kill()
    }
}

impl Drop for Child {
    fn drop(&mut self) {
        if self.kill_on_drop {
            let _ = self.start_kill();
        }

        if chaos::should_fail(ChaosOperation::ProcessOrphan) {
            return;
        }

        PROCESS_REGISTRY.remove(self.pid);
    }
}

pub struct ChildStdin {
    pid: u32,
    buffer: Vec<u8>
}

impl ChildStdin {
    fn new(pid: u32) -> Self {
        Self {
            pid,
            buffer: Vec::new()
        }
    }
}

impl AsyncWrite for ChildStdin {
    fn poll_write(
            mut self: Pin<&mut Self>,
            cx: &mut Context<'_>,
            buf: &[u8],
        ) -> Poll<std::result::Result<usize, io::Error>> {
        if chaos::should_fail(ChaosOperation::ProcessStdin) {
            return Poll::Ready(Err(io::Error::new(
                io::ErrorKind::BrokenPipe,
                "fracture: Stdin write failed (chaos)",
            )));
        }

        self.buffer.extend_from_slice(buf);

        if let Some(process) = PROCESS_REGISTRY.get(self.pid) {
            let mut state = process.lock().unwrap();
            if state.config.stdin_piped {
                state.stdin_data.extend_from_slice(buf);
            }
        }

        Poll::Ready(Ok(buf.len()))
    }

    fn poll_flush(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<std::result::Result<(), io::Error>> {
        Poll::Ready(Ok(()))
    }

    fn poll_shutdown(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<std::result::Result<(), io::Error>> {
        Poll::Ready(Ok(()))
    }
}

pub struct ChildStdout {
    pid: u32,
    position: usize
}

impl ChildStdout {
    fn new(pid: u32) -> Self {
        Self {
            pid,
            position: 0,
        }
    }
}

impl AsyncRead for ChildStdout {
    fn poll_read(
            mut self: Pin<&mut Self>,
            cx: &mut Context<'_>,
            buf: &mut ReadBuf<'_>,
        ) -> Poll<io::Result<()>> {
        if chaos::should_fail(ChaosOperation::ProcessStdout) {
            return Poll::Ready(Err(io::Error::new(
                io::ErrorKind::BrokenPipe,
                "fracture: Stdout read failed (chaos)",
            )));
        }

        let process = match PROCESS_REGISTRY.get(self.pid) {
            Some(p) => p,
            None => return Poll::Ready(Ok(()))
        };

        let state = process.lock().unwrap();
        if !state.config.stdout_piped {
            return Poll::Ready(Ok(()));
        }

        let available = &state.stdout_data[self.position..];

        if available.is_empty() {
            if state.status == ProcessStatus::Dead {
                return Poll::Ready(Ok(()));
            }
            else {
                cx.waker().wake_by_ref();
                return Poll::Pending;
            }
        }

        let to_read = available.len().min(buf.remaining());
        buf.put_slice(&available[..to_read]);
        self.position += to_read;

        Poll::Ready(Ok(()))
    }
}

pub struct ChildStderr {
    pid: u32,
    position: usize
}

impl ChildStderr {
    fn new(pid: u32) -> Self {
        Self {
            pid,
            position: 0,
        }
    }
}

impl AsyncRead for ChildStderr {
    fn poll_read(
            mut self: Pin<&mut Self>,
            cx: &mut Context<'_>,
            buf: &mut ReadBuf<'_>,
        ) -> Poll<io::Result<()>> {
        if chaos::should_fail(ChaosOperation::ProcessStderr) {
            return Poll::Ready(Err(io::Error::new(
                io::ErrorKind::BrokenPipe,
                "fracture: Stderr read failed (chaos)",
            )));
        }

        let process = match PROCESS_REGISTRY.get(self.pid) {
            Some(p) => p,
            None => return Poll::Ready(Ok(()))
        };

        let state = process.lock().unwrap();
        if !state.config.stderr_piped {
            return Poll::Ready(Ok(()));
        }

        let available = &state.stderr_data[self.position..];

        if available.is_empty() {
            if state.status == ProcessStatus::Dead {
                return Poll::Ready(Ok(()));
            }
            else {
                cx.waker().wake_by_ref();
                return Poll::Pending;
            }
        }

        let to_read = available.len().min(buf.remaining());
        buf.put_slice(&available[..to_read]);
        self.position += to_read;

        Poll::Ready(Ok(()))
    }
}

#[derive(Debug, Clone)]
pub enum Stdio {
    Inherit,
    Piped,
    Null
}

impl Stdio {
    pub fn piped() -> Self {
        Self::Piped
    }

    pub fn inherit() -> Self {
        Self::Inherit
    }

    pub fn null() -> Self {
        Self::Null
    }
}

impl From<StdStdio> for Stdio {
    fn from(value: StdStdio) -> Self {
        // Simplified, change later
        Self::Inherit
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ExitStatus {
    code: Option<i32>,
    signal: Option<i32>
}

impl ExitStatus {
    fn from_code(code: i32) -> Self {
        Self {
            code: Some(code),
            signal: None
        }
    }

    fn from_signal(signal: i32) -> Self {
        Self {
            code: None,
            signal: Some(signal)
        }
    }

    pub fn success(&self) -> bool {
        self.code == Some(0)
    }

    pub fn code(&self) -> Option<i32> {
        self.code
    }

    pub fn signal(&self) -> Option<i32> {
        self.signal
    }
}

impl From<StdExitStatus> for ExitStatus {
    fn from(status: StdExitStatus) -> Self {
        #[cfg(unix)]
        let signal = std::os::unix::process::ExitStatusExt::signal(&status);
        #[cfg(not(unix))]
        let signal = None;

        Self {
            code: status.code(),
            signal,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Output {
    pub status: ExitStatus,
    pub stdout: Vec<u8>,
    pub stderr: Vec<u8>
}

pub fn id() -> u32 {
    std::process::id()
}

pub fn abort() -> ! {
    std::process::abort()
}

pub fn exit(code: i32) -> ! {
    std::process::exit(code)
}

pub struct ProcessMonitor;

impl ProcessMonitor {
    pub fn list_processes() -> Vec<u32> {
        PROCESS_REGISTRY.list()
    }

    pub fn count() -> usize {
        PROCESS_REGISTRY.count()
    }

    pub fn get_status(pid: u32) -> Option<ProcessStatus> {
        PROCESS_REGISTRY.get(pid).map(|p| p.lock().unwrap().status)
    }

    pub fn get_runtime(pid: u32) -> Option<Duration> {
        PROCESS_REGISTRY.get(pid).map(|p| p.lock().unwrap().runtime())
    }
}