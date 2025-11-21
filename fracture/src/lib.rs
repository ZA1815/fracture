#[cfg(not(feature = "simulation"))]
pub use tokio::*;

#[cfg(feature = "simulation")]
pub mod net;
#[cfg(feature = "simulation")]
pub mod time;
#[cfg(feature = "simulation")]
pub mod fs;
#[cfg(feature = "simulation")]
pub mod task;
#[cfg(feature = "simulation")]
pub mod sync;
#[cfg(feature = "simulation")]
pub mod io;
#[cfg(feature = "simulation")]
pub mod chaos;
#[cfg(feature = "simulation")]
pub mod runtime;
#[cfg(feature = "simulation")]
pub mod process;
#[cfg(feature = "simulation")]
pub mod signal;
#[cfg(feature = "simulation")]
pub mod stream;

#[cfg(not(feature = "simulation"))]
pub use tokio::main;
#[cfg(not(feature = "simulation"))]
pub use tokio::test;

#[cfg(feature = "simulation")]
pub use fracture_macros::{test, main, select, join, try_join, pin, task_local};
#[cfg(not(feature = "simulation"))]
pub use tokio::{main, test, select, join, try_join, pin};

#[cfg(feature = "simulation")]
pub mod prelude {
    pub use crate::net::{TcpListener, TcpStream, UdpSocket};
    pub use crate::time::{sleep, timeout, interval, ChaosInstant as Instant};
    pub use crate::fs::{File, OpenOptions};
    pub use crate::task::{spawn, spawn_blocking, yield_now, JoinHandle, unconstrained};
    pub use crate::io::{AsyncReadExt, AsyncWriteExt, BufReader, BufWriter};
    pub use crate::sync::{Mutex, RwLock, Semaphore, Notify, mpsc, oneshot, watch, broadcast, Barrier};
    pub use crate::{select, join, try_join, pin};

    pub use crate::chaos::{
        inject,
        should_fail,
        partition,
        partition_oneway,
        heal_partition,
        set_packet_loss,
        set_delay,
        set_reordering,
        clear,
        ChaosOperation,
        scenario::Scenario,
        scenario::ScenarioBuilder,
        invariants::{
            Invariant,
            InvariantChecker,
            CommonInvariants,
            register as register_invariant,
            check_all as check_invariants
        }
    };

    pub use crate::process::{Command, Child, Stdio, ExitStatus};
    pub use crate::signal::{ctrl_c, SignalKind};
    
    #[cfg(unix)]
    pub use crate::signal::unix;
    
    #[cfg(windows)]
    pub use crate::signal::windows;
}