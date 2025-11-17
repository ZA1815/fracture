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

#[cfg(not(feature = "simulation"))]
pub use tokio::main;
#[cfg(not(feature = "simulation"))]
pub use tokio::test;

#[cfg(feature = "simulation")]
pub use fracture_macros::test;
#[cfg(feature = "simulation")]
pub use fracture_macros::main;

#[cfg(feature = "simulation")]
pub mod prelude {
    pub use crate::{
        net::{TcpListener, TcpStream, UdpSocket},
        time::{sleep, timeout, interval, ChaosInstant},
        fs::{File, read, write, OpenOptions},
        task::{spawn, spawn_blocking, yield_now, JoinHandle},
        io::{AsyncReadExt, AsyncWriteExt, BufReader, BufWriter, ChaosReader, ChaosWriter, ChaosSeeker},
        sync::{Mutex, RwLock, Semaphore, Notify}
    };

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