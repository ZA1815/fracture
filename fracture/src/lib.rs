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

#[cfg(not(feature = "simulation"))]
pub use tokio::main;
#[cfg(not(feature = "simulation"))]
pub use tokio::test;

#[cfg(feature = "simulation")]
pub use fracture_macros::test;
#[cfg(feature = "simulation")]
pub use fracture_macros::main;

pub mod prelude {
    #[cfg(feature = "simulation")]
    pub use crate::{
        net::{TcpListener, TcpStream, UdpSocket},
        time::{sleep, timeout, interval},
        fs::{File, read, write},
        task::{spawn, spawn_blocking, yield_now},
        io::{AsyncReadExt, AsyncWriteExt}
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
        scenario::Scenario,
        scenario::ScenarioBuilder
    };

    #[cfg(feature = "simulation")]
    pub use crate::chaos::invariants::{
        Invariant,
        InvariantChecker,
        CommonInvariants,
        register as register_invariant,
        check_all as check_invariants
    };

    pub use std::time::Duration;
}
