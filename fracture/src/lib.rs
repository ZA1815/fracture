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

pub mod chaos;

#[cfg(feature = "simulation")]
pub mod runtime;

pub use tokio::main;
pub use tokio::test;

#[cfg(feature = "simulation")]
pub use fracture_macro::test as fracture_test;

pub mod prelude {
    #[cfg(feature = "simulation")]
    pub use crate::{
        net::{TcpListener, TcpStream, UdpSocket},
        time::{sleep, timeout, interval},
        fs::{File, read, write},
        task::{spawn, spawn_blocking, yield_now},
        io::{AsyncReadExt, AsyncWriteExt}
    };

    pub use crate::chaos::{inject, should_fail};
}
