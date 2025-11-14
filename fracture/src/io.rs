pub use tokio::io::*;

#[inline]
pub fn should_fail_io_operation(_operation: &str) -> bool {
    #[cfg(not(feature = "simulation"))]
    {
        false
    }

    #[cfg(feature = "simulation")]
    {
        crate::chaos::should_fail(_operation)
    }
}