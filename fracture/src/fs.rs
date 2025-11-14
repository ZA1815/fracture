pub use tokio::fs::*;

#[inline]
pub fn should_fail_fs_operation(_operation: &str) -> bool {
    #[cfg(not(feature = "simulation"))]
    {
        false
    }

    #[cfg(feature = "simulation")]
    {
        crate::chaos::should_fail(_operation)
    }
}