pub use tokio::sync::*;

#[inline]
pub fn should_fail_sync_operation(_operation: &str) -> bool {
    #[cfg(not(feature = "simulation"))]
    {
        false
    }
    
    #[cfg(feature = "simulation")]
    {
        crate::chaos::should_fail(_operation)
    }
}