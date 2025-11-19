use std::task::{RawWaker, RawWakerVTable, Waker};
use super::task::TaskId;
use super::Handle;

static VTABLE: RawWakerVTable = RawWakerVTable::new(clone, wake, wake_by_ref, drop);

unsafe fn clone(data: *const ()) -> RawWaker {
    RawWaker::new(data, &VTABLE)
}

unsafe fn wake(data: *const ()) {
    let id = TaskId(data as usize);
    wake_task(id);
}

unsafe fn wake_by_ref(data: *const ()) {
    let id = TaskId(data as usize);
    wake_task(id);
}

unsafe fn drop(_data: *const ()) {
    // No-op: TaskId is just an integer
}

fn wake_task(id: TaskId) {
    Handle::current().core.upgrade().map(|core_rc| {
        let mut core = core_rc.borrow_mut();

        if core.tasks.contains(id.0) {
            core.ready_queue.push_back(id);
        }
    });
}

pub(crate) fn make_waker(id: TaskId) -> Waker {
    let raw = RawWaker::new(id.0 as *const (), &VTABLE);
    unsafe { Waker::from_raw(raw) }
}