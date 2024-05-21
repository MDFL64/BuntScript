use std::cell::OnceCell;

// the std method is unstable
pub fn get_or_try_init<T, E>(
    cell: &OnceCell<T>,
    init: impl FnOnce() -> Result<T, E>,
) -> Result<&T, E> {
    if let Some(res) = cell.get() {
        Ok(res)
    } else {
        let old = cell.set(init()?);
        assert!(old.is_ok());
        Ok(cell.get().unwrap())
    }
}
