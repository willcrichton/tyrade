pub trait TypeEquals<T> {}
impl<T> TypeEquals<T> for T {}

pub fn assert_type_eq<T1, T2>()
where T1: TypeEquals<T2>
{}
