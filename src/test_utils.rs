use crate::tyrant_bool::*;

pub fn assert_type_eq<T1, T2>()
where T1: ComputeTypeEquals<T2, Output=TTrue>
{}
