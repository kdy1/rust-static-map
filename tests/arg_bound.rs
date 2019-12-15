use static_map::StaticMap;

#[derive(StaticMap)]
pub struct Record<T: Default> {
    pub a: T,
    pub b: T,
    pub c: T,
}
