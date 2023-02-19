use nom_locate::LocatedSpan;
use picktok::UnwrapToken;

pub type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Location {
    pub line: u32,
    pub col: u32,
    pub offset: u32,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct Located<T> {
    pub range: (Location, Location),
    pub value: T,
}

impl<'a, T> std::ops::Deref for Located<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> UnwrapToken<T> for Located<T> {
    fn unwrap_token(&self) -> &T {
        &self.value
    }
}
