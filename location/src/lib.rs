use std::ops::Deref;

use nom_locate::LocatedSpan;

pub type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Located<'a, T> {
    pub span: Span<'a>,
    pub value: T,
}

impl<'a, T> Deref for Located<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}
