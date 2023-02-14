use nom_locate::LocatedSpan;

pub type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct Located<'a, T> {
    pub span: Span<'a>,
    pub value: T,
}
