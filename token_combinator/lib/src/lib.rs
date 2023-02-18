mod alt;
mod permutation;
mod tuple;

pub use alt::alt;
pub use permutation::permutation;
pub use token_combinator_macros::TokenParser;
pub use tuple::tuple;

// T stands for Token
// K stands for Kind
// O stands for Output
// W stands for Wrapper

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenParseErrorKind<T> {
    Expects { expects: &'static str, found: T },
    NotEnoughToken,
    Fail,
    Context(&'static str),
    Other(String),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TokenParseError<T> {
    pub errors: Vec<TokenParseErrorKind<T>>,
    pub tokens_consumed: usize,
}

impl<T> TokenParseError<T> {
    pub fn with_tokens_consumed(self, tokens_consumed: usize) -> Self {
        TokenParseError {
            errors: self.errors,
            tokens_consumed,
        }
    }
    pub fn with_error_appended(self, kind: TokenParseErrorKind<T>) -> Self {
        let mut errors = self.errors;
        errors.push(kind);
        TokenParseError {
            errors,
            tokens_consumed: self.tokens_consumed,
        }
    }
}

pub type TokenParseResult<'a, T, O, W = T> = Result<(&'a [W], O), TokenParseError<T>>;

pub trait TokenParser<'a, T, O, W: UnwrapToken<T>> {
    fn parse(&mut self, tokens: &'a [W]) -> Result<(&'a [W], O), TokenParseError<T>>;
}

impl<'a, T, O, W, F> TokenParser<'a, T, O, W> for F
where
    W: 'a + UnwrapToken<T>,
    F: FnMut(&'a [W]) -> Result<(&'a [W], O), TokenParseError<T>>,
{
    fn parse(&mut self, tokens: &'a [W]) -> Result<(&'a [W], O), TokenParseError<T>> {
        self(tokens)
    }
}

pub trait UnwrapToken<T> {
    fn unwrap_token(&self) -> &T;
}

impl<T> UnwrapToken<T> for T {
    fn unwrap_token(&self) -> &T {
        self
    }
}

pub fn context<'a, T, O, W: 'a>(
    context: &'static str,
    mut parser: impl FnMut(&'a [W]) -> TokenParseResult<'a, T, O, W>,
) -> impl FnMut(&'a [W]) -> TokenParseResult<'a, T, O, W> {
    move |tokens: &'a [W]| {
        match parser(tokens) {
            Err(err) => Err(err.with_error_appended(TokenParseErrorKind::Context(context))),
            ok => ok,
        }
    }
}

pub fn many1<'a, T, O, W>(
    mut parser: impl TokenParser<'a, T, O, W>,
) -> impl FnMut(&'a [W]) -> TokenParseResult<'a, T, Vec<O>, W>
where
    W: 'a + UnwrapToken<T>,
{
    move |tokens: &'a [W]| {
        let mut vec = Vec::new();
        let mut rest = tokens;
        let mut succeeded_at_least_once = false;
        while rest.len() > 0 {
            match parser.parse(rest) {
                Ok((rest_tokens, item)) => {
                    rest = rest_tokens;
                    succeeded_at_least_once = true;
                    vec.push(item);
                    continue;
                }
                Err(err) => {
                    if succeeded_at_least_once {
                        break;
                    } else {
                        return Err(err);
                    }
                }
            }
        }
        Ok((rest, vec))
    }
}

pub fn many0<'a, T, O, W>(
    mut parser: impl TokenParser<'a, T, O, W>,
) -> impl FnMut(&'a [W]) -> TokenParseResult<'a, T, Vec<O>, W>
where
    W: 'a + UnwrapToken<T>,
{
    move |tokens: &'a [W]| {
        let mut vec = Vec::new();
        let mut rest = tokens;
        while rest.len() > 0 {
            match parser.parse(rest) {
                Ok((rest_tokens, item)) => {
                    rest = rest_tokens;
                    vec.push(item);
                    continue;
                }
                _ => break,
            }
        }
        Ok((rest, vec))
    }
}

pub fn many0_until_end<'a, T, O, W>(
    mut parser: impl TokenParser<'a, T, O, W>,
) -> impl FnMut(&'a [W]) -> TokenParseResult<'a, T, Vec<O>, W>
where
    W: 'a + UnwrapToken<T> + std::fmt::Debug,
{
    move |tokens: &'a [W]| {
        let mut vec = Vec::new();
        let mut rest = tokens;
        while rest.len() > 0 {
            match parser.parse(rest) {
                Ok((rest_tokens, item)) => {
                    rest = rest_tokens;
                    vec.push(item);

                    if rest_tokens.is_empty() {
                        return Ok((rest, vec));
                    }
                    
                    continue;
                }
                Err(err) => return Err(err),
            }
        }
        Ok((rest, vec))
    }
}

pub fn opt<'a, T, O, W>(
    mut parser: impl TokenParser<'a, T, O, W>,
) -> impl FnMut(&'a [W]) -> TokenParseResult<'a, T, Option<O>, W>
where
    W: UnwrapToken<T>,
{
    move |tokens: &'a [W]| match parser.parse(tokens) {
        Ok((rest, output)) => Ok((rest, Some(output))),
        Err(_) => Ok((tokens, None)),
    }
}

pub fn delimited<'a, T, O1, O2, O3, W: 'a>(
    mut l: impl FnMut(&'a [W]) -> TokenParseResult<'a, T, O1, W>,
    mut main: impl FnMut(&'a [W]) -> TokenParseResult<'a, T, O2, W>,
    mut r: impl FnMut(&'a [W]) -> TokenParseResult<'a, T, O3, W>,
) -> impl FnMut(&'a [W]) -> TokenParseResult<'a, T, O2, W> {
    move |tokens: &'a [W]| {
        let (rest, _) = l(tokens)?;
        let (rest, result) = main(rest)?;
        let (rest, _) = r(rest)?;

        Ok((rest, result))
    }
}

pub fn preceded<'a, T, O1, O2, W: 'a>(
    mut first: impl FnMut(&'a [W]) -> TokenParseResult<'a, T, O1, W>,
    mut second: impl FnMut(&'a [W]) -> TokenParseResult<'a, T, O2, W>,
) -> impl FnMut(&'a [W]) -> TokenParseResult<'a, T, O2, W> {
    move |tokens: &'a [W]| {
        let (rest, _) = first(tokens)?;
        let (rest, result) = second(rest)?;

        Ok((rest, result))
    }
}

pub fn terminated<'a, T, O1, O2, W: 'a>(
    mut first: impl FnMut(&'a [W]) -> TokenParseResult<'a, T, O1, W>,
    mut second: impl FnMut(&'a [W]) -> TokenParseResult<'a, T, O2, W>,
) -> impl FnMut(&'a [W]) -> TokenParseResult<'a, T, O1, W> {
    move |tokens: &'a [W]| {
        let (rest, result) = first(tokens)?;
        let (rest, _) = second(rest)?;

        Ok((rest, result))
    }
}

pub fn separated_list0<'a, T, O, OSep, W: 'a>(
    mut separator_parser: impl FnMut(&'a [W]) -> TokenParseResult<'a, T, OSep, W>,
    mut item_parser: impl FnMut(&'a [W]) -> TokenParseResult<'a, T, O, W>,
) -> impl FnMut(&'a [W]) -> TokenParseResult<'a, T, Vec<O>, W> {
    move |tokens: &'a [W]| {
        let mut items = Vec::new();
        let mut rest = tokens;
        while !tokens.is_empty() {
            match item_parser(rest) {
                Ok((rest_tokens, item)) => {
                    rest = rest_tokens;
                    items.push(item);
                }
                Err(_) => return Ok((rest, items)),
            }
            if rest.is_empty() {
                return Ok((rest, items));
            }
            match separator_parser(rest) {
                Ok((rest_tokens, _)) => {
                    rest = rest_tokens;
                }
                Err(_) => return Ok((rest, items)),
            }
        }
        Ok((&[], Vec::new()))
    }
}

pub fn separated_list1<'a, T, O, OSep, W: 'a>(
    mut separator_parser: impl FnMut(&'a [W]) -> TokenParseResult<'a, T, OSep, W>,
    mut item_parser: impl FnMut(&'a [W]) -> TokenParseResult<'a, T, O, W>,
) -> impl FnMut(&'a [W]) -> TokenParseResult<'a, T, Vec<O>, W> {
    move |tokens: &'a [W]| {
        let num_tokens = tokens.len();
        let mut items = Vec::new();
        let mut rest = tokens;
        while !tokens.is_empty() {
            match item_parser(rest) {
                Ok((rest_tokens, item)) => {
                    rest = rest_tokens;
                    items.push(item);
                }
                Err(err) => {
                    if items.len() > 0 {
                        return Ok((rest, items));
                    } else {
                        return Err(err.with_tokens_consumed(num_tokens - rest.len()));
                    }
                }
            }
            if rest.is_empty() {
                return Ok((rest, items));
            }
            match separator_parser(rest) {
                Ok((rest_tokens, _)) => {
                    rest = rest_tokens;
                }
                Err(_) => return Ok((rest, items)),
            }
        }
        // If tokens is empty, returns error.
        return Err(TokenParseError {
            errors: vec![TokenParseErrorKind::NotEnoughToken],
            tokens_consumed: 0,
        });
    }
}

pub fn map<'a, T, OParser, O, W: 'a>(
    mut parser: impl FnMut(&'a [W]) -> TokenParseResult<'a, T, OParser, W>,
    mut mapper: impl FnMut(OParser) -> O,
) -> impl FnMut(&'a [W]) -> TokenParseResult<'a, T, O, W> {
    move |tokens: &'a [W]| {
        let (rest, result) = parser(tokens)?;
        Ok((rest, mapper(result)))
    }
}

pub fn map_res<'a, T, O1, O2, W: 'a>(
    mut parser: impl FnMut(&'a [W]) -> TokenParseResult<'a, T, O1, W>,
    mut mapper: impl FnMut(TokenParseResult<'a, T, O1, W>) -> TokenParseResult<'a, T, O2, W>,
) -> impl FnMut(&'a [W]) -> TokenParseResult<'a, T, O2, W> {
    move |tokens: &'a [W]| mapper(parser(tokens))
}

pub fn success<'a, T, W: 'a>(tokens: &'a [W]) -> TokenParseResult<'a, T, &W, W> {
    if tokens.is_empty() {
        return Err(TokenParseError {
            errors: vec![TokenParseErrorKind::NotEnoughToken],
            tokens_consumed: 0,
        });
    }
    Ok((&tokens[1..], &tokens[0]))
}

pub fn fail<'a, T, W: 'a>(tokens: &'a [W]) -> TokenParseResult<'a, T, &W, W> {
    if tokens.is_empty() {
        return Err(TokenParseError {
            errors: vec![TokenParseErrorKind::NotEnoughToken],
            tokens_consumed: 0,
        });
    }
    Err(TokenParseError {
        errors: vec![TokenParseErrorKind::Fail],
        tokens_consumed: 0,
    })
}
