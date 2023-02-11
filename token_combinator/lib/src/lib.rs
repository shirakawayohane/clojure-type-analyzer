use std::ops::Deref;

pub use token_combinator_macros::ParseToken;

// T stands for Token
// K stands for Kind
// O stands for Output
// W stands for Wrapper

pub trait GetKind<K> {
    fn get_kind(&self) -> K;
}

pub enum TokenParseError<'a, T> {
    Expects { expects: String, found: &'a T },
    ExpectsOneOneOf { expects: Vec<String>, found: &'a T },
}

pub type TokenParseResult<'a, T, O, W = T> = Result<(&'a [W], O), TokenParseError<'a, T>>;

// pub fn symbol<T, K, W>(kind: K) -> impl Fn(&[W]) -> TokenParseResult<T, K, &W, W>
// where
//     T: GetKind<K>,
//     K: Copy + PartialEq,
//     W: Deref<Target = T>,
// {
//     move |tokens: &[W]| {
//         let token: &T = &tokens[0];
//         if token.get_kind() == kind {
//             Ok((&tokens[1..], &tokens[0]))
//         } else {
//             Err(TokenParseError::Expects {
//                 expects: kind,
//                 found: token,
//             })
//         }
//     }
// }

// pub fn one_of<'a, T, K>(kinds: &'a [K]) -> impl Fn(&'a [T]) -> TokenParseResult<T, K, &'a T>
// where
//     T: GetKind<K>,
//     K: Copy + PartialEq,
// {
//     move |tokens: &'a [T]| {
//         let token = &tokens[0];
//         for kind in kinds {
//             if token.get_kind() == *kind {
//                 return Ok((&tokens[1..], token));
//             }
//         }
//         Err(TokenParseError::ExpectsOneOneOf {
//             expects: Vec::from(kinds),
//             found: token,
//         })
//     }
// }
