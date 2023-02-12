use seq_macro::seq;
use crate::{TokenParseResult, TokenParser};

pub trait Tuple<'a, T, O, W> {
    /// Tries to apply all parsers in the tuple in various orders until all of them succeed
    fn tuple(&mut self, tokens: &'a [W]) -> TokenParseResult<'a, T, O, W>;
}

pub fn tuple<'a, T: Clone, O, W: Into<T>, List: Tuple<'a, T, O, W>>(
    mut l: List,
) -> impl FnMut(&'a [W]) -> TokenParseResult<'a, T, O, W> {
    move |tokens: &'a [W]| l.tuple(tokens)
}

macro_rules! alt_trait_impl {
    ($n:expr) => {
      seq!(N in 0..$n {
        impl<'a, T, #(O~N,)* #(P~N,)* W> Tuple<'a, T, (#(O~N,)*), W> for (#(P~N,)*)
          where
          T: Copy,
          W: 'a + Copy + Into<T>,
          #(
            P~N: TokenParser<'a, T, O~N, W>,
          )*
        {
          fn tuple(&mut self, tokens: &'a [W]) -> TokenParseResult<'a, T, (#(O~N,)*), W> {
            let _num_tokens = tokens.len();
            #(
                let (tokens, result~N) = match self.N.parse(tokens) {
                    Ok((rest, result)) => (rest, result),
                    Err(err) => return Err(err.with_tokens_consumed(_num_tokens - tokens.len()))
                };
            )*
            Ok((tokens, (#(result~N,)*)))
          }
      }
      });
    };
}

macro_rules! tuple_trait {
  ($n: expr) => {
    seq!(N in 0..$n {
        alt_trait_impl!(N);
    });
  }
}

tuple_trait!(12);
