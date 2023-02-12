use seq_macro::seq;

use crate::{TokenParseError, TokenParseResult, TokenParser};

pub trait Alt<'a, T, O, W> {
    fn alt(&mut self, tokens: &'a [W]) -> TokenParseResult<'a, T, O, W>;
}

pub fn alt<'a, T: Clone, O, W: Into<T>, List: Alt<'a, T, O, W>>(
    mut l: List,
) -> impl FnMut(&'a [W]) -> TokenParseResult<'a, T, O, W> {
    move |tokens: &'a [W]| l.alt(tokens)
}

macro_rules! alt_trait_impl {
    ($n:expr) => {
      seq!(N in 0..$n {
        impl<'a, T, O, #(P~N,)* W> Alt<'a, T, O, W> for (#(P~N,)*)
          where
          T: Copy,
          W: 'a + Copy + Into<T>,
          #(
            P~N: TokenParser<'a, T, O, W>,
          )*
        {
          fn alt(&mut self, _tokens: &'a [W]) -> TokenParseResult<'a, T, O, W> {
            let mut _max_consumed_tokens_len = 0;
            let mut _max_token_consumed_error: Option<TokenParseError<T>> = None;
            #(
              match self.N.parse(_tokens) {
                Err(err) => {
                  if err.tokens_consumed >= _max_consumed_tokens_len {
                    _max_consumed_tokens_len = err.tokens_consumed;
                    _max_token_consumed_error = Some(err);
                  }
                },
                result => return result
              }
            )*

            Err(_max_token_consumed_error.unwrap())
          }
      }
      });
    };
}

macro_rules! alt_trait {
  ($n: expr) => {
    seq!(N in 0..$n {
        alt_trait_impl!(N);
    });
  }
}

alt_trait!(12);
