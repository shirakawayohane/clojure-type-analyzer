use seq_macro::seq;
use crate::*;

pub trait Permutation<'a, T, O, W> {
    /// Tries to apply all parsers in the permutation in various orders until all of them succeed
    fn permutation(&mut self, tokens: &'a [W]) -> TokenParseResult<'a, T, O, W>;
}

pub fn permutation<'a, T: Clone, O, W: Into<T>, List: Permutation<'a, T, O, W>>(
    mut l: List,
) -> impl FnMut(&'a [W]) -> TokenParseResult<'a, T, O, W> {
    move |tokens: &'a [W]| l.permutation(tokens)
}

macro_rules! alt_trait_impl {
    ($n:expr) => {
      seq!(N in 0..$n {
        impl<'a, T, #(O~N,)* #(P~N,)* W> Permutation<'a, T, (#(O~N,)*), W> for (#(P~N,)*)
          where
          W: 'a + UnwrapToken<T>,
          #(
            P~N: TokenParser<'a, T, O~N, W>,
          )*
        {
          fn permutation(&mut self, tokens: &'a [W]) -> TokenParseResult<'a, T, (#(O~N,)*), W> {
            let _num_tokens = tokens.len();
            let mut _rest = tokens;
            #(let mut _succeeded_~N = false;)*
            #(let mut _error_of_parser~N: Option<TokenParseError<T>> = None;)*
            #(let mut _result_of_parser~N: Option<O~N> = None;)*
            for i in 0..$n {
                dbg!(i);
                #(
                    if !_succeeded_~N {
                        match self.N.parse(_rest) {
                            Ok((rest_tokens, result)) => {
                                _rest = rest_tokens;
                                _result_of_parser~N = Some(result);
                                _succeeded_~N = true;
                            }
                            Err(err) => {
                                _error_of_parser~N = Some(err);
                            }
                        };
                    }
                )*
            }
            #(
                if !_succeeded_~N {
                    return Err(_error_of_parser~N.unwrap());
                }
            )*
            #(
                let result~N = _result_of_parser~N.unwrap();
            )*
            Ok((_rest, (#(result~N,)*)))
          }
      }
      });
    };
}

macro_rules! permutation_trait {
  ($n: expr) => {
    seq!(N in 0..$n {
        alt_trait_impl!(N);
    });
  }
}

permutation_trait!(16);
