use crate::{Config, Specs};
use anyhow::{anyhow, Result};
use proc_macro2::TokenStream;
use quote::quote;
use std::str::FromStr;

impl Specs<'_> {
  pub fn generate_tokens_mod(&self, config: &Config<'_>) -> Result<TokenStream> {
    let error = TokenStream::from_str(config.error).map_err(|err| anyhow!("{err}"))?;

    let enum_ = self.generate_tokens_enum();
    let fmt = self.generate_tokens_fmt();
    let parse = self.generate_tokens_parse(error);
    Ok(quote! {
      #![allow(dead_code)]
      #enum_
      #fmt
      #parse
    })
  }

  fn generate_tokens_enum(&self) -> TokenStream {
    let dynamic_tokens = self.dynamic_tokens.iter().map(|token| {
      let name = token.0.as_type();
      let pattern = token.1;
      quote! {
        #[regex(#pattern, super::#name::new)]
        #name(super::#name)
      }
    });

    let static_tokens = self.static_tokens.iter().map(|(name, keyword)| {
      let name = name.as_type();
      quote! {
        #[token(#keyword)]
        #name
      }
    });

    quote! {
      #[derive(Clone, Debug, logos::Logos, Eq, PartialEq, Hash)]
      pub enum Token {
        #[regex(r"\s+", logos::skip)]
        #[error]
        Error,

        #(#dynamic_tokens),*,
        #(#static_tokens),*,
      }
    }
  }

  fn generate_tokens_fmt(&self) -> TokenStream {
    let dynamic_tokens = self.dynamic_tokens.iter().map(|(token, _)| {
      let token = token.as_type();
      quote! { Token::#token(value) => std::fmt::Display::fmt(value, f) }
    });

    let static_tokens = self.static_tokens.iter().map(|(token, symbol)| {
      let token = token.as_type();
      quote! { Token::#token => write!(f, "{}", #symbol) }
    });

    quote! {
      impl std::fmt::Display for Token {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
          #[allow(clippy::write_literal)]
          match self {
            Token::Error => write!(f, "INVALID_TOKEN"),
            #(#dynamic_tokens),*,
            #(#static_tokens),*,
          }
        }
      }
    }
  }

  fn generate_tokens_parse(&self, error: TokenStream) -> TokenStream {
    let static_tokens = self.static_tokens.iter().map(|(ident, _)| {
      let ty = ident.as_type();
      quote! {
        pub fn #ident() -> impl Parser<Token, (), Error = Error> + Clone {
          just(Token::#ty).ignored()
        }
      }
    });

    let dynamic_tokens = self.dynamic_tokens.iter().map(|(ident, _)| {
      let ty = ident.as_type();
      quote! {
        pub fn #ident() -> impl Parser<Token, super::super::#ty, Error = Error> {
          select!{ Token::#ty(value) => value }
        }
      }
    });

    quote! {
      pub mod parse {
        use chumsky::prelude::*;
        use super::Token;

        type Error = #error;

        #(#static_tokens)*
        #(#dynamic_tokens)*
      }
    }
  }
}
