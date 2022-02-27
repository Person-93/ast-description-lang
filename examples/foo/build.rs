use anyhow::{Context, Result};
use ast_description_lang::{Config, Specs};
use std::fs;

fn main() -> Result<()> {
  println!("cargo:rerun-if-changed=example.toml");
  println!("cargo:rerun-if-changed=example.ast");

  let specs = fs::read_to_string("example.toml").context("failed to read spec file")?;
  let specs: Specs = toml::de::from_str(&specs).context("failed to deserialize specs")?;

  let config = Config::default();

  let tokens = specs
    .generate_tokens_mod(&config)
    .context("failed to generate tokens mod")?;
  fs::write("src/tokens/generated.rs", tokens.to_string()).context("failed to write tokens mod")?;

  let ast = fs::read_to_string("example.ast").context("failed to read ast file")?;
  let ast = ast_description_lang::generate_ast_mod(&ast, &specs, &config)
    .context("failed to generate ast mod")?;
  fs::write("src/ast/generated.rs", ast.to_string()).context("failed to write ast mod")?;

  Ok(())
}
