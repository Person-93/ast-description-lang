use anyhow::{anyhow, Context, Result};
use std::{env, ffi::OsString, fs, process::Command};

fn main() -> Result<()> {
  let all_ok = fs::read_dir("./examples")?
    .filter_map(|e| {
      let e = e.unwrap();
      e.metadata()
        .unwrap()
        .file_type()
        .is_dir()
        .then(|| e.path().join("Cargo.toml"))
    })
    .map(|path| {
      println!("running example at {}", path.display());
      Command::new(cargo())
        .arg("run")
        .arg("--manifest-path")
        .arg(path)
        .status()
        .context("failed to start cargo")
        .map(|r| r.success())
    })
    .try_fold(true, |accum, current| {
      current.map(|current| current && accum)
    })?;

  if all_ok {
    Ok(())
  } else {
    Err(anyhow!("some examples failed"))
  }
}

fn cargo() -> OsString {
  env::var_os("CARGO").unwrap_or_else(|| OsString::from("cargo"))
}
