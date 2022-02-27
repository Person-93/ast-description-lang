use anyhow::{ensure, Context, Result};
use escargot::CargoBuild;
use insta::{assert_snapshot, with_settings};
use std::{env, fs, process::Stdio};

fn main() -> Result<()> {
  for entry in fs::read_dir("./examples").context("failed to read examples directory")? {
    let entry = entry.context("failed to read directory entry")?;
    if !entry.metadata()?.is_dir() {
      continue;
    }
    let path = entry.path();
    println!("running example at {}", path.display());
    let output = CargoBuild::new()
      .manifest_path(path.join("Cargo.toml"))
      .run()?
      .command()
      .stdout(Stdio::piped())
      .spawn()
      .context("failed to spawn process for example")?
      .wait_with_output()
      .context("failed waiting for output from example")?;
    ensure!(
      output.status.success(),
      "example-{} exited non zero",
      path.file_name().unwrap().to_str().unwrap()
    );
    with_settings!({input_file => Some(path)}, {
      assert_snapshot!(std::str::from_utf8(&output.stdout).unwrap())
    });
  }

  Ok(())
}
