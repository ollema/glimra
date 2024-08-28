use std::env::consts::{ARCH, DLL_EXTENSION, DLL_PREFIX, OS};
use std::fs;
use std::path::PathBuf;
use std::process::Command;

use anyhow::{bail, Context, Result};
use env_logger::Env;

fn main() -> Result<()> {
    env_logger::init_from_env(Env::default().default_filter_or("info"));

    let cargo = std::env::var("CARGO").unwrap_or_else(|_| "cargo".to_string());

    let mut build_command = Command::new(&cargo);
    build_command.arg("build");
    build_command.arg("--release");

    let exit_status = build_command
        .spawn()
        .context("failed to spawn child process")?
        .wait()
        .context("failed to wait for child process")?;

    if !exit_status.success() {
        bail!("cargo build failed: {}", exit_status);
    }

    let priv_lib_dir = PathBuf::from_iter(["priv", "lib"]);

    let mut dll_input_path =
        PathBuf::from_iter(["target", "release"]).join(format!("{DLL_PREFIX}libglans"));
    dll_input_path.set_extension(DLL_EXTENSION);

    let mut dll_output_path = priv_lib_dir.join(format!("libglans-{OS}-{ARCH}"));
    dll_output_path.set_extension(if cfg!(windows) { "dll" } else { "so" });

    fs::create_dir_all(&priv_lib_dir)?;
    log::info!(
        "copying {input} to {output}",
        input = dll_input_path.display(),
        output = dll_output_path.display()
    );
    fs::copy(&dll_input_path, &dll_output_path)?;

    Ok(())
}