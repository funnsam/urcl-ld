use clap::*;

#[derive(Parser, Debug, Clone)]
pub struct Args {
    pub inputs: Vec<String>,

    #[arg(short, long)]
    pub output: Option<String>,
}
