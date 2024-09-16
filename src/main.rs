mod args;
mod linker;

fn main() {
    let args = <args::Args as clap::Parser>::parse();

    let srcs = args.inputs.iter().map(|f| std::fs::read_to_string(f).unwrap()).collect::<Vec<_>>();
    let mut files = srcs.iter().map(|s| linker::urcl::file(s).unwrap()).collect::<Vec<_>>();
    linker::link_files(&mut files);

    match args.output {
        Some(p) => {
            use std::io::Write;

            let mut file = std::fs::File::create(p).unwrap();
            for f in files.iter() {
                write!(file, "{f}").unwrap();
            }
        },
        None => for f in files.iter() {
            print!("{f}");
        },
    }
}
