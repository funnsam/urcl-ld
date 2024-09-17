mod args;
mod linker;

fn main() {
    let args = <args::Args as clap::Parser>::parse();

    let srcs = args.inputs.iter().map(|f| std::fs::read_to_string(f).unwrap()).collect::<Vec<_>>();
    let mut files = srcs.iter().map(|s| linker::urcl::file(s).unwrap()).collect::<Vec<_>>();
    linker::link_files(&mut files);

    args.output.map_or_else(
        || linker::File::write_files(&mut std::io::stdout(), &files),
        |p| linker::File::write_files(&mut std::fs::File::create(p).unwrap(), &files),
    ).unwrap();
}
