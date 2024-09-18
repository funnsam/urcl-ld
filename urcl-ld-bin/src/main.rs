mod args;

fn main() {
    let args = <args::Args as clap::Parser>::parse();

    let srcs = args.inputs.iter().map(|f| std::fs::read_to_string(f).unwrap()).collect::<Vec<_>>();
    let mut files = srcs.iter().map(|s| urcl_ld::urcl::file(s).unwrap()).collect::<Vec<_>>();
    urcl_ld::link_files(&mut files);

    args.output.map_or_else(
        || urcl_ld::File::write_files(&mut std::io::stdout(), &files),
        |p| urcl_ld::File::write_files(&mut std::fs::File::create(p).unwrap(), &files),
    ).unwrap();
}
