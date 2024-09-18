mod args;

fn main() {
    let args = <args::Args as clap::Parser>::parse();

    let srcs = args.inputs.iter().map(|f| std::fs::read_to_string(f).unwrap()).collect::<Vec<_>>();
    let mut files = srcs.iter().enumerate().map(|(f, s)| {
        match urcl_ld::urcl::file(s) {
            Ok(f) => f,
            Err(err) => {
                report(&args.inputs[f], s, err.location.offset..err.location.offset + 1, &err.expected.to_string());
                std::process::exit(1);
            },
        }
    }).collect::<Vec<_>>();

    match urcl_ld::link_files(&mut files) {
        Ok(()) => {
            args.output.map_or_else(
                || urcl_ld::File::write_files(&mut std::io::stdout(), &files),
                |p| urcl_ld::File::write_files(&mut std::fs::File::create(p).unwrap(), &files),
            ).unwrap();
        },
        Err(err) => {
            report(&args.inputs[err.file], &srcs[err.file], err.span, &err.typ.to_string());
            std::process::exit(1);
        },
    }
}

fn report(filename: &str, src: &str, span: urcl_ld::Span, err: &str) {
    let mut line = 0_usize;
    let mut line_byte = 0;

    for (nl, _) in src[..span.start].as_bytes().iter().enumerate().filter(|(_, b)| **b == b'\n') {
        line += 1;
        line_byte = nl + 1;
    }

    eprintln!("\x1b[1m{filename}:{}: \x1b[31merror:\x1b[0m {err}", line + 1);

    while line_byte < span.end {
        let (l, _) = src[line_byte..].split_once('\n').unwrap_or((&src[line_byte..], ""));
        line += 1;

        let start_col = span.start.saturating_sub(line_byte);
        let end_col = span.end.saturating_sub(line_byte).min(l.len());
        let len = end_col.saturating_sub(start_col);

        eprintln!("    {line} | {l}");
        eprintln!(
            "    {:<1$} | {2:<start_col$}\x1b[1;31m^{3:~<4$}\x1b[0m",
            "", line.ilog10() as usize + 1,
            "", "", len.saturating_sub(1),
        );

        line_byte += l.len() + 1;
    }
}
