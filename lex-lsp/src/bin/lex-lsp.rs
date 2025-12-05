use lex_lsp::LexLanguageServer;
use std::env;
use std::fs;
use std::process::ExitCode;
use tokio::io::{stdin, stdout};
use tower_lsp::{LspService, Server};

#[tokio::main]
async fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();

    // If called with "convert" subcommand, handle it and exit
    if args.len() >= 2 && args[1] == "convert" {
        return handle_convert(&args[2..]);
    }

    // Default: run as LSP server
    let stdin = stdin();
    let stdout = stdout();
    let (service, socket) = LspService::new(LexLanguageServer::new);
    Server::new(stdin, stdout, socket).serve(service).await;
    ExitCode::SUCCESS
}

fn handle_convert(args: &[String]) -> ExitCode {
    use lex_babel::format::SerializedDocument;
    use lex_babel::registry::FormatRegistry;
    use std::collections::HashMap;

    let mut input_path: Option<&str> = None;
    let mut output_path: Option<&str> = None;
    let mut to_format: Option<&str> = None;
    let mut extra_options: HashMap<String, String> = HashMap::new();

    let mut i = 0;
    while i < args.len() {
        let arg = &args[i];
        if arg == "--to" && i + 1 < args.len() {
            to_format = Some(&args[i + 1]);
            i += 2;
        } else if (arg == "-o" || arg == "--output") && i + 1 < args.len() {
            output_path = Some(&args[i + 1]);
            i += 2;
        } else if arg.starts_with("--extra-") {
            let key = arg.strip_prefix("--extra-").unwrap().to_string();
            let value = if i + 1 < args.len() && !args[i + 1].starts_with("-") {
                i += 1;
                args[i].clone()
            } else {
                "true".to_string()
            };
            extra_options.insert(key, value);
            i += 1;
        } else if !arg.starts_with("-") && input_path.is_none() {
            input_path = Some(arg);
            i += 1;
        } else {
            i += 1;
        }
    }

    let Some(input) = input_path else {
        eprintln!("Error: No input file specified");
        return ExitCode::FAILURE;
    };

    let Some(format) = to_format else {
        eprintln!("Error: No output format specified (use --to)");
        return ExitCode::FAILURE;
    };

    let Some(output) = output_path else {
        eprintln!("Error: No output path specified (use -o)");
        return ExitCode::FAILURE;
    };

    let source = match fs::read_to_string(input) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error reading {input}: {e}");
            return ExitCode::FAILURE;
        }
    };

    let registry = FormatRegistry::default();
    let doc = match registry.parse(&source, "lex") {
        Ok(d) => d,
        Err(e) => {
            eprintln!("Error parsing {input}: {e}");
            return ExitCode::FAILURE;
        }
    };

    let result = match registry.serialize_with_options(&doc, format, &extra_options) {
        Ok(r) => r,
        Err(e) => {
            eprintln!("Error converting to {format}: {e}");
            return ExitCode::FAILURE;
        }
    };

    let write_result = match result {
        SerializedDocument::Text(text) => fs::write(output, text),
        SerializedDocument::Binary(bytes) => fs::write(output, bytes),
    };

    if let Err(e) = write_result {
        eprintln!("Error writing {output}: {e}");
        return ExitCode::FAILURE;
    }

    ExitCode::SUCCESS
}
