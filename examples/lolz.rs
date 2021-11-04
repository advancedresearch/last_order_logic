use last_order_logic::*;
use std::sync::Arc;

pub fn main() {
    println!("=== Last Order Logic 0.2 ===");
    println!("Type `help` for more information.");

    let mut defs: Vec<(Arc<String>, Expr)> = vec![];
    let mut last_expr: Option<Expr> = None;
    let mut last_import: Option<String> = None;
    loop {
        use std::io::{self, Write};

        print!("> ");
        let mut input = String::new();
        io::stdout().flush().unwrap();
        match io::stdin().read_line(&mut input) {
            Ok(_) => {}
            Err(_) => {
                println!("ERROR: Could not read input");
                continue;
            }
        };

        match input.trim() {
            "" => {
                // Print separator for readability.
                print!("\n------------------------------------<o=o");
                println!("o=o>------------------------------------\n");
            }
            "clear" => defs.clear(),
            "bye" => break,
            "red" => {
                if let Some(expr) = &last_expr {
                    let expr = runtime::reduce(expr, &defs);
                    println!("{}", expr);
                } else {
                    println!("LOL: Type in an expression first");
                }
            }
            "eval" => {
                if let Some(expr) = &last_expr {
                    let expr = runtime::reduce(expr, &defs);
                    println!("{}", expr.eval());
                } else {
                    println!("LOL: Type in an expression first");
                }
            }
            "ty" => {
                if let Some(expr) = &last_expr {
                    let expr = runtime::reduce(expr, &defs);
                    if let Some(ty) = expr.ty() {
                        println!("{}", ty);
                    } else {
                        println!("LOL: No type found for expression");
                    }
                } else {
                    println!("LOL: Type in an expression first");
                }
            }
            "reload" => {
                if let Some(x) = &last_import {
                    let x = x.clone();
                    runtime::data(&x, &mut defs, &mut last_expr, &mut last_import);
                } else {
                    println!("LOL: Previous import not set, use `import \"<file>\"`");
                }
            }
            "help path" => {
                println!("{}", include_str!("../assets/help/path.txt"));
            }
            "help all" => {
                println!("{}", include_str!("../assets/help/all.txt"));
            }
            "help any" => {
                println!("{}", include_str!("../assets/help/any.txt"));
            }
            "help bool" => {
                println!("{}", include_str!("../assets/help/bool.txt"));
            }
            "help lift" => {
                println!("{}", include_str!("../assets/help/lift.txt"));
            }
            "help type" => {
                println!("{}", include_str!("../assets/help/type.txt"));
            }
            "help lambda" => {
                println!("{}", include_str!("../assets/help/lambda.txt"));
            }
            "help" => {
                println!("{}", include_str!("../assets/help/help.txt"));
            }
            x => runtime::data(x, &mut defs, &mut last_expr, &mut last_import),
        }
    }
}
