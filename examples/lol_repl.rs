use last_order_logic::*;

pub fn main() {
    println!("=== Last Order Logic 0.1 ===");
    println!("Type `help` for more information.");

    let mut last_expr: Option<Expr> = None;
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
            "bye" => break,
            "eval" => {
                if let Some(expr) = &last_expr {
                    println!("{}", expr.eval());
                } else {
                    println!("LOL: Type in an expression first");
                }
            }
            "ty" => {
                if let Some(expr) = &last_expr {
                    if let Some(ty) = expr.ty() {
                        println!("{}", ty);
                    } else {
                        println!("LOL: No type found for expression");
                    }
                } else {
                    println!("LOL: Type in an expression first");
                }
            }
            "help" => {
                println!("{}", include_str!("../assets/help.txt"));
            }
            x => {
                match parsing::parse_str(&x) {
                    Ok(expr) => {
                        println!("{}", expr);
                        last_expr = Some(expr);
                    }
                    Err(err) => println!("ERROR:\n{}", err),
                }
            }
        }
    }
}
