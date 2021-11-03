use last_order_logic::*;
use std::sync::Arc;

pub fn main() {
    println!("=== Last Order Logic 0.1 ===");
    println!("Type `help` for more information.");

    let mut defs: Vec<(Arc<String>, Expr)> = vec![];
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
            "" => {
                // Print separator for readability.
                print!("\n------------------------------------<o=o");
                println!("o=o>------------------------------------\n");
            }
            "bye" => break,
            "eval" => {
                if let Some(expr) = &last_expr {
                    let mut expr = expr.clone();
                    for (name, def) in &defs {
                        let (expr2, _) = expr.substitute(&Expr::Var(name.clone()), def);
                        expr = expr2;
                    }
                    println!("{}", expr.eval());
                } else {
                    println!("LOL: Type in an expression first");
                }
            }
            "ty" => {
                if let Some(expr) = &last_expr {
                    let mut expr = expr.clone();
                    for (name, def) in &defs {
                        let (expr2, _) = expr.substitute(&Expr::Var(name.clone()), def);
                        expr = expr2;
                    }
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
                match parsing::parse_data_str(&x) {
                    Ok(parsing::Data::Expr(expr)) => {
                        println!("{}", expr);
                        last_expr = Some(expr);
                    }
                    Ok(parsing::Data::Def(name, expr)) => {
                        println!("{} := {}", name, expr);
                        println!("LOL: Added `{}` to definitions", name);
                        defs.push((name, expr));
                    }
                    Err(err) => println!("ERROR:\n{}", err),
                }
            }
        }
    }
}
