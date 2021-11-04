//! Runtime.

use crate::*;
use std::sync::Arc;

/// Reduces expression using definitions.
pub fn reduce(expr: &Expr, defs: &[(Arc<String>, Expr)]) -> Expr {
    let mut expr = expr.clone();
    loop {
        let mut found = false;
        for (name, def) in defs.iter().rev() {
            let (expr2, n) = expr.substitute(&Expr::Var(name.clone()), def);
            if n > 0 {
                expr = expr2;
                found = true;
                break;
            }
        }
        if !found {break};
    }
    expr
}

/// Handle data.
pub fn data(x: &str, defs: &mut Vec<(Arc<String>, Expr)>, last_expr: &mut Option<Expr>) {
    match parsing::parse_data_str(&x) {
        Ok(x) => {
            for d in x.into_iter() {
                match d {
                    parsing::Data::Expr(expr) => {
                        println!("{}", expr);
                        *last_expr = Some(expr);
                    }
                    parsing::Data::Def(name, expr) => {
                        println!("{} := {}", name, expr);
                        println!("LOL: Added `{}` to definitions", name);
                        defs.push((name, expr));
                    }
                    parsing::Data::Import(file) => {
                        use std::fs::File;
                        use std::io::Read;

                        let mut s = String::new();
                        let mut file = match File::open(&*file) {
                            Ok(x) => x,
                            Err(err) => {
                                eprintln!("Could not open `{}`, {}", file, err);
                                return;
                            }
                        };
                        file.read_to_string(&mut s).unwrap();
                        data(&s, defs, last_expr);
                    }
                }
            }
        }
        Err(err) => println!("ERROR:\n{}", err),
    }
}
