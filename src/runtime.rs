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
