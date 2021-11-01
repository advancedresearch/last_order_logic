//! Parsing.

use crate::*;

use piston_meta::{Convert, Range};

/// Parses a left/right expression.
pub fn parse_left_right(
    node: &str,
    mut convert: Convert,
    ignored: &mut Vec<Range>,
) -> Result<(Range, Expr, Expr), ()> {
    let start = convert;
    let start_range = convert.start_node(node)?;
    convert.update(start_range);

    let mut left: Option<Expr> = None;
    let mut right: Option<Expr> = None;
    loop {
        if let Ok(range) = convert.end_node(node) {
            convert.update(range);
            break;
        } else if let Ok((range, v)) = parse_expr("left", convert, ignored) {
            convert.update(range);
            left = Some(v);
        } else if let Ok((range, v)) = parse_expr("right", convert, ignored) {
            convert.update(range);
            right = Some(v);
        } else {
            let range = convert.ignore();
            convert.update(range);
            ignored.push(range);
        }
    }

    let left = left.ok_or(())?;
    let right = right.ok_or(())?;
    Ok((convert.subtract(start), left, right))
}

/// Parses an expression.
pub fn parse_expr(
    node: &str,
    mut convert: Convert,
    ignored: &mut Vec<Range>,
) -> Result<(Range, Expr), ()> {
    let start = convert;
    let start_range = convert.start_node(node)?;
    convert.update(start_range);

    let mut res: Option<Expr> = None;
    loop {
        if let Ok(range) = convert.end_node(node) {
            convert.update(range);
            break;
        } else if let Ok((range, _)) = convert.meta_bool("0") {
            convert.update(range);
            res = Some(_0);
        } else if let Ok((range, _)) = convert.meta_bool("1") {
            convert.update(range);
            res = Some(_1);
        } else if let Ok((range, _)) = convert.meta_bool("I") {
            convert.update(range);
            res = Some(I);
        } else if let Ok((range, v)) = convert.meta_string("var") {
            convert.update(range);
            res = Some(v.into());
        } else if let Ok((range, left, right)) = parse_left_right("ty", convert, ignored) {
            convert.update(range);
            res = Some(ty(left, right));
        } else if let Ok((range, left, right)) = parse_left_right("pa", convert, ignored) {
            convert.update(range);
            res = Some(pa(left, right));
        } else if let Ok((range, left, right)) = parse_left_right("ind", convert, ignored) {
            convert.update(range);
            res = Some(ind(left, right));
        } else {
            let range = convert.ignore();
            convert.update(range);
            ignored.push(range);
        }
    }

    let res = res.ok_or(())?;
    Ok((convert.subtract(start), res))
}

/// Parses a string.
pub fn parse_str(data: &str) -> Result<Expr, String> {
    use piston_meta::{parse_errstr, syntax_errstr};

    let syntax_src = include_str!("../assets/syntax.txt");
    let syntax = syntax_errstr(syntax_src)?;

    let mut meta_data = vec![];
    parse_errstr(&syntax, &data, &mut meta_data)?;

    // piston_meta::json::print(&meta_data);

    let convert = Convert::new(&meta_data);
    let mut ignored = vec![];
    match parse_expr("expr", convert, &mut ignored) {
        Err(()) => Err("Could not convert meta data".into()),
        Ok((_, expr)) => Ok(expr),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn expr() {
        assert_eq!(parse_str("0"), Ok(_0));
        assert_eq!(parse_str("1"), Ok(_1));
        assert_eq!(parse_str("I"), Ok(I));
        assert_eq!(parse_str("0 : I"), Ok(ty(_0, I)));
        assert_eq!(parse_str("1 : I"), Ok(ty(_1, I)));
        assert_eq!(parse_str("0 ~= 1"), Ok(pa(_0, _1)));
        assert_eq!(parse_str("(0 ~= 1)"), Ok(pa(_0, _1)));
        assert_eq!(parse_str("(0 ~= 1) ~ 0"), Ok(ind(pa(_0, _1), _0)));
        assert_eq!(parse_str("x"), Ok("x".into()));
    }
}
