//! Parsing.

use crate::*;

use piston_meta::{Convert, Range};

/// Parses a tuple.
pub fn parse_tup(
    node: &str,
    mut convert: Convert,
    ignored: &mut Vec<Range>,
) -> Result<(Range, Expr), ()> {
    let start = convert;
    let start_range = convert.start_node(node)?;
    convert.update(start_range);

    let mut res: Vec<Expr> = vec![];
    loop {
        if let Ok(range) = convert.end_node(node) {
            convert.update(range);
            break;
        } else if let Ok((range, v)) = parse_expr("item", convert, ignored) {
            convert.update(range);
            res.push(v);
        } else {
            let range = convert.ignore();
            convert.update(range);
            ignored.push(range);
        }
    }

    Ok((convert.subtract(start), Tup(res)))
}

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
        } else if let Ok((range, left, right)) = parse_left_right("lam", convert, ignored) {
            convert.update(range);
            res = Some(lam(left, right));
        } else if let Ok((range, left, right)) = parse_left_right("app", convert, ignored) {
            convert.update(range);
            res = Some(app(left, right));
        } else if let Ok((range, left, right)) = parse_left_right("all", convert, ignored) {
            convert.update(range);
            res = Some(all(left, right));
        } else if let Ok((range, left, right)) = parse_left_right("any", convert, ignored) {
            convert.update(range);
            res = Some(any(left, right));
        } else if let Ok((range, left, right)) = parse_left_right("and", convert, ignored) {
            convert.update(range);
            res = Some(and(left, right));
        } else if let Ok((range, left, right)) = parse_left_right("or", convert, ignored) {
            convert.update(range);
            res = Some(or(left, right));
        } else if let Ok((range, left, right)) = parse_left_right("imply", convert, ignored) {
            convert.update(range);
            res = Some(imply(left, right));
        } else if let Ok((range, left, right)) = parse_left_right("eq", convert, ignored) {
            convert.update(range);
            res = Some(eq(left, right));
        } else if let Ok((range, left, right)) = parse_left_right("xor", convert, ignored) {
            convert.update(range);
            res = Some(xor(left, right));
        } else if let Ok((range, v)) = parse_expr("un", convert, ignored) {
            convert.update(range);
            res = Some(un(v));
        } else if let Ok((range, v)) = parse_expr("nu", convert, ignored) {
            convert.update(range);
            res = Some(nu(v));
        } else if let Ok((range, v)) = parse_expr("lift", convert, ignored) {
            convert.update(range);
            res = Some(lift(v));
        } else if let Ok((range, v)) = parse_expr("not", convert, ignored) {
            convert.update(range);
            res = Some(not(v));
        } else if let Ok((range, v)) = parse_tup("tup", convert, ignored) {
            convert.update(range);
            res = Some(v);
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
        assert_eq!(parse_str("un(1)"), Ok(un(_1)));
        assert_eq!(parse_str("nu(0)"), Ok(nu(_0)));
        assert_eq!(parse_str("lift(0)"), Ok(lift(_0)));
        assert_eq!(parse_str(r#"\(x : I) = x"#), Ok(lam(ty("x", I), "x")));
        assert_eq!(parse_str("f(x)"), Ok(app("f", "x")));
        assert_eq!(parse_str("(x, y, z)"), Ok(Tup(vec!["x".into(), "y".into(), "z".into()])));
        assert_eq!(parse_str("all i : I { x }"), Ok(all(ty("i", I), "x")));
        assert_eq!(parse_str("any i : I { x }"), Ok(any(ty("i", I), "x")));
        assert_eq!(parse_str("not(0)"), Ok(not(_0)));
        assert_eq!(parse_str("!0"), Ok(not(_0)));
        assert_eq!(parse_str("0 & 1"), Ok(and(_0, _1)));
        assert_eq!(parse_str("0 ⋀ 1"), Ok(and(_0, _1)));
        assert_eq!(parse_str("and(0, 1)"), Ok(and(_0, _1)));
        assert_eq!(parse_str("0 | 1"), Ok(or(_0, _1)));
        assert_eq!(parse_str("0 ⋁ 1"), Ok(or(_0, _1)));
        assert_eq!(parse_str("or(0, 1)"), Ok(or(_0, _1)));
        assert_eq!(parse_str("0 => 1"), Ok(imply(_0, _1)));
        assert_eq!(parse_str("imply(0, 1)"), Ok(imply(_0, _1)));
        assert_eq!(parse_str("1 == 1"), Ok(eq(_1, _1)));
        assert_eq!(parse_str("eq(1, 1)"), Ok(eq(_1, _1)));
        assert_eq!(parse_str("1 != 0"), Ok(xor(_1, _0)));
        assert_eq!(parse_str("1 ⊻ 0"), Ok(xor(_1, _0)));
        assert_eq!(parse_str("xor(1, 0)"), Ok(xor(_1, _0)));
    }
}
