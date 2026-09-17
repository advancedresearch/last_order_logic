#![deny(missing_docs)]
#![doc = include_str!("../README.md")]

use std::sync::Arc;
use Expr::*;
use std::fmt;

/// Alternative for `true/1`.
pub const T: Expr = _1;
/// Alternative for `false/0`.
pub const F: Expr = _0;

pub mod parsing;
pub mod runtime;

/// Stores an expression.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expr {
    /// Unit interval type.
    I,
    /// 0.
    _0,
    /// 1.
    _1,
    /// Variable.
    Var(Arc<String>),
    /// Type judgement.
    Ty(Box<Expr>, Box<Expr>),
    /// Type evaluation.
    Type(Box<Expr>),
    /// Path.
    Pa(Box<Expr>, Box<Expr>),
    /// Lambda expression.
    Lam(Box<Expr>, Box<Expr>),
    /// Lambda application.
    App(Box<Expr>, Box<Expr>),
    /// Tuple.
    Tup(Vec<Expr>),
    /// Path index.
    Ind(Box<Expr>, Box<Expr>),
    /// For-all.
    All(Box<Expr>),
    /// There-exists.
    Any(Box<Expr>),
    /// Uniform truth.
    Un(Box<Expr>),
    /// Non-uniform truth.
    Nu(Box<Expr>),
    /// Lifts value into type.
    Lift(Box<Expr>),
    /// Logical NOT.
    Not(Box<Expr>),
    /// Logical AND.
    And(Box<Expr>, Box<Expr>),
    /// Logical OR.
    Or(Box<Expr>, Box<Expr>),
    /// Logical IMPLY.
    Imply(Box<Expr>, Box<Expr>),
    /// Logical EQ.
    Eq(Box<Expr>, Box<Expr>),
    /// Logical XOR.
    Xor(Box<Expr>, Box<Expr>),
}

impl fmt::Display for Expr {
    fn fmt(&self, w: &mut fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            _0 => write!(w, "0")?,
            _1 => write!(w, "1")?,
            I => write!(w, "I")?,
            Type(a) => write!(w, "type({})", a)?,
            Un(a) => write!(w, "un({})", a)?,
            Nu(a) => write!(w, "nu({})", a)?,
            Var(name) => write!(w, "{}", name)?,
            Ind(a, b) => {
                if a.needs_parens() {write!(w, "({})", a)?} else {write!(w, "{}", a)?};
                write!(w, " ~ ")?;
                if b.needs_parens() {write!(w, "({})", b)?} else {write!(w, "{}", b)?};
            }
            Pa(a, b) => {
                if a.needs_parens() {write!(w, "({})", a)?} else {write!(w, "{}", a)?};
                write!(w, " ~= ")?;
                if b.needs_parens() {write!(w, "({})", b)?} else {write!(w, "{}", b)?};
            }
            Tup(tup) => {
                write!(w, "(")?;
                let mut first = true;
                for t in tup {
                    if !first {write!(w, ", ")?};
                    first = false;
                    write!(w, "{}", t)?;
                }
                write!(w, ")")?;
            }
            Ty(a, b) => {
                if a.needs_parens() {write!(w, "({})", a)?} else {write!(w, "{}", a)?};
                write!(w, " : ")?;
                if b.needs_parens() {write!(w, "({})", b)?} else {write!(w, "{}", b)?};
            }
            Lam(arg, body) => {
                write!(w, "\\({}) = {}", arg, body)?;
            }
            All(lam) => {
                if let Lam(arg, body) = &**lam {
                    write!(w, "∀ {} {{ {} }}", arg, body)?;
                }
            }
            Any(lam) => {
                if let Lam(arg, body) = &**lam {
                    write!(w, "∃ {} {{ {} }}", arg, body)?;
                }
            }
            App(a, b) => {
                if a.needs_parens() {
                    write!(w, "({})({})", a, b)?
                } else {
                    write!(w, "{}({})", a, b)?
                };
            }
            Lift(a) => {
                write!(w, "lift({})", a)?;
            }
            Not(a) => {
                if a.needs_parens() {write!(w, "¬({})", a)?} else {write!(w, "¬{}", a)?};
            }
            Or(a, b) => {
                if a.needs_parens() {write!(w, "({})", a)?} else {write!(w, "{}", a)?};
                write!(w, " ⋁ ")?;
                if b.needs_parens() {write!(w, "({})", b)?} else {write!(w, "{}", b)?};
            }
            And(a, b) => {
                if a.needs_parens() {write!(w, "({})", a)?} else {write!(w, "{}", a)?};
                write!(w, " ⋀ ")?;
                if b.needs_parens() {write!(w, "({})", b)?} else {write!(w, "{}", b)?};
            }
            Imply(a, b) => {
                if a.needs_parens() {write!(w, "({})", a)?} else {write!(w, "{}", a)?};
                write!(w, " => ")?;
                if b.needs_parens() {write!(w, "({})", b)?} else {write!(w, "{}", b)?};
            }
            Eq(a, b) => {
                if a.needs_parens() {write!(w, "({})", a)?} else {write!(w, "{}", a)?};
                write!(w, " == ")?;
                if b.needs_parens() {write!(w, "({})", b)?} else {write!(w, "{}", b)?};
            }
            Xor(a, b) => {
                if a.needs_parens() {write!(w, "({})", a)?} else {write!(w, "{}", a)?};
                write!(w, " ⊻ ")?;
                if b.needs_parens() {write!(w, "({})", b)?} else {write!(w, "{}", b)?};
            }
        }
        Ok(())
    }
}

impl Expr {
    /// Returns `true` if expression needs parentheses, `false` otherwise.
    pub fn needs_parens(&self) -> bool {
        match self {
            _1 | _0 | I | Var(_) | Un(_) | Nu(_) |
            All(_) | Any(_) | Tup(_) | Not(_) | Lift(_) | App(_, _) => false,
            _ => true,
        }
    }

    /// Returns `false` if the expression is a falsish expression, `false` otherwise.
    pub fn is_false(&self) -> bool {
        match self {
            _0 => true,
            Un(a) | Nu(a) => a.is_false(),
            _ => false,
        }
    }

    /// Returns `true` if the expression is truish expression, `false` otherwise.
    pub fn is_true(&self) -> bool {!self.is_false()}

    /// Gets the members of some type.
    pub fn members(&self) -> Vec<Expr> {
        match self {
            I => vec![_0, _1],
            Pa(a, b) => {
                let a_mem = a.members();
                let b_mem = b.members();
                let mut res = vec![];
                for am in &a_mem {
                    for bm in &b_mem {
                        res.push(pa(am.clone(), bm.clone()));
                    }
                }
                res
            }
            _ => vec![]
        }
    }

    /// Gets the type of an expression.
    pub fn ty(&self, args: &mut Vec<(Arc<String>, Expr)>) -> Option<Expr> {
        match self {
            _0 | _1 | Ty(_, _) => Some(I),
            I | Lam(_, _) => None,
            Var(n) => {
                for arg in args.iter().rev() {
                    if &arg.0 == n {return Some(arg.1.clone())}
                }
                None
            }
            Type(a) => a.eval(args).ty(args)?.ty(args),
            Ind(p, i) => {
                if let Pa(a, b) = &**p {
                    match &**i {
                        _0 => Some((**a).clone()),
                        _1 => Some((**b).clone()),
                        Tup(tup) => {
                            match tup.len() {
                                0 => None,
                                1 => ind((**p).clone(), tup[0].clone()).ty(args),
                                _ => ind(ind((**p).clone(), tup[0].clone()).ty(args)?,
                                        Tup(tup[1..].into())).ty(args),
                            }
                        }
                        _ => None,
                    }
                } else {
                    None
                }
            }
            Pa(a, b) => Some(pa(a.ty(args)?, b.ty(args)?)),
            App(a, b) => a.app(b, args)?.ty(args),
            Un(a) => Some(un(a.ty(args)?)),
            Nu(a) => Some(nu(a.ty(args)?)),
            Tup(vs) => {
                let mut res = vec![];
                for item in vs {
                    res.push(item.ty(args)?);
                }
                Some(Tup(res))
            }
            All(lam) => {
                if let Lam(arg, _) = &**lam {
                    if let Ty(_, ty) = &**arg {
                        let members = ty.members();
                        let mut res_ty = None;
                        for mem in &members {
                            let val = lam.app(mem, args)?;
                            let res = val.ty(args)?;
                            if res_ty.is_none() {
                                res_ty = Some(res);
                            } else if let Some(r) = &res_ty {
                                match (r.is_false(), res.is_false()) {
                                    (true, true) | (false, false) =>
                                        if &res != r {return None},
                                    (false, true) => res_ty = Some(res),
                                    (true, false) => {}
                                }
                            }
                        }
                        res_ty.map(|r| if r.is_false() {nu(r)} else {un(r)})
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            Any(lam) => {
                if let Lam(arg, _) = &**lam {
                    if let Ty(_, ty) = &**arg {
                        let members = ty.members();
                        let mut res_ty = None;
                        for mem in &members {
                            let res = lam.app(mem, args)?.ty(args)?;
                            if res_ty.is_none() {
                                res_ty = Some(res);
                            } else if let Some(r) = &res_ty {
                                match (r.is_true(), res.is_true()) {
                                    (true, true) | (false, false) =>
                                        if &res != r {return None},
                                    (false, true) => res_ty = Some(res),
                                    (true, false) => {}
                                }
                            }
                        }
                        res_ty.map(|r| if r.is_true() {nu(r)} else {un(r)})
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            Lift(a) => Some(a.eval(args)),
            Not(a) => Some(not(a.ty(args)?).eval(args)),
            And(a, b) => Some(and(a.ty(args)?, b.ty(args)?).eval(args)),
            Or(a, b) => Some(or(a.ty(args)?, b.ty(args)?).eval(args)),
            Imply(a, b) => Some(imply(a.ty(args)?, b.ty(args)?).eval(args)),
            Eq(a, b) => Some(eq(a.ty(args)?, b.ty(args)?).eval(args)),
            Xor(a, b) => Some(xor(a.ty(args)?, b.ty(args)?).eval(args)),
        }
    }

    /// Substitutes some argument with a value.
    pub fn substitute(&self, arg: &Expr, v: &Expr) -> (Expr, usize) {
        if self == arg {(v.clone(), 1)}
        else {
            match self {
                _0 | _1 | I | Var(_) => (self.clone(), 0),
                Type(a) => {
                    let (a2, n) = a.substitute(arg, v);
                    (typ(a2), n)
                }
                Un(a) => {
                    let (a2, n) = a.substitute(arg, v);
                    (un(a2), n)
                }
                Nu(a) => {
                    let (a2, n) = a.substitute(arg, v);
                    (nu(a2), n)
                }
                Ty(a, b) => {
                    let (a2, n) = a.substitute(arg, v);
                    let (b2, m) = b.substitute(arg, v);
                    (ty(a2, b2), n + m)
                }
                Ind(p, i) => {
                    let (p2, n) = p.substitute(arg, v);
                    let (i2, m) = i.substitute(arg, v);
                    (ind(p2, i2), n + m)
                }
                Pa(a, b) => {
                    let (a2, n) = a.substitute(arg, v);
                    let (b2, m) = b.substitute(arg, v);
                    (pa(a2, b2), n + m)
                }
                Lam(arg2, body) => {
                    if let Ty(a, a_ty) = &**arg2 {
                        let (a_ty2, m) = a_ty.substitute(arg, v);
                        if **a == *arg {(lam(ty((**a).clone(), a_ty2), (**body).clone()), m)}
                        else {
                            let (body2, n) = body.substitute(arg, v);
                            (lam(ty((**a).clone(), a_ty2), body2), m + n)
                        }
                    } else {
                        (self.clone(), 0)
                    }
                }
                All(lam) => {
                    let (lam2, n) = lam.substitute(arg, v);
                    (All(Box::new(lam2)), n)
                }
                Any(lam) => {
                    let (lam2, n) = lam.substitute(arg, v);
                    (Any(Box::new(lam2)), n)
                }
                Tup(tup) => {
                    let mut res = vec![];
                    let mut sum = 0;
                    for t in tup {
                        let (item, n) = t.substitute(arg, v);
                        res.push(item);
                        sum += n;
                    }
                    (Tup(res), sum)
                }
                Lift(a) => {
                    let (a2, n) = a.substitute(arg, v);
                    (lift(a2), n)
                }
                App(a, b) => {
                    let (a2, n) = a.substitute(arg, v);
                    let (b2, m) = b.substitute(arg, v);
                    (app(a2, b2), n + m)
                }
                Not(a) => {
                    let (a2, n) = a.substitute(arg, v);
                    (not(a2), n)
                }
                And(a, b) => {
                    let (a2, n) = a.substitute(arg, v);
                    let (b2, m) = b.substitute(arg, v);
                    (and(a2, b2), n + m)
                }
                Or(a, b) => {
                    let (a2, n) = a.substitute(arg, v);
                    let (b2, m) = b.substitute(arg, v);
                    (or(a2, b2), n + m)
                }
                Imply(a, b) => {
                    let (a2, n) = a.substitute(arg, v);
                    let (b2, m) = b.substitute(arg, v);
                    (imply(a2, b2), n + m)
                }
                Eq(a, b) => {
                    let (a2, n) = a.substitute(arg, v);
                    let (b2, m) = b.substitute(arg, v);
                    (eq(a2, b2), n + m)
                }
                Xor(a, b) => {
                    let (a2, n) = a.substitute(arg, v);
                    let (b2, m) = b.substitute(arg, v);
                    (xor(a2, b2), n + m)
                }
            }
        }
    }

    /// Apply value to some lambda expression.
    pub fn app(&self, v: &Expr, args: &mut Vec<(Arc<String>, Expr)>) -> Option<Expr> {
        match self {
            Lam(arg, body) => {
                if let Ty(a, arg_ty) = &**arg {
                    let v_ty = v.ty(args);
                    let same_ty = v_ty.as_ref().map(|v_ty| v_ty == &**arg_ty);
                    if let (Some(false), Some(Pa(_, _))) = (same_ty, &v_ty) {
                        return Some(pa(app(self.clone(), typ(ind(v.clone(), _0))),
                                       app(self.clone(), typ(ind(v.clone(), _1)))).eval(args));
                    };
                    if let Pa(_, _) = &**arg_ty {
                        if v_ty.is_none() {return None};
                    } else if let Some(false) = same_ty {
                        return None;
                    }
                    let (body2, _) = body.substitute(a, v);
                    Some(body2.eval(args))
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Evaluates an expression.
    pub fn eval(&self, args: &mut Vec<(Arc<String>, Expr)>) -> Expr {
        match self {
            _0 | _1 | I | Var(_) => self.clone(),
            Type(a) => if let Some(ty) = a.ty(args) {ty} else {self.clone()},
            Un(a) => un(a.eval(args)),
            Nu(a) => nu(a.eval(args)),
            Pa(a, b) => pa(a.eval(args), b.eval(args)),
            Lam(arg, v) => {
                if let Ty(a, b) = &**arg {
                    if let Var(a) = &**a {
                        let n = args.len();
                        args.push((a.clone(), (**b).clone()));
                        let res = lam((**arg).clone(), v.eval(args));
                        args.truncate(n);
                        return res;
                    }
                }
                lam((**arg).clone(), v.eval(args))
            }
            Ind(p, i) => ind(p.eval(args), i.eval(args)),
            Any(lam) => Any(Box::new(lam.eval(args))),
            All(lam) => All(Box::new(lam.eval(args))),
            App(a, b) => {
                let b2 = b.eval(args);
                if let Some(res) = a.eval(args).app(&b2, args) {res} else {self.clone()}
            }
            Lift(a) => lift(a.eval(args)),
            Tup(tup) => Tup(tup.iter().map(|n| n.eval(args)).collect()),
            Ty(a, b) => {
                let a2 = a.eval(args);
                let b2 = b.eval(args);
                if let Some(a_ty) = a2.ty(args) {
                    if a_ty == b2 {T} else {F}
                } else {
                    self.clone()
                }
            }
            Not(a) => {
                match &**a {
                    _0 => _1,
                    _1 => _0,
                    I => I,
                    Un(a) => un(not((**a).clone()).eval(args)).eval(args),
                    Nu(a) => nu(not((**a).clone()).eval(args)).eval(args),
                    Pa(a, b) => pa(not((**a).clone()).eval(args), not((**b).clone()).eval(args)),
                    a => {
                        let a2 = a.eval(args);
                        if &a2 != a {
                            not(a.eval(args)).eval(args)
                        } else {
                            self.clone()
                        }
                    }
                }
            }
            And(a, b) => {
                match (&**a, &**b) {
                    (_1, _1) => _1,
                    (_0, _) | (_, _0) => _0,
                    (I, I) => I,
                    (Pa(a0, a1), Pa(b0, b1)) => pa(
                        and((**a0).clone(), (**b0).clone()).eval(args),
                        and((**a1).clone(), (**b1).clone()).eval(args),
                    ),
                    (a, b) => {
                        let a2 = a.eval(args);
                        let b2 = b.eval(args);
                        if &a2 == a && &b2 == b {self.clone()}
                        else {and(a2, b2).eval(args)}
                    }
                }
            }
            Or(a, b) => {
                match (&**a, &**b) {
                    (_1, _) | (_, _1) => _1,
                    (_0, _0) => _0,
                    (I, I) => I,
                    (Pa(a0, a1), Pa(b0, b1)) => pa(
                        or((**a0).clone(), (**b0).clone()).eval(args),
                        or((**a1).clone(), (**b1).clone()).eval(args),
                    ),
                    (a, b) => {
                        let a2 = a.eval(args);
                        let b2 = b.eval(args);
                        if &a2 == a && &b2 == b {self.clone()}
                        else {or(a2, b2).eval(args)}
                    }
                }
            }
            Imply(a, b) => {
                match (&**a, &**b) {
                    (_0, _) | (_1, _1) => _1,
                    (_1, _0) => _0,
                    (I, I) => I,
                    (Pa(a0, a1), Pa(b0, b1)) => pa(
                        imply((**a0).clone(), (**b0).clone()).eval(args),
                        imply((**a1).clone(), (**b1).clone()).eval(args),
                    ),
                    (a, b) => {
                        let a2 = a.eval(args);
                        let b2 = b.eval(args);
                        if &a2 == a && &b2 == b {self.clone()}
                        else {imply(a2, b2).eval(args)}
                    }
                }
            }
            Eq(a, b) => {
                match (&**a, &**b) {
                    (_0, _0) | (_1, _1) => _1,
                    (_1, _0) | (_0, _1) => _0,
                    (I, I) => I,
                    (Pa(a0, a1), Pa(b0, b1)) => pa(
                        eq((**a0).clone(), (**b0).clone()).eval(args),
                        eq((**a1).clone(), (**b1).clone()).eval(args),
                    ),
                    (a, b) => {
                        let a2 = a.eval(args);
                        let b2 = b.eval(args);
                        if &a2 == a && &b2 == b {self.clone()}
                        else {eq(a2, b2).eval(args)}
                    }
                }
            }
            Xor(a, b) => {
                match (&**a, &**b) {
                    (_0, _0) | (_1, _1) => _0,
                    (_1, _0) | (_0, _1) => _1,
                    (I, I) => I,
                    (Pa(a0, a1), Pa(b0, b1)) => pa(
                        xor((**a0).clone(), (**b0).clone()).eval(args),
                        xor((**a1).clone(), (**b1).clone()).eval(args),
                    ),
                    (a, b) => {
                        let a2 = a.eval(args);
                        let b2 = b.eval(args);
                        if &a2 == a && &b2 == b {self.clone()}
                        else {xor(a2, b2).eval(args)}
                    }
                }
            }
        }
    }
}

impl From<&str> for Expr {
    fn from(v: &str) -> Expr {Var(Arc::new(v.into()))}
}

impl From<Arc<String>> for Expr {
    fn from(v: Arc<String>) -> Expr {Var(v)}
}

/// `f(a)` - Lambda application.
pub fn app<T: Into<Expr>, U: Into<Expr>>(a: T, b: U) -> Expr {
    App(Box::new(a.into()), Box::new(b.into()))
}

/// `lift(a)` - Lifts value into type.
pub fn lift<T: Into<Expr>>(a: T) -> Expr {
    Lift(Box::new(a.into()))
}

/// `a ~= b` - Path.
pub fn pa<T: Into<Expr>, U: Into<Expr>>(a: T, b: U) -> Expr {
    Pa(Box::new(a.into()), Box::new(b.into()))
}

/// `a : b` - Type judgement.
pub fn ty<T: Into<Expr>, U: Into<Expr>>(a: T, b: U) -> Expr {
    Ty(Box::new(a.into()), Box::new(b.into()))
}

/// `type(a)` - Type evaluation.
pub fn typ<T: Into<Expr>>(a: T) -> Expr {
    Type(Box::new(a.into()))
}

/// `p ~ i` - Path index.
pub fn ind<T: Into<Expr>, U: Into<Expr>>(a: T, b: U) -> Expr {
    Ind(Box::new(a.into()), Box::new(b.into()))
}

/// `\(a) = ...` - Lambda with one argument.
pub fn lam<T: Into<Expr>, U: Into<Expr>>(a: T, b: U) -> Expr {
    Lam(Box::new(a.into()), Box::new(b.into()))
}

/// `\(a, b) = ...` - Lambda with two arguments.
pub fn lam2<T0: Into<Expr>, T1: Into<Expr>, U: Into<Expr>>(a0: T0, a1: T1, b: U) -> Expr {
    lam(a0, lam(a1, b))
}

/// `∀ i { ... }` - For-all.
pub fn all<T: Into<Expr>, U: Into<Expr>>(arg: T, v: U) -> Expr {
    All(Box::new(lam(arg, v)))
}

/// `∀ i, j { ... }` - Two nested for-all loops.
pub fn all2<T0: Into<Expr>, T1: Into<Expr>, U: Into<Expr>>(a0: T0, a1: T1, b: U) -> Expr {
    all(a0, all(a1, b))
}

/// `∃ i { ... }` - There-exists.
pub fn any<T: Into<Expr>, U: Into<Expr>>(arg: T, v: U) -> Expr {
    Any(Box::new(lam(arg, v)))
}

/// `nu(a)` - Non-uniform truth.
pub fn nu<T: Into<Expr>>(a: T) -> Expr {
    Nu(Box::new(a.into()))
}

/// `un(a)` - Uniform truth.
pub fn un<T: Into<Expr>>(a: T) -> Expr {
    Un(Box::new(a.into()))
}

/// `¬a` - Logical NOT.
pub fn not<T: Into<Expr>>(a: T) -> Expr {
    Not(Box::new(a.into()))
}

/// `a ⋀ b` - Logical AND.
pub fn and<T: Into<Expr>, U: Into<Expr>>(a: T, b: U) -> Expr {
    And(Box::new(a.into()), Box::new(b.into()))
}

/// `a ⋁ b` - Logical OR.
pub fn or<T: Into<Expr>, U: Into<Expr>>(a: T, b: U) -> Expr {
    Or(Box::new(a.into()), Box::new(b.into()))
}

/// `a => b` - Logical IMPLY.
pub fn imply<T: Into<Expr>, U: Into<Expr>>(a: T, b: U) -> Expr {
    Imply(Box::new(a.into()), Box::new(b.into()))
}

/// `a == b` - Logical EQ.
pub fn eq<T: Into<Expr>, U: Into<Expr>>(a: T, b: U) -> Expr {
    Eq(Box::new(a.into()), Box::new(b.into()))
}

/// `a ⊻ b` - Logical XOR.
pub fn xor<T: Into<Expr>, U: Into<Expr>>(a: T, b: U) -> Expr {
    Xor(Box::new(a.into()), Box::new(b.into()))
}

/// `(a, b)` - A tuple of two elements.
pub fn tup2<T: Into<Expr>, U: Into<Expr>>(a: T, b: U) -> Expr {
    Tup(vec![a.into(), b.into()])
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ty_path() {
        let a = ind(pa("a", "b"), _0).ty(&mut vec![]);
        assert_eq!(a, Some("a".into()));

        let b = ind(pa("a", "b"), _1).ty(&mut vec![]);
        assert_eq!(b, Some("b".into()));

        let e = pa(_0, _0);
        assert_eq!(e.ty(&mut vec![]), Some(pa(I, I)));

        let e = pa(pa(_0, _0), _0);
        assert_eq!(e.ty(&mut vec![]), Some(pa(pa(I, I), I)));
    }

    #[test]
    fn ty_f() {
        let f = lam(ty("i", I), ind(pa(T, F), "i"));
        assert_eq!(f.app(&_0, &mut vec![]).unwrap().ty(&mut vec![]).unwrap(), T);
        assert_eq!(f.app(&_1, &mut vec![]).unwrap().ty(&mut vec![]).unwrap(), F);

        let all1 = all(ty("i", I), ind(pa(T, F), "i"));
        assert_eq!(all1.ty(&mut vec![]), Some(nu(F)));
        let any1 = any(ty("i", I), ind(pa(T, F), "i"));
        assert_eq!(any1.ty(&mut vec![]), Some(nu(T)));

        let all2 = all(ty("i", I), ind(pa(T, T), "i"));
        assert_eq!(all2.ty(&mut vec![]), Some(un(T)));
        let any2 = any(ty("i", I), ind(pa(T, T), "i"));
        assert_eq!(any2.ty(&mut vec![]), Some(nu(T)));

        let all3 = all(ty("i", I), ind(pa(F, F), "i"));
        assert_eq!(all3.ty(&mut vec![]), Some(nu(F)));
        let any3 = any(ty("i", I), ind(pa(F, F), "i"));
        assert_eq!(any3.ty(&mut vec![]), Some(un(F)));
    }

    #[test]
    fn test_not() {
        let a = not(pa(T, F)).eval(&mut vec![]);
        assert_eq!(a, pa(F, T));

        let b = not(pa(F, T)).eval(&mut vec![]);
        assert_eq!(b, pa(T, F));

        let c = not(pa(T, T)).eval(&mut vec![]);
        assert_eq!(c, pa(F, F));

        let d = not(pa(F, F)).eval(&mut vec![]);
        assert_eq!(d, pa(T, T));

        let f = not(pa(pa(T, F), pa(F, T))).eval(&mut vec![]);
        assert_eq!(f, pa(pa(F, T), pa(T, F)));

        let a = not(T).eval(&mut vec![]);
        assert_eq!(a, F);

        let a = not(I).eval(&mut vec![]);
        assert_eq!(a, I);
    }

    #[test]
    fn test_2d() {
        let a = pa(pa(F, T), pa(F, F));
        let a00 = ind(a.clone(), tup2(_0, _0));
        let a01 = ind(a.clone(), tup2(_0, _1));
        let a10 = ind(a.clone(), tup2(_1, _0));
        let a11 = ind(a, tup2(_1, _1));
        assert_eq!(a00.ty(&mut vec![]), Some(F));
        assert_eq!(a01.ty(&mut vec![]), Some(T));
        assert_eq!(a10.ty(&mut vec![]), Some(F));
        assert_eq!(a11.ty(&mut vec![]), Some(F));

        let a = pa(pa(T, T), pa(T, T));
        let f = all2(ty("i", I), ty("j", I), ind(a, tup2("i", "j")));
        assert_eq!(f.ty(&mut vec![]), Some(un(un(T))));

        let a = pa(pa(T, T), pa(T, F));
        let f = all2(ty("i", I), ty("j", I), ind(a.clone(), tup2("i", "j")));
        assert_eq!(f.ty(&mut vec![]), Some(nu(nu(F))));
    }

    #[test]
    fn test_2d_2() {
        let a = pa(pa(T, T), pa(T, F));
        let f = all(ty("i", I), any(ty("j", I), ind(a, tup2("i", "j"))));
        assert_eq!(f.ty(&mut vec![]), Some(un(nu(T))));

        let a = pa(pa(T, T), pa(F, F));
        let f = all(ty("i", I), any(ty("j", I), ind(a, tup2("i", "j"))));
        assert_eq!(f.ty(&mut vec![]), Some(nu(un(F))));
    }

    #[test]
    fn test_fail() {
        let a = all(ty("i", I), T);
        assert_eq!(a.ty(&mut vec![]), Some(un(I)));

        let a = all2(ty("i", I), ty("j", I), ind(pa(T, T), "i"));
        assert_eq!(a.ty(&mut vec![]), Some(un(un(T))));
    }

    #[test]
    fn test_ty_ty() {
        let e = ty(_0, I);
        assert_eq!(e.ty(&mut vec![]), Some(I));

        assert_eq!(I.ty(&mut vec![]), None);

        assert_eq!(nu(_0).ty(&mut vec![]), Some(nu(I)));
        assert_eq!(un(_0).ty(&mut vec![]), Some(un(I)));

        assert_eq!(tup2(_0, _1).ty(&mut vec![]), Some(tup2(I, I)));

        assert_eq!(typ(ind(pa(_0, _1), _1)).ty(&mut vec![]), Some(I));
    }

    #[test]
    fn test_all_bool_ty() {
        let e = all(ty("i", I), ty("i", I));
        assert_eq!(e.ty(&mut vec![]), Some(un(I)));
    }

    #[test]
    fn test_and() {
        let e = and(T, T);
        assert_eq!(e.eval(&mut vec![]), T);

        let e = and(T, F);
        assert_eq!(e.eval(&mut vec![]), F);

        let e = and(F, F);
        assert_eq!(e.eval(&mut vec![]), F);

        let e = and(pa(T, F), pa(T, T));
        assert_eq!(e.eval(&mut vec![]), pa(T, F));

        let a = all(ty("i", I), ind(and(pa(T, T), pa(T, T)), "i"));
        assert_eq!(a.ty(&mut vec![]), Some(un(T)));
    }

    #[test]
    fn test_or() {
        let e = or(T, T);
        assert_eq!(e.eval(&mut vec![]), T);

        let e = or(T, F);
        assert_eq!(e.eval(&mut vec![]), T);

        let e = or(F, F);
        assert_eq!(e.eval(&mut vec![]), F);

        let e = or(pa(T, F), pa(T, T));
        assert_eq!(e.eval(&mut vec![]), pa(T, T));

        let a = all(ty("i", I), ind(or(pa(T, T), pa(T, T)), "i"));
        assert_eq!(a.ty(&mut vec![]), Some(un(T)));

        let a = all(ty("i", I), ind(or(pa(T, F), pa(F, T)), "i"));
        assert_eq!(a.ty(&mut vec![]), Some(un(T)));
    }

    #[test]
    fn test_app() {
        let e = app(lam(ty("a", I), "a"), _0);
        assert_eq!(e.eval(&mut vec![]), _0);
        assert_eq!(e.ty(&mut vec![]), Some(I));

        let e = app(lam(ty("a", pa(I, I)), ind("a", _0)), pa(_1, _0));
        assert_eq!(e.eval(&mut vec![]), ind(pa(_1, _0), _0));
        assert_eq!(e.ty(&mut vec![]), Some(_1));

        let e = app(lam(ty("p", pa(I, I)), ty(all(ty("i", I), ind("p", "i")), un(_1))), pa(_1, _1));
        assert_eq!(e.eval(&mut vec![]), _1);
    }

    #[test]
    fn test_eval_ty() {
        let e = ty(ind(pa(T, F), _0), T);
        assert_eq!(e.eval(&mut vec![]), T);

        let e = ty(ind(pa(F, T), _0), T);
        assert_eq!(e.eval(&mut vec![]), F);

        let e = ty(_0, I);
        assert_eq!(e.eval(&mut vec![]), T);

        let e = all(ty("i", I), ind(pa(T, ty("i", I)), "i"));
        assert_eq!(e.ty(&mut vec![]), Some(un(T)));

        let a = ty(all2(ty("i", I), ty("j", I),
            ind(pa(pa(T, T), pa(T, T)), tup2("i", "j"))), un(un(T)));
        assert_eq!(a.eval(&mut vec![]), T);

        let b = ty(ind(pa(T, F), _0), a);
        assert_eq!(b.eval(&mut vec![]), T);

        let a = ty(T, I);
        assert_eq!(a.eval(&mut vec![]), T);
    }

    #[test]
    fn test_all_path() {
        let a = all(ty("p", pa(I, I)), ind("p", _0));
        assert_eq!(a.ty(&mut vec![]), Some(nu(F)));

        // ¬(∀ i { (0 ~= 0) ~ i } : un(1))
        let a1 = ty(all(ty("i", I), ind(pa(_0, _0), "i")), un(_1));
        assert_eq!(a1.eval(&mut vec![]), F);

        // ∃ i { (¬(0 ~= 0)) ~ i } : un(0)
        let a2 = ty(all(ty("i", I), ind(not(pa(_0, _0)), "i")), un(_0));
        assert_eq!(a2.eval(&mut vec![]), F);

        // lift((¬(∀ i { (0 ~= 0) ~ i } : un(1))) ⋁ (∃ i { (¬(0 ~= 0)) ~ i } : un(0)))
        let a3 = lift(or(a1, a2.eval(&mut vec![])));
        assert_eq!(a3.eval(&mut vec![]), lift(F));

        let a = all(ty("p", pa(I, I)), lift(imply(
            ty(all(ty("i", I), ind("p", "i")), un(T)),
            ty(any(ty("i", I), ind(not("p"), "i")), un(F)))));
        assert_eq!(a.ty(&mut vec![]), Some(un(T)));

        let a = not(un(T));
        assert_eq!(a.eval(&mut vec![]), un(F));

        let a = ty(all(ty("p", pa(I, I)), lift(imply(
            ty(all(ty("i", I), ind("p", "i")), un(T)),
            ty(not(any(ty("i", I), ind(not("p"), "i"))), un(T))))), un(T));
        assert_eq!(a.eval(&mut vec![]), T);
    }

    #[test]
    fn test_eq() {
        let a = eq(T, T);
        assert_eq!(a.eval(&mut vec![]), T);

        let a = eq(pa(T, F), pa(T, F));
        assert_eq!(a.eval(&mut vec![]), pa(T, T));
    }

    #[test]
    fn test_xor() {
        let a = xor(T, F);
        assert_eq!(a.eval(&mut vec![]), T);

        let a = xor(pa(T, F), pa(F, T));
        assert_eq!(a.eval(&mut vec![]), pa(T, T));

        let a = all2(ty("a", I), ty("b", I), lift(eq(not(eq("a", "b")), xor("a", "b"))));
        assert_eq!(a.ty(&mut vec![]), Some(un(un(T))));
        let a = ty(a, un(un(T)));
        assert_eq!(a.eval(&mut vec![]), T);
    }

    #[test]
    fn test_uniform() {
        let a = all(ty("i", I), ind(pa("a", "a"), "i"));
        assert_eq!(a.ty(&mut vec![]), Some(un("a")));
    }

    #[test]
    fn test_format() {
        let a = format!("{}", not(T));
        assert_eq!(a, "¬1");

        let a = format!("{}", ty(all2(ty("i", I), ty("j", I),
            ind(pa(pa(T, T), pa(T, T)), tup2("i", "j"))), un(un(T))));
        assert_eq!(a, "∀ i : I { ∀ j : I { ((1 ~= 1) ~= (1 ~= 1)) ~ (i, j) } } : un(un(1))");

        let a = format!("{}", app(lam(ty("a", I), "a"), _0));
        assert_eq!(a, "(\\(a : I) = a)(0)");
    }

    #[test]
    fn test_all_asym() {
        let a = all(ty("i", I), ind(pa("a", "b"), "i"));
        assert_eq!(a.ty(&mut vec![]), None);

        let a = all(ty("i", I), ind(pa("a", "a"), "i"));
        assert_eq!(a.ty(&mut vec![]), Some(un("a")));

        let a = all(ty("p", pa(I, I)), lift(and("p", "p")));
        assert_eq!(a.ty(&mut vec![]), None);
    }

    #[test]
    fn test_any_asym() {
        let a = any(ty("i", I), ind(pa("a", "b"), "i"));
        assert_eq!(a.ty(&mut vec![]), None);
    }

    #[test]
    fn test_eval() {
        let e = parsing::parse_str(r#" (\(p : (I ~= I)) = ∀ i : I { p ~ i } : un(1))(p) "#).unwrap();
        assert_eq!(e.eval(&mut vec![]), app(
            lam(
                ty("p", pa(I, I)),
                ty(all(ty("i", I), ind("p", "i")), un(_1))
            ),
            "p"
        ));

        let e = parsing::parse_str(r#"\(p : ((I ~= I) ~= (I ~= I))) = (\(p : (I ~= I)) = ∀ i : I { p ~ i } : un(1))(p)"#).unwrap();
        let e2 = e.eval(&mut vec![]);
        let expected = parsing::parse_str(r#"\(p : ((I ~= I) ~= (I ~= I))) = (\(p : (I ~= I)) = ∀ i : I { p ~ i } : un(1))(type(p ~ 0)) ~= (\(p : (I ~= I)) = ∀ i : I { p ~ i } : un(1))(type(p ~ 1))"#).unwrap();
        assert_eq!(e2, expected);

        let e = parsing::parse_str(r#"(\(p : ((I ~= I) ~= (I ~= I))) =
        (\(p : (I ~= I)) = ∀ i : I { p ~ i } : un(1))((\(p : (I ~= I)) =
        ∀ i : I { p ~ i } : un(1))(p)))((1 ~= 1) ~= (1 ~= 1))"#).unwrap();
        let e2 = e.eval(&mut vec![]);
        assert_eq!(e2, _1);
    }
}
