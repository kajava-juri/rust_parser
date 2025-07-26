use std::{
    collections::HashMap,
    fmt::{self},
    vec,
};
// Credit to Core Dumped YouTube channel for giving the idea and a starting code to build on
// https://www.youtube.com/watch?v=0c8b7YfsBKs

use crate::error_types::*;

#[derive(Clone, Debug, PartialEq)]
pub enum Lit {
    Int(i64),
    Float(f64),
    Ident(String), // identifier (previously Atom was both a single digit or character) like foo123
}

#[derive(Clone, Debug, PartialEq)]
enum Token {
    Literal(Lit),
    Op(char),
    Eof,
}

struct Lexer {
    tokens: Vec<Token>,
}

#[derive(Clone)]
pub enum Expression {
    Atom(Lit),
    Operation(char, Vec<Expression>), // e.g., Operation('+', vec![Atom('a'), Atom('b')]). TODOL support multi-character operators (e.g., '==', '!=', '&&', '||')
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Assoc {
    Left,
    Right,
}

impl fmt::Display for Lit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Lit::Float(fl) => write!(f, "{}", fl),
            Lit::Int(i) => write!(f, "{}", i),
            Lit::Ident(ident) => write!(f, "{}", ident),
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Atom(lit) => write!(f, "{}", lit),
            Expression::Operation(head, tail) => {
                write!(f, "({}", head)?; // Write opening paren + operator
                for s in tail {
                    // Iterate through operands
                    write!(f, " {}", s)? // Write space + operand (recursive)
                }
                write!(f, ")") // Write closing paren
            }
        }
    }
}

impl Expression {
    pub fn from_str(input_str: &str) -> Result<Expression, ParseError> {
        let mut lexer = match Lexer::new(input_str) {
            Ok(lexer) => lexer,
            Err(e) => {
                return Err(ParseError::UnexpectedToken(format!(
                    "Failed to create lexer: {}",
                    e
                )))
            }
        };
        parse_expression(&mut lexer, 0)
    }
    #[allow(unused)]
    pub fn is_asign(&self) -> Option<(String, &Expression)> {
        match self {
            Expression::Atom(_) => return None,
            Expression::Operation(c, operands) => {
                if *c == '=' {
                    let var_name = match operands.first().unwrap() {
                        Expression::Atom(Lit::Ident(name)) => name.clone(),
                        _ => unreachable!("Expected variable name on left side of assignment"),
                    };
                    return Some((var_name, operands.last().unwrap()));
                }
                return None;
            }
        }
    }
    #[allow(unused)]
    pub fn eval(&self, variables: &HashMap<String, f32>) -> Result<f32, EvalError> {
        match self {
            Expression::Atom(lit) => match lit {
                Lit::Int(i) => return Ok(i.to_owned() as f32),
                Lit::Float(f) => return Ok(f.to_owned() as f32),
                Lit::Ident(var) => match variables.get(var) {
                    Some(value) => Ok(*value),
                    None => Err(EvalError::UndefinedVariable(var.clone())),
                },
                _ => Err(EvalError::InvalidExpression(format!(
                    "Invalid literal: {}",
                    lit
                ))),
            },
            Expression::Operation(operator, operands) => {
                match operands.first().unwrap().eval(variables) {
                    Ok(lhs) => match operands.last().unwrap().eval(variables) {
                        Ok(rhs) => match operator {
                            '+' => Ok(lhs + rhs),
                            '-' => Ok(lhs - rhs),
                            '*' => Ok(lhs * rhs),
                            '/' => Ok(lhs / rhs),
                            '^' => Ok(lhs.powf(rhs)),
                            '√' => Ok(rhs.powf(1.0 / (lhs))),
                            op => Err(EvalError::InvalidExpression(format!(
                                "Unknown operator: {}",
                                operator
                            ))),
                        },
                        Err(e) => Err(e),
                    },
                    Err(e) => Err(e),
                }
            }
        }
    }
}

impl Lexer {
    fn new(input: &str) -> Result<Lexer, LexError> {
        let mut tokens = Vec::new();
        let mut chars = input
            .chars()
            .filter(|c| !c.is_ascii_whitespace())
            .peekable();

        while let Some(ch) = chars.next() {
            match ch {
                // ========================================
                // Is digit
                // ========================================
                c if c.is_ascii_digit() => {
                    let mut buf = String::new();
                    buf.push(c);

                    // keep reading as long as its a digit
                    // literalst cannot contain numbers first
                    let mut is_float = false;
                    while matches!(chars.peek(), Some(digit) if digit.is_ascii_digit() || *digit == '.')
                    {
                        if *chars.peek().unwrap() == '.' {
                            if is_float {
                                return Err(LexError::InvalidFloat(format!(
                                    "Invalid float literal: {}",
                                    buf
                                )));
                            }
                            is_float = true;
                        }
                        buf.push(chars.next().unwrap());
                    }

                    // edge-case: if floating literal ends with a dot
                    if is_float && buf.ends_with('.') {
                        return Err(LexError::InvalidFloat(format!(
                            "Invalid float literal: {}",
                            buf
                        )));
                    }

                    // TODO: check for double dot
                    let lit = if is_float {
                        Lit::Float(buf.parse::<f64>().unwrap())
                    } else {
                        Lit::Int(buf.parse::<i64>().unwrap())
                    };
                    tokens.push(Token::Literal(lit));
                }
                // ========================================
                // Is alphabetic
                // ========================================
                c if (c.is_ascii_alphabetic() || c == '_') => {
                    let mut buf = String::new();
                    buf.push(c);
                    while matches!(chars.peek(), Some(c) if c.is_ascii_alphanumeric() || *c == '_')
                    {
                        buf.push(chars.next().unwrap());
                    }

                    // Now we have a buffer of alphanumeric characters like 'foo123'
                    let lit = Lit::Ident(buf);
                    tokens.push(Token::Literal(lit));
                }
                // ========================================
                // Is operator
                // ========================================
                op => {
                    tokens.push(Token::Op(op));
                }
            }
        }

        tokens.reverse(); // reverse to use pop() as next()
        Ok(Lexer { tokens })
    }

    fn next(&mut self) -> Token {
        self.tokens.pop().unwrap_or(Token::Eof)
    }

    fn peek(&mut self) -> Token {
        self.tokens.last().cloned().unwrap_or(Token::Eof)
    }
}

fn infix_binding_power(op: char) -> Result<(u8, u8), LexError> {
    // get the precedence (order) and side association
    let (prec, assoc) = match op {
        '=' => (1, Assoc::Right),
        '+' | '-' => (2, Assoc::Left),
        '*' | '/' => (3, Assoc::Left),
        '^' | '√' => (4, Assoc::Right),
        _ => {
            return Err(LexError::InvalidOperator(format!(
                "Unknown operator: {:?}",
                op
            )))
        }
    };

    match assoc {
        Assoc::Left => Ok((prec, prec + 1)),
        Assoc::Right => Ok((prec + 1, prec)),
    }
}

fn parse_expression(lexer: &mut Lexer, min_bp: u8) -> Result<Expression, ParseError> {
    // First token must be an atom
    let mut lhs = match lexer.next() {
        Token::Literal(lit) => match lit {
            Lit::Int(_) => Expression::Atom(lit),
            Lit::Float(_) => Expression::Atom(lit),
            Lit::Ident(ident) => Expression::Atom(Lit::Ident(ident)),
        },
        Token::Op('(') => {
            // ensure that the function returned because it encountered a ')' not EOF
            let lhs = parse_expression(lexer, 0)?;
            assert_eq!(lexer.next(), Token::Op(')'));
            lhs
        }
        t => return Err(ParseError::UnexpectedToken(format!("Bad token: {:?}", t))),
    };

    loop {
        let op: char = match lexer.peek() {
            Token::Eof => break,
            Token::Op(')') => break, // end of expression, finsh parsing
            Token::Op(op) => op,
            t => return Err(ParseError::UnexpectedToken(format!("Bad token: {:?}", t))),
        };
        let (left_bp, right_bp) =
            infix_binding_power(op).map_err(|e| ParseError::UnexpectedToken(e.to_string()))?;
        if left_bp < min_bp {
            break;
        }
        lexer.next();
        let rhs = parse_expression(lexer, right_bp)?;
        lhs = Expression::Operation(op, vec![lhs, rhs]);
    }
    Ok(lhs)
}

// An example of simple parser that does not use precedence nor recursion
// fn parse_expression(lexer: &mut Lexer) -> Expression {
//     // First token must be an atom
//     let mut lhs = match lexer.next() {
//         Token::Atom(c) => Expression::Atom(c),
//         t => panic!("Bad token: {:?}", t),
//     };

//     // After first parse, check if there are more
//     let op = match lexer.peek() {
//         Token::Op(op) => op,
//         Token::Eof => return lhs,
//         t => panic!("Bad token: {:?}", t),
//     };
//     lexer.next();

//     let mut rhs = match lexer.next() {
//         Token::Atom(c) => Expression::Atom(c),
//         t => panic!("Bad token: {:?}", t),
//     };

//     Expression::Operation(op, vec![lhs, rhs])
// }

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_1() {
        let s = Expression::from_str("1").unwrap();
        assert_eq!(s.to_string(), "1");
    }

    #[test]
    fn test_2() {
        let s = Expression::from_str("1 + 2 * 3").unwrap();
        assert_eq!(s.to_string(), "(+ 1 (* 2 3))");
    }

    #[test]
    fn test_3() {
        let s = Expression::from_str("(a + b) * c").unwrap();
        assert_eq!(s.to_string(), "(* (+ a b) c)");
    }

    #[test]
    fn test_4() {
        let s = Expression::from_str("2 ^ 3 ^ 2").unwrap();
        let variables: HashMap<String, f32> = HashMap::new();
        assert_eq!(s.eval(&variables).unwrap(), 512.0);
    }

    #[test]
    fn test_5() {
        let s = Expression::from_str("(a + b) / c * b").unwrap();
        assert_eq!(s.to_string(), "(* (/ (+ a b) c) b)");
    }

    #[test]
    fn test_10() {
        let s = Expression::from_str("a + 2 √ 4 * b").unwrap();
        println!("{}", s.to_string());
        assert_eq!(s.to_string(), "(+ a (* (√ 2 4) b))");
    }

    #[test]
    fn precedence_power_over_multiply() {
        let s = Expression::from_str("2 * 3 ^ 4").unwrap();
        assert_eq!(s.to_string(), "(* 2 (^ 3 4))");
    }

    #[test]
    fn invalid_token_err_panic() {
        let s = Expression::from_str("2 $ 5");
        assert!(s.is_err());
    }

    #[test]
    fn multy_digit_operations() {
        let s = Expression::from_str("12 + 30").unwrap();
        let variables: HashMap<String, f32> = HashMap::new();
        assert_eq!(s.eval(&variables).unwrap(), 42.0);
    }

    #[test]
    fn floating_point_operations() {
        let s = Expression::from_str("12.5 + 1").unwrap();
        let variables: HashMap<String, f32> = HashMap::new();
        assert_eq!(s.eval(&variables).unwrap(), 13.5);
    }

    #[test]
    fn floating_literal_with_double_dot_panics() {
        let s = Expression::from_str("12.5.1 + 1");
        assert!(s.is_err());
    }

    #[test]
    fn invalid_digit_literal_single_dot() {
        let s = Expression::from_str("12. + 1");
        assert!(s.is_err());
    }

    #[test]
    fn floating_literal_with_consecutive_dots_panics() {
        let s = Expression::from_str("12..5 + 1");
        assert!(s.is_err());
    }

    #[test]
    fn exponent_inside_root_precedence() {
        let s = Expression::from_str("4√(2 ^ 4)").unwrap();
        let variables: HashMap<String, f32> = HashMap::new();
        assert_eq!(s.eval(&variables).unwrap(), 2.0);
    }

    #[test]
    fn variable_starts_with_underscore() {
        let s = Expression::from_str("_var + 1").unwrap();
        let variables: HashMap<String, f32> =
            [("_var".to_string(), 10.0)].iter().cloned().collect();
        assert_eq!(s.eval(&variables).unwrap(), 11.0);
    }
}
