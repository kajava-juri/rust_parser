use core::panic;
use std::{fmt, vec};
// Credit to Core Dumped YouTube channel
// https://www.youtube.com/watch?v=0c8b7YfsBKs

#[derive(Copy, Clone, Debug, PartialEq)]
enum Token {
    Atom(char),
    Op(char),
    Eof,
}

struct Lexer {
    tokens: Vec<Token>,
}

pub enum Expression {
    Atom(char),
    Operation(char, Vec<Expression>), // e.g., Operation('+', vec![Atom('a'), Atom('b')])
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Atom(i) => write!(f, "{}", i),
            Expression::Operation(head, tail) => {
                write!(f, "({}", head)?;
                for s in tail {
                    write!(f, " {}", s)?
                }
                write!(f, ")")
            }
        }
    }
}

impl Expression {
    pub fn from_str(input_str: &str) -> Expression {
        let mut lexer = Lexer::new(input_str);
        parse_expression(&mut lexer, 0.0)
    }
}

impl Lexer {
    fn new(input: &str) -> Lexer {
        let mut tokens = input
            .chars()
            .filter(|c| !c.is_ascii_whitespace())
            .map(|c| match c {
                '0'..='9' | 'a'..='z' | 'A'..='Z' => Token::Atom(c),
                _ => Token::Op(c),
            })
            .collect::<Vec<_>>();
        tokens.reverse(); // reverse to use pop() as next()
        Lexer { tokens }
    }

    fn next(&mut self) -> Token {
        self.tokens.pop().unwrap_or(Token::Eof)
    }

    fn peek(&mut self) -> Token {
        self.tokens.last().copied().unwrap_or(Token::Eof)
    }
}

fn infix_binding_power(op: char) -> (f32, f32) {
    match op {
        '+' | '-' => (1.0, 1.1),
        '*' | '/' => (2.0, 2.1),
        '^' | '√' => (3.1, 3.0),
        _ => panic!("Unknown operator: {:?}", op),
    }
}

fn parse_expression(lexer: &mut Lexer, min_bp: f32) -> Expression {
    // First token must be an atom
    let mut lhs = match lexer.next() {
        Token::Atom(c) => Expression::Atom(c),
        Token::Op('(') => {
            // ensure that the function returned because it encountered a ')' not EOF
            let lhs = parse_expression(lexer, 0.0);
            assert_eq!(lexer.next(), Token::Op(')'));
            lhs
        },
        t => panic!("Bad token: {:?}", t),
    };

    loop {
        let op = match lexer.peek() {
            Token::Eof => break,
            Token::Op(')') => break, // end of expression, finsh parsing
            Token::Op(op) => op,
            t => panic!("Bad token: {:?}", t),
        };
        lexer.next();
        let (left_bp, right_bp) = infix_binding_power(op);
        if left_bp < min_bp {
            break;
        }
        let rhs = parse_expression(lexer, right_bp);
        lhs = Expression::Operation(op, vec![lhs, rhs]);
    }
    lhs
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
        let s = Expression::from_str("1");
        assert_eq!(s.to_string(), "1");
    }

    fn test_2() {
        let s = Expression::from_str("1 + 2 * 3");
        assert_eq!(s.to_string(), "(+ 1 (* 2 3))");
    }

    #[test]
    fn test_3() {
        let s = Expression::from_str("(a + b) * c");
        assert_eq!(s.to_string(), "(* (+ a b) c)");
    }
}
