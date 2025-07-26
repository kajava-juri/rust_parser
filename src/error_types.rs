#[derive(PartialEq, Debug)]
pub enum LexError {
    InvalidFloat(String),
    InvalidInt(String),
    InvalidIdentifier(String),
    InvalidOperator(String),
}

impl std::fmt::Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexError::InvalidFloat(s) => write!(f, "Invalid float: {}", s),
            LexError::InvalidInt(s) => write!(f, "Invalid integer: {}", s),
            LexError::InvalidIdentifier(s) => write!(f, "Invalid identifier: {}", s),
            LexError::InvalidOperator(s) => write!(f, "Invalid operator: {}", s),
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum ParseError {
    UnexpectedToken(String),
    UnmatchedParenthesis,
    InvalidAssignment(String),
    DivisionByZero,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::UnexpectedToken(s) => write!(f, "Unexpected token: {}", s),
            ParseError::UnmatchedParenthesis => write!(f, "Unmatched parenthesis"),
            ParseError::InvalidAssignment(s) => write!(f, "Invalid assignment: {}", s),
            ParseError::DivisionByZero => write!(f, "Division by zero"),
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum EvalError {
    UndefinedVariable(String),
    DivisionByZero,
    InvalidExpression(String),
}

impl std::fmt::Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EvalError::UndefinedVariable(s) => write!(f, "Undefined variable: {}", s),
            EvalError::DivisionByZero => write!(f, "Division by zero"),
            EvalError::InvalidExpression(s) => write!(f, "Invalid expression: {}", s),
        }
    }
}