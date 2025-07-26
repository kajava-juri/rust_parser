mod parser;
pub mod error_types;
use std::collections::HashMap;
use std::io;
use std::io::Write;
use std::env;

// Credit to Core Dumped YouTube channel
// https://www.youtube.com/watch?v=0c8b7YfsBKs

fn main() {
    env::set_var("RUST_BACKTRACE", "full");

    let mut variables: HashMap<String, f32> = HashMap::new();
    loop {
        print!(">> ");
        io::stdout().flush().unwrap();
        let input = {
            let mut buf = String::new();
            std::io::stdin().read_line(&mut buf).unwrap();
            buf
        };

        if input.trim() == "exit" || input.trim() == "q" {
            break;
        }

        let expr = match parser::Expression::from_str(&input) {
            Ok(expr) => expr,
            Err(e) => {
                eprintln!("Error parsing expression ({}): {}", input.trim(), e);
                continue;
            }
        };
        if let Some((var_name, lhs)) = expr.is_asign() {
            let value = match lhs.eval(&variables) {
                Ok(val) => val,
                Err(e) => {
                    eprintln!("Error evaluating expression: {}", e);
                    continue;
                }
            };
            variables.insert(var_name, value);
            continue;
        }
        let value = match expr.eval(&variables) {
            Ok(val) => val,
            Err(e) => {
                eprintln!("Error evaluating expression ({}): {}", expr.to_string(), e.to_string());
                continue;
            }
        };
        println!("Value: {}", value);
        println!("Expression: {}", expr.to_string());
        //println!("{}", expr.to_string());
    }
}
