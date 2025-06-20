mod parser;
use std::collections::HashMap;
use std::io;
use std::io::Write;

// Credit to Core Dumped YouTube channel
// https://www.youtube.com/watch?v=0c8b7YfsBKs

fn main() {
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

        let expr = parser::Expression::from_str(&input);
        if let Some((var_name, lhs)) = expr.is_asign() {
            let value = lhs.eval(&variables);
            variables.insert(var_name, value);
            continue;
        }
        let value = expr.eval(&variables);
        println!("Value: {}", value);
        println!("Expression: {}", expr.to_string());
        //println!("{}", expr.to_string());
    }
}
