mod parser;
use std::io;
use std::io::Write;

fn main() {
    loop {
        print!(">> ");
        io::stdout().flush().unwrap();
        let input = {
            let mut buf = String::new();
            std::io::stdin().read_line(&mut buf).unwrap();
            buf
        };

        if input.trim() == "exit" {
            break;
        }

        let expr = parser::Expression::from_str(&input);
        println!("{}", expr.to_string());
    }
}
