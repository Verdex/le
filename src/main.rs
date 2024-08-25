
mod data;
mod parsing;
mod typing;
mod execution; 
mod eval; 

pub fn main() {
    let input = "";
    let mut input = input.char_indices();
    let tokens = parsing::lexer::lex(&mut input).unwrap();
    let ast = parsing::parser::parse(tokens);
}