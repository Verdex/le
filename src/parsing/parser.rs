
use super::lexer::*;

#[derive(Debug)]
enum ParseError {

}

#[derive(Debug)]
pub enum Ast {
    Number(Box<str>),
}

// TODO:  Use Jerboa
pub fn parse(input : Vec<Token>) -> Result<Vec<Ast>, ParseError> {
    let ret = input.into_iter().map(|x| match x {
        Token::Number(n, s, e) => Ast::Number(n),
    }).collect::<Vec<Ast>>();

    Ok(ret)
}