
use std::error::Error;
use std::cell::RefCell;
use std::rc::Rc;

use dealize::jerboa::{self, Rule, Match};

use super::lexer::*;

#[derive(Debug)]
enum ParseError {

}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f : &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            //ParseError::?? => write!(f, "", ... ),
            _ => todo!(),
        }
    }
}

impl Error for ParseError { }

#[derive(Debug)]
pub enum Type {
    Simple(Box<str>),
    Index(Box<str>, Vec<Type>),
}

#[derive(Debug)]
pub struct Slot {
    name : Box<str>,
    ttype : Type,
}

#[derive(Debug)]
pub enum Ast {
    Number(Box<str>),
    Slot { name : Box<str>, ttype : Box<Ast> },
    SimpleType(Box<str>),
    IndexType{ name : Box<str>, params : Vec<Ast> },
    Call { 
        name : Box<str>,
        inputs : Vec<Ast>,
    },
    Function {
        name : Box<str>,
        parameters : Vec<Slot>,
        return_type : Type,
        body : Vec<Ast>,
    },
}


thread_local!{
    static RULE : RefCell<Rc<Rule<Token, Ast>>> = RefCell::new(init_rules());
}

pub fn parse(input : Vec<Token>) -> Result<Vec<Ast>, Box<dyn Error>> {
    Ok(RULE.with_borrow(|rule| jerboa::parse(&input, Rc::clone(rule)))?)
}

fn init_rules() -> Rc<Rule<Token, Ast>> {

    // TODO: generator: yield and halt

    fn symbol(target : &Token, input : &'static str) -> bool {
        match target {
            Token::Symbol(name, _, _) if **name == *input => true,
            _ => false,
        }
    }

    let number = Rule::new( "number"
                          , vec![Match::pred(|x, _| matches!(x, Token::Number(_, _, _)))]
                          , |mut results| match results.remove(0).unwrap().unwrap() {
                                        Token::Number(n, _, _) => Ok(Ast::Number(n.clone())),
                                        _ => unreachable!(),
                          });

    let expr = Rule::new("expr", vec![Match::choice(&[&number])], |mut results| Ok(results.remove(0).unwrap_result().unwrap()));

    let fun = Rule::new( "fun" 
                       , vec![ Match::pred(|x, _| symbol(x, "fun"))
                             , Match::pred(|x, _| matches!(x, Token::Symbol(_, _, _)))
                             , Match::pred(|x, _| matches!(x, Token::LParen(_)))
                             //,  TODO until RParen
                             , Match::pred(|x, _| matches!(x, Token::RParen(_)))
                             , Match::pred(|x, _| matches!(x, Token::RArrow(_, _)))
                             // TODO type
                             // TODO block
                             ]

                       , |mut results| Ok(Ast::Number("".into()))
                        
                       );

    Rule::new("top_level", vec![Match::choice(&[&fun, &expr])], |mut results| Ok(results.remove(0).unwrap_result().unwrap()))
}


#[cfg(test)] 
mod test {
    use super::*;
    use super::super::lexer;

    #[test]
    fn should_parse_fun() {
        let s = "fun name(x : T1, y : T2) -> T3 {
            x
        }";
        let input = lexer::lex(&mut s.char_indices()).unwrap();
    }

    #[test]
    fn should_parse_number() {
        let input = lexer::lex(&mut "100".char_indices()).unwrap();
        let output = parse(input).unwrap();
        assert_eq!(output.len(), 1);

        if let Ast::Number(v) = &output[0] {
            assert_eq!(*v, "100".into());
        }
        else {
            assert!(false);
        }
    }

}