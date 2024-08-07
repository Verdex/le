
use std::error::Error;
use std::cell::RefCell;
use std::rc::Rc;

use dealize::jerboa::{self, Rule, Match, Capture, JerboaError};

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
    Never,
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

    // TODO: test that this function isn't being called for each parse invocation
    // maybe with some sort of manual pause


    fn ret<'a>(mut results : Vec<Capture<'a, Token, Ast>>) -> Result<Ast, JerboaError> {
        Ok(results.remove(0).unwrap_result().unwrap())
    }

    macro_rules! lets {
        ($target:ident, $body:block) => { return $body; };
        ($target:ident, $n:ident, $($rest:tt)*) => {
            let $n = $target.remove(0);
            lets!($target, $($rest)*);
        };
        ($target:ident, _, $($rest:tt)*) => {
            $target.remove(0);
            lets!($target, $($rest)*);
        };
    }

    macro_rules! transform {
        ($($input:tt)*) => { |mut results| { lets!(results, $($input)*); } }
    }

    macro_rules! pred_match {
        ($p:pat) => { Match::pred(|x, _| matches!(x, $p)) }
    }

    macro_rules! is_keyword {
        ($name:expr) => { Match::pred(|x, _| 
            match x { 
                Token::Symbol(name, _, _) if **name == *$name => true,
                _ => false,
            }) 
        }
    }

    macro_rules! proj {
        ($input:expr, $p:pat, $e:expr) => { 
            match $input {
                $p => $e,
                _ => unreachable!(),
            }
        }
    }

    let simple_type = Rule::new( "simple_type"
                               , vec![pred_match!(Token::Symbol(_, _, _))]
                               , |mut results| proj!( results.remove(0).unwrap().unwrap()
                                                    , Token::Symbol(n, _, _)
                                                    , Ok(Ast::SimpleType(n.clone()))
                                                    )
                               );
    // TODO: index type

    let ttype = Rule::new( "type"
                         , vec![Match::choice(&[&simple_type])]
                         , ret 
                         );

    let end_paren = Rule::new( "end_paren"
                             , vec![ Match::pred(|x, _| matches!(x, Token::RParen(_)))]
                             , |_| Ok(Ast::Never)
                             );

    let fun_param = Rule::new( "fun_param"
                             , vec![ Match::pred(|x, _| matches!(x, Token::Symbol(_, _, _)))
                                   , Match::pred(|x, _| matches!(x, Token::Colon(_)))
                                   , Match::rule(&ttype) 
                                   ]
                             , transform!(name, _, ttype, {
                                    let name = proj!( name.unwrap().unwrap(), Token::Symbol(n, _, _), n.clone() );
                                    let ttype = Box::new(ttype.unwrap_result().unwrap());
                                    Ok(Ast::Slot { name, ttype })
                             }));

    let number = Rule::new( "number"
                          , vec![Match::pred(|x, _| matches!(x, Token::Number(_, _, _)))]
                          , transform!(result, {
                                proj!( result.unwrap().unwrap()
                                     , Token::Number(n, _, _)
                                     , Ok(Ast::Number(n.clone()))
                                     )
                          }));
                        

    let fun = Rule::new( "fun" 
                       , vec![ is_keyword!("fun") 
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

    let expr = Rule::new("expr", vec![Match::choice(&[&number])], ret);

    //Rule::new("top_level", vec![Match::choice(&[&fun, &expr])], ret)
    fun_param
}


#[cfg(test)] 
mod test {
    use super::*;
    use super::super::lexer;

    #[test]
    fn blarg() {
        let mut s = "blah : SomeType1".char_indices();
        let input = lexer::lex(&mut s).unwrap();
        let output = parse(input).unwrap();
        println!("{:?}", output);
    }

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