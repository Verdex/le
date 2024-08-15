
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
        parameters : Vec<Ast>,
        return_type : Box<Ast>,
        body : Vec<Ast>,
    },
}


thread_local!{
    static RULE : RefCell<Rc<Rule<Token, Ast>>> = RefCell::new(init_rules());
}

pub fn parse(input : Vec<Token>) -> Result<Vec<Ast>, Box<dyn Error>> {
    Ok(RULE.with_borrow(|rule| jerboa::parse(&input, Rc::clone(rule)))?)
}

fn ret<'a>(mut results : Vec<Capture<'a, Token, Ast>>) -> Result<Ast, JerboaError> {
    Ok(results.remove(0).unwrap_result().unwrap())
}

macro_rules! proj {
    ($input:expr, $p:pat, $e:expr) => { 
        match $input {
            $p => $e,
            _ => unreachable!(),
        }
    }
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

fn init_rules() -> Rc<Rule<Token, Ast>> {

    // TODO: generator: yield and halt

    // TODO: test that this function isn't being called for each parse invocation
    // maybe with some sort of manual pause

    let ttype = ttype_rule();

    let fun_param = Rule::new( "fun_param"
                             , vec![ pred_match!(Token::Symbol(_, _, _))
                                   , pred_match!(Token::Colon(_))
                                   , Match::rule(&ttype) 
                                   ]
                             , transform!(name, _, ttype, {
                                    let name = proj!( name.unwrap().unwrap(), Token::Symbol(n, _, _), n.clone() );
                                    let ttype = Box::new(ttype.unwrap_result().unwrap());
                                    Ok(Ast::Slot { name, ttype })
                             }));

    let comma_fun_param = comma_gen( "comma_fun_param", Match::rule(&fun_param) );

    let number = Rule::new( "number"
                          , vec![pred_match!(Token::Number(_, _, _))]
                          , transform!(result, {
                                proj!( result.unwrap().unwrap()
                                     , Token::Number(n, _, _)
                                     , Ok(Ast::Number(n.clone()))
                                     )
                          }));
                        

    let top_level_redirect = Rule::new( "top_level_redirect"
                                      , vec![Match::late(0)]
                                      , transform!(result, { Ok(result.unwrap_result().unwrap()) })
                                      );

    let fun = Rule::new( "fun" 
                       , vec![ is_keyword!("fun") 
                             , pred_match!(Token::Symbol(_, _, _))
                             , pred_match!(Token::LParen(_))
                             , Match::option(&fun_param)
                             , Match::list(&comma_fun_param)
                             , pred_match!(Token::RParen(_))
                             , pred_match!(Token::RArrow(_, _))
                             , Match::rule(&ttype)
                             , pred_match!(Token::LCurl(_))
                             , Match::list(&top_level_redirect)
                             , pred_match!(Token::RCurl(_))
                             ]

                       , transform!(_, name, _, opt_param, list_param, _, _, ret_type, _, body, _, {

                            let name = proj!(name.unwrap().unwrap(), Token::Symbol(n, _, _), n.clone());
                            let opt_param = opt_param.unwrap_option().unwrap();
                            let mut parameters = list_param.unwrap_list().unwrap();
                            if opt_param.is_some() {
                                parameters.insert(0, opt_param.unwrap());
                            }

                            let return_type = Box::new(ret_type.unwrap_result().unwrap());
                            let body = body.unwrap_list().unwrap();

                            Ok(Ast::Function { name, parameters, return_type, body })
                       }));
                       

    let expr = Rule::new("expr", vec![Match::choice(&[&number])], ret);

    let top_level = Rule::new("top_level", vec![Match::choice(&[&fun, &expr])], ret);

    top_level_redirect.bind(&[&top_level]);

    top_level
}

fn ttype_rule() -> Rc<Rule<Token, Ast>> {

    let simple_type = Rule::new( "simple_type"
                               , vec![pred_match!(Token::Symbol(_, _, _))]
                               , |mut results| proj!( results.remove(0).unwrap().unwrap()
                                                    , Token::Symbol(n, _, _)
                                                    , Ok(Ast::SimpleType(n.clone()))
                                                    )
                               );

    let ttype_redirect = Rule::new( "type_redirect"
                                  , vec![Match::late(0)]
                                  , transform!(result, { Ok(result.unwrap_result().unwrap()) })
                                  );
    
    let comma_ttype = comma_gen("comma_type", Match::rule(&ttype_redirect));

    let index_type = Rule::new( "index_type"
                              , vec![ pred_match!(Token::Symbol(_, _, _)) 
                                    , pred_match!(Token::LAngle(_))
                                    , Match::option(&comma_ttype)
                                    , Match::list(&comma_ttype)
                                    , pred_match!(Token::RAngle(_))
                                    ]
                              , transform!( name, opt_param, list_param, {
                                    let name = proj!(name.unwrap().unwrap(), Token::Symbol(n, _, _), n.clone());
                                    let opt_param = opt_param.unwrap_option().unwrap();
                                    let mut params = list_param.unwrap_list().unwrap();
                                    if opt_param.is_some() {
                                        params.insert(0, opt_param.unwrap());
                                    }
                                    Ok(Ast::IndexType{ name, params })
                              }));

    let ttype = Rule::new( "type"
                         , vec![Match::choice(&[&index_type, &simple_type])]
                         , ret 
                         );
    
    ttype_redirect.bind(&[&ttype]);

    ttype
}

fn comma_gen(name : &'static str, m : Match<Token, Ast>) -> Rc<Rule<Token, Ast>> {
    Rule::new( name
             , vec![pred_match!(Token::Comma(_)), m]
             , transform!(_, x, {
                Ok(x.unwrap_result().unwrap())     
             }))
}

#[cfg(test)] 
mod test {
    use super::*;
    use super::super::lexer;

    #[test]
    fn should_parse_zero_param_fun() {
        let s = "fun name() -> T3 { }";
        let input = lexer::lex(&mut s.char_indices()).unwrap();
        let mut output = parse(input).unwrap();
        assert_eq!(output.len(), 1);

        let (name, params, ret_type, body) = proj!( output.remove(0)
                                                  , Ast::Function { name, parameters, return_type, body }
                                                  , (name, parameters, return_type, body)
                                                  );
        assert_eq!(name, "name".into());
        assert_eq!(params.len(), 0);
        assert_eq!(body.len(), 0);

        let ret_type = proj!(*ret_type, Ast::SimpleType(n), n);
        assert_eq!(ret_type, "T3".into());
    }

    #[test]
    fn should_parse_single_param_fun() {
        let s = "fun name(x : T) -> T3 { }";
        let input = lexer::lex(&mut s.char_indices()).unwrap();
        let mut output = parse(input).unwrap();
        assert_eq!(output.len(), 1);

        let (name, mut params, ret_type, body) = proj!( output.remove(0)
                                                      , Ast::Function { name, parameters, return_type, body }
                                                      , (name, parameters, return_type, body)
                                                      );
        assert_eq!(name, "name".into());
        assert_eq!(params.len(), 1);

        let (p_name, p_type) = proj!( params.remove(0)
                                    , Ast::Slot{ name, ttype }
                                    , (name, proj!(*ttype, Ast::SimpleType(n), n.clone()))
                                    );
        assert_eq!(p_name, "x".into());
        assert_eq!(p_type, "T".into());

        assert_eq!(body.len(), 0);

        let ret_type = proj!(*ret_type, Ast::SimpleType(n), n);
        assert_eq!(ret_type, "T3".into());
    }

    #[test]
    fn should_parse_fun() {
        let s = "fun name(x : T1, y : T2) -> T3 { }";
        let input = lexer::lex(&mut s.char_indices()).unwrap();
        let mut output = parse(input).unwrap();
        assert_eq!(output.len(), 1);

        let (name, mut params, ret_type, body) = proj!( output.remove(0)
                                                      , Ast::Function { name, parameters, return_type, body }
                                                      , (name, parameters, return_type, body)
                                                      );
        assert_eq!(name, "name".into());
        assert_eq!(params.len(), 2);

        let (p_name, p_type) = proj!( params.remove(0)
                                    , Ast::Slot{ name, ttype }
                                    , (name, proj!(*ttype, Ast::SimpleType(n), n.clone()))
                                    );
        assert_eq!(p_name, "x".into());
        assert_eq!(p_type, "T1".into());

        let (p_name, p_type) = proj!( params.remove(0)
                                    , Ast::Slot{ name, ttype }
                                    , (name, proj!(*ttype, Ast::SimpleType(n), n.clone()))
                                    );
        assert_eq!(p_name, "y".into());
        assert_eq!(p_type, "T2".into());

        assert_eq!(body.len(), 0);

        let ret_type = proj!(*ret_type, Ast::SimpleType(n), n);
        assert_eq!(ret_type, "T3".into());
    }

    #[test]
    fn should_parse_number() {
        let input = lexer::lex(&mut "100".char_indices()).unwrap();
        let mut output = parse(input).unwrap();
        assert_eq!(output.len(), 1);

        let output = proj!(output.remove(0), Ast::Number(v), v);
        assert_eq!(output, "100".into());
    }

}