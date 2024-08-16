
use std::error::Error;
use std::cell::RefCell;
use std::rc::Rc;

use dealize::jerboa::{self, Rule, Match, Capture, JerboaError};
use dealize::pattern::*;

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

#[derive(Debug, PartialEq)]
pub enum Ast {
    Number(Box<str>), // TODO : should probably be a symbol instead of str
    Symbol(Box<str>),
    Slot { name : Box<Ast>, ttype : Box<Ast> },
    SimpleType(Box<Ast>),
    IndexType{ name : Box<Ast>, params : Box<Ast> },
    Call { 
        name : Box<Ast>,
        inputs : Box<Ast>,
    },
    Function {
        name : Box<Ast>,
        params : Box<Ast>,
        return_type : Box<Ast>,
        body : Box<Ast>,
    },
    SyntaxList(Vec<Ast>),
}

impl Matchable for Ast {
    type Atom = Box<str>;

    fn kind<'a>(&'a self) -> MatchKind<'a, Self> {
        match self {
            Ast::Symbol(n) => MatchKind::Atom(n),
            Ast::SyntaxList(ls) => MatchKind::List(ls),
            Ast::Number(_) => MatchKind::Cons("number", vec![]),
            Ast::Slot { name, ttype } => MatchKind::Cons("slot", vec![&*name, ttype]),
            Ast::SimpleType(name) => MatchKind::Cons("simple-type", vec![&*name]),
            Ast::IndexType { name, params } => MatchKind::Cons("index-type", vec![&*name, params]),
            Ast::Call { name, inputs } => MatchKind::Cons("call", vec![&*name, inputs]),
            Ast::Function { name, params, return_type, body } => 
                MatchKind::Cons("function", vec![name, params, return_type, body]),
            _ => todo!(),
        }
    }
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
                                    let name = proj!( name.unwrap().unwrap()
                                                    , Token::Symbol(n, _, _)
                                                    , Box::new(Ast::Symbol(n.clone()))
                                                    );
                                    let ttype = Box::new(ttype.unwrap_result().unwrap());
                                    Ok(Ast::Slot { name, ttype })
                             }));

    let param_list = comma_list_gen("param_list", &fun_param);

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
                             , Match::rule(&param_list)
                             , pred_match!(Token::RParen(_))
                             , pred_match!(Token::RArrow(_, _))
                             , Match::rule(&ttype)
                             , pred_match!(Token::LCurl(_))
                             , Match::list(&top_level_redirect)
                             , pred_match!(Token::RCurl(_))
                             ]

                       , transform!(_, name, _, params, _, _, ret_type, _, body, _, {

                            let name = proj!( name.unwrap().unwrap()
                                            , Token::Symbol(n, _, _)
                                            , Box::new(Ast::Symbol(n.clone()))
                                            );
                            let params = Box::new(params.unwrap_result().unwrap());

                            let return_type = Box::new(ret_type.unwrap_result().unwrap());
                            let body = Box::new(Ast::SyntaxList(body.unwrap_list().unwrap()));

                            Ok(Ast::Function { name, params, return_type, body })
                       }));
                       

    let expr = Rule::new("expr", vec![Match::choice(&[&number])], ret);

    let top_level = Rule::new("top_level", vec![Match::choice(&[&fun, &expr])], ret);

    top_level_redirect.bind(&[&top_level]);

    top_level
}

fn ttype_rule() -> Rc<Rule<Token, Ast>> {
    // Note:  Arrow types are an option, but just doing Fun< ... > is going to
    // be just as easy and doesn't require a bunch of special handling.

    let simple_type = Rule::new( "simple_type"
                               , vec![pred_match!(Token::Symbol(_, _, _))]
                               , transform!( name, {
                                    let name = proj!( name.unwrap().unwrap()
                                                    , Token::Symbol(n, _, _)
                                                    , Box::new(Ast::Symbol(n.clone()))
                                                    );
                                    Ok(Ast::SimpleType(name))
                               }));

    let ttype_redirect = Rule::new( "type_redirect"
                                  , vec![Match::late(0)]
                                  , transform!(result, { Ok(result.unwrap_result().unwrap()) })
                                  );
    
    let type_list = comma_list_gen("type_list", &ttype_redirect);

    let index_type = Rule::new( "index_type"
                              , vec![ pred_match!(Token::Symbol(_, _, _)) 
                                    , pred_match!(Token::LAngle(_))
                                    , Match::rule(&type_list)
                                    , pred_match!(Token::RAngle(_))
                                    ]
                              , transform!( name, _, params, {
                                    let name = proj!( name.unwrap().unwrap()
                                                    , Token::Symbol(n, _, _)
                                                    , Box::new(Ast::Symbol(n.clone()))
                                                    );
                                    let params = Box::new(params.unwrap_result().unwrap());
                                    Ok(Ast::IndexType{ name, params })
                              }));

    let ttype = Rule::new( "type"
                         , vec![Match::choice(&[&index_type, &simple_type])]
                         , ret 
                         );
    
    ttype_redirect.bind(&[&ttype]);

    ttype
}

fn comma_list_gen(name : &'static str, rule : &Rc<Rule<Token, Ast>>) -> Rc<Rule<Token, Ast>> {
    let comma_rule = Rule::new( format!("comma_{}", name) 
                              , vec![pred_match!(Token::Comma(_)), Match::rule(rule)]
                              , transform!(_, x, {
                                    Ok(x.unwrap_result().unwrap())     
                              }));
    
    Rule::new( name
             , vec![Match::option(rule), Match::list(&comma_rule)]
             , transform!(opt, list, {
                    let opt = opt.unwrap_option().unwrap();
                    let mut list = list.unwrap_list().unwrap();
                    if opt.is_some() {
                        list.insert(0, opt.unwrap());
                    }
                    Ok(Ast::SyntaxList(list))
             }))
}

#[cfg(test)] 
mod test {
    use std::collections::HashMap;

    use super::*;
    use super::super::lexer;

    // TODO: top level items defined in function test

    // TODO: let results = results.remove(0).into_iter().collect::<HashMap<Box<str>, &Ast>>();

    #[test]
    fn should_parse_fun_with_complex_index_type() {
        let s = "fun name() -> T3<T1<T4>, T2, T5> { }";
        let input = lexer::lex(&mut s.char_indices()).unwrap();
        let mut output = parse(input).unwrap();
        assert_eq!(output.len(), 1);

        let output = output.remove(0);

        let first_pattern = cons("index-type", &[atom("T1".into()), exact_list(&[cons("simple-type", &[atom("T4".into())])])]);
        let second_pattern = cons("simple-type", &[atom("T2".into())]);
        let third_pattern = cons("simple-type", &[atom("T5".into())]);

        let pattern = cons("function", &[ atom("name".into())
                                        , exact_list(&[])
                                        , cons("index-type", 
                                            &[ atom("T3".into())
                                             , exact_list(&[ first_pattern, second_pattern, third_pattern ])
                                             ])
                                        , exact_list(&[])
                                        ]);
        let mut results = find(pattern, &output).collect::<Vec<_>>();
        
        assert_eq!(results.len(), 1);
    }

    #[test]
    fn should_parse_fun_with_index_type() {
        let s = "fun name() -> T3<T1> { }";
        let input = lexer::lex(&mut s.char_indices()).unwrap();
        let mut output = parse(input).unwrap();
        assert_eq!(output.len(), 1);

        let output = output.remove(0);

        let pattern = cons("function", &[ atom("name".into())
                                        , exact_list(&[])
                                        , cons("index-type", &[ atom("T3".into())
                                                              , exact_list(&[cons("simple-type", &[atom("T1".into())])])
                                                              ])
                                        , exact_list(&[])
                                        ]);
        let mut results = find(pattern, &output).collect::<Vec<_>>();
        
        assert_eq!(results.len(), 1);
    }

    #[test]
    fn should_parse_zero_param_fun() {
        let s = "fun name() -> T3 { }";
        let input = lexer::lex(&mut s.char_indices()).unwrap();
        let mut output = parse(input).unwrap();
        assert_eq!(output.len(), 1);

        let output = output.remove(0);

        let pattern = cons("function", &[ atom("name".into())
                                        , exact_list(&[])
                                        , cons("simple-type", &[atom("T3".into())])
                                        , exact_list(&[])
                                        ]);
        let mut results = find(pattern, &output).collect::<Vec<_>>();
        
        assert_eq!(results.len(), 1);
    }

    #[test]
    fn should_parse_single_param_fun2() {
        let s = "fun name(x : T) -> T3 { }";
        let input = lexer::lex(&mut s.char_indices()).unwrap();
        let mut output = parse(input).unwrap();
        assert_eq!(output.len(), 1);

        let output = output.remove(0);

        let pattern = cons("function", &[ atom("name".into())
                                        , exact_list(&[cons("slot", &[atom("x".into()), cons("simple-type", &[atom("T".into())])])])
                                        , cons("simple-type", &[atom("T3".into())])
                                        , exact_list(&[])
                                        ]);
        let mut results = find(pattern, &output).collect::<Vec<_>>();

        assert_eq!(results.len(), 1);
    }
    
    #[test]
    fn should_parse_fun() {
        let s = "fun name(x : T1, y : T2) -> T3 { }";
        let input = lexer::lex(&mut s.char_indices()).unwrap();
        let mut output = parse(input).unwrap();
        assert_eq!(output.len(), 1);

        let output = output.remove(0);

        let pattern = cons("function", &[ atom("name".into())
                                        , exact_list(&[ cons("slot", &[atom("x".into()), cons("simple-type", &[atom("T1".into())])])
                                                      , cons("slot", &[atom("y".into()), cons("simple-type", &[atom("T2".into())])])
                                                      ])
                                        , cons("simple-type", &[atom("T3".into())])
                                        , exact_list(&[])
                                        ]);
        let mut results = find(pattern, &output).collect::<Vec<_>>();

        assert_eq!(results.len(), 1);
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