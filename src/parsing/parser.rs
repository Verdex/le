
use std::error::Error;
use jlnexus::Parser;
use crate::data::{ Token, Ast };


macro_rules! proj {
    ($input:ident, $target:pat, $e:expr) => {
        match $input.get(ParseError::UnexpectedEof)? {
            $target => Ok($e),
            t => Err(ParseError::UnexpectedToken(t.clone())),
        }
    };
    ($input:ident, $target:pat if $p:expr, $e:expr) => {
        match $input.get(ParseError::UnexpectedEof)? {
            $target if $p => Ok($e),
            t => Err(ParseError::UnexpectedToken(t.clone())),
        }
    };
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken(Token),
    UnexpectedEof,
    Aggregate(Vec<ParseError>),
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, _f : &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            //ParseError::?? => write!(f, "", ... ),
            _ => todo!(),
        }
    }
}

impl Error for ParseError { }

impl From<Vec<ParseError>> for ParseError{
    fn from(item : Vec<ParseError>) -> Self {
        ParseError::Aggregate(item)
    }
}

pub fn parse(input : Vec<Token>) -> Result<Vec<Ast>, ParseError> {
    let mut buffer = Parser::new(&input);
    let mut top_level = vec![];

    while !buffer.end() {

        let item = buffer.or(
            [
                fun,
                expr
            ])?;
        top_level.push(item);
    }

    Ok(top_level)
}

fn type_sig(input : &mut Parser<Token>) -> Result<Ast, ParseError> {
    fn simple(input : &mut Parser<Token>) -> Result<Ast, ParseError> {
        match input.get(ParseError::UnexpectedEof)? {
            Token::Symbol(s, _) => Ok(Ast::SimpleType(Box::new(Ast::Symbol(s.clone())))),
            t => Err(ParseError::UnexpectedToken(t.clone())),
        }
    }
    fn index_end(input : &mut Parser<Token>) -> Result<Ast, ParseError> {
        input.with_rollback(|input| {
            proj!(input, Token::LAngle(_), ())?;
            let first = type_sig(input)?;
            let mut rest = input.list(|input| {
                proj!(input, Token::Comma(_), ())?;
                type_sig(input)
            })?;
            proj!(input, Token::RAngle(_), ())?;
            rest.insert(0, first);
            Ok(Ast::SyntaxList(rest))
        })
    }

    // TODO a bunch of the stuff here can be cleaned up after syntax list is removed
    let s = simple(input)?;
    match (s, index_end(input)) {
        (Ast::SimpleType(n), Ok(index)) => Ok(Ast::IndexType{ name: Box::new(*n), params: Box::new(index) }),
        (s, _) => Ok(s),
    }
}

fn fun(input : &mut Parser<Token>) -> Result<Ast, ParseError> {
    fn param(input : &mut Parser<Token>) -> Result<Ast, ParseError> {
        let n = proj!(input, Token::Symbol(n, _), Ast::Symbol(n.clone()))?;
        proj!(input, Token::Colon(_), ())?;
        let t = type_sig(input)?;
        Ok(Ast::Slot { name: Box::new(n), ttype: Box::new(t) })
    }

    proj!(input, Token::Symbol(n, _) if **n == *"fun", ())?;
    let name = proj!(input, Token::Symbol(n, _), Ast::Symbol(n.clone()))?;
    proj!(input, Token::LParen(_), ())?;
    let params = match input.option(|input| param(input))? {
        Some(first) => {
            let mut rest = input.list(|input| {
                proj!(input, Token::Comma(_), ())?;
                param(input)
            })?;
            rest.insert(0, first);
            rest
        },
        None => vec![],
    };
    proj!(input, Token::RParen(_), ())?;
    proj!(input, Token::RArrow(_), ())?;
    let t = type_sig(input)?;
    proj!(input, Token::LCurl(_), ())?;
    let e = expr(input)?;
    proj!(input, Token::RCurl(_), ())?;

    Ok(Ast::Function { 
        name: Box::new(name), 
        params: Box::new(Ast::SyntaxList(params)),
        return_type: Box::new(t),
        body: Box::new(e),
    })
}

fn expr(input : &mut Parser<Token>) -> Result<Ast, ParseError> {
    fn number(input : &mut Parser<Token>) -> Result<Ast, ParseError> {
        proj!(input, Token::Number(n, _), Ast::Number(n.clone()))
    }
    fn symbol(input : &mut Parser<Token>) -> Result<Ast, ParseError> {
        proj!(input, Token::Symbol(s, _), Ast::Variable(Box::new(Ast::Symbol(s.clone()))))
    }
    fn call(input : &mut Parser<Token>) -> Result<Box<dyn FnOnce(Ast) -> Ast>, ParseError> {
        proj!(input, Token::LParen(_), ())?;
        let params = match input.option(|input| expr(input))? {
            Some(first) => {
                let mut rest = input.list(|input| {
                    proj!(input, Token::Comma(_), ())?;
                    expr(input)
                })?;
                rest.insert(0, first);
                rest
            },
            None => vec![],
        };
        proj!(input, Token::RParen(_), ())?;
        Ok(Box::new(move |x| Ast::Call { fun_expr: Box::new(x), inputs: Box::new(Ast::SyntaxList(params)) }))
    }
    
    let mut e = input.or([number, symbol])?;

    loop {
        match input.or([call]) {
            Ok(follow) => {
                e = follow(e);
            },
            Err(_) => { break; },
        }
    }

    Ok(e)
}

#[cfg(test)] 
mod test {
    use dealize::pattern::*;

    use super::*;
    use super::super::lexer;

    macro_rules! proj {
        ($input:expr, $p:pat, $e:expr) => { 
            match $input {
                $p => $e,
                _ => unreachable!(),
            }
        }
    }
    // TODO: top level items defined in function test

    // TODO: let results = results.remove(0).into_iter().collect::<HashMap<Box<str>, &Ast>>();

    #[test]
    fn should_parse_fun_with_complex_index_type() {
        let s = "fun name() -> T3<T1<T4>, T2, T5> { z }";
        let input = lexer::lex(s.into()).unwrap();
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
                                        , cons("variable", &[atom("z".into())])
                                        ]);
        let results = find(pattern, &output).collect::<Vec<_>>();
        
        assert_eq!(results.len(), 1);
    }

    #[test]
    fn should_parse_fun_with_index_type() {
        let s = "fun name() -> T3<T1> { z }";
        let input = lexer::lex(s.into()).unwrap();
        let mut output = parse(input).unwrap();
        assert_eq!(output.len(), 1);

        let output = output.remove(0);

        let pattern = cons("function", &[ atom("name".into())
                                        , exact_list(&[])
                                        , cons("index-type", &[ atom("T3".into())
                                                              , exact_list(&[cons("simple-type", &[atom("T1".into())])])
                                                              ])
                                        , cons("variable", &[atom("z".into())])
                                        ]);
        let results = find(pattern, &output).collect::<Vec<_>>();
        
        assert_eq!(results.len(), 1);
    }

    #[test]
    fn should_parse_zero_param_fun() {
        let s = "fun name() -> T3 { z }";
        let input = lexer::lex(s.into()).unwrap();
        let mut output = parse(input).unwrap();
        assert_eq!(output.len(), 1);

        let output = output.remove(0);

        let pattern = cons("function", &[ atom("name".into())
                                        , exact_list(&[])
                                        , cons("simple-type", &[atom("T3".into())])
                                        , cons("variable", &[atom("z".into())])
                                        ]);
        let results = find(pattern, &output).collect::<Vec<_>>();
        
        assert_eq!(results.len(), 1);
    }

    #[test]
    fn should_parse_single_param_fun() {
        let s = "fun name(x : T) -> T3 { x }";
        let input = lexer::lex(s.into()).unwrap();
        let mut output = parse(input).unwrap();
        assert_eq!(output.len(), 1);

        let output = output.remove(0);

        let pattern = cons("function", &[ atom("name".into())
                                        , exact_list(&[cons("slot", &[atom("x".into()), cons("simple-type", &[atom("T".into())])])])
                                        , cons("simple-type", &[atom("T3".into())])
                                        , cons("variable", &[atom("x".into())])
                                        ]);
        let results = find(pattern, &output).collect::<Vec<_>>();

        assert_eq!(results.len(), 1);
    }
    
    #[test]
    fn should_parse_fun() {
        let s = "fun name(x : T1, y : T2) -> T3 { x }";
        let input = lexer::lex(s.into()).unwrap();
        let mut output = parse(input).unwrap();
        assert_eq!(output.len(), 1);

        let output = output.remove(0);

        let pattern = cons("function", &[ atom("name".into())
                                        , exact_list(&[ cons("slot", &[atom("x".into()), cons("simple-type", &[atom("T1".into())])])
                                                      , cons("slot", &[atom("y".into()), cons("simple-type", &[atom("T2".into())])])
                                                      ])
                                        , cons("simple-type", &[atom("T3".into())])
                                        , cons("variable", &[atom("x".into())])
                                        ]);
        let results = find(pattern, &output).collect::<Vec<_>>();

        assert_eq!(results.len(), 1);
    }

    #[test]
    fn should_parse_variable() {
        let s = "var";
        let input = lexer::lex(s.into()).unwrap();
        let mut output = parse(input).unwrap();
        assert_eq!(output.len(), 1);

        let output = output.remove(0);

        let pattern = cons("variable", &[atom("var".into())]);

        let results = find(pattern, &output).collect::<Vec<_>>();

        assert_eq!(results.len(), 1);
    }

    #[test]
    fn should_parse_fun_call_with_complex_param() {
        let s = "blah(ah(b()), c, d()(e, i))()";
        let input = lexer::lex(s.into()).unwrap();
        let mut output = parse(input).unwrap();
        assert_eq!(output.len(), 1);

        let output = output.remove(0);

        let pattern = cons("call", &[wild(), wild()]);

        let results = find(pattern, &output).collect::<Vec<_>>();

        assert_eq!(results.len(), 1);
    }

    #[test]
    fn should_parse_fun_call_with_multiple_param() {
        let s = "blah(val, two, other)";
        let input = lexer::lex(s.into()).unwrap();
        let mut output = parse(input).unwrap();
        assert_eq!(output.len(), 1);

        let output = output.remove(0);

        let param_1 = cons("variable", &[atom("val".into())]);
        let param_2 = cons("variable", &[atom("two".into())]);
        let param_3 = cons("variable", &[atom("other".into())]);
        let pattern = cons("call", &[cons("variable", &[atom("blah".into())]), exact_list(&[param_1, param_2, param_3])]);

        let results = find(pattern, &output).collect::<Vec<_>>();

        assert_eq!(results.len(), 1);
    }

    #[test]
    fn should_parse_fun_call_with_param() {
        let s = "blah(val)";
        let input = lexer::lex(s.into()).unwrap();
        let mut output = parse(input).unwrap();
        assert_eq!(output.len(), 1);

        let output = output.remove(0);

        let param_pattern = cons("variable", &[atom("val".into())]);
        let pattern = cons("call", &[cons("variable", &[atom("blah".into())]), exact_list(&[param_pattern])]);

        let results = find(pattern, &output).collect::<Vec<_>>();

        assert_eq!(results.len(), 1);
    }

    #[test]
    fn should_parse_fun_call_call() {
        let s = "blah()()";
        let input = lexer::lex(s.into()).unwrap();
        let mut output = parse(input).unwrap();
        assert_eq!(output.len(), 1);

        let output = output.remove(0);

        let inner_pattern = cons("call", &[cons("variable", &[atom("blah".into())]), exact_list(&[])]);
        let pattern = cons("call", &[inner_pattern, exact_list(&[])]);

        let results = find(pattern, &output).collect::<Vec<_>>();

        assert_eq!(results.len(), 1);
    }

    #[test]
    fn should_parse_fun_call() {
        let s = "blah()";
        let input = lexer::lex(s.into()).unwrap();
        let mut output = parse(input).unwrap();
        assert_eq!(output.len(), 1);

        let output = output.remove(0);

        let pattern = cons("call", &[cons("variable", &[atom("blah".into())]), exact_list(&[])]);

        let results = find(pattern, &output).collect::<Vec<_>>();

        assert_eq!(results.len(), 1);
    }

    #[test]
    fn should_parse_number() {
        let input = lexer::lex("100".into()).unwrap();
        let mut output = parse(input).unwrap();
        assert_eq!(output.len(), 1);

        let output = proj!(output.remove(0), Ast::Number(v), v);
        assert_eq!(output, "100".into());
    }

}