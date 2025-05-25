
use std::error::Error;
use std::rc::Rc;
use jlnexus::{ Parser, JlnError };
use crate::data::{ Token, Ast, Slot, LeType };


macro_rules! proj {
    ($input:ident, $target:pat, $e:expr) => {
        match $input.get()? {
            $target => Ok($e),
            t => Err(ParseError::UnexpectedToken(t.clone())),
        }
    };
    ($input:ident, $target:pat if $p:expr, $e:expr) => {
        match $input.get()? {
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
    Fatal(Box<ParseError>)
}

trait Fatal {
    fn fatal(self) -> Self;
}

impl<T> Fatal for Result<T, ParseError> {
    fn fatal(self) -> Self {
        self.map_err(|x| ParseError::Fatal(Box::new(x)))
    }
}

impl JlnError for ParseError {
    fn is_fatal(&self) -> bool { false }
    fn eof() -> Self { ParseError::UnexpectedEof }
    fn aggregate(errors : Vec<Self>) -> Self { ParseError::Aggregate(errors) }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f : &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ParseError::UnexpectedToken(t) => write!(f, "encountered unexpected token {:?}", t),
            ParseError::UnexpectedEof => write!(f, "encountered unexpected eof"),
            ParseError::Aggregate(errors) => write!(f, "encountered error list:\n{}", 
                errors.into_iter().map(|x| format!("  {}\n", x)).collect::<Vec<_>>().join("")),
            ParseError::Fatal(error) => write!(f, "FATAL: {error}"),
        }
    }
}

impl Error for ParseError { }

pub fn parse(input : Vec<Token>) -> Result<Vec<Ast>, ParseError> {
    let mut buffer : Parser<Token> = input.into(); 
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

fn type_sig(input : &mut Parser<Token>) -> Result<LeType, ParseError> {
    fn simple(input : &mut Parser<Token>) -> Result<LeType, ParseError> {
        match input.get()? {
            Token::Symbol(s, _) => Ok(LeType::SimpleType(Rc::clone(s))),
            t => Err(ParseError::UnexpectedToken(t.clone())),
        }
    }
    fn index_end(input : &mut Parser<Token>) -> Result<Vec<LeType>, ParseError> {
        input.with_rollback(|input| {
            proj!(input, Token::LAngle(_), ())?;
            let first = type_sig(input)?;
            let mut rest = input.list(|input| {
                proj!(input, Token::Comma(_), ())?;
                type_sig(input)
            })?;
            proj!(input, Token::RAngle(_), ())?;
            rest.insert(0, first);
            Ok(rest)
        })
    }

    let s = simple(input)?;
    match (s, index_end(input)) {
        (LeType::SimpleType(n), Ok(index)) => Ok(LeType::IndexType{ name: n, params: index }),
        (s, _) => Ok(s),
    }
}

fn fun(input : &mut Parser<Token>) -> Result<Ast, ParseError> {
    fn param(input : &mut Parser<Token>) -> Result<Slot, ParseError> {
        let n = proj!(input, Token::Symbol(n, _), Rc::clone(n))?;
        proj!(input, Token::Colon(_), ())?;
        let t = type_sig(input)?;
        Ok(Slot { name: n, ttype: t })
    }

    proj!(input, Token::Symbol(n, _) if **n == *"fun", ())?;
    let name = proj!(input, Token::Symbol(n, _), Rc::clone(n))?;
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

    Ok(Ast::Fun { 
        name: name, 
        params: params,
        return_type: t,
        body: Box::new(e),
    })
}

fn expr(input : &mut Parser<Token>) -> Result<Ast, ParseError> {
    fn number(input : &mut Parser<Token>) -> Result<Ast, ParseError> {
        proj!(input, Token::Number(n, _), Ast::Number(Rc::clone(n)))
    }
    fn symbol(input : &mut Parser<Token>) -> Result<Ast, ParseError> {
        proj!(input, Token::Symbol(s, _), Ast::Var(Rc::clone(s)))
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
        Ok(Box::new(move |x| Ast::Call { fun_expr: Box::new(x), args: params }))
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
    use ordan::*;

    use super::*;
    use super::super::lexer;

    macro_rules! proj {
        ($input:expr, $p:pat, $e:expr) => { 
            match $input {
                $p => $e,
                _ => panic!("proj failed"),
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

        let (name, params, body, return_type) = proj!(output, 
            Ast::Fun { name, params, body, return_type }, 
            (name, params, *body, return_type));

        assert_eq!(name, "name".into());
        assert_eq!(params.len(), 0);
        
        let var_name = proj!(body, Ast::Var(x), x);
        assert_eq!(var_name, "z".into());

        let (index_name, mut index_params) = proj!(return_type, LeType::IndexType { name, params }, (name, params));
        assert_eq!(index_name, "T3".into());
        assert_eq!(index_params.len(), 3);

        let t5 = proj!(index_params.pop().unwrap(), LeType::SimpleType(x), x);
        assert_eq!(t5, "T5".into());

        let t2 = proj!(index_params.pop().unwrap(), LeType::SimpleType(x), x);
        assert_eq!(t2, "T2".into());
        
        let (index_name, mut index_params) = proj!(index_params.pop().unwrap(), LeType::IndexType{name, params}, (name, params));
        assert_eq!(index_name, "T1".into());

        let t4 = proj!(index_params.pop().unwrap(), LeType::SimpleType(x), x);
        assert_eq!(t4, "T4".into());
    }

    #[test]
    fn should_parse_fun_with_index_type() {
        let s = "fun name() -> T3<T1> { z }";
        let input = lexer::lex(s.into()).unwrap();
        let mut output = parse(input).unwrap();
        assert_eq!(output.len(), 1);

        let output = output.remove(0);

        let return_type = proj!(output, Ast::Fun { return_type, .. }, return_type);

        let (name, params) = proj!(return_type, LeType::IndexType { name, params}, (name, params));

        assert_eq!(name, "T3".into());
        assert_eq!(params.len(), 1);
    }

    #[test]
    fn should_parse_zero_param_fun() {
        let s = "fun name() -> T3 { z }";
        let input = lexer::lex(s.into()).unwrap();
        let mut output = parse(input).unwrap();
        assert_eq!(output.len(), 1);

        let output = output.remove(0);

        let params = proj!(output, Ast::Fun { params, .. }, params);
        assert_eq!(params.len(), 0);
    }

    #[test]
    fn should_parse_single_param_fun() {
        let s = "fun name(x : T) -> T3 { x }";
        let input = lexer::lex(s.into()).unwrap();
        let mut output = parse(input).unwrap();
        assert_eq!(output.len(), 1);

        let output = output.remove(0);

        let params = proj!(output, Ast::Fun { params, .. }, params);
        assert_eq!(params.len(), 1);
    }
    
    #[test]
    fn should_parse_fun() {
        let s = "fun name(x : T1, y : T2) -> T3 { x }";
        let input = lexer::lex(s.into()).unwrap();
        let mut output = parse(input).unwrap();
        assert_eq!(output.len(), 1);

        let output = output.remove(0);

        let params = proj!(output, Ast::Fun { params, .. }, params);
        assert_eq!(params.len(), 2);
    }

    #[test]
    fn should_parse_variable() {
        let s = "var";
        let input = lexer::lex(s.into()).unwrap();
        let mut output = parse(input).unwrap();
        assert_eq!(output.len(), 1);

        let output = output.remove(0);

        let name = proj!(output, Ast::Var(x), x);

        assert_eq!(name, "var".into());
    }

    #[test]
    fn should_parse_fun_call_with_complex_param() {
        let s = "blah(ah(b()), c, d()(e, i))()";
        let input = lexer::lex(s.into()).unwrap();
        let mut output = parse(input).unwrap();
        assert_eq!(output.len(), 1);

        let output = output.remove(0);

        let mut results = s_pattern!(output => 
            [Ast::Call { fun_expr, args }] fun_expr;
            [Ast::Call { fun_expr: inner, args: inner_args }] inner; 
            [Ast::Var(x)] 
            => (args, inner_args, x)).collect::<Vec<_>>();
        
        assert_eq!(results.len(), 1);

        let (inputs, inner_inputs, name) = results.remove(0);

        assert_eq!(*name, "blah".into());
        assert_eq!(inputs.len(), 0);
        assert_eq!(inner_inputs.len(), 3);
    }

    #[test]
    fn should_parse_fun_call_with_multiple_param() {
        let s = "blah(val, two, other)";
        let input = lexer::lex(s.into()).unwrap();
        let mut output = parse(input).unwrap();
        assert_eq!(output.len(), 1);

        let output = output.remove(0);

        let (fun_expr, mut args) = proj!(output, Ast::Call { fun_expr, args }, (*fun_expr, args));

        let name = proj!(fun_expr, Ast::Var(x), x);
        assert_eq!(name, "blah".into());

        assert_eq!(args.len(), 3);

        let name = proj!(args.remove(0), Ast::Var(x), x);
        assert_eq!(name, "val".into());

        let name = proj!(args.remove(0), Ast::Var(x), x);
        assert_eq!(name, "two".into());

        let name = proj!(args.remove(0), Ast::Var(x), x);
        assert_eq!(name, "other".into());
    }

    #[test]
    fn should_parse_fun_call_with_param() {
        let s = "blah(val)";
        let input = lexer::lex(s.into()).unwrap();
        let mut output = parse(input).unwrap();
        assert_eq!(output.len(), 1);

        let output = output.remove(0);

        let (fun_expr, mut args) = proj!(output, Ast::Call{fun_expr, args}, (*fun_expr, args));

        let name = proj!(fun_expr, Ast::Var(x), x);
        assert_eq!(name, "blah".into());

        assert_eq!(args.len(), 1);

        let arg = args.remove(0);

        let name = proj!(arg, Ast::Var(x), x);
        assert_eq!(name, "val".into());
    }

    #[test]
    fn should_parse_fun_call_call() {
        let s = "blah()()";
        let input = lexer::lex(s.into()).unwrap();
        let mut output = parse(input).unwrap();
        assert_eq!(output.len(), 1);

        let output = output.remove(0);

        let mut results = s_pattern!(output => 
            [Ast::Call { fun_expr, args } ] fun_expr; 
            [Ast::Call { fun_expr: inner_expr, args: inner_args }] inner_expr; 
            [Ast::Var(name)]
            => (args, inner_args, name)).collect::<Vec<_>>();
        
        assert_eq!(results.len(), 1);

        let (inputs, inner_input, name) = results.remove(0);

        assert_eq!(*name, "blah".into());
        assert_eq!(inputs.len(), 0);
        assert_eq!(inner_input.len(), 0);
    }

    #[test]
    fn should_parse_fun_call() {
        let s = "blah()";
        let input = lexer::lex(s.into()).unwrap();
        let mut output = parse(input).unwrap();
        assert_eq!(output.len(), 1);

        let output = output.remove(0);
        let (fun_expr, inputs) = proj!(output, Ast::Call { fun_expr, args}, (*fun_expr, args));

        assert_eq!(inputs.len(), 0);

        let var = proj!(fun_expr, Ast::Var(x), x);

        assert_eq!(var, "blah".into());
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