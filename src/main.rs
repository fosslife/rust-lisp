use std::collections::HashMap;
use std::fmt;
use std::io;
use std::num::ParseFloatError;

/// A Risp expression is any statement that can be executed.
/// consists of (, ), or +, 1,2, etc. +/- are functions
#[derive(Clone)]
enum RispExp {
    Symbol(String),
    Number(f64),
    List(Vec<RispExp>),
    Func(fn(&[RispExp]) -> Result<RispExp, RispErr>),
}

/// Risp error object. simply prints to stdin
#[derive(Debug)]
enum RispErr {
    Reason(String),
}

/// hashmap for storing defined variables,
/// builtin functions etc
/// ex:
#[derive(Clone)]
struct RispEnv {
    data: HashMap<String, RispExp>,
}

impl fmt::Display for RispExp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let str = match self {
            RispExp::Symbol(s) => s.clone(),
            RispExp::Number(n) => n.to_string(),
            RispExp::List(list) => {
                let xs: Vec<String> = list.iter().map(|x| x.to_string()).collect();
                format!("({})", xs.join(","))
            }
            RispExp::Func(_) => "Function {}".to_string(),
        };

        write!(f, "{}", str)
    }
}

/// split the entire string into chunks of tokens. every single
/// thing on small scale is a token
fn tokenize(expr: String) -> Vec<String> {
    expr.replace("(", " ( ")
        .replace(")", " ) ")
        .split_whitespace()
        .map(|x| x.to_string())
        .collect()
}

/// parse function to deside what to do based on
/// what type of token is recieved. like if it's
/// a ( then seq is started, if it's a ) then an
/// unexpected token is recieved otherwise we need
/// to evaluate the further expressions by parse_atom()  method
fn parse<'a>(tokens: &'a [String]) -> Result<(RispExp, &'a [String]), RispErr> {
    // get first token
    let (token, rest) = tokens
        .split_first()
        .ok_or(RispErr::Reason("could not get token".to_string()))?;

    // Parsing each token individually. token is a slice, we match against it.
    // token.as_str() will do the same.
    match &token[..] {
        "(" => read_seq(rest),
        ")" => Err(RispErr::Reason("Unexpected )".to_string())),
        _ => Ok((parse_atom(token), rest)),
    }
}

/// it's definitly a sequence after ( as per out logic
fn read_seq<'a>(tokens: &'a [String]) -> Result<(RispExp, &'a [String]), RispErr> {
    let mut res: Vec<RispExp> = vec![];
    let mut xs = tokens;
    loop {
        let (next_token, rest) = xs
            .split_first()
            .ok_or(RispErr::Reason("could not find closing )".to_string()))?;

        if next_token == ")" {
            return Ok((RispExp::List(res), rest));
        }

        let (exp, new_xs) = parse(&xs)?;
        res.push(exp);
        xs = new_xs;
    }
}

/// atom is the smallest unit. if the atom is a number
/// return Risp Number else return Risp Symbol
fn parse_atom(token: &str) -> RispExp {
    let potential_float: Result<f64, ParseFloatError> = token.parse();
    match potential_float {
        Ok(v) => RispExp::Number(v),
        Err(_) => RispExp::Symbol(token.to_string().clone()),
    }
}

/// default env that stores a map of default functions like +/- and their
/// implementation.
fn default_env() -> RispEnv {
    let mut data: HashMap<String, RispExp> = HashMap::new();

    //Insert the functions in Risp Env
    data.insert(
        "+".to_string(),
        RispExp::Func(|args: &[RispExp]| -> Result<RispExp, RispErr> {
            let sum = parse_list_of_floats(args)?
                .iter()
                .sum();

            Ok(RispExp::Number(sum))
        }),
    );

    data.insert(
        "-".to_string(),
        RispExp::Func(|args: &[RispExp]| -> Result<RispExp, RispErr> {
            let floats: Vec<f64> = parse_list_of_floats(args)?;
            let first: f64 = *floats
                .first()
                .ok_or(RispErr::Reason("expected at least one number".to_string()))?;
            let sum_of_rest: f64 = floats[1..].iter().sum();

            Ok(RispExp::Number(first - sum_of_rest))
        }),
    );

    RispEnv { data }
}

/// Take a reference to a vector and parse it. i.e. convert
/// Each number in vector to Risp Number
fn parse_list_of_floats(args: &[RispExp]) -> Result<Vec<f64>, RispErr> {
    args.iter().map(|x| parse_single_float(x)).collect()
}

/// Convert given float to Risp Number
fn parse_single_float(exp: &RispExp) -> Result<f64, RispErr> {
    match exp {
        RispExp::Number(n) => Ok(*n),
        _ => Err(RispErr::Reason("Expected a number".to_string())),
    }
}

/// Basic evaluator, takes an expression and environment (rispenv)
/// and `evaluates` it based on pattern matching
fn eval(exp: &RispExp, env: &mut RispEnv) -> Result<RispExp, RispErr> {
    match exp {
        RispExp::Symbol(k) => env
            .data
            .get(k)
            .ok_or(RispErr::Reason(format!("unexpected symbol k={}", k)))
            .map(|x| x.clone()),
        RispExp::Number(_a) => Ok(exp.clone()),
        RispExp::List(list) => {
            let first_form = list
                .first()
                .ok_or(RispErr::Reason("expected a non empty list".to_string()))?;
            let arg_forms = &list[1..];
            let first_eval = eval(first_form, env)?;
            match first_eval {
                RispExp::Func(f) => {
                    let args_eval = arg_forms
                        .iter()
                        .map(|x| eval(x, env))
                        .collect::<Result<Vec<RispExp>, RispErr>>();
                    f(&args_eval?)
                }
                _ => Err(RispErr::Reason("first form must be a function".to_string())),
            }
        }
        RispExp::Func(_) => Err(RispErr::Reason("unexpected form".to_string())),
    }
}

/// Basically entry point of compiler itself, (not the app, the compiler)
/// The tokenization, parsing and evaluation is done here
fn parse_eval(expr: String, env: &mut RispEnv) -> Result<RispExp, RispErr> {
    let (parsed_exp, _) = parse(&tokenize(expr))?;
    let evaled_exp = eval(&parsed_exp, env)?;

    Ok(evaled_exp)
}

/// Reads input from stdin
fn slurp_expr() -> String {
    let mut expr = String::new();

    io::stdin()
        .read_line(&mut expr)
        .expect("Failed to read line");

    expr
}

/// Entry point of application. 
fn main() {
    let env = &mut default_env();
    loop {
        println!("risp >");
        let expr = slurp_expr();
        match parse_eval(expr, env) {
            Ok(res) => println!("{}", res),
            Err(e) => match e {
                RispErr::Reason(msg) => println!("X {}", msg),
            },
        }
    }
}
