use std::collections::HashMap;
use std::io;
use std::num::ParseFloatError;

#[derive(Clone)]
enum RispExp {
    Symbol(String),
    Number(f64),
    List(Vec<RispExp>),
    Func(fn(&[RispExp]) -> Result<RispExp, RispErr>),
}

#[derive(Debug)]
enum RispErr {
    Reason(String),
}

#[derive(Clone)]
struct RispEnv {
    data: HashMap<String, RispExp>,
}

impl ToString for RispExp {
    fn to_string(&self) -> String {
        match self {
            RispExp::Symbol(s) => s.clone(),
            RispExp::Number(n) => n.to_string(),
            RispExp::List(list) => {
                let xs: Vec<String> = list.iter().map(|x| x.to_string()).collect();
                format!("({})", xs.join(","))
            }
            RispExp::Func(_) => "Function {}".to_string(),
        }
    }
}

fn tokenize(expr: String) -> Vec<String> {
    expr.replace("(", " ( ")
        .replace(")", " ) ")
        .split_whitespace()
        .map(|x| x.to_string())
        .collect()
}

fn parse(tokens: &[String], pos: usize) -> Result<(RispExp, usize), RispErr> {
    let token = tokens.get(pos).ok_or(RispErr::Reason(format!(
        "Could not get token for pos={}",
        pos
    )))?;

    match &token[..] {
        "(" => read_seq(tokens, pos + 1),
        ")" => Err(RispErr::Reason("unexpected `)`".to_string())),
        _ => Ok((parse_atom(token), pos + 1)),
    }
}

fn read_seq(tokens: &[String], start: usize) -> Result<(RispExp, usize), RispErr> {
    let mut res: Vec<RispExp> = vec![];
    let mut next = start;
    loop {
        let next_token = tokens
            .get(next)
            .ok_or(RispErr::Reason("could not find closing `)`".to_string()))?;

        if next_token == ")" {
            return Ok((RispExp::List(res), next + 1));
        }

        let (exp, new_next) = parse(&tokens, next)?;
        res.push(exp);
        next = new_next;
    }
}

fn parse_atom(token: &str) -> RispExp {
    let pot_float: Result<f64, ParseFloatError> = token.parse();

    match pot_float {
        Ok(v) => RispExp::Number(v),
        Err(_) => RispExp::Symbol(token.to_string().clone()),
    }
}

fn default_env() -> RispEnv {
    let mut data: HashMap<String, RispExp> = HashMap::new();
    data.insert(
        "+".to_string(),
        RispExp::Func(|args: &[RispExp]| -> Result<RispExp, RispErr> {
            let sum = parse_list_of_floats(args)?
                .iter()
                .fold(0.0, |sum, a| sum + a);

            Ok(RispExp::Number(sum))
        }),
    );

    data.insert(
        "-".to_string(),
        RispExp::Func(|args: &[RispExp]| -> Result<RispExp, RispErr> {
            let floats = parse_list_of_floats(args)?;
            let first = *floats
                .first()
                .ok_or(RispErr::Reason("expected at least one number".to_string()))?;
            let sum_of_rest = floats[1..].iter().fold(0.0, |sum, a| sum + a);

            Ok(RispExp::Number(first - sum_of_rest))
        }),
    );

    RispEnv { data }
}

fn parse_list_of_floats(args: &[RispExp]) -> Result<Vec<f64>, RispErr> {
    args.iter()
        .map(|x| parse_single_float(x))
        .collect::<Result<Vec<f64>, RispErr>>()
}

fn parse_single_float(exp: &RispExp) -> Result<f64, RispErr> {
    match exp {
        RispExp::Number(num) => Ok(*num),
        _ => Err(RispErr::Reason("expected a number".to_string())),
    }
}

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

fn parse_eval_print(expr: String, env: &mut RispEnv) -> Result<String, RispErr> {
    let (parsed_exp, _) = parse(&tokenize(expr), 0)?;
    let evaled_exp = eval(&parsed_exp, env)?;

    Ok(evaled_exp.to_string())
}

fn slurp_expr() -> String {
    let mut expr = String::new();

    io::stdin()
        .read_line(&mut expr)
        .expect("Failed to read line");

    expr
}

fn main() {
    let env = &mut default_env();
    loop {
        println!("risp >");
        let expr = slurp_expr();;
        match parse_eval_print(expr, env) {
            Ok(res) => println!("// 🔥 => {}", res),
            Err(e) => match e {
                RispErr::Reason(msg) => println!("// 🙀 => {}", msg),
            },
        }
    }
}
