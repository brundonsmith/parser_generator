#[cfg(test)]
mod tests {
    use crate::sample_parser::{parse, Ast};

    #[test]
    fn test_t() {
        let res = parse("T");

        assert_eq!(res, Some(Ast::t(())));
    }

    #[test]
    fn test_nil() {
        let res = parse("NIL");

        assert_eq!(res, Some(Ast::nil(())));
    }

    #[test]
    fn test_number_1() {
        let res = parse("123.4");

        assert_eq!(
            res,
            Some(Ast::number {
                front: vec![Ast::_Char('1'), Ast::_Char('2'), Ast::_Char('3')],
                decimal: Some(vec![Ast::_Char('4')])
            })
        );
    }

    #[test]
    fn test_number_2() {
        let res = parse("123");

        assert_eq!(
            res,
            Some(Ast::number {
                front: vec![Ast::_Char('1'), Ast::_Char('2'), Ast::_Char('3')],
                decimal: None
            })
        );
    }

    #[test]
    fn test_number_3() {
        let res = parse("123.");

        assert_eq!(res, None);
    }

    #[test]
    fn test_number_4() {
        let res = parse(".1");

        assert_eq!(res, None);
    }

    #[test]
    fn test_symbol() {
        let res = parse("foo");

        assert_eq!(
            res,
            Some(Ast::symbol {
                first: Box::new(Ast::_Char('f')),
                others: vec![Ast::_Char('o'), Ast::_Char('o')]
            })
        );
    }

    #[test]
    fn test_string() {
        let res = parse("\"hello\"");

        assert_eq!(
            res,
            Some(Ast::string {
                contents: vec![
                    Ast::_Char('h'),
                    Ast::_Char('e'),
                    Ast::_Char('l'),
                    Ast::_Char('l'),
                    Ast::_Char('o')
                ]
            })
        );
    }

    #[test]
    fn test_list_1() {
        let res = parse("(foo bar)");

        assert_eq!(
            res,
            Some(Ast::list {
                els: vec![
                    Ast::symbol {
                        first: Box::new(Ast::_Char('f')),
                        others: vec![Ast::_Char('o'), Ast::_Char('o')]
                    },
                    Ast::symbol {
                        first: Box::new(Ast::_Char('b')),
                        others: vec![Ast::_Char('a'), Ast::_Char('r')]
                    }
                ]
            })
        );
    }

    #[test]
    fn test_list_2() {
        let res = parse("(foo (bar 12))");

        assert_eq!(
            res,
            Some(Ast::list {
                els: vec![
                    Ast::symbol {
                        first: Box::new(Ast::_Char('f')),
                        others: vec![Ast::_Char('o'), Ast::_Char('o')]
                    },
                    Ast::list {
                        els: vec![
                            Ast::symbol {
                                first: Box::new(Ast::_Char('b')),
                                others: vec![Ast::_Char('a'), Ast::_Char('r')]
                            },
                            Ast::number {
                                front: vec![Ast::_Char('1'), Ast::_Char('2')],
                                decimal: None
                            }
                        ]
                    }
                ]
            })
        );
    }
}


macro_rules! generate_parse_function {
    ($rule_name:ident, $parse_expr:tt) => {
        fn $rule_name<'a>(code: &'a str) -> ParseResult<'a> {
            #[allow(unused_mut)]
            let mut rest = skip_whitespace(code);

            generate_parse_logic! { rest, $rule_name, $parse_expr }
        }
    };
}

macro_rules! generate_parse_logic {
    // ($rest:ident, $current_rule_name:ident, $expr:tt *) => {

    // };
    // ($rest:ident, $current_rule_name:ident, $expr:tt +) => {

    // };
    // ($rest:ident, $current_rule_name:ident, $expr:tt ?) => {

    // };
    ($rest:ident, $current_rule_name:ident, $first:tt $($others:tt)+) => {
        {
            let matcher = |r| generate_parse_expression! { r, $current_rule_name, $first };

            #[allow(unused_variables)]
            if let (Some(matched_ast), rest_after_parse) = matcher(rest) {
                rest = rest_after_parse;

                generate_parse_logic! { rest, $current_rule_name, $others }
            }

            (None, rest)
        }
    };
    ($rest:ident, $current_rule_name:ident, $expr:tt) => {
        {
            let matcher = |r| generate_parse_expression! { r, $current_rule_name, $expr };

            matcher($rest)
        }
    };
}

macro_rules! generate_parse_expression {
    ($rest:ident, $current_rule_name:ident, $exact:literal) => { // "NIL"
        parse_exact($rest, $exact, Ast::$current_rule_name(()))
    };
    ($rest:ident, $current_rule_name:ident, $rule:ident) => { // atom
        $rule($rest)
    };
    ($rest:ident, $current_rule_name:ident, ($start_char:literal .. $end_char:literal)) => {
        parse_char(
            $rest,
            &CharDescriptor::Range {
                start: $start_char,
                end: $end_char,
            },
        )
    };
    ($rest:ident, $current_rule_name:ident, (c $ch:literal)) => {
        parse_char($rest, &CharDescriptor::Exact($ch))
    };
    ($rest:ident, $current_rule_name:ident, (! $ch:literal)) => {
        parse_char($rest, &CharDescriptor::Except(vec![$ch]))
    };
    ($rest:ident, $current_rule_name:ident, ($opt1:tt | $($opts:tt)|+)) => { // list | atom
        one_of($rest, vec![ 
              Box::new(|r| { generate_parse_expression! { r, $current_rule_name, $opt1 } }),
            $(Box::new(|r| { generate_parse_expression! { r, $current_rule_name, $opts } })),* 
        ])
    };
    // ($rest:ident, $current_rule_name:ident, $other:tt *) => { // expr*
    //     {
    //         let matcher = |code| {
    //             generate_parse_expression! { code, $current_rule_name, $other }
    //         };
    //         let mut nodes = vec![];
    //         while let (Some(ast), rest_after) = matcher($rest) {
    //             rest = rest_after;
    //             nodes.push(ast);
    //         }

    //         nodes
    //     }
    // };
    // ($rest:ident, $current_rule_name:ident, $other:tt +) => { // expr+

    // };
    // ($rest:ident, $current_rule_name:ident, $other:tt ?) => { // expr?

    // };
    // ($rest:ident, $current_rule_name:ident, $token1:tt $($tokens:tt)*) => { // "(" expr ")"
    //     {
    //         let matcher = |code| {
    //             generate_parse_expression! { code, $current_rule_name, $token1 }
    //         };
    //         let result = matcher($rest);

    //         if let (Some(ast), rest_after) = matcher(rest) {
    //             generate_parse_expression! { rest_after, $current_rule_name, $($tokens)* }
    //         }
    //     }

    //     (None, $rest)
    // };
}


// expr   -> (list | atom);
// list   -> ("(" $els:expr* ")");
// atom   -> (t | nil | number | string | symbol);
// t      -> "T";
// nil    -> "NIL";
// number -> $front:('0'..'9')+ ("." $decimal:('0'..'9')+)?;
// symbol -> $first:('a'..'z' | 'A'..'Z') $others:('a'..'z' | 'A'..'Z' | '0'..'9' | '-')*;
// string -> "\"" $contents:(!"\"")* "\"";

#[derive(Debug, PartialEq)]
pub enum Ast {
    expr(Box<Ast>), // collapse?
    list {
        els: Vec<Ast>,
    },
    atom(Box<Ast>), // collapse?
    t(()),
    nil(()),
    number {
        front: Vec<Ast>,
        decimal: Option<Vec<Ast>>,
    },
    symbol {
        first: Box<Ast>,
        others: Vec<Ast>,
    },
    string {
        contents: Vec<Ast>,
    },

    _Char(char),
}

pub fn parse<'a>(code: &'a str) -> Option<Ast> {
    assert!(code.is_ascii());

    return expr(code).0;
}

// expr ->
generate_parse_function! { expr, (list | atom) }

// list ->
fn list<'a>(code: &'a str) -> ParseResult<'a> {
    #[allow(unused_mut)]
    let mut rest = skip_whitespace(code);

    {
        // "("
        let matcher = |r| generate_parse_expression! { r, list, (c'(') };

        #[allow(unused_variables)]
        if let (Some(matched_ast), rest_after_parse) = matcher(rest) {
            rest = rest_after_parse;

            // expr
            let matcher = |r| generate_parse_expression! { r, list, expr };

            // *
            let mut els = vec![];
            while let (Some(matched_ast), rest_after_parse) = matcher(rest) {
                rest = rest_after_parse;
                els.push(matched_ast);
            }

            // ")"
            let matcher = |r| generate_parse_expression! { r, list, (c')') };

            #[allow(unused_variables)]
            if let (Some(matched_ast), rest_after_parse) = matcher(rest) {
                return (Some(Ast::list { els }), rest_after_parse);
            }
        }

        (None, rest)
    }
}

// atom ->
generate_parse_function! { atom, (t | nil | number | string | symbol) }

// t ->
generate_parse_function! { t, "T" }

// nil ->
generate_parse_function! { nil, "NIL" }

// number ->
fn number<'a>(code: &'a str) -> ParseResult<'a> {
    #[allow(unused_mut)]
    let mut rest = skip_whitespace(code);

    {
        // '0'..'9'
        let matcher = |r| generate_parse_expression! { r, number, ('0'..'9') };

        // +
        #[allow(unused_variables)]
        if let (Some(matched_ast), rest_after_parse) = matcher(rest) {
            let mut front = vec![];
            while let (Some(matched_ast), rest_after_parse) = matcher(rest) {
                rest = rest_after_parse;
                front.push(matched_ast);
            }

            // "."
            let matcher = |r| generate_parse_expression! { r, number, (c'.') };

            #[allow(unused_variables)]
            if let (Some(matched_ast), rest_after_parse) = matcher(rest) {
                rest = rest_after_parse;

                // '0'..'9'
                let matcher = |r| generate_parse_expression! { r, number, ('0'..'9') };

                // +
                if let (Some(matched_ast), rest_after_parse) = matcher(rest) {
                    let mut decimal = vec![];
                    while let (Some(matched_ast), rest_after_parse) = matcher(rest) {
                        rest = rest_after_parse;
                        decimal.push(matched_ast);
                    }

                    return (
                        Some(Ast::number {
                            front,
                            decimal: Some(decimal),
                        }),
                        rest,
                    );
                }
            } else {
                // ?
                return (
                    Some(Ast::number {
                        front,
                        decimal: None,
                    }),
                    rest,
                );
            }
        }

        (None, rest)
    }
}

// symbol ->
fn symbol<'a>(code: &'a str) -> ParseResult<'a> {
    #[allow(unused_mut)]
    let mut rest = skip_whitespace(code);

    let matcher = |r| generate_parse_expression! { r, symbol, (('a'..'z') | ('A'..'Z')) };

    #[allow(unused_variables)]
    if let (Some(matched_ast), rest_after_parse) = matcher(rest) {
        rest = rest_after_parse;
        let first = Box::new(matched_ast);

        let matcher = |r| generate_parse_expression! { r, symbol, (('a'..'z') | ('A'..'Z') | ('0'..'9') | (c'-')) };

        // *
        let mut others = vec![];
        while let (Some(matched_ast), rest_after_parse) = matcher(rest) {
            rest = rest_after_parse;
            others.push(matched_ast);
        }

        return (Some(Ast::symbol { first, others }), rest);
    }

    return (None, rest);
}

fn string<'a>(code: &'a str) -> ParseResult<'a> {
    #[allow(unused_mut)]
    let mut rest = skip_whitespace(code);

    {
        // "\""
        let matcher = |r| generate_parse_expression! { r, string, (c'"') };

        #[allow(unused_variables)]
        if let (Some(matched_ast), rest_after_parse) = matcher(rest) {
            rest = rest_after_parse;

            {
                // !"\""
                let matcher = |r| generate_parse_expression! { r, string, (!'"') };

                // *
                let mut contents = vec![];
                while let (Some(matched_ast), rest_after_parse) = matcher(rest) {
                    rest = rest_after_parse;
                    contents.push(matched_ast);
                }

                // "\""
                let matcher = |r| generate_parse_expression! { r, string, (c'"') };

                #[allow(unused_variables)]
                if let (Some(matched_ast), rest_after_parse) = matcher(rest) {
                    rest = rest_after_parse;

                    return (Some(Ast::string { contents }), rest);
                }
            }
        }

        (None, rest)
    }
}




// utils
type ParseResult<'a> = (Option<Ast>, &'a str);

fn parse_exact<'a>(code: &'a str, token: &str, ast: Ast) -> ParseResult<'a> {
    #[allow(unused_mut)]
    let mut rest = skip_whitespace(code);

    if peek_exact(rest, token) {
        return (Some(ast), &rest[token.len()..]);
    } else {
        return (None, rest);
    }
}

fn peek_exact(code: &str, token: &str) -> bool {
    return code.chars().zip(token.chars()).all(|(c, t)| c == t);
}

fn parse_char<'a>(code: &'a str, c: &CharDescriptor) -> ParseResult<'a> {
    #[allow(unused_mut)]
    let mut rest = skip_whitespace(code);

    if let Some(ch) = peek_char(code, c) {
        return (Some(Ast::_Char(ch)), &rest[1..]);
    } else {
        return (None, rest);
    }
}

fn peek_char(code: &str, c: &CharDescriptor) -> Option<char> {
    let n = code.chars().next();
    if let Some(n) = n {
        if c.matches(n) {
            return Some(n);
        }
    }

    return None;
}

fn skip_whitespace<'a>(code: &'a str) -> &'a str {
    let offset = code.chars().take_while(|c| c.is_whitespace()).count();
    return &code[offset..];
}

fn one_of<'a>(
    rest: &'a str,
    possibilities: Vec<Box<dyn FnOnce(&'a str) -> ParseResult<'a>>>,
) -> ParseResult<'a> {
    for pos in possibilities {
        let result = pos(rest);

        if result.0.is_some() {
            return result;
        }
    }

    return (None, rest);
}

enum CharDescriptor {
    Exact(char),
    Range { start: char, end: char },
    Except(Vec<char>),
}

impl CharDescriptor {
    fn matches(&self, c: char) -> bool {
        match self {
            CharDescriptor::Exact(d) => c == *d,
            CharDescriptor::Range { start, end } => *start <= c && c <= *end,
            CharDescriptor::Except(d) => d.iter().all(|e| c != *e),
        }
    }
}
