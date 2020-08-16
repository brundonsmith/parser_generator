
// macro_rules! bar {
//     ($a:ident, ->, $r:) => (3 + 4)
// }

#![allow(dead_code,unused_macros,non_camel_case_types)]

mod sample_parser;

fn main() { }


macro_rules! generate_parser {
    (
        $first_rule_name:ident -> $first_parse_expr:tt;
        $($other_rule_names:ident -> $other_parse_exprs:tt);*;
    ) => {

        #[derive(Debug,PartialEq)]
        pub enum Ast {
            $first_rule_name (generate_parse_expression_ast_data!($first_parse_expr)),
            $( $other_rule_names (generate_parse_expression_ast_data!($other_parse_exprs)) ),*,

            _Char(char),
        }

        pub fn parse<'a>(code: &'a str) -> Option<Ast> {
            assert!(code.is_ascii());

            return $first_rule_name(code).0;
        }

        generate_parse_function! { $first_rule_name, $first_parse_expr }
        $( generate_parse_function! { $other_rule_names, $other_parse_exprs } )*
    };
}

macro_rules! generate_parse_function {
    ($rule_name:ident, $parse_expr:tt) => {
        fn $rule_name<'a>(code: &'a str) -> (Option<Ast>, &'a str) {
            #[allow(unused_mut)]
            let mut rest = skip_whitespace(code);

            generate_parse_logic! { rest, $rule_name, $parse_expr }
        }
    };
}

macro_rules! generate_parse_expression_ast_data {
    ($exact:literal) => (()); // "NIL"
    ($rule:ident) => (Box<Ast>); // atom
    (($opt1:tt | $($opts:tt)|+)) => (Box<Ast>); // list | atom
    ($other:tt *) => (Vec< generate_parse_expression_ast_data!{ $other } >>); // expr*
    ($other:tt +) => (Vec< generate_parse_expression_ast_data!{ $other } >>); // expr+
    ($other:tt ?) => (Option< generate_parse_expression_ast_data!{ $other } >>); // expr?
    ($token1:tt $($tokens:tt)+) => ( // "(" expr ")"
        // $($tokens),+
        generate_parse_expression_ast_data! { $token1 },
        $( generate_parse_expression_ast_data! { $tokens } ),+
    );
}

macro_rules! generate_parse_logic {
    ($rest:ident, $current_rule_name:ident, $exact:literal) => { // "NIL"
        {
            let matcher = |code| {
                parse_exact(code, $exact, Ast::$current_rule_name(()))
            };
            let res = matcher($rest);
            
            res
        }
    };
    ($rest:ident, $current_rule_name:ident, $rule:ident) => { // atom
        $rule($rest)
    };
    ($rest:ident, $current_rule_name:ident, ($opt1:tt | $($opts:tt)|+)) => { // list | atom
        one_of($rest, vec![ 
              Box::new(|r| { generate_parse_logic! { r, $current_rule_name, $opt1 } }),
            $(Box::new(|r| { generate_parse_logic! { r, $current_rule_name, $opts } })),* 
        ])
    };
    ($rest:ident, $current_rule_name:ident, $other:tt *) => { // expr*
        {
            let matcher = |code| {
                generate_parse_logic! { code, $current_rule_name, $other }
            };
            let mut nodes = vec![];
            while let (Some(ast), rest_after) = matcher($rest) {
                rest = rest_after;
                nodes.push(ast);
            }

            nodes
        }
    };
    ($rest:ident, $current_rule_name:ident, $other:tt +) => { // expr+

    };
    ($rest:ident, $current_rule_name:ident, $other:tt ?) => { // expr?

    };
    ($rest:ident, $current_rule_name:ident, $token1:tt $($tokens:tt)*) => { // "(" expr ")"
        {
            let matcher = |code| {
                generate_parse_logic! { code, $current_rule_name, $token1 }
            };
            let result = matcher($rest);

            if let (Some(ast), rest_after) = matcher(rest) {
                generate_parse_logic! { rest_after, $current_rule_name, $($tokens)* }
            }
        }

        (None, $rest)
    };
}

// mod generated_parser {
//     // generate_parser!{t -> "T";}

//     generate_parser!(
//         // expr   -> (list | atom);
//         // list   -> ("(" (expr*) ")");
//         foo -> ("(" atom ")");
//         atom   -> (t | nil); //| number | string | symbol;
//         t      -> "T";
//         nil    -> "NIL";
//         // number -> '0'..'9'+ ("." '0'..'9'+)?;
//         // symbol -> ('a'..'z' | 'A'..'Z') ('a'..'z' | 'A'..'Z' | '0'..'9' | '-')*;
//         // string -> "\"" !"\""* "\"";
//     );

//     // utils
//     fn parse_exact<'a>(code: &'a str, token: &str, ast: Ast) -> (Option<Ast>, &'a str) {
//         let rest = skip_whitespace(code);
    
//         if peek_exact(rest, token) {
//             return (Some(ast), &rest[token.len()..]);
//         } else {
//             return (None, rest);
//         }
//     }
    
//     fn peek_exact(code: &str, token: &str) -> bool {
//         return code.chars().zip(token.chars()).all(|(c, t)| c == t);
//     }
    
//     fn parse_char<'a>(code: &'a str, c: &CharDescriptor) -> (Option<Ast>, &'a str) {
//         let rest = skip_whitespace(code);
    
//         if let Some(ch) = peek_char(code, c) {
//             return (Some(Ast::_Char(ch)), &rest[1..]);
//         } else {
//             return (None, rest);
//         }
//     }
    
//     fn peek_char(code: &str, c: &CharDescriptor) -> Option<char> {
//         let n = code.chars().next();
//         if let Some(n) = n {
//             if c.matches(n) {
//                 return Some(n);
//             }
//         }
    
//         return None;
//     }
    
//     fn skip_whitespace<'a>(code: &'a str) -> &'a str {
//         let offset = code.chars().take_while(|c| c.is_whitespace()).count();
//         return &code[offset..];
//     }
    
//     fn one_of<'a>(rest: &'a str, possibilities: Vec<Box<dyn FnOnce(&'a str) -> (Option<Ast>, &'a str)>>) -> (Option<Ast>, &'a str) {
//         for opt in possibilities {
//             let result = opt(rest);
      
//             if result.0.is_some() {
//                 return result;
//             }
//         }
      
//         return (None, rest);
//       }
    
//     enum CharDescriptor {
//         Exact(char),
//         Range{ start: char, end: char },
//         Except(Vec<char>),
//     }
    
//     impl CharDescriptor {
//         fn matches(&self, c: char) -> bool {
//             match self {
//                 CharDescriptor::Exact(d) => c == *d,
//                 CharDescriptor::Range{ start, end} => *start <= c && c <= *end,
//                 CharDescriptor::Except(d) => d.iter().all(|e| c != *e),
//             }
//         }
//     }
// }


// #[cfg(test)]
// mod tests {
//     use crate::generated_parser::{parse, Ast};

//     #[test]
//     fn test_t() {
//         let res = parse("T");

//         assert_eq!(res, Some(Ast::t(())));
//     }

//     #[test]
//     fn test_nil() {
//         let res = parse("NIL");

//         assert_eq!(res, Some(Ast::nil(())));
//     }

//     // #[test]
//     // fn test_number_1() {
//     //     let res = parse("123.4");

//     //     assert_eq!(res, Some(Ast::Number(vec![Ast::_Char('1'), Ast::_Char('2'), Ast::_Char('3')], Some(vec![Ast::_Char('4')]))));
//     // }

//     // #[test]
//     // fn test_number_2() {
//     //     let res = parse("123");

//     //     assert_eq!(res, Some(Ast::Number(vec![Ast::_Char('1'), Ast::_Char('2'), Ast::_Char('3')], None)));
//     // }

//     // #[test]
//     // fn test_number_3() {
//     //     let res = parse("123.");

//     //     assert_eq!(res, None);
//     // }

//     // #[test]
//     // fn test_number_4() {
//     // let res = parse(".1");

//     // assert_eq!(res, None);
//     // }

//     // #[test]
//     // fn test_symbol() {
//     //     let res = parse("foo");

//     //     assert_eq!(res, Some(Ast::Symbol(Box::new(Ast::_Char('f')), vec![Ast::_Char('o'), Ast::_Char('o')])));
//     // }

//     // #[test]
//     // fn test_string() {
//     //     let res = parse("\"hello\"");

//     //     assert_eq!(res, Some(Ast::String(vec![Ast::_Char('h'), Ast::_Char('e'), Ast::_Char('l'), Ast::_Char('l'), Ast::_Char('o')])));
//     // }

//     // #[test]
//     // fn test_list_1() {
//     //     let res = parse("(foo bar)");

//     //     assert_eq!(res, Some(Ast::List(vec![
//     //         Ast::Symbol(Box::new(Ast::_Char('f')), vec![Ast::_Char('o'), Ast::_Char('o')]),
//     //         Ast::Symbol(Box::new(Ast::_Char('b')), vec![Ast::_Char('a'), Ast::_Char('r')])
//     //     ])));
//     // }


//     // #[test]
//     // fn test_list_2() {
//     //     let res = parse("(foo (bar 12))");

//     //     assert_eq!(res, Some(Ast::List(vec![
//     //         Ast::Symbol(Box::new(Ast::_Char('f')), vec![Ast::_Char('o'), Ast::_Char('o')]),
//     //         Ast::List(vec![
//     //             Ast::Symbol(Box::new(Ast::_Char('b')), vec![Ast::_Char('a'), Ast::_Char('r')]),
//     //             Ast::Number(vec![Ast::_Char('1'), Ast::_Char('2')], None)
//     //         ])
//     //     ])));
//     // }
// }



// expression     → assignment ;

// assignment     → ( call "." )? IDENTIFIER "=" assignment
//                | logic_or ;

// logic_or       → logic_and ( "or" logic_and )* ;
// logic_and      → equality ( "and" equality )* ;
// equality       → comparison ( ( "!=" | "==" ) comparison )* ;
// comparison     → addition ( ( ">" | ">=" | "<" | "<=" ) addition )* ;
// addition       → multiplication ( ( "-" | "+" ) multiplication )* ;
// multiplication → unary ( ( "/" | "*" ) unary )* ;

// unary          → ( "!" | "-" ) unary | call ;
// call           → primary ( "(" arguments? ")" | "." IDENTIFIER )* ;
// primary        → "true" | "false" | "nil" | "this"
//                | NUMBER | STRING | IDENTIFIER | "(" expression ")"
//                | "super" "." IDENTIFIER ;

// NUMBER         → DIGIT+ ( "." DIGIT+ )? ;
// STRING         → "\"" <any char except "\"">* "\"" ;
// IDENTIFIER     → ALPHA ( ALPHA | DIGIT )* ;
// ALPHA          → "a" ... "z" | "A" ... "Z" | "_" ;
// DIGIT          → "0" ... "9" ;


