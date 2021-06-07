use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, tag_no_case},
    character::complete::multispace0,
    error::{context, ErrorKind, ParseError, VerboseError},
    multi::{many0, many1},
    sequence::{delimited, terminated, tuple},
    AsChar, Err as NomErr, InputTakeAtPosition,
};
use http_types::Method;

use std::str::FromStr;

use crate::expr::{Probe, Scheme};

type IResult<I, O, E = (I, ErrorKind)> = Result<(I, O), NomErr<E>>;
type Res<T, U> = IResult<T, U, VerboseError<T>>;

fn alphanumerichyphen1<T>(i: T) -> Res<T, T>
where
    T: InputTakeAtPosition,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    i.split_at_position1_complete(
        |item| {
            let char_item = item.as_char();
            !(char_item == '-') && !char_item.is_alphanum()
        },
        ErrorKind::AlphaNumeric,
    )
}

fn alphanumerichyphenperiod1<T>(i: T) -> Res<T, T>
where
    T: InputTakeAtPosition,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    i.split_at_position1_complete(
        |item| {
            let char_item = item.as_char();
            char_item != '-' && char_item != '.' && !char_item.is_alphanum()
        },
        ErrorKind::AlphaNumeric,
    )
}

fn header(input: &str) -> Res<&str, (String, String)> {
    context(
        "header",
        tuple((
            terminated(many1(alphanumerichyphen1), tag(": ")),
            many1(alphanumerichyphenperiod1),
        )),
    )(input)
    .map(|(next_input, res)| (next_input, (res.0.join(""), res.1.join(""))))
}

fn path(input: &str) -> Res<&str, String> {
    context("path", delimited(multispace0, is_not(" "), multispace0))(input)
        .map(|(next_input, res)| (next_input, res.to_string()))
}

fn scheme(input: &str) -> Res<&str, Scheme> {
    context(
        "scheme",
        alt((tag_no_case("HTTP/1.1"), tag_no_case("HTTP/1.2"))),
    )(input)
    .map(|(next_input, res)| (next_input, res.into()))
}

fn method(input: &str) -> Res<&str, Method> {
    context("method", alt((tag_no_case("HEAD"), tag_no_case("GET"))))(input)
        .map(|(next_input, res)| (next_input, Method::from_str(res).unwrap()))
}

/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
/// trailing whitespace, returning the output of `inner`.
fn ws<'a, F: 'a, O, E: ParseError<&'a str>>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: Fn(&'a str) -> IResult<&'a str, O, E>,
{
    delimited(multispace0, inner, multispace0)
}

pub fn parse_probe(input: &str) -> Res<&str, Probe> {
    context(
        "parse_probe",
        tuple((ws(method), ws(path), ws(scheme), many0(ws(header)))),
    )(input)
    .map(|(next_input, res)| {
        let (method, path, scheme, headers) = res;
        (
            next_input,
            Probe {
                method,
                scheme,
                path,
                headers,
            },
        )
    })
}

#[cfg(test)]
mod tests {
    use crate::expr::Scheme;

    use super::*;
    use nom::{
        error::{ErrorKind, VerboseError, VerboseErrorKind},
        Err as NomErr,
    };

    #[test]
    fn test_scheme() {
        assert_eq!(
            scheme("HTTP/1.1 Host: storage.googleapis.com Connection: close"),
            Ok((
                " Host: storage.googleapis.com Connection: close",
                Scheme::Http11
            ))
        );
        assert_eq!(
            scheme("HTTP/1.2 Host: storage.googleapis.com Connection: close"),
            Ok((
                " Host: storage.googleapis.com Connection: close",
                Scheme::Https12
            ))
        );
        assert_eq!(
            scheme("bla/1.1"),
            Err(NomErr::Error(VerboseError {
                errors: vec![
                    ("bla/1.1", VerboseErrorKind::Nom(ErrorKind::Tag)),
                    ("bla/1.1", VerboseErrorKind::Nom(ErrorKind::Alt)),
                    ("bla/1.1", VerboseErrorKind::Context("scheme")),
                ]
            }))
        );
    }

    #[test]
    fn test_path() {
        assert_eq!(path("/a/b/c?d"), Ok(("", "/a/b/c?d".to_string())));
        assert_eq!(path("/a/b/c/?d"), Ok(("", "/a/b/c/?d".to_string())));
        assert_eq!(path("/a/b-c-d/c/?d"), Ok(("", "/a/b-c-d/c/?d".to_string())));
        assert_eq!(path("/a/1234/c/?d"), Ok(("", "/a/1234/c/?d".to_string())));
        assert_eq!(
            path("/a/1234/c.txt?d"),
            Ok(("", "/a/1234/c.txt?d".to_string()))
        );
    }
}
