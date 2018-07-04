/* Misc support code and specialization for eetf */

/* A simple Erlang port in Rust. */
#![feature(conservative_impl_trait)]
#![feature(slice_patterns,advanced_slice_patterns)]
extern crate eetf;
use std::vec::Vec;
use std::option::Option;
use std::io::{Read, Write, Cursor, Result};
use eetf::{Term,Tuple,Atom,FixInteger};
// use eetf::{Term,Tuple,Atom};


/* Read */

fn read_u32be<Stream: Read>(s: &mut Stream) -> Result<u32> {
    let mut b : [u8; 4]= [0; 4];
    s.read_exact(&mut b)?;
    Ok(((b[0] as u32) << 24) + ((b[1] as u32) << 16) + 
       ((b[2] as u32) << 8) +  b[3] as u32)
}
pub fn read_packet4<Stream: Read>(s: &mut Stream) -> Result<Vec<u8>> {
    let nb = read_u32be(s)?;
    //eprintln!("nb = {}", nb);
    let mut inbuf = vec![0; nb as usize];
    s.read_exact(&mut inbuf)?;
    Ok(inbuf)
}
/* Write */
fn write<Stream: Write>(s: &mut Stream, buf: &[u8]) -> Result<()> {
    let len = s.write(buf)?;
    if len == buf.len() { return Ok(()) }
    write(s, &buf[len..])
}
fn write_u32be<Stream: Write>(s: &mut Stream, v: u32) -> Result<()> {
    let b = [(v >> 24) as u8, (v >> 16) as u8, (v >> 8) as u8, v as u8];
    write(s, &b)
}
pub fn write_packet4<Stream: Write>(s: &mut Stream, buf: &[u8]) -> Result<()> {
    write_u32be(s, buf.len() as u32)?;
    write(s, &buf)?;
    s.flush()?;
    Ok(())
}

/* Specialized functions. */

pub fn tag(tag: &str, term: Term) -> Term {
    Term::from(Tuple::from(vec![atom(tag), term]))
}
pub fn i32(i: i32) -> Term {
    Term::from(FixInteger::from(i))
}
pub fn atom(tag: &str) -> Term {
    Term::from(Atom::from(tag))
}

/* Matching deeply nested structures is really awkward, su use Option
 * converters, which are easier to use with ? syntax. */
pub fn as_vec(arg: &Term, size: Option<usize>) -> Option<&Vec<Term>> {
    match arg {
        &Term::Tuple(Tuple{elements: ref terms}) => {
            match size {
                Some(len) => if terms.len() != len { return None; }
                _ => ()
            }
            Some(&terms)
        },
        _ =>
            None
    }
} 
pub fn as_i32(arg: &Term) -> Option<i32> {
    match arg {
        &Term::FixInteger(FixInteger {value: v}) => Some(v),
        _ => None
    }
}
pub fn as_str(arg: &Term) -> Option<&str> {
    match arg {
        &Term::Atom(Atom {name: ref str}) => Some(str),
        _ => None
    }
}

// pub fn i32_3<T>(arg: &Term, f: &Fn(i32, i32, i32) -> Option<T>) {
//     match arg {
//         &Term::Tuple(Tuple{elements: terms}) =>
//             match &terms[..] {
//                 &[Term::FixInteger(FixInteger {value: a}),
//                   Term::FixInteger(FixInteger {value: b}),
//                   Term::FixInteger(FixInteger {value: c})] => Some(f(a,b,c)),
//                 _ => None
//             }
//         _ => None
//     };
// }


/* Dispatcher for {atom(),_} commands */
pub fn apply_etf(f: &Fn(&Term) -> Option<Term>, in_bin: &[u8]) -> Vec<u8>
{
    /* Decode and dispatch */
    let in_term = Term::decode(Cursor::new(in_bin)).unwrap();
    let out_term = match f(&in_term) {
        Some(t) => t,
        None => tag("error", atom("bad_command"))
    };
    /* Wrap it up and reply. */
    let mut out_bin = Vec::new();
    out_term.encode(&mut out_bin).unwrap();
    return out_bin;
}

// FIXME: Handle single/dual stream case more elegantly.
// stdio needs 2, while sockets need 1.

pub fn loop_apply_etf_2<In: Read, Out: Write>(
    f: &Fn(&Term) -> Option<Term>,
    i: &mut In,
    o: &mut Out
) -> Result<()>
{
    loop {
        let in_bin = read_packet4(i)?;
        let out_bin = apply_etf(f, &in_bin);
        write_packet4(o, &out_bin)?;
    }
}

pub fn loop_apply_etf_1<IO: Read+Write> (
    f: &Fn(&Term) -> Option<Term>,
    io: &mut IO, 
) -> Result<()>
{
    loop {
        let in_bin = read_packet4(io)?;
        let out_bin = apply_etf(f, &in_bin);
        write_packet4(io, &out_bin)?;
    }
}

pub fn loop_event_etf<O: Write> ( 
    f: &Fn() -> Option<Term>,
    o: &mut O, 
) -> Result<()>
{
    loop {
        match f() {
            Some(out_term) => {
                let mut out_bin = Vec::new();
                out_term.encode(&mut out_bin).unwrap();
                write_packet4(o, &out_bin)?
            },
            None =>
                ()
        }
    }
}
