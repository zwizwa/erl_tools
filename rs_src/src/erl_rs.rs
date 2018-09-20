/* Misc support code and specialization for eetf */

/* A simple Erlang port in Rust. */
extern crate eetf;
use std::vec::Vec;
use std::option::Option;
use std::io::{Read, Write, Cursor, Result};
use eetf::{Term,Tuple,Atom,FixInteger,Binary};

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

/* Constructors. */
pub fn tag(tag: &str, term: Term) -> Term {
    Term::from(Tuple::from(vec![atom(tag), term]))
}
pub fn i32(i: i32) -> Term {
    Term::from(FixInteger::from(i))
}
pub fn i32_vec(v: &[i32]) -> Term {
    let tv: Vec<Term> = v.into_iter().map(|i| i32(*i)).collect();
    Term::from(Tuple::from(tv))
}
pub fn atom(tag: &str) -> Term {
    Term::from(Atom::from(tag))
}
pub fn binary(bs: &[u8]) -> Term {
    Term::from(Binary::from(bs))
}

/* Destructors.  Matching deeply nested structures is really awkward.
 * Use Option instead, which is easier to use with ? syntax. */
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
pub fn as_u8_slice(arg: &Term) -> Option<&[u8]> {
    match arg {
        &Term::Binary(Binary {bytes: ref vec}) => Some(&vec[..]),
        _ => None
    }
}

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
