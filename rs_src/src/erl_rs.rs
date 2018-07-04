/* Misc support code and specialization for eetf */

/* A simple Erlang port in Rust. */
#![feature(conservative_impl_trait)]
#![feature(slice_patterns,advanced_slice_patterns)]
extern crate eetf;
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

// just re-use io::Error
fn error(str: String) -> std::io::Error {
    std::io::Error::new(std::io::ErrorKind::Other, str)
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


/* Dispatcher for {atom(),_} commands */
pub fn dispatch_tagged_etf(
    in_bin: &[u8],
    dispatch_tag: &Fn(&str, &Term) -> Term
) -> Result<Vec<u8>>
{
    let in_term = Term::decode(Cursor::new(in_bin)).unwrap();
    /* Unpack {Cmd=atom(),Arg} and dispatch. */
    match in_term {
        Term::Tuple(Tuple{elements: terms}) => {
            match &terms[..] {
                &[Term::Atom(ref tag), ref arg] => {
                    let out_term = dispatch_tag(&tag.name.as_ref(), arg);
                    /* Wrap it up and reply. */
                    let mut out_bin = Vec::new();
                    out_term.encode(&mut out_bin).unwrap();
                    return Ok(out_bin);
                },
                bad => { Err(error(format!("need {{atom(),_}}: {:?}",bad))) }
            }
        },
        bad => { Err(error(format!("not a tuple: {:?}",bad))) }
    }
}

// FIXME: Handle single/dual stream case more elegantly.
// stdio needs 2, while sockets need 1.

pub fn loop_dispatch_tagged_etf<In: Read, Out: Write>(
    i: &mut In, o: &mut Out,
    dispatch_tag: &Fn(&str, &Term) -> Term
) -> Result<()>
{
    loop {
        let in_bin = read_packet4(i)?;
        let out_bin = dispatch_tagged_etf(&in_bin, &dispatch_tag)?;
        write_packet4(o, &out_bin)?;
    }
}

pub fn loop_dispatch_tagged1_etf<IO: Read+Write>(
    io: &mut IO,
    dispatch_tag: &Fn(&str, &Term) -> Term
) -> Result<()>
{
    loop {
        let in_bin = read_packet4(io)?;
        let out_bin = dispatch_tagged_etf(&in_bin, &dispatch_tag)?;
        write_packet4(io, &out_bin)?;
    }
}
