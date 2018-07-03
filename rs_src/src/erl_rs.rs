/* A simple Erlang port in Rust. */
#![feature(conservative_impl_trait)]
#![feature(slice_patterns,advanced_slice_patterns)]
extern crate eetf;
use std::io::{self, Read, Write, Cursor};
use eetf::{Term,Tuple,Atom};


/* Run time I/O errors are failures. */
fn io_assert<T>(res: io::Result<T>) -> T {
    match res {
        Ok(v) => v,
        Err(str) => {
            panic!("error: {}", str);
        }
    }
}
/* Read */
fn read(buf: &mut[u8]) {
    io_assert(io::stdin().read_exact(buf));
}
fn read_u32be() -> u32 {
    let mut b : [u8; 4]= [0; 4];
    read(&mut b);
    ((b[0] as u32) << 24) + ((b[1] as u32) << 16) + ((b[2] as u32) << 8) +  b[3] as u32
}
fn read_packet4() -> Vec<u8> {
    let nb = read_u32be();
    eprintln!("nb = {}", nb);
    let mut inbuf = vec![0; nb as usize];
    read(&mut inbuf);
    inbuf
}
/* Write */
fn write(buf: &[u8]) {
    let len = io_assert(io::stdout().write(buf));
    assert!(len == buf.len());
}
fn write_u32be(v: u32) {
    let b = [(v >> 24) as u8, (v >> 16) as u8, (v >> 8) as u8, v as u8];
    write(&b);
}
fn flush() {
    io_assert(io::stdout().flush())
}
fn write_packet4(buf: &[u8]) {
    write_u32be(buf.len() as u32);
    write(&buf);
    flush();
}
/* Behavior */
fn dispatch_cmd(cmd: &str, _arg: &Term) -> Term {
    match cmd {
        "test" => Term::from(Atom::from("test")),
        bad => panic!("bad command: {:?}",bad)
    }
}
fn dispatch_etf(in_bin: &[u8]) -> Vec<u8> {
    let in_term = Term::decode(Cursor::new(in_bin)).unwrap();
    /* Unpack {Cmd=atom(),Arg} and dispatch. */
    match in_term {
        Term::Tuple(Tuple{elements: terms}) => {
            match &terms[..] {
                &[Term::Atom(ref tag), ref arg] => {
                    let out_term = dispatch_cmd(&tag.name.as_ref(), arg);
                    /* Wrap it up and reply. */
                    let mut out_bin = Vec::new();
                    out_term.encode(&mut out_bin).unwrap();
                    return out_bin;
                },
                bad => { panic!("need {{atom(),_}}: {:?}",bad); }
            }
        },
        bad => { panic!("not a tuple: {:?}",bad); }
    }
}
/* Packet handler */
fn main() {
    eprintln!("Rust {{packet,4}} port.");
    loop {
        let in_bin = read_packet4();
        let out_bin = dispatch_etf(&in_bin);
        write_packet4(&out_bin);
    }
}

