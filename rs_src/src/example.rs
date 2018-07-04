#![feature(conservative_impl_trait)]
#![feature(slice_patterns,advanced_slice_patterns)]

extern crate erl_rs;
extern crate eetf;
use eetf::{Term,Atom};

/* Example behavior */
fn dispatch_tag(cmd: &str, _arg: &Term) -> Term {
    match cmd {
        "test" => Term::from(Atom::from("test")),
        bad => panic!("bad command: {:?}",bad)
    }
}

/* Packet handler */
fn main() {
    eprintln!("Rust {{packet,4}} port.");
    erl_rs::loop_dispatch_tagged_etf(
        &mut std::io::stdin(),
        &mut std::io::stdout(),
        &dispatch_tag).unwrap();
}
