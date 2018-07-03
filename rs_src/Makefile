
# Put the binary in the priv/ dir
# Note that rebar3 doesn't really seem to handle multi-platform builds very well.
# It's best to just rebuild, or re-link _build to the platform dir before rebar runs

.PHONY: all

ELF:=../priv/erl_rs.elf
all: $(ELF) 

clean:
	rm -rf $(ELF) target

$(ELF): src/erl_rs.rs
	[ ! -z "$$RUST_TARGET" ]
	cargo build --target=$$RUST_TARGET --release
	cp -av target/$$RUST_TARGET/release/erl_rs $@
	ls -l $@

