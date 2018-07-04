
# Put the binary in the priv/ dir
# Note that rebar3 doesn't really seem to handle multi-platform builds very well.
# It's best to just rebuild, or re-link _build to the platform dir before rebar runs

.PHONY: all

ELF:=../priv/erl_rs_example.elf
all: $(ELF) 

clean:
	rm -rf $(ELF) target

$(ELF): $(wildcard src/*.rs)
	echo -e "\"$$RUST_TARGET\""
	[ ! -z "$$RUST_TARGET" ] && CARGO_FLAGS="--target=$$RUST_TARGET" ; cargo build $$CARG_FLAGS --release
	[ ! -z "$$RUST_TARGET" ] && TARGET_DOR=$$RUST_TARGET/ ; cp -av target/$$TARGET_DIR/release/erl_rs_example $@
	ls -l $@

