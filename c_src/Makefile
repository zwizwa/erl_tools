ELF := ../priv/sqlite3.elf
all: ../priv/sqlite3.elf

clean:
	rm -rf *.o *.elf $(ELF)

# Clean this up.
DEFS := -DMAIN=main -DREAD=read -DWRITE=write

%.o: %.c
	. ./default.env ; $$CC $$CFLAGS -I../include -Wall -c $< -o $@ $(DEFS)

$(ELF): sqlite3.o bert.o
	mkdir -p ../priv
	. ./default.env ; $$CC $$LDFLAGS -o $@ sqlite3.o bert.o -lsqlite3
	file $@


