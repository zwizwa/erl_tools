all: ../priv/sqlite3.elf

clean:
	rm -rf *.o *.elf ../priv/sqlite3.elf

# Clean this up.
DEFS := -DMAIN=main -DREAD=read -DWRITE=write

%.o: %.c
	gcc -I../include -Wall -c $< -o $@ $(DEFS)
../priv/sqlite3.elf: sqlite3.o bert.o
	gcc -o $@ sqlite3.o bert.o -lsqlite3


