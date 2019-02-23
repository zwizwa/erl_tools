redo-ifchange elf.list
ELF_INSTALL=$(cat elf.list | sed -e s/\\.elf/.elf.install/g)
redo-ifchange $ELF_INSTALL
