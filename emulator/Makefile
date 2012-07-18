OBJ_DIR=.obj
OBJ=$(OBJ_DIR)/alm_main.o $(OBJ_DIR)/alm_loader.o $(OBJ_DIR)/alm_debug.o $(OBJ_DIR)/alm_emu.o

H_FILES=$(wildcard src/*.h)

all: alm

alm: $(OBJ_DIR) $(OBJ)
	gcc -g -o $@ $(OBJ)

.obj/%.o: src/%.c $(H_FILES)
	gcc -g -c -Werror -Isrc -o $@ $<

$(OBJ_DIR):
	mkdir -p $@

clean:
	rm -rf .obj alm
