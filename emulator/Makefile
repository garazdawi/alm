OBJ_DIR=.obj
OBJ=$(OBJ_DIR)/alm_main.o $(OBJ_DIR)/alm_loader.o $(OBJ_DIR)/alm_debug.o $(OBJ_DIR)/alm_emu.o

all: alm

alm: $(OBJ_DIR) $(OBJ)
	gcc -g -o $@ $(OBJ)

.obj/%.o: src/%.c
	gcc -g -c -werror -Isrc -o $@ $<

$(OBJ_DIR):
	mkdir -p $@

clean:
	rm -rf .obj alm
