OBJ_DIR=.obj
OBJ=$(OBJ_DIR)/alm_main.o $(OBJ_DIR)/alm_loader.o $(OBJ_DIR)/alm_debug.o

all: alm

alm: $(OBJ)
	gcc -g -o $@ $^

.obj/%.o: src/%.c .obj
	gcc -g -c -werror -Isrc -o $@ $<

.obj:
	mkdir -p .obj

clean:
	rm -rf .obj alm
