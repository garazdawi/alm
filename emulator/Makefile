all: alm

alm: .obj/alm_main.o .obj/alm_loader.o
	gcc -o $@ $^

.obj/%.o: src/%.c .obj
	gcc -g -c -werror -Isrc -o $@ $<

.obj:
	mkdir -p .obj

clean:
	rm -rf .obj alm
