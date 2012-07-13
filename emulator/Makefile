all: alm

alm: .obj/alm_main.o
	gcc -o $@ $^

.obj/%.o: src/%.c .obj
	gcc -g -c -werror -o $@ $<

.obj:
	mkdir -p .obj

clean:
	rm -rf .obj alm
