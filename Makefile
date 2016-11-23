all: doodle2


doodle2:
	SDL2_FLAGS="-I/usr/include/SDL2 -D_REENTRANT -w -L/usr/lib/x86_64-linux-gnu -lSDL2" chicken-install -s sdl2
	 chicken-install -s (inside the doodle2 directory)

clean:
	rm -f *.import.* *.so *~ 

