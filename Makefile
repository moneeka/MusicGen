all: ps6

FILES=music.ml matrix.ml read.ml

ps6: $(FILES)
	@echo "Compiling..."
	corebuild music.native
	corebuild matrix.native
	corebuild read.native

check: $(FILES)
	chmod u+x ../check_width
	../check_width music.ml
	../check_width matrix.ml
	../check_width read.ml

clean:
	rm -rf _build *.native
