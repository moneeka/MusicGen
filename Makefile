all: ps6

FILES=music.ml

ps6: $(FILES)
	@echo "Compiling..."
	corebuild music.native

check: $(FILES)
	chmod u+x ../check_width
	../check_width music.ml

clean:
	rm -rf _build *.native
