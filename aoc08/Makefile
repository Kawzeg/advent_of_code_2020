objects = main.obj

all: $(objects)
	alink $(objects)

$(objects): %.obj: %.asm
	nasm $< -f obj -o $@
clean:
	rm *.obj
