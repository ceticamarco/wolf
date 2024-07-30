TARGET = wolf
DEBUG_TARGET = wolfdebug

CC = gcc

DEBUG_CFLAGS = -Wall -Wextra -Werror -pedantic-errors -fstack-protector-strong \
		 -fsanitize=address -fsanitize=undefined -fstack-clash-protection \
		 -Wwrite-strings -std=c99 -g
CFLAGS = -Wall -Wextra -Werror -pedantic-errors -Wwrite-strings -std=c99 -O3

GIT_HASH := $(shell git rev-parse --short HEAD)
BUILD_FLAGS = -DVERSION=\"0.0.1\" -DHASH=\"$(GIT_HASH)\"

build: $(TARGET)
debug: $(DEBUG_TARGET)

$(TARGET): main.c
	$(CC) $(CFLAGS) $(BUILD_FLAGS) $^ -o $@

$(DEBUG_TARGET): main.c
	$(CC) $(DEBUG_CFLAGS) $(BUILD_FLAGS) $^ -o $@

clean:
	rm -f *.o *.a $(TARGET) $(DEBUG_TARGET)
