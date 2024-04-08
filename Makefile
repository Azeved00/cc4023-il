DIR=./runtime
BIN=./bin
CC = gcc -Wall
LIB= $(DIR)/heap.c $(DIR)/secd.h $(DIR)/heap.h

secd : $(DIR)/secd.c $(LIB)
	mkdir -p $(BIN)
	$(CC) -o $(BIN)/secd $(DIR)/secd.c $(DIR)/heap.c

sec : $(DIR)/sec.c $(LIB)
	mkdir -p $(BIN)
	$(CC) -o $(BIN)/sec $(DIR)/sec.c $(DIR)/heap.c

