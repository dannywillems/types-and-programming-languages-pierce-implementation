OCAMLBUILD = ocamlbuild
OCAMLBUILD_OPT = -classic-display

SRC_DIR = src
TARGET = main.native

all:
	@$(OCAMLBUILD) $(OCAMLBUILD_OPT) $(SRC_DIR)/$(TARGET)

run: all
	@./$(TARGET)

clean:
	@$(OCAMLBUILD) -clean
