# Project's directory configuration

BIN_DIR = bin
LIB_DIR = lib
SRC_DIR = src

LANG_SRC_DIR = $(SRC_DIR)/ProtoLang
TYPE_SRC_DIR = $(SRC_DIR)/ProtoType
TC_SRC_DIR   = $(SRC_DIR)/ProtoTC

LANG_LIB_SRC = $(LANG_SRC_DIR)/ProtoOperators.ml $(LANG_SRC_DIR)/ProtoLangScanner.ml $(LANG_SRC_DIR)/ProtoLangParser.ml 
TYPE_LIB_SRC = $(TYPE_SRC_DIR)/ProtoTypeScanner.ml $(TYPE_SRC_DIR)/ProtoTypeParser.ml
TC_LIB_SRC   = $(TC_SRC_DIR)/ProtoTypeChecker.ml
MAIN_SRC 	 = $(SRC_DIR)/Main.ml


# Compilation tasks

all: $(BIN_DIR)/prototc

test: install $(BIN_DIR)/testProtoLangParser $(BIN_DIR)/testProtoTypeParser

$(BIN_DIR)/prototc: install $(LIB_DIR)/ProtoTCLib.cma $(SRC_DIR)/Main.ml
	ocamlc -o $(BIN_DIR)/prototc -I $(TC_SRC_DIR) -I $(TYPE_SRC_DIR) -I $(LANG_SRC_DIR) $(LIB_DIR)/ProtoLangLib.cma $(LIB_DIR)/ProtoTypeLib.cma $(LIB_DIR)/ProtoTCLib.cma $(MAIN_SRC)

$(BIN_DIR)/testProtoLangParser: install $(LIB_DIR)/ProtoLangLib.cma $(LANG_SRC_DIR)/TestProtoLangParser.ml
	ocamlc -o $(BIN_DIR)/testProtoLangParser -I $(LANG_SRC_DIR) $(LIB_DIR)/ProtoLangLib.cma $(LANG_SRC_DIR)/TestProtoLangParser.ml

$(BIN_DIR)/testProtoTypeParser: install $(LIB_DIR)/ProtoTypeLib.cma $(TYPE_SRC_DIR)/TestProtoTypeParser.ml $(LANG_SRC_DIR)/ProtoOperators.ml
	ocamlc -o $(BIN_DIR)/testProtoTypeParser -I $(TYPE_SRC_DIR) -I $(LANG_SRC_DIR) $(LIB_DIR)/ProtoTypeLib.cma $(LANG_SRC_DIR)/ProtoOperators.ml $(TYPE_SRC_DIR)/TestProtoTypeParser.ml

$(LIB_DIR)/ProtoLangLib.cma: install $(LANG_LIB_SRC)
	ocamlc -a -o $(LIB_DIR)/ProtoLangLib.cma -I $(LANG_SRC_DIR) $(LANG_LIB_SRC)

$(LIB_DIR)/ProtoTypeLib.cma: install $(TYPE_LIB_SRC)
	ocamlc -a -o $(LIB_DIR)/ProtoTypeLib.cma -I $(TYPE_SRC_DIR) $(TYPE_LIB_SRC)

$(LIB_DIR)/ProtoTCLib.cma: install $(TC_LIB_SRC) $(LIB_DIR)/ProtoLangLib.cma $(LIB_DIR)/ProtoTypeLib.cma
	ocamlc -a -o $(LIB_DIR)/ProtoTCLib.cma -I $(TC_SRC_DIR) -I $(TYPE_SRC_DIR) -I $(LANG_SRC_DIR) $(TC_LIB_SRC)

install:
	mkdir -p $(BIN_DIR) $(LIB_DIR)
	
clean:
	rm -rf $(LANG_SRC_DIR)/*.cmi $(LANG_SRC_DIR)/*.cmo $(TC_SRC_DIR)/*.cmi $(TC_SRC_DIR)/*.cmo $(TYPE_SRC_DIR)/*.cmi $(TYPE_SRC_DIR)/*.cmo $(LIB_DIR)/* $(BIN_DIR)/*

.PHONY: test clean
