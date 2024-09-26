BIN=compiler
SRCDIR=src
SOURCES=$(wildcard $(SRCDIR)/*.hs)
TESTDIR=test
TEST_FILES=$(wildcard $(TESTDIR)/*.ol)
TEST_ASM=$(patsubst $(TESTDIR)/%.ol, $(TESTDIR)/%.asm, $(TEST_FILES))
TEST_O=$(patsubst $(TESTDIR)/%.asm, $(TESTDIR)/%.o, $(TEST_ASM))
TEST_BIN=$(patsubst $(TESTDIR)/%.o, $(TESTDIR)/%.bin, $(TEST_O))

.PHONY: default test asm_tests compile_tests clean_tests

$(BIN): $(SOURCES)
	ghc -i$(SRCDIR) -no-keep-o-files -no-keep-hi-files -dynamic -o $(BIN) $(SRCDIR)/Main.hs

$(TESTDIR)/%.asm: $(TESTDIR)/%.ol $(BIN)
	./$(BIN) $<

$(TESTDIR)/%.o: $(TESTDIR)/%.asm
	nasm -felf64 $<

$(TESTDIR)/%.bin: $(TESTDIR)/%.o
	ld -o $(@D)/$(*F).bin $<

default: $(BIN)

compile_tests: $(TEST_BIN)

test: $(TEST_BIN)
	cd $(TESTDIR); \
	sh run_tests.sh

clean_test:
	rm $(TEST_BIN)
