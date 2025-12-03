EXE=_build/default/mgoc.exe

all: $(EXE)

$(EXE): *.ml*
	dune build @all

test: $(EXE) tests/test.go
	-./$(EXE) tests/arith.go
	-./$(EXE) tests/div.go
	-./$(EXE) tests/instr.go
	-./$(EXE) tests/min.go
	-./$(EXE) tests/point.go
	-./$(EXE) tests/test.go
	-./$(EXE) tests/var.go

mips: test
	@echo "New File to spim"
	spim -file tests/arith.s
	@echo "\n"
	@echo "New File to spim"
	spim -file tests/div.s
	@echo "\n"
	@echo "New File to spim"
	spim -file tests/instr.s
	@echo "\n"
	@echo "New File to spim"
	spim -file tests/min.s
	@echo "\n"
	@echo "New File to spim"
	spim -file tests/point.s
	@echo "\n"
	@echo "New File to spim"
	spim -file tests/test.s
	@echo "\n"
	@echo "New File to spim"
	spim -file tests/var.s
	@echo "\n"


.PHONY: clean

clean:
	dune clean
	rm -f *~ tests/*.s
