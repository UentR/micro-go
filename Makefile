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

.PHONY: clean

clean:
	dune clean
	rm -f *~ tests/*~
