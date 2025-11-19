EXE=_build/default/mgoc.exe

all: $(EXE)

$(EXE): *.ml*
	dune build @all

test: $(EXE) tests/test.go
	-./$(EXE) --parse-only tests/arith.go
	-./$(EXE) --parse-only tests/div.go
	-./$(EXE) --parse-only tests/instr.go
	-./$(EXE) --parse-only tests/min.go
	-./$(EXE) --parse-only tests/point.go
	-./$(EXE) --parse-only tests/test.go
	-./$(EXE) --parse-only tests/var.go

.PHONY: clean

clean:
	dune clean
	rm -f *~ tests/*~
