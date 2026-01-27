all:
	ocamlc -o sudoku2cnf A0_cnf_generator.ml
	ocamlc -o sol2grid A0_sol2grid.ml
	ocamlc -o check_multiple check_multiple.ml

run:
	./sudoku2cnf > problem.cnf
	z3 -dimacs problem.cnf > sat_output.txt
	./sol2grid sat_output.txt

check:
	./check_sudoku output.txt input.txt

unique:
	@if grep -q "UNSAT" sat_output.txt; then \
		echo "NO SOLUTION"; \
	else \
		./check_multiple sat_output.txt problem.cnf > problem2.cnf; \
		z3 -dimacs problem2.cnf > sat_output2.txt; \
		if grep -q "UNSAT" sat_output2.txt; then \
			echo "UNIQUE SOLUTION"; \
		else \
			echo "MULTIPLE SOLUTIONS"; \
		fi \
	fi
clean:
	rm -f sudoku2cnf sol2grid check_multiple *.cmi *.cmo problem*.cnf sat_output*.txt output.txt