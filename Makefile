##
## EPITECH PROJECT, 2025
## glados
## File description:
## Makefile
##

BINARY = glados
STACK_BIN = $(shell stack path --local-install-root)/bin/$(BINARY)

.PHONY: all re clean fclean test_run

all: $(BINARY)

$(BINARY):
	stack build
	cp $(STACK_BIN) ./$(BINARY)

clean:
	stack clean

fclean: clean
	rm -f $(BINARY)

re: fclean all

test_run:
	@echo "Running tests with coverage..."
	@stack test --coverage
	@echo ""
	@echo "=== Coverage Summary ==="
	@stack hpc report glados-test --all 2>/dev/null || echo "Coverage report generated. Check .stack-work/install/.../hpc/index.html"
	@echo ""
	@echo "For detailed HTML coverage, run: make coverage-html"

coverage-html:
	@HPC_DIR=$$(stack path --local-install-root)/hpc; \
	if [ -f "$$HPC_DIR/index.html" ]; then \
		xdg-open "$$HPC_DIR/index.html" 2>/dev/null || open "$$HPC_DIR/index.html" 2>/dev/null || echo "Please open: $$HPC_DIR/index.html"; \
	else \
		echo "No coverage report found. Run 'make test_run' first."; \
	fi
