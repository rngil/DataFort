.PHONY: all build test docs format clean help install

# Compiler settings
FC = gfortran
FFLAGS = -O2 -fPIC
SRC_DIR = src
TEST_DIR = test
BUILD_DIR = build
DOCS_DIR = docs

# Source files
SOURCES = $(SRC_DIR)/precision.f90 \
          $(SRC_DIR)/types.f90 \
          $(SRC_DIR)/column_class.f90 \
          $(SRC_DIR)/utilities.f90 \
          $(SRC_DIR)/datafort.f90

# Default target
all: build test

## help: Show this help message
help:
	@echo "DataFort - Makefile Commands"
	@echo "============================="
	@echo ""
	@echo "Available targets:"
	@echo "  make build    - Build the library"
	@echo "  make test     - Run all tests"
	@echo "  make docs     - Generate documentation with FORD"
	@echo "  make format   - Format code with fprettify"
	@echo "  make clean    - Clean build artifacts"
	@echo "  make install  - Install fprettify and check dependencies"
	@echo "  make all      - Build and test (default)"
	@echo ""

## build: Compile the library
build:
	@echo "Building DataFort library..."
	@mkdir -p $(BUILD_DIR)
	@$(FC) $(FFLAGS) -c $(SOURCES) -J$(BUILD_DIR)
	@echo "✓ Build complete"

## test: Run all tests
test:
	@echo "========================================"
	@echo "Running All DataFort Tests"
	@echo "========================================"
	@echo ""
	@PASSED=0; FAILED=0; \
	for test_file in test/unit/*.f90 test/integration/*.f90; do \
		test_name=$$(basename $$test_file .f90); \
		echo "Running $$test_name..."; \
		if $(FC) -o $$test_name $(SOURCES) $$test_file 2>&1 | grep -i "error" > /dev/null; then \
			echo "✗ $$test_name - COMPILATION FAILED"; \
			FAILED=$$((FAILED + 1)); \
		elif ./$$test_name > /dev/null 2>&1; then \
			echo "✓ $$test_name - PASSED"; \
			PASSED=$$((PASSED + 1)); \
		else \
			echo "✗ $$test_name - RUNTIME FAILED"; \
			FAILED=$$((FAILED + 1)); \
		fi; \
		echo ""; \
	done; \
	echo "========================================"; \
	echo "Test Summary"; \
	echo "========================================"; \
	echo "Passed: $$PASSED"; \
	echo "Failed: $$FAILED"; \
	echo "Total:  $$((PASSED + FAILED))"; \
	echo ""; \
	rm -f *.mod test_* check; \
	rm -rf test_data

## docs: Generate documentation with FORD
docs:
	@echo "Generating documentation with FORD..."
	@if ! command -v ford > /dev/null 2>&1; then \
		echo "Error: FORD is not installed."; \
		echo "Install with: pip install ford"; \
		exit 1; \
	fi
	@ford ford.md && open docs/index.html
	@echo "✓ Documentation generated in $(DOCS_DIR)/"
	@echo "  Open $(DOCS_DIR)/index.html to view"

## format: Format all Fortran code
format:
	@echo "========================================"
	@echo "Formatting Fortran Code with fprettify"
	@echo "========================================"
	@echo ""
	@if ! command -v fprettify > /dev/null 2>&1; then \
		echo "Error: fprettify is not installed"; \
		echo ""; \
		echo "Install with:"; \
		echo "  pip install --upgrade fprettify"; \
		echo "or:"; \
		echo "  pip3 install --upgrade fprettify"; \
		exit 1; \
	fi
	@COUNT=0; \
	for file in $$(find src test -type f \( -name "*.f90" -o -name "*.F90" \) 2>/dev/null); do \
		echo "Formatting: $$file"; \
		fprettify --config-file .fprettify.rc --indent 4 --line-length 120 $$file; \
		COUNT=$$((COUNT + 1)); \
	done; \
	echo ""; \
	echo "✓ Formatted $$COUNT files"; \
	echo ""; \
	echo "To check formatting without changing files, run:"; \
	echo "  fprettify --diff src/datafort.f90"

## clean: Remove build artifacts
clean:
	@echo "Cleaning build artifacts..."
	@rm -rf $(BUILD_DIR)
	@rm -f *.mod *.o *.smod
	@rm -f test_* check
	@rm -rf test_data
	@echo "✓ Clean complete"

## install: Install development dependencies
install:
	@echo "Installing development dependencies..."
	@echo ""
	@echo "Checking Python packages..."
	@if ! command -v pip > /dev/null 2>&1 && ! command -v pip3 > /dev/null 2>&1; then \
		echo "Error: pip/pip3 not found. Please install Python first."; \
		exit 1; \
	fi
	@echo "Installing fprettify (code formatter)..."
	@pip install fprettify || pip3 install fprettify
	@echo "Installing FORD (documentation generator)..."
	@pip install ford || pip3 install ford
	@echo ""
	@echo "✓ Dependencies installed"
	@echo ""
	@echo "Optional: Install pFUnit for advanced testing"
	@echo "  See TESTING.md for instructions"

# Advanced targets

## fpm-build: Build with fpm (Fortran Package Manager)
fpm-build:
	@if ! command -v fpm > /dev/null 2>&1; then \
		echo "Error: fpm is not installed."; \
		echo "Install from: https://fpm.fortran-lang.org/install/"; \
		exit 1; \
	fi
	fpm build

## fpm-test: Run tests with fpm
fpm-test:
	@if ! command -v fpm > /dev/null 2>&1; then \
		echo "Error: fpm is not installed."; \
		exit 1; \
	fi
	fpm test

## coverage: Generate code coverage report
coverage:
	@echo "Generating coverage report..."
	@$(FC) -coverage -o test_coverage $(SOURCES) $(TEST_DIR)/test_datafort.f90
	@./test_coverage > /dev/null 2>&1
	@gcov $(SRC_DIR)/datafort.f90
	@echo "✓ Coverage report generated (datafort.f90.gcov)"
	@rm -f test_coverage *.gcda *.gcno

## check: Run format and test
check: format test

## dev: Full development cycle (format, build, test, docs)
dev: format build test docs
	@echo ""
	@echo "✓ Development cycle complete!"