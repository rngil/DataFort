# DataFort

Dataframes in Fortran

[![Stable](https://img.shields.io/badge/docs-stable-blue.svg)](https://rngil.github.io/DataFort/index.html)

## Quick Start

```fortran
program example
    use datafort
    use precision
    implicit none

    type(data_frame) :: df
    real(rk), dimension(3) :: temps = [23.1_rk, 25.3_rk, 24.8_rk]

    call df%new()
    call df%append(temps, "Temperature")
    call df%write_console()
    print*, "Mean:", df%mean_real(1)
    call df%destroy()
end program
```

## Installation

### Using fpm

```toml
[dependencies]
DataFort = { git = "https://github.com/rngil/DataFort" }
```

### Manual

```bash
git clone https://github.com/rngil/DataFort
cd DataFort
make test  # Run test suite
```

## Development

```bash
make help      # Show all available commands
make build     # Build the library
make test      # Run all tests
make docs      # Generate documentation
make format    # Format code
make clean     # Clean build artifacts
```

## Acknowledgments

DataFort is built upon the initial work from [fortranDF](https://github.com/jaiken17/fortranDF) by Joshua Aiken. While the project has been significantly extended and refactored with new features, the original skeleton provided a valuable foundation.
