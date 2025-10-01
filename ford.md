---
project: DataFort
summary: Dataframes In Fortran
author: Renee Gil
github: https://github.com/rngil/DataFort
project_github: https://github.com/rngil/DataFort
project_website: https://rngil.github.io/DataFort
email: renee.gil@example.com
src_dir: ./src
output_dir: ./docs
exclude_dir: ./build
page_dir: ./pages
media_dir: ./media
css: ./media/ford_custom.css
favicon: ./media/favicon.png
docmark_alt: @
predocmark: >
display: public
         protected
         private
source: true
graph: true
search: true
macro: TEST
       LOGIC=.true.
extra_mods: json_module: http://jacobwilliams.github.io/json-fortran/
            futility: http://cmacmackin.github.io
lower: false
sort: alpha
extra_filetypes: sh #
print_creation_date: true
creation_date: %Y-%m-%d %H:%M %z
md_extensions: markdown.extensions.toc
               markdown.extensions.smarty
incl_src: false
---
---

<p align="center" style="margin: 0; padding: 0;">
  <img src="media/logo.png" alt="DataFort logo" width="100" style="vertical-align:middle; margin-right: 10px;"/> <span style="font-size:3.5em; font-weight:bold; vertical-align:middle;">DataFort</span>
</p>

---

DataFort is a dataframes library for Fortran, providing pandas/polars/dplyr-like functionality for scientific computing and data analysis.

<div style="background-color: #fff3cd; border-left: 4px solid #ffc107; padding: 15px; margin: 20px 0; border-radius: 4px;">
  <strong>⚠️ Warning:</strong> Project is very much a WIP. Don't use in production.
</div>


---
## Installation
---

### Using fpm

```toml
[dependencies]
DataFort = { git = "https://github.com/rngil/DataFort" }
```

### From Source

```bash
git clone https://github.com/rngil/DataFort.git
cd DataFort
fpm build
```

To run tests:
```bash
fpm test
```

---
## Quick Start
--- 

```fortran
program example
    use datafort
    use precision
    implicit none

    type(data_frame) :: df
    real(rk), dimension(3) :: temps = [23.1_rk, 25.3_rk, 24.8_rk]
    integer(ik), dimension(3) :: pressures = [1013_ik, 1015_ik, 1012_ik]

    ! Create and populate
    call df%new()
    call df_append_real(df, temps, "Temperature")
    call df_append_integer(df, pressures, "Pressure")

    ! Display
    call df_write_console(df)

    ! Statistics
    print*, "Mean temperature:", df_mean_real(df, 1)

    ! Export
    call df_write_csv(df, "data.csv")

    ! Cleanup
    call df%destroy()
end program example
```

--- 
## Development
--- 

```bash
make help      # Show all available commands
make build     # Build the library
make test      # Run all tests
make docs      # Generate documentation
make format    # Format code
make clean     # Clean build artifacts
```

--- 
## Acknowledgments
--- 

DataFort is built upon the initial work from [fortranDF](https://github.com/jaiken17/fortranDF) by Joshua Aiken. While the project has been significantly extended and refactored with new features, the original skeleton provided a valuable foundation.
