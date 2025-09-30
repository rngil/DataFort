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

DataFort is a dataframes library for Fortran, providing pandas-like functionality.

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
./run_all_tests.sh  # Run test suite
```

## Quick Start

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
    call df%append(temps, "Temperature")
    call df%append(pressures, "Pressure")

    ! Display
    call df%write_console()

    ! Statistics
    print*, "Mean temperature:", df%mean_real(1)

    ! Export
    call df%write_csv("data.csv")

    ! Cleanup
    call df%destroy()
end program example
```