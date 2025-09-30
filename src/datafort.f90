!> DataFort - Modern Data Frame Library for Fortran
!!
!! This module provides a comprehensive data frame implementation for Fortran,
!! offering pandas-like functionality for scientific computing and data analysis.
!!
!! ## Features
!!
!! - **Mixed-type columns**: real, integer, logical, character, complex
!! - **Statistical operations**: mean, std, median, percentile, variance, correlation
!! - **Data manipulation**: filter, sort, slice, transpose, join
!! - **Mathematical functions**: cumsum, diff, normalize, log, exp, sqrt
!! - **I/O operations**: CSV import/export, console display
!! - **Convenience methods**: head, tail, info, describe, sample, shuffle
!!
!! ## Example Usage
!!
!! ```fortran
!! use datafort
!! use precision
!! type(data_frame) :: df
!! real(rk) :: temps(3) = [20.0_rk, 25.0_rk, 22.0_rk]
!!
!! call df%new()
!! call df%append(temps, "Temperature")
!! call df%write_console()
!! print*, "Mean:", df%mean_real(1)
!! call df%destroy()
!! ```
module datafort
    use precision
    use types
    use column_class
    use utilities
    implicit none
    private

    public :: data_frame

    integer, parameter :: MAX_CHAR_LEN_DEFAULT = 100

    !> Main data frame type for storing heterogeneous tabular data
    !!
    !! A data frame consists of columns of potentially different types,
    !! similar to a spreadsheet or database table. Each column must have
    !! the same number of rows.
    !!
    !! ## Type-bound Procedures
    !!
    !! ### Constructor/Destructor
    !! - `new([char_len])` - Initialize data frame
    !! - `destroy()` - Free memory
    !!
    !! ### Data Manipulation
    !! - `append(column, [header])` - Add column (generic for all types)
    !! - `getr/geti/getl/getch/getc(index)` - Get column by type
    !! - `select_columns(indices)` - Extract subset of columns
    !! - `slice_rows(start, end)` - Extract row range
    !! - `filter_rows_*()` - Filter by condition
    !! - `sort_by_column(col)` - Sort by column values
    !!
    !! ### Statistical Functions
    !! - `mean_*/std_*/median_*()` - Statistical measures
    !! - `sum_*/min_*/max_*()` - Aggregate functions
    !! - `correlation_real()` - Pearson correlation
    !!
    !! ### Join Operations
    !! - `inner_join/left_join/right_join/outer_join()` - SQL-style joins
    !!
    !! ### I/O
    !! - `write_csv(filename)` - Export to CSV
    !! - `read_csv(filename)` - Import from CSV
    !! - `write_console()` - Display in terminal
    type :: data_frame
        private

        integer :: num_cols = 0, max_char_len = MAX_CHAR_LEN_DEFAULT
        logical :: with_headers = .false.
        character(len=:), dimension(:), allocatable :: headers
        type(column), dimension(:), allocatable :: data_cols
        logical :: initialized = .false.

    contains
        private

        ! Constructor/Destructor
        procedure, public :: new => df_constructor
        procedure, public :: destroy => df_destructor
        procedure, public :: is_initialized => df_is_initialized

        ! Basic info
        procedure, public :: ncols => df_get_num_cols
        procedure, public :: nrows => df_get_num_rows
        procedure, public :: get_max_char_len => df_get_max_char_len
        procedure, public :: header => get_header
        procedure :: df_get_col_type_header, df_get_col_type_index
        generic, public :: dtype => df_get_col_type_header, df_get_col_type_index

        ! Data manipulation
        procedure :: add_col_real, add_col_integer, add_col_logical, &
            add_col_character, add_col_complex
        generic, public :: append => add_col_real, add_col_integer, add_col_logical, &
            add_col_character, add_col_complex

        ! Getters (get entire columns)
        procedure :: get_col_real, get_col_integer, get_col_logical, &
            get_col_character, get_col_complex
        generic, public :: getr => get_col_real
        generic, public :: geti => get_col_integer
        generic, public :: getl => get_col_logical
        generic, public :: getch => get_col_character
        generic, public :: getc => get_col_complex

        ! Getters (get single values)
        procedure, public :: get_val_real, get_val_integer, get_val_logical, &
            get_val_character, get_val_complex

        ! Setters (set entire columns)
        procedure :: set_col_real, set_col_integer, set_col_logical, &
            set_col_character, set_col_complex
        generic, public :: setr => set_col_real
        generic, public :: seti => set_col_integer
        generic, public :: setl => set_col_logical
        generic, public :: setch => set_col_character
        generic, public :: setc => set_col_complex

        ! Setters (set single values)
        procedure, public :: set_val_real, set_val_integer, set_val_logical, &
            set_val_character, set_val_complex

        ! Mathematical operations
        procedure, public :: sum_real, sum_integer
        procedure, public :: mean_real, mean_integer
        procedure, public :: std_real, std_integer
        procedure, public :: median_real, median_integer
        procedure, public :: percentile_real, percentile_integer
        procedure, public :: variance_real, variance_integer
        procedure, public :: correlation_real
        procedure, public :: normalize_column_real
        procedure, public :: standardize_column_real
        procedure, public :: abs_column_real, abs_column_integer
        procedure, public :: cumsum_real, cumsum_integer
        procedure, public :: diff_real, diff_integer
        procedure, public :: replace_value_real, replace_value_integer
        procedure, public :: clip_real, clip_integer
        procedure, public :: is_sorted_real, is_sorted_integer
        procedure, public :: round_column
        procedure, public :: log_column, exp_column, sqrt_column
        procedure, public :: pow_column

        ! I/O operations
        procedure, public :: write_csv
        procedure, public :: read_csv
        procedure, public :: write_console

        ! Data manipulation operations
        procedure, public :: select_columns
        procedure, public :: slice_rows
        procedure, public :: min_real, min_integer
        procedure, public :: max_real, max_integer
        procedure, public :: sort_by_column
        procedure, public :: copy
        procedure, public :: filter_rows_logical
        procedure, public :: filter_rows_real_range
        procedure, public :: filter_rows_integer_range
        procedure, public :: filter_rows_string_pattern
        procedure, public :: rename_column
        procedure, public :: drop_column
        procedure, public :: reorder_columns
        procedure, public :: transpose

        ! Convenience functions
        procedure, public :: head
        procedure, public :: tail
        procedure, public :: shape
        procedure, public :: info
        procedure, public :: empty
        procedure, public :: clear
        procedure, public :: describe_numeric
        procedure, public :: sample
        procedure, public :: shuffle

        ! Join operations
        procedure, public :: inner_join
        procedure, public :: left_join
        procedure, public :: right_join
        procedure, public :: outer_join

        ! NaN handling
        procedure, public :: isna_real, isna_integer
        procedure, public :: fillna_real, fillna_integer
        procedure, public :: dropna

        ! Advanced data operations
        procedure, public :: unique_real, unique_integer, unique_character
        procedure, public :: value_counts_real, value_counts_integer, value_counts_character
        procedure, public :: concat
        procedure, public :: merge
        procedure, public :: rank_real, rank_integer
        procedure, public :: duplicated
        procedure, public :: drop_duplicates
        procedure, public :: drop_duplicates_subset

        ! Row operations
        procedure, public :: apply_to_row_real
        procedure, public :: apply_to_all_rows_real

        ! Utility procedures
        procedure :: already_header
        procedure :: resize_storage
        procedure :: validate_column_addition
        procedure :: find_header_index
    end type data_frame

    ! Abstract interface for row functions
    abstract interface
        function row_func_real(row_values, num_cols) result(output)
            import :: rk
            real(rk), dimension(:), intent(in) :: row_values
            integer, intent(in) :: num_cols
            real(rk) :: output
        end function row_func_real
    end interface

contains

    !> Initialize a new data frame
    !!
    !! Creates a new empty data frame with optional character length specification
    !!
    !! @param[in,out] this The data frame instance
    !! @param[in] char_len Optional maximum character length for string columns (default: 100)
    !!
    !! @note If the data frame is already initialized, it will be destroyed first
    subroutine df_constructor(this, char_len)
        class(data_frame), intent(inout) :: this
        integer, intent(in), optional :: char_len

        if (this % initialized) call this % destroy()

        this % num_cols = 0
        this % with_headers = .false.

        if (present(char_len)) then
            this % max_char_len = char_len
        else
            this % max_char_len = MAX_CHAR_LEN_DEFAULT
        end if

        this % initialized = .true.
    end subroutine df_constructor

    !> Destroy a data frame and free all memory
    !!
    !! Deallocates all columns and resets the data frame to uninitialized state
    !!
    !! @param[in,out] this The data frame instance
    subroutine df_destructor(this)
        class(data_frame), intent(inout) :: this

        integer :: i

        if (allocated(this % data_cols)) then
            do i = 1, size(this % data_cols)
                call this % data_cols(i) % destroy()
            end do
            deallocate (this % data_cols)
        end if

        if (allocated(this % headers)) deallocate (this % headers)

        this % num_cols = 0
        this % with_headers = .false.
        this % initialized = .false.
    end subroutine df_destructor

    ! Check if initialized
    pure function df_is_initialized(this) result(is_init)
        class(data_frame), intent(in) :: this
        logical :: is_init
        is_init = this % initialized
    end function df_is_initialized

    ! Get number of columns
    pure function df_get_num_cols(this) result(num_cols)
        class(data_frame), intent(in) :: this
        integer :: num_cols
        num_cols = this % num_cols
    end function df_get_num_cols

    ! Get number of rows
    pure function df_get_num_rows(this) result(num_rows)
        class(data_frame), intent(in) :: this
        integer :: num_rows

        if (this % num_cols == 0) then
            num_rows = 0
        else
            num_rows = this % data_cols(1) % n
        end if
    end function df_get_num_rows

    ! Get max char length
    pure function df_get_max_char_len(this) result(max_len)
        class(data_frame), intent(in) :: this
        integer :: max_len
        max_len = this % max_char_len
    end function df_get_max_char_len

    ! Get header by index
    function get_header(this, index) result(header)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: index
        character(len=:), allocatable :: header

        if (.not. this % with_headers) error stop "data frame has no headers"
        if (index < 1 .or. index > this % num_cols) error stop "header index out of range"

        header = trim(this % headers(index))
    end function get_header

    ! Get column type by header
    pure function df_get_col_type_header(this, header) result(dtype)
        class(data_frame), intent(in) :: this
        character(len=*), intent(in) :: header
        integer :: dtype

        integer :: ind
        ind = this % find_header_index(header)
        dtype = this % data_cols(ind) % get_type()
    end function df_get_col_type_header

    ! Get column type by index
    pure function df_get_col_type_index(this, index) result(dtype)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: index
        integer :: dtype

        if (index < 1 .or. index > this % num_cols) error stop "column index out of range"
        dtype = this % data_cols(index) % get_type()
    end function df_get_col_type_index

    ! Find header index
    pure function find_header_index(this, header) result(index)
        class(data_frame), intent(in) :: this
        character(len=*), intent(in) :: header
        integer :: index

        integer :: i
        character(len=:), allocatable :: trimmed_header

        if (.not. this % with_headers) error stop "data frame has no headers"

        trimmed_header = trim(adjustl(header))
        index = -1

        do i = 1, this % num_cols
            if (trim(adjustl(this % headers(i))) == trimmed_header) then
                index = i
                exit
            end if
        end do

        if (index == -1) error stop "header not found"
    end function find_header_index

    ! Check if header already exists
    function already_header(this, header) result(exists)
        class(data_frame), intent(in) :: this
        character(len=*), intent(in) :: header
        logical :: exists

        integer :: i
        character(len=:), allocatable :: trimmed_header

        exists = .false.
        if (.not. this % with_headers) return

        trimmed_header = trim(adjustl(header))

        do i = 1, this % num_cols
            if (trim(adjustl(this % headers(i))) == trimmed_header) then
                exists = .true.
                exit
            end if
        end do
    end function already_header

    ! Resize storage arrays
    subroutine resize_storage(this)
        class(data_frame), intent(inout) :: this

        type(column), dimension(:), allocatable :: temp_cols
        character(len=:), dimension(:), allocatable :: temp_headers
        integer :: new_size, i

        new_size = this % num_cols + 1

        ! Resize columns array
        if (allocated(this % data_cols)) then
            allocate (temp_cols(size(this % data_cols)))
            do i = 1, size(this % data_cols)
                temp_cols(i) = this % data_cols(i)
            end do
            deallocate (this % data_cols)
        end if

        allocate (this % data_cols(new_size))

        if (allocated(temp_cols)) then
            do i = 1, size(temp_cols)
                this % data_cols(i) = temp_cols(i)
            end do
            deallocate (temp_cols)
        end if

        ! Resize headers array if needed
        if (this % with_headers) then
            if (allocated(this % headers)) then
                allocate (character(len=this % max_char_len) :: temp_headers(size(this % headers)))
                temp_headers = this % headers
                deallocate (this % headers)
            end if

            allocate (character(len=this % max_char_len) :: this % headers(new_size))

            if (allocated(temp_headers)) then
                this % headers(1:size(temp_headers)) = temp_headers
                deallocate (temp_headers)
            end if
        end if
    end subroutine resize_storage

    !> Append a real-valued column to the data frame
    !!
    !! Adds a new column of real numbers to the data frame with optional header
    !!
    !! @param[in,out] this The data frame instance
    !! @param[in] col Array of real values to append
    !! @param[in] header Optional column name (if not provided, no header is set)
    !!
    !! @warning All columns must have the same number of rows
    subroutine add_col_real(this, col, header)
        class(data_frame), intent(inout) :: this
        real(rk), dimension(:), intent(in) :: col
        character(len=*), intent(in), optional :: header

        call this % validate_column_addition(header, size(col))
        call this % resize_storage()

        this % num_cols = this % num_cols + 1
        call this % data_cols(this % num_cols) % new(col)

        if (present(header)) then
            this % headers(this % num_cols) = header
        end if
    end subroutine add_col_real

    ! Add integer column
    subroutine add_col_integer(this, col, header)
        class(data_frame), intent(inout) :: this
        integer(ik), dimension(:), intent(in) :: col
        character(len=*), intent(in), optional :: header

        call this % validate_column_addition(header, size(col))
        call this % resize_storage()

        this % num_cols = this % num_cols + 1
        call this % data_cols(this % num_cols) % new(col)

        if (present(header)) then
            this % headers(this % num_cols) = header
        end if
    end subroutine add_col_integer

    ! Add logical column
    subroutine add_col_logical(this, col, header)
        class(data_frame), intent(inout) :: this
        logical, dimension(:), intent(in) :: col
        character(len=*), intent(in), optional :: header

        call this % validate_column_addition(header, size(col))
        call this % resize_storage()

        this % num_cols = this % num_cols + 1
        call this % data_cols(this % num_cols) % new(col)

        if (present(header)) then
            this % headers(this % num_cols) = header
        end if
    end subroutine add_col_logical

    ! Add character column
    subroutine add_col_character(this, col, header)
        class(data_frame), intent(inout) :: this
        character(len=*), dimension(:), intent(in) :: col
        character(len=*), intent(in), optional :: header

        call this % validate_column_addition(header, size(col))
        call this % resize_storage()

        this % num_cols = this % num_cols + 1
        call this % data_cols(this % num_cols) % new(col)

        if (present(header)) then
            this % headers(this % num_cols) = header
        end if
    end subroutine add_col_character

    ! Add complex column
    subroutine add_col_complex(this, col, header)
        class(data_frame), intent(inout) :: this
        complex(rk), dimension(:), intent(in) :: col
        character(len=*), intent(in), optional :: header

        call this % validate_column_addition(header, size(col))
        call this % resize_storage()

        this % num_cols = this % num_cols + 1
        call this % data_cols(this % num_cols) % new(col)

        if (present(header)) then
            this % headers(this % num_cols) = header
        end if
    end subroutine add_col_complex

    ! Validate column addition
    subroutine validate_column_addition(this, header, col_size)
        class(data_frame), intent(inout) :: this
        character(len=*), intent(in), optional :: header
        integer, intent(in) :: col_size

        ! Check row size consistency
        if (this % num_cols > 0 .and. col_size /= this % nrows()) then
            error stop "column size must match existing columns"
        end if

        ! Handle headers
        if (present(header)) then
            if (this % num_cols == 0) then
                this % with_headers = .true.
                allocate (character(len=this % max_char_len) :: this % headers(0))
            else if (.not. this % with_headers) then
                error stop "cannot add header to data frame without headers"
            end if

            if (this % already_header(header)) then
                error stop "header already exists"
            end if
        else
            if (this % with_headers) then
                error stop "cannot add column without header to data frame with headers"
            end if
        end if
    end subroutine validate_column_addition

    ! Get real column by index
    function get_col_real(this, index) result(col)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: index
        real(rk), dimension(:), allocatable :: col

        if (index < 1 .or. index > this % num_cols) error stop "column index out of range"
        if (this % data_cols(index) % get_type() /= REAL_NUM) error stop "column is not real type"

        col = this % data_cols(index) % getr()
    end function get_col_real

    ! Get integer column by index
    function get_col_integer(this, index) result(col)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: index
        integer(ik), dimension(:), allocatable :: col

        if (index < 1 .or. index > this % num_cols) error stop "column index out of range"
        if (this % data_cols(index) % get_type() /= INTEGER_NUM) error stop "column is not integer type"

        col = this % data_cols(index) % geti()
    end function get_col_integer

    ! Get logical column by index
    function get_col_logical(this, index) result(col)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: index
        logical, dimension(:), allocatable :: col

        if (index < 1 .or. index > this % num_cols) error stop "column index out of range"
        if (this % data_cols(index) % get_type() /= LOGICAL_NUM) error stop "column is not logical type"

        col = this % data_cols(index) % getl()
    end function get_col_logical

    ! Get character column by index
    function get_col_character(this, index) result(col)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: index
        character(len=:), dimension(:), allocatable :: col

        if (index < 1 .or. index > this % num_cols) error stop "column index out of range"
        if (this % data_cols(index) % get_type() /= CHARACTER_NUM) error stop "column is not character type"

        col = this % data_cols(index) % getch()
    end function get_col_character

    ! Get complex column by index
    function get_col_complex(this, index) result(col)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: index
        complex(rk), dimension(:), allocatable :: col

        if (index < 1 .or. index > this % num_cols) error stop "column index out of range"
        if (this % data_cols(index) % get_type() /= COMPLEX_NUM) error stop "column is not complex type"

        col = this % data_cols(index) % getc()
    end function get_col_complex

    ! Mathematical operations - Sum
    function sum_real(this, col_index) result(total)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: col_index
        real(rk) :: total

        real(rk), dimension(:), allocatable :: col

        col = this % getr(col_index)
        total = sum(col)
    end function sum_real

    function sum_integer(this, col_index) result(total)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: col_index
        integer(ik) :: total

        integer(ik), dimension(:), allocatable :: col

        col = this % geti(col_index)
        total = sum(col)
    end function sum_integer

    !> Calculate the arithmetic mean of a real column
    !!
    !! Computes the average value of all elements in a real-valued column
    !!
    !! @param[in] this The data frame instance
    !! @param[in] col_index Index of the column (1-based)
    !! @return Mean value of the column
    function mean_real(this, col_index) result(avg)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: col_index
        real(rk) :: avg

        real(rk), dimension(:), allocatable :: col

        col = this % getr(col_index)
        avg = sum(col) / real(size(col), rk)
    end function mean_real

    function mean_integer(this, col_index) result(avg)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: col_index
        real(rk) :: avg

        integer(ik), dimension(:), allocatable :: col

        col = this % geti(col_index)
        avg = real(sum(col), rk) / real(size(col), rk)
    end function mean_integer

    ! Mathematical operations - Standard deviation
    function std_real(this, col_index) result(stddev)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: col_index
        real(rk) :: stddev

        real(rk), dimension(:), allocatable :: col
        real(rk) :: avg

        col = this % getr(col_index)
        avg = this % mean_real(col_index)
        stddev = sqrt(sum((col - avg)**2) / real(size(col) - 1, rk))
    end function std_real

    function std_integer(this, col_index) result(stddev)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: col_index
        real(rk) :: stddev

        integer(ik), dimension(:), allocatable :: col
        real(rk) :: avg

        col = this % geti(col_index)
        avg = this % mean_integer(col_index)
        stddev = sqrt(sum((real(col, rk) - avg)**2) / real(size(col) - 1, rk))
    end function std_integer

    ! Mathematical operations - Median
    function median_real(this, col_index) result(med)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: col_index
        real(rk) :: med

        real(rk), dimension(:), allocatable :: col, sorted_col
        integer :: n, mid

        col = this % getr(col_index)
        n = size(col)
        sorted_col = col

        ! Simple bubble sort for median calculation
        call quick_sort_real(sorted_col, 1, n)

        if (mod(n, 2) == 1) then
            mid = (n + 1) / 2
            med = sorted_col(mid)
        else
            mid = n / 2
            med = (sorted_col(mid) + sorted_col(mid + 1)) / 2.0_rk
        end if
    end function median_real

    function median_integer(this, col_index) result(med)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: col_index
        real(rk) :: med

        integer(ik), dimension(:), allocatable :: col, sorted_col
        integer :: n, mid

        col = this % geti(col_index)
        n = size(col)
        sorted_col = col

        call quick_sort_integer(sorted_col, 1, n)

        if (mod(n, 2) == 1) then
            mid = (n + 1) / 2
            med = real(sorted_col(mid), rk)
        else
            mid = n / 2
            med = (real(sorted_col(mid), rk) + real(sorted_col(mid + 1), rk)) / 2.0_rk
        end if
    end function median_integer

    ! Mathematical operations - Percentile
    function percentile_real(this, col_index, percentile) result(perc)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: col_index
        real(rk), intent(in) :: percentile
        real(rk) :: perc

        real(rk), dimension(:), allocatable :: col, sorted_col
        integer :: n, idx
        real(rk) :: pos

        if (percentile < 0.0_rk .or. percentile > 100.0_rk) error stop "percentile must be between 0 and 100"

        col = this % getr(col_index)
        n = size(col)
        sorted_col = col

        call quick_sort_real(sorted_col, 1, n)

        pos = (percentile / 100.0_rk) * real(n - 1, rk) + 1.0_rk
        idx = int(pos)

        if (idx >= n) then
            perc = sorted_col(n)
        else if (idx < 1) then
            perc = sorted_col(1)
        else
            ! Linear interpolation
            perc = sorted_col(idx) + (pos - real(idx, rk)) * (sorted_col(idx + 1) - sorted_col(idx))
        end if
    end function percentile_real

    function percentile_integer(this, col_index, percentile) result(perc)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: col_index
        real(rk), intent(in) :: percentile
        real(rk) :: perc

        integer(ik), dimension(:), allocatable :: col, sorted_col
        integer :: n, idx
        real(rk) :: pos

        if (percentile < 0.0_rk .or. percentile > 100.0_rk) error stop "percentile must be between 0 and 100"

        col = this % geti(col_index)
        n = size(col)
        sorted_col = col

        call quick_sort_integer(sorted_col, 1, n)

        pos = (percentile / 100.0_rk) * real(n - 1, rk) + 1.0_rk
        idx = int(pos)

        if (idx >= n) then
            perc = real(sorted_col(n), rk)
        else if (idx < 1) then
            perc = real(sorted_col(1), rk)
        else
            ! Linear interpolation
            perc = real(sorted_col(idx), rk) + (pos - real(idx, rk)) * &
                   (real(sorted_col(idx + 1), rk) - real(sorted_col(idx), rk))
        end if
    end function percentile_integer

    ! Mathematical operations - Variance
    function variance_real(this, col_index) result(var)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: col_index
        real(rk) :: var

        real(rk), dimension(:), allocatable :: col
        real(rk) :: avg

        col = this % getr(col_index)
        avg = this % mean_real(col_index)
        var = sum((col - avg)**2) / real(size(col) - 1, rk)
    end function variance_real

    function variance_integer(this, col_index) result(var)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: col_index
        real(rk) :: var

        integer(ik), dimension(:), allocatable :: col
        real(rk) :: avg

        col = this % geti(col_index)
        avg = this % mean_integer(col_index)
        var = sum((real(col, rk) - avg)**2) / real(size(col) - 1, rk)
    end function variance_integer

    ! Get single value procedures
    function get_val_real(this, col_index, row_index) result(val)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: col_index, row_index
        real(rk) :: val

        if (col_index < 1 .or. col_index > this % num_cols) error stop "column index out of range"
        if (this % data_cols(col_index) % get_type() /= REAL_NUM) error stop "column is not real type"

        val = this % data_cols(col_index) % getr(row_index)
    end function get_val_real

    function get_val_integer(this, col_index, row_index) result(val)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: col_index, row_index
        integer(ik) :: val

        if (col_index < 1 .or. col_index > this % num_cols) error stop "column index out of range"
        if (this % data_cols(col_index) % get_type() /= INTEGER_NUM) error stop "column is not integer type"

        val = this % data_cols(col_index) % geti(row_index)
    end function get_val_integer

    function get_val_logical(this, col_index, row_index) result(val)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: col_index, row_index
        logical :: val

        if (col_index < 1 .or. col_index > this % num_cols) error stop "column index out of range"
        if (this % data_cols(col_index) % get_type() /= LOGICAL_NUM) error stop "column is not logical type"

        val = this % data_cols(col_index) % getl(row_index)
    end function get_val_logical

    function get_val_character(this, col_index, row_index) result(val)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: col_index, row_index
        character(len=:), allocatable :: val

        if (col_index < 1 .or. col_index > this % num_cols) error stop "column index out of range"
        if (this % data_cols(col_index) % get_type() /= CHARACTER_NUM) error stop "column is not character type"

        val = this % data_cols(col_index) % getch(row_index)
    end function get_val_character

    function get_val_complex(this, col_index, row_index) result(val)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: col_index, row_index
        complex(rk) :: val

        if (col_index < 1 .or. col_index > this % num_cols) error stop "column index out of range"
        if (this % data_cols(col_index) % get_type() /= COMPLEX_NUM) error stop "column is not complex type"

        val = this % data_cols(col_index) % getc(row_index)
    end function get_val_complex

    ! Set single value procedures
    subroutine set_val_real(this, col_index, row_index, val)
        class(data_frame), intent(inout) :: this
        integer, intent(in) :: col_index, row_index
        real(rk), intent(in) :: val

        if (col_index < 1 .or. col_index > this % num_cols) error stop "column index out of range"
        if (this % data_cols(col_index) % get_type() /= REAL_NUM) error stop "column is not real type"

        call this % data_cols(col_index) % changer(row_index, val)
    end subroutine set_val_real

    subroutine set_val_integer(this, col_index, row_index, val)
        class(data_frame), intent(inout) :: this
        integer, intent(in) :: col_index, row_index
        integer(ik), intent(in) :: val

        if (col_index < 1 .or. col_index > this % num_cols) error stop "column index out of range"
        if (this % data_cols(col_index) % get_type() /= INTEGER_NUM) error stop "column is not integer type"

        call this % data_cols(col_index) % changei(row_index, val)
    end subroutine set_val_integer

    subroutine set_val_logical(this, col_index, row_index, val)
        class(data_frame), intent(inout) :: this
        integer, intent(in) :: col_index, row_index
        logical, intent(in) :: val

        if (col_index < 1 .or. col_index > this % num_cols) error stop "column index out of range"
        if (this % data_cols(col_index) % get_type() /= LOGICAL_NUM) error stop "column is not logical type"

        call this % data_cols(col_index) % changel(row_index, val)
    end subroutine set_val_logical

    subroutine set_val_character(this, col_index, row_index, val)
        class(data_frame), intent(inout) :: this
        integer, intent(in) :: col_index, row_index
        character(len=*), intent(in) :: val

        if (col_index < 1 .or. col_index > this % num_cols) error stop "column index out of range"
        if (this % data_cols(col_index) % get_type() /= CHARACTER_NUM) error stop "column is not character type"

        call this % data_cols(col_index) % changech(row_index, val)
    end subroutine set_val_character

    subroutine set_val_complex(this, col_index, row_index, val)
        class(data_frame), intent(inout) :: this
        integer, intent(in) :: col_index, row_index
        complex(rk), intent(in) :: val

        if (col_index < 1 .or. col_index > this % num_cols) error stop "column index out of range"
        if (this % data_cols(col_index) % get_type() /= COMPLEX_NUM) error stop "column is not complex type"

        call this % data_cols(col_index) % changec(row_index, val)
    end subroutine set_val_complex

    ! Set entire column procedures
    subroutine set_col_real(this, col_index, col)
        class(data_frame), intent(inout) :: this
        integer, intent(in) :: col_index
        real(rk), dimension(:), intent(in) :: col

        if (col_index < 1 .or. col_index > this % num_cols) error stop "column index out of range"
        if (this % data_cols(col_index) % get_type() /= REAL_NUM) error stop "column is not real type"
        if (size(col) /= this % nrows()) error stop "column size mismatch"

        call this % data_cols(col_index) % destroy()
        call this % data_cols(col_index) % new(col)
    end subroutine set_col_real

    subroutine set_col_integer(this, col_index, col)
        class(data_frame), intent(inout) :: this
        integer, intent(in) :: col_index
        integer(ik), dimension(:), intent(in) :: col

        if (col_index < 1 .or. col_index > this % num_cols) error stop "column index out of range"
        if (this % data_cols(col_index) % get_type() /= INTEGER_NUM) error stop "column is not integer type"
        if (size(col) /= this % nrows()) error stop "column size mismatch"

        call this % data_cols(col_index) % destroy()
        call this % data_cols(col_index) % new(col)
    end subroutine set_col_integer

    subroutine set_col_logical(this, col_index, col)
        class(data_frame), intent(inout) :: this
        integer, intent(in) :: col_index
        logical, dimension(:), intent(in) :: col

        if (col_index < 1 .or. col_index > this % num_cols) error stop "column index out of range"
        if (this % data_cols(col_index) % get_type() /= LOGICAL_NUM) error stop "column is not logical type"
        if (size(col) /= this % nrows()) error stop "column size mismatch"

        call this % data_cols(col_index) % destroy()
        call this % data_cols(col_index) % new(col)
    end subroutine set_col_logical

    subroutine set_col_character(this, col_index, col)
        class(data_frame), intent(inout) :: this
        integer, intent(in) :: col_index
        character(len=*), dimension(:), intent(in) :: col

        if (col_index < 1 .or. col_index > this % num_cols) error stop "column index out of range"
        if (this % data_cols(col_index) % get_type() /= CHARACTER_NUM) error stop "column is not character type"
        if (size(col) /= this % nrows()) error stop "column size mismatch"

        call this % data_cols(col_index) % destroy()
        call this % data_cols(col_index) % new(col)
    end subroutine set_col_character

    subroutine set_col_complex(this, col_index, col)
        class(data_frame), intent(inout) :: this
        integer, intent(in) :: col_index
        complex(rk), dimension(:), intent(in) :: col

        if (col_index < 1 .or. col_index > this % num_cols) error stop "column index out of range"
        if (this % data_cols(col_index) % get_type() /= COMPLEX_NUM) error stop "column is not complex type"
        if (size(col) /= this % nrows()) error stop "column size mismatch"

        call this % data_cols(col_index) % destroy()
        call this % data_cols(col_index) % new(col)
    end subroutine set_col_complex

    !> Export data frame to a CSV file
    !!
    !! Writes the data frame to a CSV (Comma-Separated Values) file with headers
    !!
    !! @param[in] this The data frame instance
    !! @param[in] filename Path to the output CSV file
    !!
    !! @note All data types are automatically converted to appropriate string representations
    subroutine write_csv(this, filename)
        class(data_frame), intent(in) :: this
        character(len=*), intent(in) :: filename

        integer :: unit, i, j, dtype
        character(len=32) :: fmt_str

        open (newunit=unit, file=filename, status='replace', action='write')

        ! Write headers if present
        if (this % with_headers) then
            do i = 1, this % num_cols
                if (i > 1) write (unit, '(a)', advance='no') ','
                write (unit, '(a)', advance='no') trim(this % headers(i))
            end do
            write (unit, '(a)') ''
        end if

        ! Write data
        do j = 1, this % nrows()
            do i = 1, this % num_cols
                if (i > 1) write (unit, '(a)', advance='no') ','

                dtype = this % data_cols(i) % get_type()
                select case (dtype)
                case (REAL_NUM)
                    write (unit, '(g0)', advance='no') this % data_cols(i) % getr(j)
                case (INTEGER_NUM)
                    write (unit, '(i0)', advance='no') this % data_cols(i) % geti(j)
                case (LOGICAL_NUM)
                    write (unit, '(l1)', advance='no') this % data_cols(i) % getl(j)
                case (CHARACTER_NUM)
                    write (unit, '(a)', advance='no') '"'//trim(this % data_cols(i) % getch(j))//'"'
                case (COMPLEX_NUM)
                    write (unit, '("(",g0,",",g0,")")', advance='no') this % data_cols(i) % getc(j)
                end select
            end do
            write (unit, '(a)') ''
        end do

        close (unit)
    end subroutine write_csv

    subroutine read_csv(this, filename, has_headers)
        class(data_frame), intent(inout) :: this
        character(len=*), intent(in) :: filename
        logical, intent(in) :: has_headers

        integer :: unit, iostat, num_lines, num_cols, i, j
        character(len=1000) :: line
        character(len=100), allocatable :: fields(:), headers(:)
        character(len=100), allocatable :: all_data(:, :)

        ! Open file
        open (newunit=unit, file=filename, status='old', action='read', iostat=iostat)
        if (iostat /= 0) error stop "Cannot open CSV file"

        ! Count lines
        num_lines = 0
        do
            read (unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            num_lines = num_lines + 1
        end do
        rewind (unit)

        if (num_lines == 0) then
            close (unit)
            error stop "Empty CSV file"
        end if

        ! Read first line to determine number of columns
        read (unit, '(a)', iostat=iostat) line
        if (iostat /= 0) then
            close (unit)
            error stop "Cannot read CSV file"
        end if

        call parse_csv_line(line, fields)
        num_cols = size(fields)

        ! Allocate storage
        if (has_headers) then
            allocate (headers(num_cols))
            allocate (all_data(num_lines - 1, num_cols))
            headers = fields
        else
            allocate (all_data(num_lines, num_cols))
            all_data(1, :) = fields
            rewind (unit)
            read (unit, '(a)') line ! skip the line we already processed
        end if

        ! Read remaining data
        do i = 1, size(all_data, 1)
            read (unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit

            call parse_csv_line(line, fields)
            if (size(fields) /= num_cols) then
                close (unit)
                error stop "Inconsistent number of columns in CSV"
            end if
            all_data(i, :) = fields
        end do

        close (unit)

        ! Initialize data frame
        call this % new()

        ! Add columns with automatic type detection
        do j = 1, num_cols
            call add_csv_column(this, all_data(:, j), headers, j, has_headers)
        end do

        deallocate (fields, all_data)
        if (allocated(headers)) deallocate (headers)
    end subroutine read_csv

    ! Helper subroutine to parse a CSV line
    subroutine parse_csv_line(line, fields)
        character(len=*), intent(in) :: line
        character(len=100), allocatable, intent(out) :: fields(:)

        integer :: i, start, field_count, len_line
        logical :: in_quotes, is_delimiter
        character(len=100) :: temp_fields(50)  ! Max 50 fields
        character(len=:), allocatable :: trimmed_line

        trimmed_line = trim(line)
        len_line = len(trimmed_line)

        if (len_line == 0) then
            allocate (fields(0))
            return
        end if

        field_count = 0
        start = 1
        in_quotes = .false.

        do i = 1, len_line + 1
            is_delimiter = .false.

            ! Check for quotes within bounds
            if (i <= len_line) then
                if (trimmed_line(i:i) == '"') then
                    in_quotes = .not. in_quotes
                    cycle
                end if

                ! Check if current character is a delimiter
                if (trimmed_line(i:i) == ',' .and. .not. in_quotes) then
                    is_delimiter = .true.
                end if
            end if

            ! Process field if we hit a delimiter or end of line
            if (i > len_line .or. is_delimiter) then
                field_count = field_count + 1
                if (field_count > 50) error stop "Too many fields in CSV line"

                ! Extract the field
                if (i == start .or. start > len_line) then
                    temp_fields(field_count) = ""
                else
                    if (i > len_line) then
                        temp_fields(field_count) = trim(adjustl(trimmed_line(start:len_line)))
                    else
                        temp_fields(field_count) = trim(adjustl(trimmed_line(start:i - 1)))
                    end if

                    ! Remove surrounding quotes if present
                    if (len(trim(temp_fields(field_count))) >= 2) then
                        if (temp_fields(field_count) (1:1) == '"' .and. &
         temp_fields(field_count) (len(trim(temp_fields(field_count))):len(trim(temp_fields(field_count)))) == '"') then
                         temp_fields(field_count) = temp_fields(field_count) (2:len(trim(temp_fields(field_count))) - 1)
                        end if
                    end if
                end if
                start = i + 1
            end if
        end do

        allocate (fields(field_count))
        fields = temp_fields(1:field_count)
    end subroutine parse_csv_line

    ! Helper to add a column with automatic type detection
    subroutine add_csv_column(df, data_strings, headers, col_index, has_headers)
        class(data_frame), intent(inout) :: df
        character(len=*), dimension(:), intent(in) :: data_strings
        character(len=*), dimension(:), intent(in), optional :: headers
        integer, intent(in) :: col_index
        logical, intent(in) :: has_headers

        integer :: data_type, i, iostat
        real(rk), allocatable :: real_data(:)
        integer(ik), allocatable :: int_data(:)
        logical, allocatable :: logical_data(:)
        real(rk) :: real_val
        integer(ik) :: int_val
        logical :: logical_val
        character(len=100) :: trimmed_str

        ! Initialize NaN constant if needed
        call init_nan()

        ! Detect data type from first non-empty, non-NaN value
        data_type = CHARACTER_NUM
        do i = 1, size(data_strings)
            trimmed_str = trim(adjustl(data_strings(i)))

            ! Skip empty strings and common NaN representations
            if (len(trim(trimmed_str)) == 0 .or. &
                trimmed_str == "NaN" .or. trimmed_str == "nan" .or. &
                trimmed_str == "NA" .or. trimmed_str == "na" .or. &
                trimmed_str == "NULL" .or. trimmed_str == "null" .or. &
                trimmed_str == "N/A" .or. trimmed_str == "n/a" .or. &
                trimmed_str == "-" .or. trimmed_str == "") then
                cycle
            end if

            ! Try integer first
            read (data_strings(i), *, iostat=iostat) int_val
            if (iostat == 0) then
                data_type = INTEGER_NUM
                exit
            end if

            ! Try real
            read (data_strings(i), *, iostat=iostat) real_val
            if (iostat == 0) then
                data_type = REAL_NUM
                exit
            end if

            ! Try logical
            if (trimmed_str == "T" .or. trimmed_str == "F" .or. &
                trimmed_str == "true" .or. trimmed_str == "false" .or. &
                trimmed_str == ".true." .or. trimmed_str == ".false.") then
                data_type = LOGICAL_NUM
                exit
            end if

            ! Default to character
            exit
        end do

        ! Convert and add the column
        select case (data_type)
        case (INTEGER_NUM)
            allocate (int_data(size(data_strings)))
            do i = 1, size(data_strings)
                trimmed_str = trim(adjustl(data_strings(i)))

                ! Check for NaN representations
                if (len(trim(trimmed_str)) == 0 .or. &
                    trimmed_str == "NaN" .or. trimmed_str == "nan" .or. &
                    trimmed_str == "NA" .or. trimmed_str == "na" .or. &
                    trimmed_str == "NULL" .or. trimmed_str == "null" .or. &
                    trimmed_str == "N/A" .or. trimmed_str == "n/a" .or. &
                    trimmed_str == "-") then
                    int_data(i) = NaN_ik
                else
                    read (data_strings(i), *, iostat=iostat) int_data(i)
                    if (iostat /= 0) int_data(i) = NaN_ik  ! Use NaN for invalid data
                end if
            end do
            if (has_headers .and. present(headers)) then
                call df % append(int_data, headers(col_index))
            else
                call df % append(int_data)
            end if
        case (REAL_NUM)
            allocate (real_data(size(data_strings)))
            do i = 1, size(data_strings)
                trimmed_str = trim(adjustl(data_strings(i)))

                ! Check for NaN representations
                if (len(trim(trimmed_str)) == 0 .or. &
                    trimmed_str == "NaN" .or. trimmed_str == "nan" .or. &
                    trimmed_str == "NA" .or. trimmed_str == "na" .or. &
                    trimmed_str == "NULL" .or. trimmed_str == "null" .or. &
                    trimmed_str == "N/A" .or. trimmed_str == "n/a" .or. &
                    trimmed_str == "-") then
                    real_data(i) = NaN_rk
                else
                    read (data_strings(i), *, iostat=iostat) real_data(i)
                    if (iostat /= 0) real_data(i) = NaN_rk  ! Use NaN for invalid data
                end if
            end do
            if (has_headers .and. present(headers)) then
                call df % append(real_data, headers(col_index))
            else
                call df % append(real_data)
            end if
        case (LOGICAL_NUM)
            allocate (logical_data(size(data_strings)))
            do i = 1, size(data_strings)
                select case (trim(adjustl(data_strings(i))))
                case ("T", "true", ".true.")
                    logical_data(i) = .true.
                case ("F", "false", ".false.")
                    logical_data(i) = .false.
                case default
                    logical_data(i) = .false.  ! Default for invalid data
                end select
            end do
            if (has_headers .and. present(headers)) then
                call df % append(logical_data, headers(col_index))
            else
                call df % append(logical_data)
            end if
        case default ! CHARACTER_NUM
            if (has_headers .and. present(headers)) then
                call df % append(data_strings, headers(col_index))
            else
                call df % append(data_strings)
            end if
        end select
    end subroutine add_csv_column

    subroutine write_console(this, unit)
        class(data_frame), intent(in) :: this
        integer, intent(in), optional :: unit

        integer :: out_unit, i, j, dtype, col_width
        character(len=32) :: fmt_str

        if (present(unit)) then
            out_unit = unit
        else
            out_unit = 6  ! stdout
        end if

        if (this % num_cols == 0) then
            write (out_unit, '(a)') "Empty data frame"
            return
        end if

        col_width = 12  ! Fixed column width for formatting

        ! Write headers if present
        if (this % with_headers) then
            do i = 1, this % num_cols
                write (out_unit, '(a12)', advance='no') this % headers(i)
            end do
            write (out_unit, '(a)') ''

            ! Write separator line
            do i = 1, this % num_cols
                write (out_unit, '(a12)', advance='no') repeat('-', col_width)
            end do
            write (out_unit, '(a)') ''
        end if

        ! Write data
        do j = 1, this % nrows()
            do i = 1, this % num_cols
                dtype = this % data_cols(i) % get_type()
                select case (dtype)
                case (REAL_NUM)
                    write (out_unit, '(g12.4)', advance='no') this % data_cols(i) % getr(j)
                case (INTEGER_NUM)
                    write (out_unit, '(i12)', advance='no') this % data_cols(i) % geti(j)
                case (LOGICAL_NUM)
                    if (this % data_cols(i) % getl(j)) then
                        write (out_unit, '(a12)', advance='no') 'T'
                    else
                        write (out_unit, '(a12)', advance='no') 'F'
                    end if
                case (CHARACTER_NUM)
                    write (out_unit, '(a12)', advance='no') this % data_cols(i) % getch(j)
                case (COMPLEX_NUM)
                    write (out_unit, '("(",f5.2,",",f5.2,")")', advance='no') this % data_cols(i) % getc(j)
                end select
            end do
            write (out_unit, '(a)') ''
        end do
    end subroutine write_console

    ! Data Manipulation Operations

    ! Select specific columns to create a new data frame
    function select_columns(this, column_indices) result(new_df)
        class(data_frame), intent(in) :: this
        integer, dimension(:), intent(in) :: column_indices
        type(data_frame) :: new_df

        integer :: i, col_idx

        call new_df % new(this % max_char_len)

        do i = 1, size(column_indices)
            col_idx = column_indices(i)
            if (col_idx < 1 .or. col_idx > this % num_cols) then
                error stop "Column index out of range in select_columns"
            end if

            select case (this % data_cols(col_idx) % get_type())
            case (REAL_NUM)
                if (this % with_headers) then
                    call new_df % append(this % data_cols(col_idx) % getr(), this % headers(col_idx))
                else
                    call new_df % append(this % data_cols(col_idx) % getr())
                end if
            case (INTEGER_NUM)
                if (this % with_headers) then
                    call new_df % append(this % data_cols(col_idx) % geti(), this % headers(col_idx))
                else
                    call new_df % append(this % data_cols(col_idx) % geti())
                end if
            case (LOGICAL_NUM)
                if (this % with_headers) then
                    call new_df % append(this % data_cols(col_idx) % getl(), this % headers(col_idx))
                else
                    call new_df % append(this % data_cols(col_idx) % getl())
                end if
            case (CHARACTER_NUM)
                if (this % with_headers) then
                    call new_df % append(this % data_cols(col_idx) % getch(), this % headers(col_idx))
                else
                    call new_df % append(this % data_cols(col_idx) % getch())
                end if
            case (COMPLEX_NUM)
                if (this % with_headers) then
                    call new_df % append(this % data_cols(col_idx) % getc(), this % headers(col_idx))
                else
                    call new_df % append(this % data_cols(col_idx) % getc())
                end if
            end select
        end do
    end function select_columns

    ! Slice rows to create a new data frame
    function slice_rows(this, start_row, end_row) result(new_df)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: start_row, end_row
        type(data_frame) :: new_df

        integer :: i, j, new_size
        real(rk), allocatable :: real_slice(:)
        integer(ik), allocatable :: int_slice(:)
        logical, allocatable :: logical_slice(:)
        character(len=:), allocatable :: char_slice(:)
        complex(rk), allocatable :: complex_slice(:)

        if (start_row < 1 .or. end_row > this % nrows() .or. start_row > end_row) then
            error stop "Invalid row range in slice_rows"
        end if

        new_size = end_row - start_row + 1
        call new_df % new(this % max_char_len)

        do i = 1, this % num_cols
            select case (this % data_cols(i) % get_type())
            case (REAL_NUM)
                allocate (real_slice(new_size))
                do j = 1, new_size
                    real_slice(j) = this % data_cols(i) % getr(start_row + j - 1)
                end do
                if (this % with_headers) then
                    call new_df % append(real_slice, this % headers(i))
                else
                    call new_df % append(real_slice)
                end if
                deallocate (real_slice)
            case (INTEGER_NUM)
                allocate (int_slice(new_size))
                do j = 1, new_size
                    int_slice(j) = this % data_cols(i) % geti(start_row + j - 1)
                end do
                if (this % with_headers) then
                    call new_df % append(int_slice, this % headers(i))
                else
                    call new_df % append(int_slice)
                end if
                deallocate (int_slice)
            case (LOGICAL_NUM)
                allocate (logical_slice(new_size))
                do j = 1, new_size
                    logical_slice(j) = this % data_cols(i) % getl(start_row + j - 1)
                end do
                if (this % with_headers) then
                    call new_df % append(logical_slice, this % headers(i))
                else
                    call new_df % append(logical_slice)
                end if
                deallocate (logical_slice)
            case (CHARACTER_NUM)
                allocate (character(len=len(this % data_cols(i) % getch(1))) :: char_slice(new_size))
                do j = 1, new_size
                    char_slice(j) = this % data_cols(i) % getch(start_row + j - 1)
                end do
                if (this % with_headers) then
                    call new_df % append(char_slice, this % headers(i))
                else
                    call new_df % append(char_slice)
                end if
                deallocate (char_slice)
            case (COMPLEX_NUM)
                allocate (complex_slice(new_size))
                do j = 1, new_size
                    complex_slice(j) = this % data_cols(i) % getc(start_row + j - 1)
                end do
                if (this % with_headers) then
                    call new_df % append(complex_slice, this % headers(i))
                else
                    call new_df % append(complex_slice)
                end if
                deallocate (complex_slice)
            end select
        end do
    end function slice_rows

    ! Min/Max functions
    function min_real(this, col_index) result(min_val)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: col_index
        real(rk) :: min_val

        real(rk), dimension(:), allocatable :: col

        col = this % getr(col_index)
        min_val = minval(col)
    end function min_real

    function min_integer(this, col_index) result(min_val)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: col_index
        integer(ik) :: min_val

        integer(ik), dimension(:), allocatable :: col

        col = this % geti(col_index)
        min_val = minval(col)
    end function min_integer

    function max_real(this, col_index) result(max_val)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: col_index
        real(rk) :: max_val

        real(rk), dimension(:), allocatable :: col

        col = this % getr(col_index)
        max_val = maxval(col)
    end function max_real

    function max_integer(this, col_index) result(max_val)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: col_index
        integer(ik) :: max_val

        integer(ik), dimension(:), allocatable :: col

        col = this % geti(col_index)
        max_val = maxval(col)
    end function max_integer

    ! Filter rows based on a logical column
    function filter_rows_logical(this, logical_col_index) result(new_df)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: logical_col_index
        type(data_frame) :: new_df

        logical, dimension(:), allocatable :: mask
        integer, allocatable :: selected_rows(:)
        integer :: i, count_true, idx

        if (this % data_cols(logical_col_index) % get_type() /= LOGICAL_NUM) then
            error stop "Column must be logical type for filtering"
        end if

        mask = this % data_cols(logical_col_index) % getl()
        count_true = count(mask)

        if (count_true == 0) then
            call new_df % new(this % max_char_len)
            return
        end if

        ! Get indices of true values
        allocate (selected_rows(count_true))
        idx = 1
        do i = 1, size(mask)
            if (mask(i)) then
                selected_rows(idx) = i
                idx = idx + 1
            end if
        end do

        call new_df % new(this % max_char_len)

        ! Copy selected rows for each column
        do i = 1, this % num_cols
            call copy_filtered_column(this, new_df, i, selected_rows)
        end do

        deallocate (selected_rows)
    end function filter_rows_logical

    ! Copy data frame
    function copy(this) result(new_df)
        class(data_frame), intent(in) :: this
        type(data_frame) :: new_df

        integer :: i

        call new_df % new(this % max_char_len)

        do i = 1, this % num_cols
            select case (this % data_cols(i) % get_type())
            case (REAL_NUM)
                if (this % with_headers) then
                    call new_df % append(this % data_cols(i) % getr(), this % headers(i))
                else
                    call new_df % append(this % data_cols(i) % getr())
                end if
            case (INTEGER_NUM)
                if (this % with_headers) then
                    call new_df % append(this % data_cols(i) % geti(), this % headers(i))
                else
                    call new_df % append(this % data_cols(i) % geti())
                end if
            case (LOGICAL_NUM)
                if (this % with_headers) then
                    call new_df % append(this % data_cols(i) % getl(), this % headers(i))
                else
                    call new_df % append(this % data_cols(i) % getl())
                end if
            case (CHARACTER_NUM)
                if (this % with_headers) then
                    call new_df % append(this % data_cols(i) % getch(), this % headers(i))
                else
                    call new_df % append(this % data_cols(i) % getch())
                end if
            case (COMPLEX_NUM)
                if (this % with_headers) then
                    call new_df % append(this % data_cols(i) % getc(), this % headers(i))
                else
                    call new_df % append(this % data_cols(i) % getc())
                end if
            end select
        end do
    end function copy

    ! Sort data frame by a specific column (simplified - real and integer only)
    subroutine sort_by_column(this, col_index, ascending)
        class(data_frame), intent(inout) :: this
        integer, intent(in) :: col_index
        logical, intent(in), optional :: ascending

        logical :: asc
        integer, allocatable :: indices(:)
        integer :: i

        asc = .true.
        if (present(ascending)) asc = ascending

        if (col_index < 1 .or. col_index > this % num_cols) then
            error stop "Column index out of range in sort_by_column"
        end if

        allocate (indices(this % nrows()))
        do i = 1, this % nrows()
            indices(i) = i
        end do

        select case (this % data_cols(col_index) % get_type())
        case (REAL_NUM)
            call sort_indices_real(this % data_cols(col_index) % getr(), indices, asc)
        case (INTEGER_NUM)
            call sort_indices_integer(this % data_cols(col_index) % geti(), indices, asc)
        case default
            error stop "Sorting only supported for real and integer columns"
        end select

        call reorder_all_columns(this, indices)

        deallocate (indices)
    end subroutine sort_by_column

    ! Column manipulation functions
    subroutine rename_column(this, col_index, new_name)
        class(data_frame), intent(inout) :: this
        integer, intent(in) :: col_index
        character(len=*), intent(in) :: new_name

        if (col_index < 1 .or. col_index > this % num_cols) error stop "column index out of range"
        if (.not. this % with_headers) error stop "data frame has no headers to rename"

        this % headers(col_index) = new_name
    end subroutine rename_column

    subroutine drop_column(this, col_index)
        class(data_frame), intent(inout) :: this
        integer, intent(in) :: col_index

        integer :: i
        type(column), dimension(:), allocatable :: temp_cols
        character(len=:), dimension(:), allocatable :: temp_headers

        if (col_index < 1 .or. col_index > this % num_cols) error stop "column index out of range"

        ! Create temporary arrays without the dropped column
        allocate (temp_cols(this % num_cols - 1))
        if (this % with_headers) allocate (character(len=len(this % headers)) :: temp_headers(this % num_cols - 1))

        ! Copy columns before the dropped one
        do i = 1, col_index - 1
            temp_cols(i) = this % data_cols(i)
            if (this % with_headers) temp_headers(i) = this % headers(i)
        end do

        ! Copy columns after the dropped one
        do i = col_index + 1, this % num_cols
            temp_cols(i - 1) = this % data_cols(i)
            if (this % with_headers) temp_headers(i - 1) = this % headers(i)
        end do

        ! Destroy the dropped column
        call this % data_cols(col_index) % destroy()

        ! Replace arrays
        deallocate (this % data_cols)
        this % data_cols = temp_cols
        this % num_cols = this % num_cols - 1

        if (this % with_headers) then
            deallocate (this % headers)
            this % headers = temp_headers
        end if
    end subroutine drop_column

    subroutine reorder_columns(this, new_order)
        class(data_frame), intent(inout) :: this
        integer, dimension(:), intent(in) :: new_order

        integer :: i
        type(column), dimension(:), allocatable :: temp_cols
        character(len=:), dimension(:), allocatable :: temp_headers

        if (size(new_order) /= this % num_cols) error stop "new_order size must equal number of columns"

        ! Validate indices
        do i = 1, size(new_order)
            if (new_order(i) < 1 .or. new_order(i) > this % num_cols) error stop "invalid column index in new_order"
        end do

        ! Create temporary arrays in new order
        allocate (temp_cols(this % num_cols))
        if (this % with_headers) allocate (character(len=len(this % headers)) :: temp_headers(this % num_cols))

        do i = 1, this % num_cols
            temp_cols(i) = this % data_cols(new_order(i))
            if (this % with_headers) temp_headers(i) = this % headers(new_order(i))
        end do

        ! Replace arrays
        this % data_cols = temp_cols
        if (this % with_headers) this % headers = temp_headers
    end subroutine reorder_columns

    ! Transpose functionality (limited - converts to all character)
    function transpose(this) result(transposed_df)
        class(data_frame), intent(in) :: this
        type(data_frame) :: transposed_df

        integer :: i, j
        character(len=50), dimension(:), allocatable :: row_data
        character(len=20) :: temp_str

        call transposed_df % new()

        ! Add headers as first column if present
        if (this % with_headers) then
            call transposed_df % append(this % headers, "Headers")
        end if

        ! Add each row as a column
        do i = 1, this % nrows()
            allocate (row_data(this % num_cols))

            do j = 1, this % num_cols
                select case (this % data_cols(j) % get_type())
                case (REAL_NUM)
                    write (temp_str, '(f0.6)') this % data_cols(j) % getr(i)
                    row_data(j) = trim(temp_str)
                case (INTEGER_NUM)
                    write (temp_str, '(i0)') this % data_cols(j) % geti(i)
                    row_data(j) = trim(temp_str)
                case (LOGICAL_NUM)
                    if (this % data_cols(j) % getl(i)) then
                        row_data(j) = "T"
                    else
                        row_data(j) = "F"
                    end if
                case (CHARACTER_NUM)
                    row_data(j) = this % data_cols(j) % getch(i)
                case (COMPLEX_NUM)
                    write (temp_str, '("(",f0.3,",",f0.3,")")') this % data_cols(j) % getc(i)
                    row_data(j) = trim(temp_str)
                end select
            end do

            write (temp_str, '("Row_",i0)') i
            call transposed_df % append(row_data, trim(temp_str))
            deallocate (row_data)
        end do
    end function transpose

    ! Range filtering functions
    function filter_rows_real_range(this, col_index, min_val, max_val) result(filtered_df)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: col_index
        real(rk), intent(in) :: min_val, max_val
        type(data_frame) :: filtered_df

        integer, dimension(:), allocatable :: selected_rows
        integer :: i, count, idx
        real(rk) :: val

        if (this % data_cols(col_index) % get_type() /= REAL_NUM) error stop "column is not real type"

        ! Count matching rows
        count = 0
        do i = 1, this % nrows()
            val = this % data_cols(col_index) % getr(i)
            if (val >= min_val .and. val <= max_val) count = count + 1
        end do

        ! Collect matching row indices
        allocate (selected_rows(count))
        idx = 0
        do i = 1, this % nrows()
            val = this % data_cols(col_index) % getr(i)
            if (val >= min_val .and. val <= max_val) then
                idx = idx + 1
                selected_rows(idx) = i
            end if
        end do

        ! Create filtered dataframe
        call filtered_df % new(this % max_char_len)
        do i = 1, this % num_cols
            call copy_filtered_column(this, filtered_df, i, selected_rows)
        end do

        deallocate (selected_rows)
    end function filter_rows_real_range

    function filter_rows_integer_range(this, col_index, min_val, max_val) result(filtered_df)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: col_index
        integer(ik), intent(in) :: min_val, max_val
        type(data_frame) :: filtered_df

        integer, dimension(:), allocatable :: selected_rows
        integer :: i, count, idx
        integer(ik) :: val

        if (this % data_cols(col_index) % get_type() /= INTEGER_NUM) error stop "column is not integer type"

        ! Count matching rows
        count = 0
        do i = 1, this % nrows()
            val = this % data_cols(col_index) % geti(i)
            if (val >= min_val .and. val <= max_val) count = count + 1
        end do

        ! Collect matching row indices
        allocate (selected_rows(count))
        idx = 0
        do i = 1, this % nrows()
            val = this % data_cols(col_index) % geti(i)
            if (val >= min_val .and. val <= max_val) then
                idx = idx + 1
                selected_rows(idx) = i
            end if
        end do

        ! Create filtered dataframe
        call filtered_df % new(this % max_char_len)
        do i = 1, this % num_cols
            call copy_filtered_column(this, filtered_df, i, selected_rows)
        end do

        deallocate (selected_rows)
    end function filter_rows_integer_range

    function filter_rows_string_pattern(this, col_index, pattern) result(filtered_df)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: col_index
        character(len=*), intent(in) :: pattern
        type(data_frame) :: filtered_df

        integer, dimension(:), allocatable :: selected_rows
        integer :: i, count, idx
        character(len=:), allocatable :: val

        if (this % data_cols(col_index) % get_type() /= CHARACTER_NUM) error stop "column is not character type"

        ! Count matching rows
        count = 0
        do i = 1, this % nrows()
            val = this % data_cols(col_index) % getch(i)
            if (index(val, pattern) > 0) count = count + 1
        end do

        ! Collect matching row indices
        allocate (selected_rows(count))
        idx = 0
        do i = 1, this % nrows()
            val = this % data_cols(col_index) % getch(i)
            if (index(val, pattern) > 0) then
                idx = idx + 1
                selected_rows(idx) = i
            end if
        end do

        ! Create filtered dataframe
        call filtered_df % new(this % max_char_len)
        do i = 1, this % num_cols
            call copy_filtered_column(this, filtered_df, i, selected_rows)
        end do

        deallocate (selected_rows)
    end function filter_rows_string_pattern

    ! Helper subroutines (simplified implementations)
    subroutine copy_filtered_column(source_df, target_df, col_index, selected_rows)
        class(data_frame), intent(in) :: source_df
        class(data_frame), intent(inout) :: target_df
        integer, intent(in) :: col_index
        integer, dimension(:), intent(in) :: selected_rows

        integer :: i, n_selected
        real(rk), allocatable :: real_filtered(:)
        integer(ik), allocatable :: int_filtered(:)
        logical, allocatable :: logical_filtered(:)
        character(len=:), allocatable :: char_filtered(:)
        complex(rk), allocatable :: complex_filtered(:)

        n_selected = size(selected_rows)

        select case (source_df % data_cols(col_index) % get_type())
        case (REAL_NUM)
            allocate (real_filtered(n_selected))
            do i = 1, n_selected
                real_filtered(i) = source_df % data_cols(col_index) % getr(selected_rows(i))
            end do
            if (source_df % with_headers) then
                call target_df % append(real_filtered, source_df % headers(col_index))
            else
                call target_df % append(real_filtered)
            end if
            deallocate (real_filtered)
        case (INTEGER_NUM)
            allocate (int_filtered(n_selected))
            do i = 1, n_selected
                int_filtered(i) = source_df % data_cols(col_index) % geti(selected_rows(i))
            end do
            if (source_df % with_headers) then
                call target_df % append(int_filtered, source_df % headers(col_index))
            else
                call target_df % append(int_filtered)
            end if
            deallocate (int_filtered)
        case (LOGICAL_NUM)
            allocate (logical_filtered(n_selected))
            do i = 1, n_selected
                logical_filtered(i) = source_df % data_cols(col_index) % getl(selected_rows(i))
            end do
            if (source_df % with_headers) then
                call target_df % append(logical_filtered, source_df % headers(col_index))
            else
                call target_df % append(logical_filtered)
            end if
            deallocate (logical_filtered)
        case (CHARACTER_NUM)
            allocate (character(len=len(source_df % data_cols(col_index) % getch(1))) :: char_filtered(n_selected))
            do i = 1, n_selected
                char_filtered(i) = source_df % data_cols(col_index) % getch(selected_rows(i))
            end do
            if (source_df % with_headers) then
                call target_df % append(char_filtered, source_df % headers(col_index))
            else
                call target_df % append(char_filtered)
            end if
            deallocate (char_filtered)
        case (COMPLEX_NUM)
            allocate (complex_filtered(n_selected))
            do i = 1, n_selected
                complex_filtered(i) = source_df % data_cols(col_index) % getc(selected_rows(i))
            end do
            if (source_df % with_headers) then
                call target_df % append(complex_filtered, source_df % headers(col_index))
            else
                call target_df % append(complex_filtered)
            end if
            deallocate (complex_filtered)
        end select
    end subroutine copy_filtered_column

    subroutine sort_indices_real(values, indices, ascending)
        real(rk), dimension(:), intent(in) :: values
        integer, dimension(:), intent(inout) :: indices
        logical, intent(in) :: ascending

        if (size(values) > 0) then
            call quicksort_indices_real(values, indices, 1, size(values), ascending)
        end if
    end subroutine sort_indices_real

    recursive subroutine quicksort_indices_real(values, indices, low, high, ascending)
        real(rk), dimension(:), intent(in) :: values
        integer, dimension(:), intent(inout) :: indices
        integer, intent(in) :: low, high
        logical, intent(in) :: ascending
        integer :: pivot_idx

        if (low < high) then
            pivot_idx = partition_indices_real(values, indices, low, high, ascending)
            call quicksort_indices_real(values, indices, low, pivot_idx - 1, ascending)
            call quicksort_indices_real(values, indices, pivot_idx + 1, high, ascending)
        end if
    end subroutine quicksort_indices_real

    function partition_indices_real(values, indices, low, high, ascending) result(pivot_idx)
        real(rk), dimension(:), intent(in) :: values
        integer, dimension(:), intent(inout) :: indices
        integer, intent(in) :: low, high
        logical, intent(in) :: ascending
        integer :: pivot_idx
        real(rk) :: pivot
        integer :: i, j, temp

        pivot = values(indices(high))
        i = low - 1

        do j = low, high - 1
            if ((ascending .and. values(indices(j)) <= pivot) .or. &
                (.not. ascending .and. values(indices(j)) >= pivot)) then
                i = i + 1
                temp = indices(i)
                indices(i) = indices(j)
                indices(j) = temp
            end if
        end do

        temp = indices(i + 1)
        indices(i + 1) = indices(high)
        indices(high) = temp
        pivot_idx = i + 1
    end function partition_indices_real

    subroutine sort_indices_integer(values, indices, ascending)
        integer(ik), dimension(:), intent(in) :: values
        integer, dimension(:), intent(inout) :: indices
        logical, intent(in) :: ascending

        if (size(values) > 0) then
            call quicksort_indices_integer(values, indices, 1, size(values), ascending)
        end if
    end subroutine sort_indices_integer

    recursive subroutine quicksort_indices_integer(values, indices, low, high, ascending)
        integer(ik), dimension(:), intent(in) :: values
        integer, dimension(:), intent(inout) :: indices
        integer, intent(in) :: low, high
        logical, intent(in) :: ascending
        integer :: pivot_idx

        if (low < high) then
            pivot_idx = partition_indices_integer(values, indices, low, high, ascending)
            call quicksort_indices_integer(values, indices, low, pivot_idx - 1, ascending)
            call quicksort_indices_integer(values, indices, pivot_idx + 1, high, ascending)
        end if
    end subroutine quicksort_indices_integer

    function partition_indices_integer(values, indices, low, high, ascending) result(pivot_idx)
        integer(ik), dimension(:), intent(in) :: values
        integer, dimension(:), intent(inout) :: indices
        integer, intent(in) :: low, high
        logical, intent(in) :: ascending
        integer :: pivot_idx
        integer(ik) :: pivot
        integer :: i, j, temp

        pivot = values(indices(high))
        i = low - 1

        do j = low, high - 1
            if ((ascending .and. values(indices(j)) <= pivot) .or. &
                (.not. ascending .and. values(indices(j)) >= pivot)) then
                i = i + 1
                temp = indices(i)
                indices(i) = indices(j)
                indices(j) = temp
            end if
        end do

        temp = indices(i + 1)
        indices(i + 1) = indices(high)
        indices(high) = temp
        pivot_idx = i + 1
    end function partition_indices_integer

    subroutine reorder_all_columns(this, indices)
        class(data_frame), intent(inout) :: this
        integer, dimension(:), intent(in) :: indices

        integer :: i, j
        real(rk), allocatable :: real_temp(:)
        integer(ik), allocatable :: int_temp(:)
        logical, allocatable :: logical_temp(:)
        character(len=:), allocatable :: char_temp(:)
        complex(rk), allocatable :: complex_temp(:)

        do i = 1, this % num_cols
            select case (this % data_cols(i) % get_type())
            case (REAL_NUM)
                allocate (real_temp(this % nrows()))
                do j = 1, this % nrows()
                    real_temp(j) = this % data_cols(i) % getr(indices(j))
                end do
                call this % data_cols(i) % destroy()
                call this % data_cols(i) % new(real_temp)
                deallocate (real_temp)
            case (INTEGER_NUM)
                allocate (int_temp(this % nrows()))
                do j = 1, this % nrows()
                    int_temp(j) = this % data_cols(i) % geti(indices(j))
                end do
                call this % data_cols(i) % destroy()
                call this % data_cols(i) % new(int_temp)
                deallocate (int_temp)
            case (LOGICAL_NUM)
                allocate (logical_temp(this % nrows()))
                do j = 1, this % nrows()
                    logical_temp(j) = this % data_cols(i) % getl(indices(j))
                end do
                call this % data_cols(i) % destroy()
                call this % data_cols(i) % new(logical_temp)
                deallocate (logical_temp)
            case (CHARACTER_NUM)
                allocate (character(len=len(this % data_cols(i) % getch(1))) :: char_temp(this % nrows()))
                do j = 1, this % nrows()
                    char_temp(j) = this % data_cols(i) % getch(indices(j))
                end do
                call this % data_cols(i) % destroy()
                call this % data_cols(i) % new(char_temp)
                deallocate (char_temp)
            case (COMPLEX_NUM)
                allocate (complex_temp(this % nrows()))
                do j = 1, this % nrows()
                    complex_temp(j) = this % data_cols(i) % getc(indices(j))
                end do
                call this % data_cols(i) % destroy()
                call this % data_cols(i) % new(complex_temp)
                deallocate (complex_temp)
            end select
        end do
    end subroutine reorder_all_columns

    ! ========================================================================
    ! ADDITIONAL MATHEMATICAL OPERATIONS
    ! ========================================================================

    ! Calculate Pearson correlation coefficient between two real columns
    function correlation_real(this, col_index1, col_index2) result(corr)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: col_index1, col_index2
        real(rk) :: corr

        real(rk), dimension(:), allocatable :: col1, col2
        real(rk) :: mean1, mean2, std1, std2, covariance
        integer :: i, n

        col1 = this % getr(col_index1)
        col2 = this % getr(col_index2)
        n = size(col1)

        if (n /= size(col2)) error stop "columns must have same length"

        mean1 = this % mean_real(col_index1)
        mean2 = this % mean_real(col_index2)
        std1 = this % std_real(col_index1)
        std2 = this % std_real(col_index2)

        ! Calculate covariance
        covariance = 0.0_rk
        do i = 1, n
            covariance = covariance + (col1(i) - mean1) * (col2(i) - mean2)
        end do
        covariance = covariance / real(n - 1, rk)

        ! Pearson correlation = covariance / (std1 * std2)
        if (std1 > 0.0_rk .and. std2 > 0.0_rk) then
            corr = covariance / (std1 * std2)
        else
            corr = 0.0_rk  ! undefined if either std is zero
        end if
    end function correlation_real

    ! Normalize a real column to [0, 1] range (min-max scaling)
    subroutine normalize_column_real(this, col_index)
        class(data_frame), intent(inout) :: this
        integer, intent(in) :: col_index

        real(rk), dimension(:), allocatable :: col
        real(rk) :: min_val, max_val, range
        integer :: i

        col = this % getr(col_index)
        min_val = this % min_real(col_index)
        max_val = this % max_real(col_index)
        range = max_val - min_val

        if (range > 0.0_rk) then
            do i = 1, size(col)
                col(i) = (col(i) - min_val) / range
            end do
            call this % setr(col_index, col)
        end if
    end subroutine normalize_column_real

    ! Standardize a real column (z-score: mean=0, std=1)
    subroutine standardize_column_real(this, col_index)
        class(data_frame), intent(inout) :: this
        integer, intent(in) :: col_index

        real(rk), dimension(:), allocatable :: col
        real(rk) :: mean_val, std_val
        integer :: i

        col = this % getr(col_index)
        mean_val = this % mean_real(col_index)
        std_val = this % std_real(col_index)

        if (std_val > 0.0_rk) then
            do i = 1, size(col)
                col(i) = (col(i) - mean_val) / std_val
            end do
            call this % setr(col_index, col)
        end if
    end subroutine standardize_column_real

    ! Take absolute value of all elements in a real column
    subroutine abs_column_real(this, col_index)
        class(data_frame), intent(inout) :: this
        integer, intent(in) :: col_index

        real(rk), dimension(:), allocatable :: col

        col = this % getr(col_index)
        col = abs(col)
        call this % setr(col_index, col)
    end subroutine abs_column_real

    ! Take absolute value of all elements in an integer column
    subroutine abs_column_integer(this, col_index)
        class(data_frame), intent(inout) :: this
        integer, intent(in) :: col_index

        integer(ik), dimension(:), allocatable :: col

        col = this % geti(col_index)
        col = abs(col)
        call this % seti(col_index, col)
    end subroutine abs_column_integer

    ! Calculate cumulative sum for a real column (modifies in place)
    subroutine cumsum_real(this, col_index)
        class(data_frame), intent(inout) :: this
        integer, intent(in) :: col_index

        real(rk), dimension(:), allocatable :: col
        integer :: i

        col = this % getr(col_index)
        do i = 2, size(col)
            col(i) = col(i) + col(i - 1)
        end do
        call this % setr(col_index, col)
    end subroutine cumsum_real

    ! Calculate cumulative sum for an integer column (modifies in place)
    subroutine cumsum_integer(this, col_index)
        class(data_frame), intent(inout) :: this
        integer, intent(in) :: col_index

        integer(ik), dimension(:), allocatable :: col
        integer :: i

        col = this % geti(col_index)
        do i = 2, size(col)
            col(i) = col(i) + col(i - 1)
        end do
        call this % seti(col_index, col)
    end subroutine cumsum_integer

    ! Calculate differences between consecutive rows (result has n-1 elements)
    function diff_real(this, col_index) result(differences)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: col_index
        real(rk), dimension(:), allocatable :: differences

        real(rk), dimension(:), allocatable :: col
        integer :: i, n

        col = this % getr(col_index)
        n = size(col)

        if (n < 2) then
            allocate (differences(0))
            return
        end if

        allocate (differences(n - 1))
        do i = 1, n - 1
            differences(i) = col(i + 1) - col(i)
        end do
    end function diff_real

    ! Calculate differences between consecutive rows (result has n-1 elements)
    function diff_integer(this, col_index) result(differences)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: col_index
        integer(ik), dimension(:), allocatable :: differences

        integer(ik), dimension(:), allocatable :: col
        integer :: i, n

        col = this % geti(col_index)
        n = size(col)

        if (n < 2) then
            allocate (differences(0))
            return
        end if

        allocate (differences(n - 1))
        do i = 1, n - 1
            differences(i) = col(i + 1) - col(i)
        end do
    end function diff_integer

    ! Replace all occurrences of a value in a real column
    subroutine replace_value_real(this, col_index, old_value, new_value)
        class(data_frame), intent(inout) :: this
        integer, intent(in) :: col_index
        real(rk), intent(in) :: old_value, new_value

        real(rk), dimension(:), allocatable :: col
        integer :: i
        real(rk), parameter :: tol = 1.0e-10_rk

        col = this % getr(col_index)
        do i = 1, size(col)
            if (abs(col(i) - old_value) < tol) then
                col(i) = new_value
            end if
        end do
        call this % setr(col_index, col)
    end subroutine replace_value_real

    ! Replace all occurrences of a value in an integer column
    subroutine replace_value_integer(this, col_index, old_value, new_value)
        class(data_frame), intent(inout) :: this
        integer, intent(in) :: col_index
        integer(ik), intent(in) :: old_value, new_value

        integer(ik), dimension(:), allocatable :: col
        integer :: i

        col = this % geti(col_index)
        do i = 1, size(col)
            if (col(i) == old_value) then
                col(i) = new_value
            end if
        end do
        call this % seti(col_index, col)
    end subroutine replace_value_integer

    ! Clip (clamp) values in a real column to [min_val, max_val]
    subroutine clip_real(this, col_index, min_val, max_val)
        class(data_frame), intent(inout) :: this
        integer, intent(in) :: col_index
        real(rk), intent(in) :: min_val, max_val

        real(rk), dimension(:), allocatable :: col
        integer :: i

        col = this % getr(col_index)
        do i = 1, size(col)
            col(i) = max(min_val, min(max_val, col(i)))
        end do
        call this % setr(col_index, col)
    end subroutine clip_real

    ! Clip (clamp) values in an integer column to [min_val, max_val]
    subroutine clip_integer(this, col_index, min_val, max_val)
        class(data_frame), intent(inout) :: this
        integer, intent(in) :: col_index
        integer(ik), intent(in) :: min_val, max_val

        integer(ik), dimension(:), allocatable :: col
        integer :: i

        col = this % geti(col_index)
        do i = 1, size(col)
            col(i) = max(min_val, min(max_val, col(i)))
        end do
        call this % seti(col_index, col)
    end subroutine clip_integer

    ! Check if a real column is sorted
    function is_sorted_real(this, col_index, ascending) result(sorted)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: col_index
        logical, intent(in), optional :: ascending
        logical :: sorted

        real(rk), dimension(:), allocatable :: col
        logical :: asc
        integer :: i

        asc = .true.
        if (present(ascending)) asc = ascending

        col = this % getr(col_index)
        sorted = .true.

        if (asc) then
            do i = 1, size(col) - 1
                if (col(i) > col(i + 1)) then
                    sorted = .false.
                    return
                end if
            end do
        else
            do i = 1, size(col) - 1
                if (col(i) < col(i + 1)) then
                    sorted = .false.
                    return
                end if
            end do
        end if
    end function is_sorted_real

    ! Check if an integer column is sorted
    function is_sorted_integer(this, col_index, ascending) result(sorted)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: col_index
        logical, intent(in), optional :: ascending
        logical :: sorted

        integer(ik), dimension(:), allocatable :: col
        logical :: asc
        integer :: i

        asc = .true.
        if (present(ascending)) asc = ascending

        col = this % geti(col_index)
        sorted = .true.

        if (asc) then
            do i = 1, size(col) - 1
                if (col(i) > col(i + 1)) then
                    sorted = .false.
                    return
                end if
            end do
        else
            do i = 1, size(col) - 1
                if (col(i) < col(i + 1)) then
                    sorted = .false.
                    return
                end if
            end do
        end if
    end function is_sorted_integer

    ! Round real column to specified number of decimal places
    subroutine round_column(this, col_index, decimals)
        class(data_frame), intent(inout) :: this
        integer, intent(in) :: col_index
        integer, intent(in) :: decimals

        real(rk), dimension(:), allocatable :: col
        real(rk) :: multiplier
        integer :: i

        col = this % getr(col_index)
        multiplier = 10.0_rk**decimals

        do i = 1, size(col)
            col(i) = nint(col(i) * multiplier) / multiplier
        end do
        call this % setr(col_index, col)
    end subroutine round_column

    ! Apply natural logarithm to column
    subroutine log_column(this, col_index)
        class(data_frame), intent(inout) :: this
        integer, intent(in) :: col_index

        real(rk), dimension(:), allocatable :: col

        col = this % getr(col_index)
        col = log(col)
        call this % setr(col_index, col)
    end subroutine log_column

    ! Apply exponential to column
    subroutine exp_column(this, col_index)
        class(data_frame), intent(inout) :: this
        integer, intent(in) :: col_index

        real(rk), dimension(:), allocatable :: col

        col = this % getr(col_index)
        col = exp(col)
        call this % setr(col_index, col)
    end subroutine exp_column

    ! Apply square root to column
    subroutine sqrt_column(this, col_index)
        class(data_frame), intent(inout) :: this
        integer, intent(in) :: col_index

        real(rk), dimension(:), allocatable :: col

        col = this % getr(col_index)
        col = sqrt(col)
        call this % setr(col_index, col)
    end subroutine sqrt_column

    ! Raise column to a power
    subroutine pow_column(this, col_index, power)
        class(data_frame), intent(inout) :: this
        integer, intent(in) :: col_index
        real(rk), intent(in) :: power

        real(rk), dimension(:), allocatable :: col

        col = this % getr(col_index)
        col = col**power
        call this % setr(col_index, col)
    end subroutine pow_column

    ! ========================================================================
    ! CONVENIENCE FUNCTIONS
    ! ========================================================================

    ! Return first n rows as a new dataframe
    function head(this, n) result(head_df)
        class(data_frame), intent(in) :: this
        integer, intent(in), optional :: n
        type(data_frame) :: head_df

        integer :: num_rows

        if (present(n)) then
            num_rows = min(n, this % nrows())
        else
            num_rows = min(6, this % nrows())  ! Default to 6 rows like pandas
        end if

        if (num_rows == 0 .or. this % nrows() == 0) then
            call head_df % new(this % max_char_len)
            return
        end if

        head_df = this % slice_rows(1, num_rows)
    end function head

    ! Return last n rows as a new dataframe
    function tail(this, n) result(tail_df)
        class(data_frame), intent(in) :: this
        integer, intent(in), optional :: n
        type(data_frame) :: tail_df

        integer :: num_rows, start_row

        if (present(n)) then
            num_rows = min(n, this % nrows())
        else
            num_rows = min(6, this % nrows())  ! Default to 6 rows like pandas
        end if

        if (num_rows == 0 .or. this % nrows() == 0) then
            call tail_df % new(this % max_char_len)
            return
        end if

        start_row = this % nrows() - num_rows + 1
        tail_df = this % slice_rows(start_row, this % nrows())
    end function tail

    ! Return shape as [nrows, ncols]
    function shape(this) result(dims)
        class(data_frame), intent(in) :: this
        integer, dimension(2) :: dims

        dims(1) = this % nrows()
        dims(2) = this % ncols()
    end function shape

    ! Print information about the dataframe
    subroutine info(this, unit)
        class(data_frame), intent(in) :: this
        integer, intent(in), optional :: unit

        integer :: out_unit, i, dtype
        character(len=20) :: type_name
        integer, dimension(2) :: dims

        if (present(unit)) then
            out_unit = unit
        else
            out_unit = 6  ! stdout
        end if

        dims = this % shape()

        write (out_unit, '(a)') repeat('=', 60)
        write (out_unit, '(a)') 'DataFrame Information'
        write (out_unit, '(a)') repeat('=', 60)
        write (out_unit, '(a,i0,a,i0,a)') 'Shape: (', dims(1), ' rows, ', dims(2), ' columns)'
        write (out_unit, '(a,l1)') 'Has headers: ', this % with_headers

        if (this % num_cols > 0) then
            write (out_unit, '(a)') ''
            write (out_unit, '(a)') 'Columns:'
            write (out_unit, '(a)') repeat('-', 60)

            do i = 1, this % num_cols
                dtype = this % data_cols(i) % get_type()

                select case (dtype)
                case (REAL_NUM)
                    type_name = 'real'
                case (INTEGER_NUM)
                    type_name = 'integer'
                case (LOGICAL_NUM)
                    type_name = 'logical'
                case (CHARACTER_NUM)
                    type_name = 'character'
                case (COMPLEX_NUM)
                    type_name = 'complex'
                case default
                    type_name = 'unknown'
                end select

                if (this % with_headers) then
                    write (out_unit, '(i4,2a,t30,3a)') i, '. ', trim(this % headers(i)), &
                        '(', trim(type_name), ')'
                else
                    write (out_unit, '(i4,2a,a)') i, '. Column (', trim(type_name), ')'
                end if
            end do
        end if

        write (out_unit, '(a)') repeat('=', 60)
    end subroutine info

    ! Check if dataframe is empty (no rows or no columns)
    pure function empty(this) result(is_empty)
        class(data_frame), intent(in) :: this
        logical :: is_empty

        is_empty = (this % nrows() == 0 .or. this % ncols() == 0)
    end function empty

    ! Clear the dataframe (destroy and re-initialize)
    subroutine clear(this)
        class(data_frame), intent(inout) :: this
        integer :: saved_char_len

        saved_char_len = this % max_char_len
        call this % destroy()
        call this % new(saved_char_len)
    end subroutine clear

    ! Print summary statistics for all numeric columns
    subroutine describe_numeric(this, unit)
        class(data_frame), intent(in) :: this
        integer, intent(in), optional :: unit

        integer :: out_unit, i, dtype
        character(len=25) :: col_name

        if (present(unit)) then
            out_unit = unit
        else
            out_unit = 6  ! stdout
        end if

        write (out_unit, '(a)') ""
        write (out_unit, '(a)') repeat('=', 80)
        write (out_unit, '(a)') "Numeric Column Statistics"
        write (out_unit, '(a)') repeat('=', 80)
        write (out_unit, '(a)') ""

        do i = 1, this % num_cols
            dtype = this % data_cols(i) % get_type()

            ! Only process real and integer columns
            if (dtype == REAL_NUM .or. dtype == INTEGER_NUM) then
                if (this % with_headers) then
                    col_name = trim(this % headers(i))
                else
                    write (col_name, '(a,i0)') "Column ", i
                end if

                write (out_unit, '(a)') trim(col_name)
                write (out_unit, '(a)') repeat('-', len(trim(col_name)))

                if (dtype == REAL_NUM) then
                    write (out_unit, '(a,f12.4)') "  Count:       ", real(this % nrows(), rk)
                    write (out_unit, '(a,f12.4)') "  Mean:        ", this % mean_real(i)
                    write (out_unit, '(a,f12.4)') "  Std Dev:     ", this % std_real(i)
                    write (out_unit, '(a,f12.4)') "  Min:         ", this % min_real(i)
                    write (out_unit, '(a,f12.4)') "  25%:         ", this % percentile_real(i, 25.0_rk)
                    write (out_unit, '(a,f12.4)') "  Median (50%):", this % median_real(i)
                    write (out_unit, '(a,f12.4)') "  75%:         ", this % percentile_real(i, 75.0_rk)
                    write (out_unit, '(a,f12.4)') "  Max:         ", this % max_real(i)
                else ! INTEGER_NUM
                    write (out_unit, '(a,i12)') "  Count:       ", this % nrows()
                    write (out_unit, '(a,f12.2)') "  Mean:        ", this % mean_integer(i)
                    write (out_unit, '(a,f12.2)') "  Std Dev:     ", this % std_integer(i)
                    write (out_unit, '(a,i12)') "  Min:         ", this % min_integer(i)
                    write (out_unit, '(a,f12.2)') "  25%:         ", this % percentile_integer(i, 25.0_rk)
                    write (out_unit, '(a,f12.2)') "  Median (50%):", this % median_integer(i)
                    write (out_unit, '(a,f12.2)') "  75%:         ", this % percentile_integer(i, 75.0_rk)
                    write (out_unit, '(a,i12)') "  Max:         ", this % max_integer(i)
                end if

                write (out_unit, '(a)') ""
            end if
        end do

        write (out_unit, '(a)') repeat('=', 80)
    end subroutine describe_numeric

    ! Get n random rows from the dataframe
    function sample(this, n, seed) result(sampled_df)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: n
        integer, intent(in), optional :: seed
        type(data_frame) :: sampled_df

        integer, dimension(:), allocatable :: indices, selected_indices, seed_array
        integer :: i, j, temp, num_samples, seed_size
        real :: rand_val

        num_samples = min(n, this % nrows())

        if (num_samples == 0) then
            call sampled_df % new(this % max_char_len)
            return
        end if

        ! Initialize random seed if provided
        if (present(seed)) then
            call random_seed(size=seed_size)
            allocate (seed_array(seed_size))
            seed_array = seed
            call random_seed(put=seed_array)
            deallocate (seed_array)
        end if

        ! Create array of all indices
        allocate (indices(this % nrows()))
        do i = 1, this % nrows()
            indices(i) = i
        end do

        ! Fisher-Yates shuffle to get random sample
        do i = this % nrows(), 2, -1
            call random_number(rand_val)
            j = int(rand_val * i) + 1
            temp = indices(i)
            indices(i) = indices(j)
            indices(j) = temp
        end do

        ! Take first n shuffled indices
        allocate (selected_indices(num_samples))
        selected_indices = indices(1:num_samples)

        ! Create sampled dataframe
        call sampled_df % new(this % max_char_len)
        do i = 1, this % num_cols
            call copy_filtered_column(this, sampled_df, i, selected_indices)
        end do

        deallocate (indices, selected_indices)
    end function sample

    ! Shuffle all rows randomly in place
    subroutine shuffle(this, seed)
        class(data_frame), intent(inout) :: this
        integer, intent(in), optional :: seed

        integer, dimension(:), allocatable :: indices, seed_array
        integer :: i, j, temp, seed_size
        real :: rand_val

        if (this % nrows() < 2) return

        ! Initialize random seed if provided
        if (present(seed)) then
            call random_seed(size=seed_size)
            allocate (seed_array(seed_size))
            seed_array = seed
            call random_seed(put=seed_array)
            deallocate (seed_array)
        end if

        ! Create array of indices
        allocate (indices(this % nrows()))
        do i = 1, this % nrows()
            indices(i) = i
        end do

        ! Fisher-Yates shuffle
        do i = this % nrows(), 2, -1
            call random_number(rand_val)
            j = int(rand_val * i) + 1
            temp = indices(i)
            indices(i) = indices(j)
            indices(j) = temp
        end do

        ! Reorder all columns according to shuffled indices
        call reorder_all_columns(this, indices)

        deallocate (indices)
    end subroutine shuffle

    ! ========================================================================
    ! JOIN OPERATIONS
    ! ========================================================================

    !> Perform an inner join between two data frames
    !!
    !! Returns a new data frame containing only rows where the key columns match
    !! in both data frames. Similar to SQL INNER JOIN.
    !!
    !! @param[in] this The left data frame
    !! @param[in] other The right data frame
    !! @param[in] this_key_col Column index for join key in left data frame
    !! @param[in] other_key_col Column index for join key in right data frame
    !! @return A new data frame with matched rows from both tables
    !!
    !! @note Key columns must have the same data type (integer, real, or character)
    !! @note Right table columns are suffixed with "_right" to avoid name conflicts
    function inner_join(this, other, this_key_col, other_key_col) result(joined_df)
        class(data_frame), intent(in) :: this, other
        integer, intent(in) :: this_key_col, other_key_col
        type(data_frame) :: joined_df

        integer :: i, j, this_dtype, other_dtype, num_matches
        integer, dimension(:), allocatable :: match_indices_this, match_indices_other
        logical :: match_found

        ! Check if key columns are compatible types
        this_dtype = this % dtype(this_key_col)
        other_dtype = other % dtype(other_key_col)

        if (this_dtype /= other_dtype) then
            print *, "Error: Key columns must have the same data type"
            call joined_df % new()
            return
        end if

        ! Count matches first
        num_matches = 0
        do i = 1, this % nrows()
            do j = 1, other % nrows()
                match_found = .false.

                select case (this_dtype)
                case (INTEGER_NUM)
                    if (this % get_val_integer(this_key_col, i) == other % get_val_integer(other_key_col, j)) then
                        match_found = .true.
                    end if
                case (REAL_NUM)
               if (abs(this % get_val_real(this_key_col, i) - other % get_val_real(other_key_col, j)) < 1.0e-10_rk) then
                        match_found = .true.
                    end if
                case (CHARACTER_NUM)
          if (trim(this % get_val_character(this_key_col, i)) == trim(other % get_val_character(other_key_col, j))) then
                        match_found = .true.
                    end if
                end select

                if (match_found) then
                    num_matches = num_matches + 1
                end if
            end do
        end do

        if (num_matches == 0) then
            call joined_df % new()
            return
        end if

        ! Allocate arrays to store matching indices
        allocate (match_indices_this(num_matches))
        allocate (match_indices_other(num_matches))

        ! Find all matching pairs
        num_matches = 0
        do i = 1, this % nrows()
            do j = 1, other % nrows()
                match_found = .false.

                select case (this_dtype)
                case (INTEGER_NUM)
                    if (this % get_val_integer(this_key_col, i) == other % get_val_integer(other_key_col, j)) then
                        match_found = .true.
                    end if
                case (REAL_NUM)
               if (abs(this % get_val_real(this_key_col, i) - other % get_val_real(other_key_col, j)) < 1.0e-10_rk) then
                        match_found = .true.
                    end if
                case (CHARACTER_NUM)
          if (trim(this % get_val_character(this_key_col, i)) == trim(other % get_val_character(other_key_col, j))) then
                        match_found = .true.
                    end if
                end select

                if (match_found) then
                    num_matches = num_matches + 1
                    match_indices_this(num_matches) = i
                    match_indices_other(num_matches) = j
                end if
            end do
        end do

        ! Build the joined dataframe
        call build_joined_dataframe(this, other, match_indices_this, match_indices_other, &
                                    num_matches, joined_df)

        deallocate (match_indices_this)
        deallocate (match_indices_other)
    end function inner_join

    ! Left join: Returns all rows from left df, with matching rows from right df (or NULL)
    function left_join(this, other, this_key_col, other_key_col) result(joined_df)
        class(data_frame), intent(in) :: this, other
        integer, intent(in) :: this_key_col, other_key_col
        type(data_frame) :: joined_df

        integer :: i, j, this_dtype, other_dtype
        integer, dimension(:), allocatable :: match_indices_this, match_indices_other
        logical :: match_found
        integer :: num_rows

        this_dtype = this % dtype(this_key_col)
        other_dtype = other % dtype(other_key_col)

        if (this_dtype /= other_dtype) then
            print *, "Error: Key columns must have the same data type"
            call joined_df % new()
            return
        end if

        ! For left join, we need one row for each row in 'this', possibly more if multiple matches
        num_rows = 0
        allocate (match_indices_this(this % nrows() * max(1, other % nrows())))
        allocate (match_indices_other(this % nrows() * max(1, other % nrows())))

        do i = 1, this % nrows()
            match_found = .false.

            do j = 1, other % nrows()
                select case (this_dtype)
                case (INTEGER_NUM)
                    if (this % get_val_integer(this_key_col, i) == other % get_val_integer(other_key_col, j)) then
                        num_rows = num_rows + 1
                        match_indices_this(num_rows) = i
                        match_indices_other(num_rows) = j
                        match_found = .true.
                    end if
                case (REAL_NUM)
               if (abs(this % get_val_real(this_key_col, i) - other % get_val_real(other_key_col, j)) < 1.0e-10_rk) then
                        num_rows = num_rows + 1
                        match_indices_this(num_rows) = i
                        match_indices_other(num_rows) = j
                        match_found = .true.
                    end if
                case (CHARACTER_NUM)
          if (trim(this % get_val_character(this_key_col, i)) == trim(other % get_val_character(other_key_col, j))) then
                        num_rows = num_rows + 1
                        match_indices_this(num_rows) = i
                        match_indices_other(num_rows) = j
                        match_found = .true.
                    end if
                end select
            end do

            ! If no match found, still include the row from 'this' with NULL for 'other'
            if (.not. match_found) then
                num_rows = num_rows + 1
                match_indices_this(num_rows) = i
                match_indices_other(num_rows) = -1  ! -1 indicates no match
            end if
        end do

        call build_joined_dataframe(this, other, match_indices_this(1:num_rows), &
                                    match_indices_other(1:num_rows), num_rows, joined_df)

        deallocate (match_indices_this)
        deallocate (match_indices_other)
    end function left_join

    ! Right join: Returns all rows from right df, with matching rows from left df (or NULL)
    function right_join(this, other, this_key_col, other_key_col) result(joined_df)
        class(data_frame), intent(in) :: this, other
        integer, intent(in) :: this_key_col, other_key_col
        type(data_frame) :: joined_df

        ! Right join is just a left join with arguments swapped
        joined_df = other % left_join(this, other_key_col, this_key_col)
    end function right_join

    ! Outer join: Returns all rows from both dataframes
    function outer_join(this, other, this_key_col, other_key_col) result(joined_df)
        class(data_frame), intent(in) :: this, other
        integer, intent(in) :: this_key_col, other_key_col
        type(data_frame) :: joined_df

        integer :: i, j, this_dtype, other_dtype
        integer, dimension(:), allocatable :: match_indices_this, match_indices_other
        logical, dimension(:), allocatable :: other_matched
        integer :: num_rows
        logical :: found_match

        this_dtype = this % dtype(this_key_col)
        other_dtype = other % dtype(other_key_col)

        if (this_dtype /= other_dtype) then
            print *, "Error: Key columns must have the same data type"
            call joined_df % new()
            return
        end if

        allocate (other_matched(other % nrows()))
        other_matched = .false.

        num_rows = 0
        allocate (match_indices_this((this % nrows() + other % nrows()) * max(1, max(this % nrows(), other % nrows()))))
       allocate (match_indices_other((this % nrows() + other % nrows()) * max(1, max(this % nrows(), other % nrows()))))

        ! First pass: all rows from 'this' with matches from 'other'
        do i = 1, this % nrows()
            found_match = .false.

            do j = 1, other % nrows()
                select case (this_dtype)
                case (INTEGER_NUM)
                    if (this % get_val_integer(this_key_col, i) == other % get_val_integer(other_key_col, j)) then
                        num_rows = num_rows + 1
                        match_indices_this(num_rows) = i
                        match_indices_other(num_rows) = j
                        other_matched(j) = .true.
                        found_match = .true.
                    end if
                case (REAL_NUM)
               if (abs(this % get_val_real(this_key_col, i) - other % get_val_real(other_key_col, j)) < 1.0e-10_rk) then
                        num_rows = num_rows + 1
                        match_indices_this(num_rows) = i
                        match_indices_other(num_rows) = j
                        other_matched(j) = .true.
                        found_match = .true.
                    end if
                case (CHARACTER_NUM)
          if (trim(this % get_val_character(this_key_col, i)) == trim(other % get_val_character(other_key_col, j))) then
                        num_rows = num_rows + 1
                        match_indices_this(num_rows) = i
                        match_indices_other(num_rows) = j
                        other_matched(j) = .true.
                        found_match = .true.
                    end if
                end select
            end do

            if (.not. found_match) then
                num_rows = num_rows + 1
                match_indices_this(num_rows) = i
                match_indices_other(num_rows) = -1
            end if
        end do

        ! Second pass: unmatched rows from 'other'
        do j = 1, other % nrows()
            if (.not. other_matched(j)) then
                num_rows = num_rows + 1
                match_indices_this(num_rows) = -1
                match_indices_other(num_rows) = j
            end if
        end do

        call build_joined_dataframe(this, other, match_indices_this(1:num_rows), &
                                    match_indices_other(1:num_rows), num_rows, joined_df)

        deallocate (match_indices_this)
        deallocate (match_indices_other)
        deallocate (other_matched)
    end function outer_join

    ! Helper subroutine to build joined dataframe from match indices
    subroutine build_joined_dataframe(df1, df2, indices1, indices2, num_rows, result_df)
        type(data_frame), intent(in) :: df1, df2
        integer, dimension(:), intent(in) :: indices1, indices2
        integer, intent(in) :: num_rows
        type(data_frame), intent(out) :: result_df

        integer :: i, j, row, col_idx, dtype
        real(rk), dimension(:), allocatable :: real_col
        integer(ik), dimension(:), allocatable :: int_col
        logical, dimension(:), allocatable :: log_col
        character(len=:), allocatable :: char_col(:)
        complex(rk), dimension(:), allocatable :: cmplx_col
        character(len=100) :: header_name

        call result_df % new(max(df1 % max_char_len, df2 % max_char_len))

        ! Add columns from df1
        do i = 1, df1 % ncols()
            dtype = df1 % dtype(i)

            select case (dtype)
            case (REAL_NUM)
                allocate (real_col(num_rows))
                do row = 1, num_rows
                    if (indices1(row) > 0) then
                        real_col(row) = df1 % get_val_real(i, indices1(row))
                    else
                        real_col(row) = 0.0_rk  ! NULL value
                    end if
                end do
                if (df1 % with_headers) then
                    header_name = df1 % header(i)
                    call result_df % append(real_col, trim(header_name))
                else
                    call result_df % append(real_col)
                end if
                deallocate (real_col)

            case (INTEGER_NUM)
                allocate (int_col(num_rows))
                do row = 1, num_rows
                    if (indices1(row) > 0) then
                        int_col(row) = df1 % get_val_integer(i, indices1(row))
                    else
                        int_col(row) = 0_ik  ! NULL value
                    end if
                end do
                if (df1 % with_headers) then
                    header_name = df1 % header(i)
                    call result_df % append(int_col, trim(header_name))
                else
                    call result_df % append(int_col)
                end if
                deallocate (int_col)

            case (LOGICAL_NUM)
                allocate (log_col(num_rows))
                do row = 1, num_rows
                    if (indices1(row) > 0) then
                        log_col(row) = df1 % get_val_logical(i, indices1(row))
                    else
                        log_col(row) = .false.  ! NULL value
                    end if
                end do
                if (df1 % with_headers) then
                    header_name = df1 % header(i)
                    call result_df % append(log_col, trim(header_name))
                else
                    call result_df % append(log_col)
                end if
                deallocate (log_col)

            case (CHARACTER_NUM)
                allocate (character(len=df1 % max_char_len) :: char_col(num_rows))
                do row = 1, num_rows
                    if (indices1(row) > 0) then
                        char_col(row) = df1 % get_val_character(i, indices1(row))
                    else
                        char_col(row) = "NULL"
                    end if
                end do
                if (df1 % with_headers) then
                    header_name = df1 % header(i)
                    call result_df % append(char_col, trim(header_name))
                else
                    call result_df % append(char_col)
                end if
                deallocate (char_col)

            case (COMPLEX_NUM)
                allocate (cmplx_col(num_rows))
                do row = 1, num_rows
                    if (indices1(row) > 0) then
                        cmplx_col(row) = df1 % get_val_complex(i, indices1(row))
                    else
                        cmplx_col(row) = cmplx(0.0_rk, 0.0_rk, rk)
                    end if
                end do
                if (df1 % with_headers) then
                    header_name = df1 % header(i)
                    call result_df % append(cmplx_col, trim(header_name))
                else
                    call result_df % append(cmplx_col)
                end if
                deallocate (cmplx_col)
            end select
        end do

        ! Add columns from df2
        do i = 1, df2 % ncols()
            dtype = df2 % dtype(i)

            select case (dtype)
            case (REAL_NUM)
                allocate (real_col(num_rows))
                do row = 1, num_rows
                    if (indices2(row) > 0) then
                        real_col(row) = df2 % get_val_real(i, indices2(row))
                    else
                        real_col(row) = 0.0_rk
                    end if
                end do
                if (df2 % with_headers) then
                    header_name = df2 % header(i)
                    call result_df % append(real_col, trim(header_name)//"_right")
                else
                    call result_df % append(real_col)
                end if
                deallocate (real_col)

            case (INTEGER_NUM)
                allocate (int_col(num_rows))
                do row = 1, num_rows
                    if (indices2(row) > 0) then
                        int_col(row) = df2 % get_val_integer(i, indices2(row))
                    else
                        int_col(row) = 0_ik
                    end if
                end do
                if (df2 % with_headers) then
                    header_name = df2 % header(i)
                    call result_df % append(int_col, trim(header_name)//"_right")
                else
                    call result_df % append(int_col)
                end if
                deallocate (int_col)

            case (LOGICAL_NUM)
                allocate (log_col(num_rows))
                do row = 1, num_rows
                    if (indices2(row) > 0) then
                        log_col(row) = df2 % get_val_logical(i, indices2(row))
                    else
                        log_col(row) = .false.
                    end if
                end do
                if (df2 % with_headers) then
                    header_name = df2 % header(i)
                    call result_df % append(log_col, trim(header_name)//"_right")
                else
                    call result_df % append(log_col)
                end if
                deallocate (log_col)

            case (CHARACTER_NUM)
                allocate (character(len=df2 % max_char_len) :: char_col(num_rows))
                do row = 1, num_rows
                    if (indices2(row) > 0) then
                        char_col(row) = df2 % get_val_character(i, indices2(row))
                    else
                        char_col(row) = "NULL"
                    end if
                end do
                if (df2 % with_headers) then
                    header_name = df2 % header(i)
                    call result_df % append(char_col, trim(header_name)//"_right")
                else
                    call result_df % append(char_col)
                end if
                deallocate (char_col)

            case (COMPLEX_NUM)
                allocate (cmplx_col(num_rows))
                do row = 1, num_rows
                    if (indices2(row) > 0) then
                        cmplx_col(row) = df2 % get_val_complex(i, indices2(row))
                    else
                        cmplx_col(row) = cmplx(0.0_rk, 0.0_rk, rk)
                    end if
                end do
                if (df2 % with_headers) then
                    header_name = df2 % header(i)
                    call result_df % append(cmplx_col, trim(header_name)//"_right")
                else
                    call result_df % append(cmplx_col)
                end if
                deallocate (cmplx_col)
            end select
        end do
    end subroutine build_joined_dataframe

    ! ========================================================================
    ! NaN HANDLING
    ! ========================================================================

    !> Check for NaN values in a real column
    !!
    !! Returns a logical array indicating which values are NaN
    !!
    !! @param[in] this The data frame instance
    !! @param[in] col_index Column index to check
    !! @return Logical array where .true. indicates NaN
    function isna_real(this, col_index) result(mask)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: col_index
        logical, dimension(:), allocatable :: mask

        real(rk), dimension(:), allocatable :: col
        integer :: i

        col = this % getr(col_index)
        allocate (mask(size(col)))

        do i = 1, size(col)
            mask(i) = is_nan_real(col(i))
        end do
    end function isna_real

    !> Check for NaN values in an integer column
    !!
    !! Returns a logical array indicating which values are NaN (sentinel value)
    !!
    !! @param[in] this The data frame instance
    !! @param[in] col_index Column index to check
    !! @return Logical array where .true. indicates NaN
    function isna_integer(this, col_index) result(mask)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: col_index
        logical, dimension(:), allocatable :: mask

        integer(ik), dimension(:), allocatable :: col
        integer :: i

        col = this % geti(col_index)
        allocate (mask(size(col)))

        do i = 1, size(col)
            mask(i) = is_nan_integer(col(i))
        end do
    end function isna_integer

    !> Replace NaN values in a real column with a fill value
    !!
    !! Replaces all NaN values in the specified column with the given value
    !!
    !! @param[in,out] this The data frame instance
    !! @param[in] col_index Column index to fill
    !! @param[in] fill_value Value to replace NaN with
    subroutine fillna_real(this, col_index, fill_value)
        class(data_frame), intent(inout) :: this
        integer, intent(in) :: col_index
        real(rk), intent(in) :: fill_value

        real(rk), dimension(:), allocatable :: col
        integer :: i

        col = this % getr(col_index)

        do i = 1, size(col)
            if (is_nan_real(col(i))) then
                col(i) = fill_value
            end if
        end do

        call this % setr(col_index, col)
    end subroutine fillna_real

    !> Replace NaN values in an integer column with a fill value
    !!
    !! Replaces all NaN sentinel values in the specified column with the given value
    !!
    !! @param[in,out] this The data frame instance
    !! @param[in] col_index Column index to fill
    !! @param[in] fill_value Value to replace NaN with
    subroutine fillna_integer(this, col_index, fill_value)
        class(data_frame), intent(inout) :: this
        integer, intent(in) :: col_index
        integer(ik), intent(in) :: fill_value

        integer(ik), dimension(:), allocatable :: col
        integer :: i

        col = this % geti(col_index)

        do i = 1, size(col)
            if (is_nan_integer(col(i))) then
                col(i) = fill_value
            end if
        end do

        call this % seti(col_index, col)
    end subroutine fillna_integer

    !> Remove rows containing NaN values
    !!
    !! Returns a new data frame with all rows containing NaN in any column removed.
    !! Checks real and integer columns for NaN values.
    !!
    !! @param[in] this The data frame instance
    !! @return New data frame with NaN-containing rows removed
    function dropna(this) result(clean_df)
        class(data_frame), intent(in) :: this
        type(data_frame) :: clean_df

        logical, dimension(:), allocatable :: keep_mask
        integer :: i, j, dtype, num_clean_rows
        integer, dimension(:), allocatable :: clean_indices
        real(rk), dimension(:), allocatable :: real_col, real_clean_col
        integer(ik), dimension(:), allocatable :: int_col, int_clean_col
        logical, dimension(:), allocatable :: log_col, log_clean_col
        character(len=:), allocatable :: char_col(:), char_clean_col(:)
        complex(rk), dimension(:), allocatable :: cmplx_col, cmplx_clean_col
        character(len=100) :: header_name

        allocate (keep_mask(this % nrows()))
        keep_mask = .true.

        ! Check all columns for NaN
        do i = 1, this % ncols()
            dtype = this % dtype(i)

            if (dtype == REAL_NUM) then
                real_col = this % getr(i)
                do j = 1, size(real_col)
                    if (is_nan_real(real_col(j))) then
                        keep_mask(j) = .false.
                    end if
                end do
                deallocate (real_col)
            else if (dtype == INTEGER_NUM) then
                int_col = this % geti(i)
                do j = 1, size(int_col)
                    if (is_nan_integer(int_col(j))) then
                        keep_mask(j) = .false.
                    end if
                end do
                deallocate (int_col)
            end if
        end do

        ! Count rows to keep
        num_clean_rows = count(keep_mask)

        if (num_clean_rows == 0) then
            call clean_df % new(this % max_char_len)
            return
        end if

        ! Build index array of rows to keep
        allocate (clean_indices(num_clean_rows))
        j = 0
        do i = 1, this % nrows()
            if (keep_mask(i)) then
                j = j + 1
                clean_indices(j) = i
            end if
        end do

        ! Create new dataframe with clean rows
        call clean_df % new(this % max_char_len)

        do i = 1, this % ncols()
            dtype = this % dtype(i)

            select case (dtype)
            case (REAL_NUM)
                real_col = this % getr(i)
                allocate (real_clean_col(num_clean_rows))
                do j = 1, num_clean_rows
                    real_clean_col(j) = real_col(clean_indices(j))
                end do
                if (this % with_headers) then
                    header_name = this % header(i)
                    call clean_df % append(real_clean_col, trim(header_name))
                else
                    call clean_df % append(real_clean_col)
                end if
                deallocate (real_col)
                deallocate (real_clean_col)

            case (INTEGER_NUM)
                int_col = this % geti(i)
                allocate (int_clean_col(num_clean_rows))
                do j = 1, num_clean_rows
                    int_clean_col(j) = int_col(clean_indices(j))
                end do
                if (this % with_headers) then
                    header_name = this % header(i)
                    call clean_df % append(int_clean_col, trim(header_name))
                else
                    call clean_df % append(int_clean_col)
                end if
                deallocate (int_col)
                deallocate (int_clean_col)

            case (LOGICAL_NUM)
                log_col = this % getl(i)
                allocate (log_clean_col(num_clean_rows))
                do j = 1, num_clean_rows
                    log_clean_col(j) = log_col(clean_indices(j))
                end do
                if (this % with_headers) then
                    header_name = this % header(i)
                    call clean_df % append(log_clean_col, trim(header_name))
                else
                    call clean_df % append(log_clean_col)
                end if
                deallocate (log_col)
                deallocate (log_clean_col)

            case (CHARACTER_NUM)
                char_col = this % getch(i)
                allocate (character(len=len(char_col)) :: char_clean_col(num_clean_rows))
                do j = 1, num_clean_rows
                    char_clean_col(j) = char_col(clean_indices(j))
                end do
                if (this % with_headers) then
                    header_name = this % header(i)
                    call clean_df % append(char_clean_col, trim(header_name))
                else
                    call clean_df % append(char_clean_col)
                end if
                deallocate (char_col)
                deallocate (char_clean_col)

            case (COMPLEX_NUM)
                cmplx_col = this % getc(i)
                allocate (cmplx_clean_col(num_clean_rows))
                do j = 1, num_clean_rows
                    cmplx_clean_col(j) = cmplx_col(clean_indices(j))
                end do
                if (this % with_headers) then
                    header_name = this % header(i)
                    call clean_df % append(cmplx_clean_col, trim(header_name))
                else
                    call clean_df % append(cmplx_clean_col)
                end if
                deallocate (cmplx_col)
                deallocate (cmplx_clean_col)
            end select
        end do

        deallocate (keep_mask)
        deallocate (clean_indices)
    end function dropna

    ! ========================================================================
    ! ADVANCED DATA OPERATIONS
    ! ========================================================================

    !> Get unique values from a real column
    !!
    !! @param[in] this The data frame instance
    !! @param[in] col_index Column index
    !! @return Array of unique values (sorted)
    function unique_real(this, col_index) result(unique_vals)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: col_index
        real(rk), dimension(:), allocatable :: unique_vals

        real(rk), dimension(:), allocatable :: col, temp_unique
        integer :: i, j, n_unique
        logical :: is_unique

        col = this % getr(col_index)
        allocate (temp_unique(size(col)))
        n_unique = 0

        do i = 1, size(col)
            is_unique = .true.
            do j = 1, n_unique
                if (abs(col(i) - temp_unique(j)) < 1.0e-10_rk) then
                    is_unique = .false.
                    exit
                end if
            end do
            if (is_unique) then
                n_unique = n_unique + 1
                temp_unique(n_unique) = col(i)
            end if
        end do

        allocate (unique_vals(n_unique))
        unique_vals = temp_unique(1:n_unique)

        ! Sort unique values
        call quick_sort_real(unique_vals, 1, n_unique)

        deallocate (temp_unique)
    end function unique_real

    !> Get unique values from an integer column
    function unique_integer(this, col_index) result(unique_vals)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: col_index
        integer(ik), dimension(:), allocatable :: unique_vals

        integer(ik), dimension(:), allocatable :: col, temp_unique
        integer :: i, j, n_unique
        logical :: is_unique

        col = this % geti(col_index)
        allocate (temp_unique(size(col)))
        n_unique = 0

        do i = 1, size(col)
            is_unique = .true.
            do j = 1, n_unique
                if (col(i) == temp_unique(j)) then
                    is_unique = .false.
                    exit
                end if
            end do
            if (is_unique) then
                n_unique = n_unique + 1
                temp_unique(n_unique) = col(i)
            end if
        end do

        allocate (unique_vals(n_unique))
        unique_vals = temp_unique(1:n_unique)

        ! Sort unique values
        call quick_sort_integer(unique_vals, 1, n_unique)

        deallocate (temp_unique)
    end function unique_integer

    !> Get unique values from a character column
    function unique_character(this, col_index) result(unique_vals)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: col_index
        character(len=:), allocatable :: unique_vals(:)

        character(len=:), allocatable :: col(:), temp_unique(:)
        integer :: i, j, n_unique
        logical :: is_unique

        col = this % getch(col_index)
        allocate (character(len=len(col)) :: temp_unique(size(col)))
        n_unique = 0

        do i = 1, size(col)
            is_unique = .true.
            do j = 1, n_unique
                if (trim(col(i)) == trim(temp_unique(j))) then
                    is_unique = .false.
                    exit
                end if
            end do
            if (is_unique) then
                n_unique = n_unique + 1
                temp_unique(n_unique) = col(i)
            end if
        end do

        allocate (character(len=len(col)) :: unique_vals(n_unique))
        unique_vals = temp_unique(1:n_unique)

        deallocate (temp_unique)
    end function unique_character

    !> Count occurrences of each value in a real column
    !!
    !! Returns a dataframe with two columns: unique values and their counts
    function value_counts_real(this, col_index) result(counts_df)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: col_index
        type(data_frame) :: counts_df

        real(rk), dimension(:), allocatable :: col, unique_vals
        integer(ik), dimension(:), allocatable :: counts
        integer :: i, j

        unique_vals = this % unique_real(col_index)
        allocate (counts(size(unique_vals)))
        counts = 0

        col = this % getr(col_index)
        do i = 1, size(col)
            do j = 1, size(unique_vals)
                if (abs(col(i) - unique_vals(j)) < 1.0e-10_rk) then
                    counts(j) = counts(j) + 1
                    exit
                end if
            end do
        end do

        call counts_df % new()
        call counts_df % append(unique_vals, "Value")
        call counts_df % append(counts, "Count")
    end function value_counts_real

    !> Count occurrences of each value in an integer column
    function value_counts_integer(this, col_index) result(counts_df)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: col_index
        type(data_frame) :: counts_df

        integer(ik), dimension(:), allocatable :: col, unique_vals, counts
        integer :: i, j

        unique_vals = this % unique_integer(col_index)
        allocate (counts(size(unique_vals)))
        counts = 0

        col = this % geti(col_index)
        do i = 1, size(col)
            do j = 1, size(unique_vals)
                if (col(i) == unique_vals(j)) then
                    counts(j) = counts(j) + 1
                    exit
                end if
            end do
        end do

        call counts_df % new()
        call counts_df % append(unique_vals, "Value")
        call counts_df % append(counts, "Count")
    end function value_counts_integer

    !> Count occurrences of each value in a character column
    function value_counts_character(this, col_index) result(counts_df)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: col_index
        type(data_frame) :: counts_df

        character(len=:), allocatable :: col(:), unique_vals(:)
        integer(ik), dimension(:), allocatable :: counts
        integer :: i, j

        unique_vals = this % unique_character(col_index)
        allocate (counts(size(unique_vals)))
        counts = 0

        col = this % getch(col_index)
        do i = 1, size(col)
            do j = 1, size(unique_vals)
                if (trim(col(i)) == trim(unique_vals(j))) then
                    counts(j) = counts(j) + 1
                    exit
                end if
            end do
        end do

        call counts_df % new()
        call counts_df % append(unique_vals, "Value")
        call counts_df % append(counts, "Count")
    end function value_counts_character

    !> Concatenate two dataframes vertically (rows) or horizontally (columns)
    !!
    !! @param[in] this First dataframe
    !! @param[in] other Second dataframe
    !! @param[in] axis 0 for vertical (stack rows), 1 for horizontal (add columns)
    !! @return Concatenated dataframe
    function concat(this, other, axis) result(result_df)
        class(data_frame), intent(in) :: this, other
        integer, intent(in) :: axis
        type(data_frame) :: result_df

        integer :: i, j, dtype
        character(len=100) :: header_name
        real(rk), dimension(:), allocatable :: real_col1, real_col2, real_combined, real_col
        integer(ik), dimension(:), allocatable :: int_col1, int_col2, int_combined, int_col
        logical, dimension(:), allocatable :: log_col1, log_col2, log_combined, log_col
        character(len=:), allocatable :: char_col1(:), char_col2(:), char_combined(:), char_col(:)
        complex(rk), dimension(:), allocatable :: cmplx_col1, cmplx_col2, cmplx_combined, cmplx_col

        if (axis == 0) then
            ! Vertical concatenation (stack rows)
            if (this % ncols() /= other % ncols()) then
                print *, "Error: DataFrames must have same number of columns"
                call result_df % new()
                return
            end if

            call result_df % new(max(this % max_char_len, other % max_char_len))

            do i = 1, this % ncols()
                dtype = this % dtype(i)

                select case (dtype)
                case (REAL_NUM)
                    real_col1 = this % getr(i)
                    real_col2 = other % getr(i)
                    allocate (real_combined(size(real_col1) + size(real_col2)))
                    real_combined(1:size(real_col1)) = real_col1
                    real_combined(size(real_col1) + 1:) = real_col2
                    if (this % with_headers) then
                        header_name = this % header(i)
                        call result_df % append(real_combined, trim(header_name))
                    else
                        call result_df % append(real_combined)
                    end if
                    deallocate (real_col1, real_col2, real_combined)

                case (INTEGER_NUM)
                    int_col1 = this % geti(i)
                    int_col2 = other % geti(i)
                    allocate (int_combined(size(int_col1) + size(int_col2)))
                    int_combined(1:size(int_col1)) = int_col1
                    int_combined(size(int_col1) + 1:) = int_col2
                    if (this % with_headers) then
                        header_name = this % header(i)
                        call result_df % append(int_combined, trim(header_name))
                    else
                        call result_df % append(int_combined)
                    end if
                    deallocate (int_col1, int_col2, int_combined)

                case (LOGICAL_NUM)
                    log_col1 = this % getl(i)
                    log_col2 = other % getl(i)
                    allocate (log_combined(size(log_col1) + size(log_col2)))
                    log_combined(1:size(log_col1)) = log_col1
                    log_combined(size(log_col1) + 1:) = log_col2
                    if (this % with_headers) then
                        header_name = this % header(i)
                        call result_df % append(log_combined, trim(header_name))
                    else
                        call result_df % append(log_combined)
                    end if
                    deallocate (log_col1, log_col2, log_combined)

                case (CHARACTER_NUM)
                    char_col1 = this % getch(i)
                    char_col2 = other % getch(i)
       allocate (character(len=max(len(char_col1), len(char_col2))) :: char_combined(size(char_col1) + size(char_col2)))
                    char_combined(1:size(char_col1)) = char_col1
                    char_combined(size(char_col1) + 1:) = char_col2
                    if (this % with_headers) then
                        header_name = this % header(i)
                        call result_df % append(char_combined, trim(header_name))
                    else
                        call result_df % append(char_combined)
                    end if
                    deallocate (char_col1, char_col2, char_combined)

                case (COMPLEX_NUM)
                    cmplx_col1 = this % getc(i)
                    cmplx_col2 = other % getc(i)
                    allocate (cmplx_combined(size(cmplx_col1) + size(cmplx_col2)))
                    cmplx_combined(1:size(cmplx_col1)) = cmplx_col1
                    cmplx_combined(size(cmplx_col1) + 1:) = cmplx_col2
                    if (this % with_headers) then
                        header_name = this % header(i)
                        call result_df % append(cmplx_combined, trim(header_name))
                    else
                        call result_df % append(cmplx_combined)
                    end if
                    deallocate (cmplx_col1, cmplx_col2, cmplx_combined)
                end select
            end do

        else if (axis == 1) then
            ! Horizontal concatenation (add columns)
            if (this % nrows() /= other % nrows()) then
                print *, "Error: DataFrames must have same number of rows"
                call result_df % new()
                return
            end if

            result_df = this % copy()

            ! Add columns from other
            do i = 1, other % ncols()
                dtype = other % dtype(i)

                select case (dtype)
                case (REAL_NUM)
                    real_col = other % getr(i)
                    if (other % with_headers) then
                        header_name = other % header(i)
                        call result_df % append(real_col, trim(header_name))
                    else
                        call result_df % append(real_col)
                    end if
                    deallocate (real_col)

                case (INTEGER_NUM)
                    int_col = other % geti(i)
                    if (other % with_headers) then
                        header_name = other % header(i)
                        call result_df % append(int_col, trim(header_name))
                    else
                        call result_df % append(int_col)
                    end if
                    deallocate (int_col)

                case (LOGICAL_NUM)
                    log_col = other % getl(i)
                    if (other % with_headers) then
                        header_name = other % header(i)
                        call result_df % append(log_col, trim(header_name))
                    else
                        call result_df % append(log_col)
                    end if
                    deallocate (log_col)

                case (CHARACTER_NUM)
                    char_col = other % getch(i)
                    if (other % with_headers) then
                        header_name = other % header(i)
                        call result_df % append(char_col, trim(header_name))
                    else
                        call result_df % append(char_col)
                    end if
                    deallocate (char_col)

                case (COMPLEX_NUM)
                    cmplx_col = other % getc(i)
                    if (other % with_headers) then
                        header_name = other % header(i)
                        call result_df % append(cmplx_col, trim(header_name))
                    else
                        call result_df % append(cmplx_col)
                    end if
                    deallocate (cmplx_col)
                end select
            end do
        else
            print *, "Error: axis must be 0 (vertical) or 1 (horizontal)"
            call result_df % new()
        end if
    end function concat

    !> Merge two dataframes on column names (similar to SQL join but using names)
    !!
    !! @param[in] this Left dataframe
    !! @param[in] other Right dataframe
    !! @param[in] on_column Name of column to join on
    !! @param[in] how Type of join: "inner", "left", "right", "outer"
    !! @return Merged dataframe
    function merge(this, other, on_column, how) result(merged_df)
        class(data_frame), intent(in) :: this, other
        character(len=*), intent(in) :: on_column, how
        type(data_frame) :: merged_df

        integer :: this_col_idx, other_col_idx

        ! Find column indices
        this_col_idx = this % find_header_index(on_column)
        other_col_idx = other % find_header_index(on_column)

        if (this_col_idx == -1 .or. other_col_idx == -1) then
            print *, "Error: Column '", trim(on_column), "' not found in one or both dataframes"
            call merged_df % new()
            return
        end if

        ! Perform appropriate join based on 'how'
        select case (trim(how))
        case ("inner")
            merged_df = this % inner_join(other, this_col_idx, other_col_idx)
        case ("left")
            merged_df = this % left_join(other, this_col_idx, other_col_idx)
        case ("right")
            merged_df = this % right_join(other, this_col_idx, other_col_idx)
        case ("outer")
            merged_df = this % outer_join(other, this_col_idx, other_col_idx)
        case default
            print *, "Error: 'how' must be 'inner', 'left', 'right', or 'outer'"
            call merged_df % new()
        end select
    end function merge

    !> Rank values in a real column
    !!
    !! @param[in] this The dataframe
    !! @param[in] col_index Column to rank
    !! @param[in] ascending Rank in ascending order (default .true.)
    !! @return Integer array of ranks (1 = smallest/largest)
    function rank_real(this, col_index, ascending) result(ranks)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: col_index
        logical, intent(in), optional :: ascending
        integer(ik), dimension(:), allocatable :: ranks

        real(rk), dimension(:), allocatable :: col, sorted_col
        integer :: i, j, n
        logical :: asc

        asc = .true.
        if (present(ascending)) asc = ascending

        col = this % getr(col_index)
        n = size(col)
        allocate (ranks(n))
        allocate (sorted_col(n))
        sorted_col = col

        call quick_sort_real(sorted_col, 1, n)

        if (.not. asc) then
            ! Reverse for descending
            sorted_col = sorted_col(n:1:-1)
        end if

        do i = 1, n
            do j = 1, n
                if (abs(col(i) - sorted_col(j)) < 1.0e-10_rk) then
                    ranks(i) = j
                    exit
                end if
            end do
        end do

        deallocate (sorted_col)
    end function rank_real

    !> Rank values in an integer column
    function rank_integer(this, col_index, ascending) result(ranks)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: col_index
        logical, intent(in), optional :: ascending
        integer(ik), dimension(:), allocatable :: ranks

        integer(ik), dimension(:), allocatable :: col, sorted_col
        integer :: i, j, n
        logical :: asc

        asc = .true.
        if (present(ascending)) asc = ascending

        col = this % geti(col_index)
        n = size(col)
        allocate (ranks(n))
        allocate (sorted_col(n))
        sorted_col = col

        call quick_sort_integer(sorted_col, 1, n)

        if (.not. asc) then
            sorted_col = sorted_col(n:1:-1)
        end if

        do i = 1, n
            do j = 1, n
                if (col(i) == sorted_col(j)) then
                    ranks(i) = j
                    exit
                end if
            end do
        end do

        deallocate (sorted_col)
    end function rank_integer

    !> Check which rows are duplicates
    !!
    !! Returns logical array where .true. indicates a duplicate row
    !! (first occurrence is not marked as duplicate)
    function duplicated(this) result(is_dup)
        class(data_frame), intent(in) :: this
        logical, dimension(:), allocatable :: is_dup

        integer :: i, j, k, dtype
        logical :: rows_match

        allocate (is_dup(this % nrows()))
        is_dup = .false.

        do i = 2, this % nrows()
            do j = 1, i - 1
                rows_match = .true.

                ! Compare all columns
                do k = 1, this % ncols()
                    dtype = this % dtype(k)

                    select case (dtype)
                    case (REAL_NUM)
                        if (abs(this % get_val_real(k, i) - this % get_val_real(k, j)) >= 1.0e-10_rk) then
                            rows_match = .false.
                            exit
                        end if
                    case (INTEGER_NUM)
                        if (this % get_val_integer(k, i) /= this % get_val_integer(k, j)) then
                            rows_match = .false.
                            exit
                        end if
                    case (LOGICAL_NUM)
                        if (this % get_val_logical(k, i) .neqv. this % get_val_logical(k, j)) then
                            rows_match = .false.
                            exit
                        end if
                    case (CHARACTER_NUM)
                        if (trim(this % get_val_character(k, i)) /= trim(this % get_val_character(k, j))) then
                            rows_match = .false.
                            exit
                        end if
                    case (COMPLEX_NUM)
                        if (this % get_val_complex(k, i) /= this % get_val_complex(k, j)) then
                            rows_match = .false.
                            exit
                        end if
                    end select
                end do

                if (rows_match) then
                    is_dup(i) = .true.
                    exit
                end if
            end do
        end do
    end function duplicated

    !> Remove duplicate rows
    !!
    !! Returns new dataframe with duplicate rows removed (keeps first occurrence)
    function drop_duplicates(this) result(unique_df)
        class(data_frame), intent(in) :: this
        type(data_frame) :: unique_df

        logical, dimension(:), allocatable :: is_dup, keep_mask
        integer :: i, j, num_unique, dtype
        integer, dimension(:), allocatable :: unique_indices
        character(len=100) :: header_name
        real(rk), dimension(:), allocatable :: real_col, real_unique_col
        integer(ik), dimension(:), allocatable :: int_col, int_unique_col
        logical, dimension(:), allocatable :: log_col, log_unique_col
        character(len=:), allocatable :: char_col(:), char_unique_col(:)
        complex(rk), dimension(:), allocatable :: cmplx_col, cmplx_unique_col

        is_dup = this % duplicated()
        allocate (keep_mask(this % nrows()))
        keep_mask = .not. is_dup

        num_unique = count(keep_mask)

        if (num_unique == 0) then
            call unique_df % new()
            return
        end if

        ! Build index array
        allocate (unique_indices(num_unique))
        j = 0
        do i = 1, this % nrows()
            if (keep_mask(i)) then
                j = j + 1
                unique_indices(j) = i
            end if
        end do

        ! Build result dataframe
        call unique_df % new(this % max_char_len)

        do i = 1, this % ncols()
            dtype = this % dtype(i)

            select case (dtype)
            case (REAL_NUM)
                real_col = this % getr(i)
                allocate (real_unique_col(num_unique))
                do j = 1, num_unique
                    real_unique_col(j) = real_col(unique_indices(j))
                end do
                if (this % with_headers) then
                    header_name = this % header(i)
                    call unique_df % append(real_unique_col, trim(header_name))
                else
                    call unique_df % append(real_unique_col)
                end if
                deallocate (real_unique_col)

            case (INTEGER_NUM)
                int_col = this % geti(i)
                allocate (int_unique_col(num_unique))
                do j = 1, num_unique
                    int_unique_col(j) = int_col(unique_indices(j))
                end do
                if (this % with_headers) then
                    header_name = this % header(i)
                    call unique_df % append(int_unique_col, trim(header_name))
                else
                    call unique_df % append(int_unique_col)
                end if
                deallocate (int_unique_col)

            case (LOGICAL_NUM)
                log_col = this % getl(i)
                allocate (log_unique_col(num_unique))
                do j = 1, num_unique
                    log_unique_col(j) = log_col(unique_indices(j))
                end do
                if (this % with_headers) then
                    header_name = this % header(i)
                    call unique_df % append(log_unique_col, trim(header_name))
                else
                    call unique_df % append(log_unique_col)
                end if
                deallocate (log_unique_col)

            case (CHARACTER_NUM)
                char_col = this % getch(i)
                allocate (character(len=len(char_col)) :: char_unique_col(num_unique))
                do j = 1, num_unique
                    char_unique_col(j) = char_col(unique_indices(j))
                end do
                if (this % with_headers) then
                    header_name = this % header(i)
                    call unique_df % append(char_unique_col, trim(header_name))
                else
                    call unique_df % append(char_unique_col)
                end if
                deallocate (char_unique_col)

            case (COMPLEX_NUM)
                cmplx_col = this % getc(i)
                allocate (cmplx_unique_col(num_unique))
                do j = 1, num_unique
                    cmplx_unique_col(j) = cmplx_col(unique_indices(j))
                end do
                if (this % with_headers) then
                    header_name = this % header(i)
                    call unique_df % append(cmplx_unique_col, trim(header_name))
                else
                    call unique_df % append(cmplx_unique_col)
                end if
                deallocate (cmplx_unique_col)
            end select
        end do

        deallocate (is_dup, keep_mask, unique_indices)
    end function drop_duplicates

    !> Drop duplicate rows based on specific columns (subset)
    !!
    !! @param[in] this The dataframe
    !! @param[in] col_indices Array of column indices to check for duplicates
    !! @return New dataframe with duplicate rows removed
    function drop_duplicates_subset(this, col_indices) result(unique_df)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: col_indices(:)
        type(data_frame) :: unique_df
        logical, dimension(:), allocatable :: is_dup, keep_mask
        integer, dimension(:), allocatable :: unique_indices
        integer :: i, j, k, row1, row2, dtype, num_unique, col_idx
        logical :: is_duplicate
        character(len=:), allocatable :: header_name
        real(rk), dimension(:), allocatable :: real_col, real_unique_col
        integer(ik), dimension(:), allocatable :: int_col, int_unique_col
        logical, dimension(:), allocatable :: log_col, log_unique_col
        character(len=:), allocatable :: char_col(:), char_unique_col(:)
        complex(rk), dimension(:), allocatable :: cmplx_col, cmplx_unique_col

        allocate (is_dup(this % nrows()))
        is_dup = .false.

        ! Check each row against all previous rows (only comparing subset columns)
        do row1 = 1, this % nrows()
            if (is_dup(row1)) cycle

            do row2 = row1 + 1, this % nrows()
                if (is_dup(row2)) cycle

                is_duplicate = .true.
                ! Only check columns in subset
                do k = 1, size(col_indices)
                    col_idx = col_indices(k)
                    dtype = this % dtype(col_idx)

                    select case (dtype)
                    case (REAL_NUM)
                        real_col = this % getr(col_idx)
                        if (.not. ieee_is_nan(real_col(row1)) .and. .not. ieee_is_nan(real_col(row2))) then
                            if (real_col(row1) /= real_col(row2)) then
                                is_duplicate = .false.
                                exit
                            end if
                        else if (ieee_is_nan(real_col(row1)) .neqv. ieee_is_nan(real_col(row2))) then
                            is_duplicate = .false.
                            exit
                        end if
                    case (INTEGER_NUM)
                        int_col = this % geti(col_idx)
                        if (.not. is_nan_integer(int_col(row1)) .and. .not. is_nan_integer(int_col(row2))) then
                            if (int_col(row1) /= int_col(row2)) then
                                is_duplicate = .false.
                                exit
                            end if
                        else if (is_nan_integer(int_col(row1)) .neqv. is_nan_integer(int_col(row2))) then
                            is_duplicate = .false.
                            exit
                        end if
                    case (LOGICAL_NUM)
                        log_col = this % getl(col_idx)
                        if (log_col(row1) .neqv. log_col(row2)) then
                            is_duplicate = .false.
                            exit
                        end if
                    case (CHARACTER_NUM)
                        char_col = this % getch(col_idx)
                        if (char_col(row1) /= char_col(row2)) then
                            is_duplicate = .false.
                            exit
                        end if
                    case (COMPLEX_NUM)
                        cmplx_col = this % getc(col_idx)
                        if (cmplx_col(row1) /= cmplx_col(row2)) then
                            is_duplicate = .false.
                            exit
                        end if
                    end select
                end do

                if (is_duplicate) then
                    is_dup(row2) = .true.
                end if
            end do
        end do

        ! Build mask for rows to keep
        allocate (keep_mask(this % nrows()))
        keep_mask = .not. is_dup
        num_unique = count(keep_mask)

        ! Get indices of unique rows
        allocate (unique_indices(num_unique))
        j = 0
        do i = 1, this % nrows()
            if (keep_mask(i)) then
                j = j + 1
                unique_indices(j) = i
            end if
        end do

        ! Build result dataframe with all columns
        call unique_df % new(this % max_char_len)

        do i = 1, this % ncols()
            dtype = this % dtype(i)

            select case (dtype)
            case (REAL_NUM)
                real_col = this % getr(i)
                allocate (real_unique_col(num_unique))
                do j = 1, num_unique
                    real_unique_col(j) = real_col(unique_indices(j))
                end do
                if (this % with_headers) then
                    header_name = this % header(i)
                    call unique_df % append(real_unique_col, trim(header_name))
                else
                    call unique_df % append(real_unique_col)
                end if
                deallocate (real_unique_col)

            case (INTEGER_NUM)
                int_col = this % geti(i)
                allocate (int_unique_col(num_unique))
                do j = 1, num_unique
                    int_unique_col(j) = int_col(unique_indices(j))
                end do
                if (this % with_headers) then
                    header_name = this % header(i)
                    call unique_df % append(int_unique_col, trim(header_name))
                else
                    call unique_df % append(int_unique_col)
                end if
                deallocate (int_unique_col)

            case (LOGICAL_NUM)
                log_col = this % getl(i)
                allocate (log_unique_col(num_unique))
                do j = 1, num_unique
                    log_unique_col(j) = log_col(unique_indices(j))
                end do
                if (this % with_headers) then
                    header_name = this % header(i)
                    call unique_df % append(log_unique_col, trim(header_name))
                else
                    call unique_df % append(log_unique_col)
                end if
                deallocate (log_unique_col)

            case (CHARACTER_NUM)
                char_col = this % getch(i)
                allocate (character(len=len(char_col)) :: char_unique_col(num_unique))
                do j = 1, num_unique
                    char_unique_col(j) = char_col(unique_indices(j))
                end do
                if (this % with_headers) then
                    header_name = this % header(i)
                    call unique_df % append(char_unique_col, trim(header_name))
                else
                    call unique_df % append(char_unique_col)
                end if
                deallocate (char_unique_col)

            case (COMPLEX_NUM)
                cmplx_col = this % getc(i)
                allocate (cmplx_unique_col(num_unique))
                do j = 1, num_unique
                    cmplx_unique_col(j) = cmplx_col(unique_indices(j))
                end do
                if (this % with_headers) then
                    header_name = this % header(i)
                    call unique_df % append(cmplx_unique_col, trim(header_name))
                else
                    call unique_df % append(cmplx_unique_col)
                end if
                deallocate (cmplx_unique_col)
            end select
        end do

        deallocate (is_dup, keep_mask, unique_indices)
    end function drop_duplicates_subset

    !> Apply a function to a single row
    !!
    !! Extracts all numeric values from a specified row and applies
    !! a user-defined function to them. Non-numeric columns are skipped.
    !!
    !! @param[in] row_idx Row index to process
    !! @param[in] func Function to apply (must match row_func_real interface)
    !! @return Result of applying the function to the row
    !!
    !! ## Example
    !!
    !! ```fortran
    !! ! Define a function that computes row sum
    !! function row_sum(values, n) result(s)
    !!     real(rk), dimension(:), intent(in) :: values
    !!     integer, intent(in) :: n
    !!     real(rk) :: s
    !!     s = sum(values(1:n))
    !! end function
    !!
    !! ! Apply it
    !! result = df%apply_to_row_real(5, row_sum)
    !! ```
    function apply_to_row_real(this, row_idx, func) result(output)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: row_idx
        procedure(row_func_real) :: func
        real(rk) :: output

        real(rk), dimension(:), allocatable :: row_values
        integer :: i, n_numeric, col_type
        real(rk) :: temp_real
        integer(ik) :: temp_int

        ! Count numeric columns
        n_numeric = 0
        do i = 1, this % num_cols
            col_type = this % data_cols(i) % dtype
            if (col_type == 1 .or. col_type == 2) then  ! 1=REAL_NUM, 2=INT_NUM
                n_numeric = n_numeric + 1
            end if
        end do

        ! Allocate and fill row values
        allocate (row_values(n_numeric))
        n_numeric = 0
        do i = 1, this % num_cols
            col_type = this % data_cols(i) % dtype
            if (col_type == 1) then  ! REAL_NUM
                n_numeric = n_numeric + 1
                row_values(n_numeric) = this % get_val_real(i, row_idx)
            else if (col_type == 2) then  ! INT_NUM
                n_numeric = n_numeric + 1
                row_values(n_numeric) = real(this % get_val_integer(i, row_idx), rk)
            end if
        end do

        ! Apply function
        output = func(row_values, n_numeric)

        deallocate (row_values)
    end function apply_to_row_real

    !> Apply a function to all rows
    !!
    !! Applies a user-defined function to each row in the data frame,
    !! returning an array of results.
    !!
    !! @param[in] func Function to apply (must match row_func_real interface)
    !! @return Array of results, one per row
    !!
    !! ## Example
    !!
    !! ```fortran
    !! real(rk), dimension(:), allocatable :: row_sums
    !! row_sums = df%apply_to_all_rows_real(row_sum)
    !! ```
    function apply_to_all_rows_real(this, func) result(outputs)
        class(data_frame), intent(in) :: this
        procedure(row_func_real) :: func
        real(rk), dimension(:), allocatable :: outputs
        integer :: i, n_rows

        n_rows = this % nrows()
        allocate (outputs(n_rows))

        do i = 1, n_rows
            outputs(i) = this % apply_to_row_real(i, func)
        end do
    end function apply_to_all_rows_real

end module datafort
