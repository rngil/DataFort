!> DataFort I/O Module
!!
!! This module provides I/O operations for data frames including CSV reading/writing
!! and console output.
!!
!! ## Functions
!!
!! ### CSV Operations
!! - `df_write_csv(df, filename)` - Export data frame to CSV file
!! - `df_read_csv(df, filename, has_headers)` - Import CSV file into data frame
!!
!! ### Console Output
!! - `df_write_console(df, unit)` - Display data frame in console
module datafort_io
    use precision
    use types
    use column_class
    use datafort_types
    use datafort_accessors
    implicit none
    private

    ! Public I/O functions
    public :: df_write_csv, df_read_csv, df_write_console

contains

    !> Export data frame to CSV file
    !!
    !! Writes the data frame to a CSV file with optional headers.
    !! Handles all data types: real, integer, logical, character, and complex.
    !!
    !! @param[in] df The data frame to export
    !! @param[in] filename Path to the output CSV file
    !!
    !! @note Character values are quoted in the output
    !! @note Complex values are written as (real,imag)
    subroutine df_write_csv(df, filename)
        type(data_frame), intent(in) :: df
        character(len=*), intent(in) :: filename

        integer :: unit, i, j, dtype
        integer :: num_cols, num_rows
        type(column) :: data_col
        logical :: has_headers

        open (newunit=unit, file=filename, status='replace', action='write')

        num_cols = df%ncols()
        num_rows = df%nrows()
        has_headers = df%get_with_headers()

        ! Write headers if present
        if (has_headers) then
            do i = 1, num_cols
                if (i > 1) write (unit, '(a)', advance='no') ','
                write (unit, '(a)', advance='no') trim(df%header(i))
            end do
            write (unit, '(a)') ''
        end if

        ! Write data
        do j = 1, num_rows
            do i = 1, num_cols
                if (i > 1) write (unit, '(a)', advance='no') ','

                data_col = df%get_data_col(i)
                dtype = data_col%get_type()

                select case (dtype)
                case (REAL_NUM)
                    write (unit, '(g0)', advance='no') data_col%getr(j)
                case (INTEGER_NUM)
                    write (unit, '(i0)', advance='no') data_col%geti(j)
                case (LOGICAL_NUM)
                    write (unit, '(l1)', advance='no') data_col%getl(j)
                case (CHARACTER_NUM)
                    write (unit, '(a)', advance='no') '"'//trim(data_col%getch(j))//'"'
                case (COMPLEX_NUM)
                    write (unit, '("(",g0,",",g0,")")', advance='no') data_col%getc(j)
                end select
            end do
            write (unit, '(a)') ''
        end do

        close (unit)
    end subroutine df_write_csv

    !> Import CSV file into data frame
    !!
    !! Reads a CSV file and populates the data frame with automatic type detection.
    !! Supports integer, real, logical, and character data types.
    !!
    !! @param[in,out] df The data frame to populate (will be initialized)
    !! @param[in] filename Path to the input CSV file
    !! @param[in] has_headers Whether the first row contains column headers
    !!
    !! @note The data frame will be reinitialized (destroyed and recreated)
    !! @note Type detection occurs on the first non-empty, non-NaN value
    !! @note Supported NaN representations: NaN, NA, NULL, N/A, -, (empty)
    subroutine df_read_csv(df, filename, has_headers)
        type(data_frame), intent(inout) :: df
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
        call df%new()

        ! Add columns with automatic type detection
        do j = 1, num_cols
            call add_csv_column(df, all_data(:, j), headers, j, has_headers)
        end do

        deallocate (fields, all_data)
        if (allocated(headers)) deallocate (headers)
    end subroutine df_read_csv

    !> Display data frame in console
    !!
    !! Prints the data frame in a formatted table to the console or specified unit.
    !! Includes headers (if present) and a separator line.
    !!
    !! @param[in] df The data frame to display
    !! @param[in] unit Optional output unit (default: 6 for stdout)
    !!
    !! @note Column width is fixed at 12 characters
    subroutine df_write_console(df, unit)
        type(data_frame), intent(in) :: df
        integer, intent(in), optional :: unit

        integer :: out_unit, i, j, dtype, col_width
        integer :: num_cols, num_rows
        type(column) :: data_col
        logical :: has_headers

        if (present(unit)) then
            out_unit = unit
        else
            out_unit = 6  ! stdout
        end if

        num_cols = df%ncols()
        num_rows = df%nrows()
        has_headers = df%get_with_headers()

        if (num_cols == 0) then
            write (out_unit, '(a)') "Empty data frame"
            return
        end if

        col_width = 12  ! Fixed column width for formatting

        ! Write headers if present
        if (has_headers) then
            do i = 1, num_cols
                write (out_unit, '(a12)', advance='no') df%header(i)
            end do
            write (out_unit, '(a)') ''

            ! Write separator line
            do i = 1, num_cols
                write (out_unit, '(a12)', advance='no') repeat('-', col_width)
            end do
            write (out_unit, '(a)') ''
        end if

        ! Write data
        do j = 1, num_rows
            do i = 1, num_cols
                data_col = df%get_data_col(i)
                dtype = data_col%get_type()

                select case (dtype)
                case (REAL_NUM)
                    write (out_unit, '(g12.4)', advance='no') data_col%getr(j)
                case (INTEGER_NUM)
                    write (out_unit, '(i12)', advance='no') data_col%geti(j)
                case (LOGICAL_NUM)
                    if (data_col%getl(j)) then
                        write (out_unit, '(a12)', advance='no') 'T'
                    else
                        write (out_unit, '(a12)', advance='no') 'F'
                    end if
                case (CHARACTER_NUM)
                    write (out_unit, '(a12)', advance='no') data_col%getch(j)
                case (COMPLEX_NUM)
                    write (out_unit, '("(",f5.2,",",f5.2,")")', advance='no') data_col%getc(j)
                end select
            end do
            write (out_unit, '(a)') ''
        end do
    end subroutine df_write_console

    !========================================================================
    ! INTERNAL HELPER PROCEDURES
    !========================================================================

    !> Parse a CSV line into fields
    !!
    !! Helper subroutine that splits a CSV line into individual fields,
    !! handling quoted strings and commas within quotes.
    !!
    !! @param[in] line The CSV line to parse
    !! @param[out] fields Array of parsed field values
    !!
    !! @note Supports quoted fields with embedded commas
    !! @note Maximum 50 fields per line
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

    !> Add a column with automatic type detection
    !!
    !! Helper subroutine that detects the data type from string data and
    !! adds the appropriate typed column to the data frame.
    !!
    !! @param[in,out] df The data frame to add the column to
    !! @param[in] data_strings Array of string values to convert and add
    !! @param[in] headers Optional array of header names
    !! @param[in] col_index Index of the column being added
    !! @param[in] has_headers Whether the data frame has headers
    !!
    !! @note Type detection priority: integer -> real -> logical -> character
    !! @note NaN values are preserved for numeric types
    subroutine add_csv_column(df, data_strings, headers, col_index, has_headers)
        type(data_frame), intent(inout) :: df
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
                call df_append_integer(df, int_data, headers(col_index))
            else
                call df_append_integer(df, int_data)
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
                call df_append_real(df, real_data, headers(col_index))
            else
                call df_append_real(df, real_data)
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
                call df_append_logical(df, logical_data, headers(col_index))
            else
                call df_append_logical(df, logical_data)
            end if
        case default ! CHARACTER_NUM
            if (has_headers .and. present(headers)) then
                call df_append_character(df, data_strings, headers(col_index))
            else
                call df_append_character(df, data_strings)
            end if
        end select
    end subroutine add_csv_column

end module datafort_io
