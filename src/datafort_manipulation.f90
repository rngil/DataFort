!> DataFort Manipulation Module
!!
!! This module provides standalone functions for data frame manipulation operations.
!! All functions take a data_frame as the first argument instead of being type-bound procedures.
!!
!! ## Functions
!!
!! ### Selection and Slicing
!! - `df_select_columns(df, column_indices)` - Extract subset of columns
!! - `df_slice_rows(df, start_row, end_row)` - Extract row range
!!
!! ### Filtering
!! - `df_filter_rows_logical(df, logical_col_index)` - Filter rows by logical column
!! - `df_filter_rows_real_range(df, col_index, min_val, max_val)` - Filter rows by real value range
!! - `df_filter_rows_integer_range(df, col_index, min_val, max_val)` - Filter rows by integer value range
!! - `df_filter_rows_string_pattern(df, col_index, pattern)` - Filter rows by string pattern
!!
!! ### Copy and Transform
!! - `df_copy(df)` - Create a deep copy of the data frame
!! - `df_transpose(df)` - Transpose the data frame (converts to character)
!!
!! ### Column Manipulation
!! - `df_rename_column(df, col_index, new_name)` - Rename a column
!! - `df_drop_column(df, col_index)` - Remove a column
!! - `df_reorder_columns(df, new_order)` - Reorder columns
module datafort_manipulation
    use precision
    use types
    use column_class
    use datafort_types
    use datafort_accessors
    implicit none
    private

    ! Public manipulation functions
    public :: df_select_columns
    public :: df_slice_rows
    public :: df_filter_rows_logical
    public :: df_filter_rows_real_range
    public :: df_filter_rows_integer_range
    public :: df_filter_rows_string_pattern
    public :: df_copy
    public :: df_transpose
    public :: df_rename_column
    public :: df_drop_column
    public :: df_reorder_columns

contains

    !========================================================================
    ! SELECTION AND SLICING FUNCTIONS
    !========================================================================

    !> Select specific columns from a data frame
    !!
    !! Creates a new data frame containing only the specified columns
    !!
    !! @param[in] df The source data frame
    !! @param[in] column_indices Array of column indices to select
    !! @return new_df New data frame with selected columns
    function df_select_columns(df, column_indices) result(new_df)
        type(data_frame), intent(in) :: df
        integer, dimension(:), intent(in) :: column_indices
        type(data_frame) :: new_df

        integer :: i, col_idx
        type(column) :: col

        call new_df % new(df % get_max_char_len())

        do i = 1, size(column_indices)
            col_idx = column_indices(i)
            if (col_idx < 1 .or. col_idx > df % ncols()) then
                error stop "Column index out of range in df_select_columns"
            end if

            col = df % get_data_col(col_idx)

            select case (col % get_type())
            case (REAL_NUM)
                if (df % get_with_headers()) then
                    call df_append_real(new_df, col % getr(), df % header(col_idx))
                else
                    call df_append_real(new_df, col % getr())
                end if
            case (INTEGER_NUM)
                if (df % get_with_headers()) then
                    call df_append_integer(new_df, col % geti(), df % header(col_idx))
                else
                    call df_append_integer(new_df, col % geti())
                end if
            case (LOGICAL_NUM)
                if (df % get_with_headers()) then
                    call df_append_logical(new_df, col % getl(), df % header(col_idx))
                else
                    call df_append_logical(new_df, col % getl())
                end if
            case (CHARACTER_NUM)
                if (df % get_with_headers()) then
                    call df_append_character(new_df, col % getch(), df % header(col_idx))
                else
                    call df_append_character(new_df, col % getch())
                end if
            case (COMPLEX_NUM)
                if (df % get_with_headers()) then
                    call df_append_complex(new_df, col % getc(), df % header(col_idx))
                else
                    call df_append_complex(new_df, col % getc())
                end if
            end select
        end do
    end function df_select_columns

    !> Slice rows to create a new data frame
    !!
    !! Creates a new data frame containing only rows in the specified range
    !!
    !! @param[in] df The source data frame
    !! @param[in] start_row First row to include (1-indexed)
    !! @param[in] end_row Last row to include (inclusive)
    !! @return new_df New data frame with sliced rows
    function df_slice_rows(df, start_row, end_row) result(new_df)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: start_row, end_row
        type(data_frame) :: new_df

        integer :: i, j, new_size
        real(rk), allocatable :: real_slice(:)
        integer(ik), allocatable :: int_slice(:)
        logical, allocatable :: logical_slice(:)
        character(len=:), allocatable :: char_slice(:)
        complex(rk), allocatable :: complex_slice(:)
        type(column) :: col

        if (start_row < 1 .or. end_row > df % nrows() .or. start_row > end_row) then
            error stop "Invalid row range in df_slice_rows"
        end if

        new_size = end_row - start_row + 1
        call new_df % new(df % get_max_char_len())

        do i = 1, df % ncols()
            col = df % get_data_col(i)

            select case (col % get_type())
            case (REAL_NUM)
                allocate (real_slice(new_size))
                do j = 1, new_size
                    real_slice(j) = col % getr(start_row + j - 1)
                end do
                if (df % get_with_headers()) then
                    call df_append_real(new_df, real_slice, df % header(i))
                else
                    call df_append_real(new_df, real_slice)
                end if
                deallocate (real_slice)
            case (INTEGER_NUM)
                allocate (int_slice(new_size))
                do j = 1, new_size
                    int_slice(j) = col % geti(start_row + j - 1)
                end do
                if (df % get_with_headers()) then
                    call df_append_integer(new_df, int_slice, df % header(i))
                else
                    call df_append_integer(new_df, int_slice)
                end if
                deallocate (int_slice)
            case (LOGICAL_NUM)
                allocate (logical_slice(new_size))
                do j = 1, new_size
                    logical_slice(j) = col % getl(start_row + j - 1)
                end do
                if (df % get_with_headers()) then
                    call df_append_logical(new_df, logical_slice, df % header(i))
                else
                    call df_append_logical(new_df, logical_slice)
                end if
                deallocate (logical_slice)
            case (CHARACTER_NUM)
                allocate (character(len=len(col % getch(1))) :: char_slice(new_size))
                do j = 1, new_size
                    char_slice(j) = col % getch(start_row + j - 1)
                end do
                if (df % get_with_headers()) then
                    call df_append_character(new_df, char_slice, df % header(i))
                else
                    call df_append_character(new_df, char_slice)
                end if
                deallocate (char_slice)
            case (COMPLEX_NUM)
                allocate (complex_slice(new_size))
                do j = 1, new_size
                    complex_slice(j) = col % getc(start_row + j - 1)
                end do
                if (df % get_with_headers()) then
                    call df_append_complex(new_df, complex_slice, df % header(i))
                else
                    call df_append_complex(new_df, complex_slice)
                end if
                deallocate (complex_slice)
            end select
        end do
    end function df_slice_rows

    !========================================================================
    ! FILTERING FUNCTIONS
    !========================================================================

    !> Filter rows based on a logical column
    !!
    !! Creates a new data frame containing only rows where the specified
    !! logical column is .true.
    !!
    !! @param[in] df The source data frame
    !! @param[in] logical_col_index Index of the logical column to use for filtering
    !! @return new_df New data frame with filtered rows
    function df_filter_rows_logical(df, logical_col_index) result(new_df)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: logical_col_index
        type(data_frame) :: new_df

        logical, dimension(:), allocatable :: mask
        integer, allocatable :: selected_rows(:)
        integer :: i, count_true, idx
        type(column) :: col

        col = df % get_data_col(logical_col_index)

        if (col % get_type() /= LOGICAL_NUM) then
            error stop "Column must be logical type for df_filter_rows_logical"
        end if

        mask = col % getl()
        count_true = count(mask)

        if (count_true == 0) then
            call new_df % new(df % get_max_char_len())
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

        call new_df % new(df % get_max_char_len())

        ! Copy selected rows for each column
        do i = 1, df % ncols()
            call copy_filtered_column(df, new_df, i, selected_rows)
        end do

        deallocate (selected_rows)
    end function df_filter_rows_logical

    !> Filter rows by real value range
    !!
    !! Creates a new data frame containing only rows where the specified
    !! real column value is within [min_val, max_val]
    !!
    !! @param[in] df The source data frame
    !! @param[in] col_index Index of the real column to filter by
    !! @param[in] min_val Minimum value (inclusive)
    !! @param[in] max_val Maximum value (inclusive)
    !! @return filtered_df New data frame with filtered rows
    function df_filter_rows_real_range(df, col_index, min_val, max_val) result(filtered_df)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: col_index
        real(rk), intent(in) :: min_val, max_val
        type(data_frame) :: filtered_df

        integer, dimension(:), allocatable :: selected_rows
        integer :: i, count, idx
        real(rk) :: val
        type(column) :: col

        col = df % get_data_col(col_index)

        if (col % get_type() /= REAL_NUM) error stop "column is not real type"

        ! Count matching rows
        count = 0
        do i = 1, df % nrows()
            val = col % getr(i)
            if (val >= min_val .and. val <= max_val) count = count + 1
        end do

        ! Collect matching row indices
        allocate (selected_rows(count))
        idx = 0
        do i = 1, df % nrows()
            val = col % getr(i)
            if (val >= min_val .and. val <= max_val) then
                idx = idx + 1
                selected_rows(idx) = i
            end if
        end do

        ! Create filtered dataframe
        call filtered_df % new(df % get_max_char_len())
        do i = 1, df % ncols()
            call copy_filtered_column(df, filtered_df, i, selected_rows)
        end do

        deallocate (selected_rows)
    end function df_filter_rows_real_range

    !> Filter rows by integer value range
    !!
    !! Creates a new data frame containing only rows where the specified
    !! integer column value is within [min_val, max_val]
    !!
    !! @param[in] df The source data frame
    !! @param[in] col_index Index of the integer column to filter by
    !! @param[in] min_val Minimum value (inclusive)
    !! @param[in] max_val Maximum value (inclusive)
    !! @return filtered_df New data frame with filtered rows
    function df_filter_rows_integer_range(df, col_index, min_val, max_val) result(filtered_df)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: col_index
        integer(ik), intent(in) :: min_val, max_val
        type(data_frame) :: filtered_df

        integer, dimension(:), allocatable :: selected_rows
        integer :: i, count, idx
        integer(ik) :: val
        type(column) :: col

        col = df % get_data_col(col_index)

        if (col % get_type() /= INTEGER_NUM) error stop "column is not integer type"

        ! Count matching rows
        count = 0
        do i = 1, df % nrows()
            val = col % geti(i)
            if (val >= min_val .and. val <= max_val) count = count + 1
        end do

        ! Collect matching row indices
        allocate (selected_rows(count))
        idx = 0
        do i = 1, df % nrows()
            val = col % geti(i)
            if (val >= min_val .and. val <= max_val) then
                idx = idx + 1
                selected_rows(idx) = i
            end if
        end do

        ! Create filtered dataframe
        call filtered_df % new(df % get_max_char_len())
        do i = 1, df % ncols()
            call copy_filtered_column(df, filtered_df, i, selected_rows)
        end do

        deallocate (selected_rows)
    end function df_filter_rows_integer_range

    !> Filter rows by string pattern
    !!
    !! Creates a new data frame containing only rows where the specified
    !! character column contains the given pattern
    !!
    !! @param[in] df The source data frame
    !! @param[in] col_index Index of the character column to filter by
    !! @param[in] pattern String pattern to search for
    !! @return filtered_df New data frame with filtered rows
    function df_filter_rows_string_pattern(df, col_index, pattern) result(filtered_df)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: col_index
        character(len=*), intent(in) :: pattern
        type(data_frame) :: filtered_df

        integer, dimension(:), allocatable :: selected_rows
        integer :: i, count, idx
        character(len=:), allocatable :: val
        type(column) :: col

        col = df % get_data_col(col_index)

        if (col % get_type() /= CHARACTER_NUM) error stop "column is not character type"

        ! Count matching rows
        count = 0
        do i = 1, df % nrows()
            val = col % getch(i)
            if (index(val, pattern) > 0) count = count + 1
        end do

        ! Collect matching row indices
        allocate (selected_rows(count))
        idx = 0
        do i = 1, df % nrows()
            val = col % getch(i)
            if (index(val, pattern) > 0) then
                idx = idx + 1
                selected_rows(idx) = i
            end if
        end do

        ! Create filtered dataframe
        call filtered_df % new(df % get_max_char_len())
        do i = 1, df % ncols()
            call copy_filtered_column(df, filtered_df, i, selected_rows)
        end do

        deallocate (selected_rows)
    end function df_filter_rows_string_pattern

    !========================================================================
    ! COPY AND TRANSFORM FUNCTIONS
    !========================================================================

    !> Create a deep copy of a data frame
    !!
    !! Creates a new data frame with all columns and data copied from the source
    !!
    !! @param[in] df The source data frame
    !! @return new_df New data frame that is a copy of the source
    function df_copy(df) result(new_df)
        type(data_frame), intent(in) :: df
        type(data_frame) :: new_df

        integer :: i
        type(column) :: col

        call new_df % new(df % get_max_char_len())

        do i = 1, df % ncols()
            col = df % get_data_col(i)

            select case (col % get_type())
            case (REAL_NUM)
                if (df % get_with_headers()) then
                    call df_append_real(new_df, col % getr(), df % header(i))
                else
                    call df_append_real(new_df, col % getr())
                end if
            case (INTEGER_NUM)
                if (df % get_with_headers()) then
                    call df_append_integer(new_df, col % geti(), df % header(i))
                else
                    call df_append_integer(new_df, col % geti())
                end if
            case (LOGICAL_NUM)
                if (df % get_with_headers()) then
                    call df_append_logical(new_df, col % getl(), df % header(i))
                else
                    call df_append_logical(new_df, col % getl())
                end if
            case (CHARACTER_NUM)
                if (df % get_with_headers()) then
                    call df_append_character(new_df, col % getch(), df % header(i))
                else
                    call df_append_character(new_df, col % getch())
                end if
            case (COMPLEX_NUM)
                if (df % get_with_headers()) then
                    call df_append_complex(new_df, col % getc(), df % header(i))
                else
                    call df_append_complex(new_df, col % getc())
                end if
            end select
        end do
    end function df_copy

    !> Transpose a data frame
    !!
    !! Creates a transposed data frame where rows become columns and columns become rows.
    !! Note: All data is converted to character type in the transposed frame.
    !!
    !! @param[in] df The source data frame
    !! @return transposed_df New data frame that is the transpose of the source
    function df_transpose(df) result(transposed_df)
        type(data_frame), intent(in) :: df
        type(data_frame) :: transposed_df

        integer :: i, j
        character(len=50), dimension(:), allocatable :: row_data
        character(len=20) :: temp_str
        type(column) :: col

        call transposed_df % new()

        ! Add headers as first column if present
        if (df % get_with_headers()) then
            allocate (character(len=df % get_max_char_len()) :: row_data(df % ncols()))
            do i = 1, df % ncols()
                row_data(i) = df % header(i)
            end do
            call df_append_character(transposed_df, row_data, "Headers")
            deallocate (row_data)
        end if

        ! Add each row as a column
        do i = 1, df % nrows()
            allocate (row_data(df % ncols()))

            do j = 1, df % ncols()
                col = df % get_data_col(j)

                select case (col % get_type())
                case (REAL_NUM)
                    write (temp_str, '(f0.6)') col % getr(i)
                    row_data(j) = trim(temp_str)
                case (INTEGER_NUM)
                    write (temp_str, '(i0)') col % geti(i)
                    row_data(j) = trim(temp_str)
                case (LOGICAL_NUM)
                    if (col % getl(i)) then
                        row_data(j) = "T"
                    else
                        row_data(j) = "F"
                    end if
                case (CHARACTER_NUM)
                    row_data(j) = col % getch(i)
                case (COMPLEX_NUM)
                    write (temp_str, '("(",f0.3,",",f0.3,")")') col % getc(i)
                    row_data(j) = trim(temp_str)
                end select
            end do

            write (temp_str, '("Row_",i0)') i
            call df_append_character(transposed_df, row_data, trim(temp_str))
            deallocate (row_data)
        end do
    end function df_transpose

    !========================================================================
    ! COLUMN MANIPULATION FUNCTIONS
    !========================================================================

    !> Rename a column in the data frame
    !!
    !! Changes the header name of a specified column
    !!
    !! @param[in,out] df The data frame to modify
    !! @param[in] col_index Index of the column to rename
    !! @param[in] new_name New name for the column
    subroutine df_rename_column(df, col_index, new_name)
        type(data_frame), intent(inout) :: df
        integer, intent(in) :: col_index
        character(len=*), intent(in) :: new_name

        if (col_index < 1 .or. col_index > df % ncols()) error stop "column index out of range"
        if (.not. df % get_with_headers()) error stop "data frame has no headers to rename"

        call df % set_header_at_index(col_index, new_name)
    end subroutine df_rename_column

    !> Drop a column from the data frame
    !!
    !! Removes the specified column and all its data from the data frame
    !!
    !! @param[in,out] df The data frame to modify
    !! @param[in] col_index Index of the column to drop
    subroutine df_drop_column(df, col_index)
        type(data_frame), intent(inout) :: df
        integer, intent(in) :: col_index

        integer :: i
        type(data_frame) :: temp_df

        if (col_index < 1 .or. col_index > df % ncols()) error stop "column index out of range"

        ! Create a new data frame with all columns except the one to drop
        call temp_df % new(df % get_max_char_len())

        do i = 1, df % ncols()
            if (i /= col_index) then
                select case (df % get_data_col(i) % get_type())
                case (REAL_NUM)
                    if (df % get_with_headers()) then
                        call df_append_real(temp_df, df % get_data_col(i) % getr(), df % header(i))
                    else
                        call df_append_real(temp_df, df % get_data_col(i) % getr())
                    end if
                case (INTEGER_NUM)
                    if (df % get_with_headers()) then
                        call df_append_integer(temp_df, df % get_data_col(i) % geti(), df % header(i))
                    else
                        call df_append_integer(temp_df, df % get_data_col(i) % geti())
                    end if
                case (LOGICAL_NUM)
                    if (df % get_with_headers()) then
                        call df_append_logical(temp_df, df % get_data_col(i) % getl(), df % header(i))
                    else
                        call df_append_logical(temp_df, df % get_data_col(i) % getl())
                    end if
                case (CHARACTER_NUM)
                    if (df % get_with_headers()) then
                        call df_append_character(temp_df, df % get_data_col(i) % getch(), df % header(i))
                    else
                        call df_append_character(temp_df, df % get_data_col(i) % getch())
                    end if
                case (COMPLEX_NUM)
                    if (df % get_with_headers()) then
                        call df_append_complex(temp_df, df % get_data_col(i) % getc(), df % header(i))
                    else
                        call df_append_complex(temp_df, df % get_data_col(i) % getc())
                    end if
                end select
            end if
        end do

        ! Destroy original and move temp_df data to df
        call df % destroy()
        df = temp_df
    end subroutine df_drop_column

    !> Reorder columns in the data frame
    !!
    !! Rearranges columns according to the specified order
    !!
    !! @param[in,out] df The data frame to modify
    !! @param[in] new_order Array specifying the new column order (permutation of 1:ncols)
    subroutine df_reorder_columns(df, new_order)
        type(data_frame), intent(inout) :: df
        integer, dimension(:), intent(in) :: new_order

        integer :: i
        type(data_frame) :: temp_df

        if (size(new_order) /= df % ncols()) error stop "new_order size must equal number of columns"

        ! Validate indices
        do i = 1, size(new_order)
            if (new_order(i) < 1 .or. new_order(i) > df % ncols()) error stop "invalid column index in new_order"
        end do

        ! Create new data frame with reordered columns
        call temp_df % new(df % get_max_char_len())

        do i = 1, df % ncols()
            select case (df % get_data_col(new_order(i)) % get_type())
            case (REAL_NUM)
                if (df % get_with_headers()) then
                    call df_append_real(temp_df, df % get_data_col(new_order(i)) % getr(), df % header(new_order(i)))
                else
                    call df_append_real(temp_df, df % get_data_col(new_order(i)) % getr())
                end if
            case (INTEGER_NUM)
                if (df % get_with_headers()) then
                    call df_append_integer(temp_df, df % get_data_col(new_order(i)) % geti(), df % header(new_order(i)))
                else
                    call df_append_integer(temp_df, df % get_data_col(new_order(i)) % geti())
                end if
            case (LOGICAL_NUM)
                if (df % get_with_headers()) then
                    call df_append_logical(temp_df, df % get_data_col(new_order(i)) % getl(), df % header(new_order(i)))
                else
                    call df_append_logical(temp_df, df % get_data_col(new_order(i)) % getl())
                end if
            case (CHARACTER_NUM)
                if (df % get_with_headers()) then
                    call df_append_character(temp_df, df % get_data_col(new_order(i)) % getch(), df % header(new_order(i)))
                else
                    call df_append_character(temp_df, df % get_data_col(new_order(i)) % getch())
                end if
            case (COMPLEX_NUM)
                if (df % get_with_headers()) then
                    call df_append_complex(temp_df, df % get_data_col(new_order(i)) % getc(), df % header(new_order(i)))
                else
                    call df_append_complex(temp_df, df % get_data_col(new_order(i)) % getc())
                end if
            end select
        end do

        ! Destroy original and move temp_df data to df
        call df % destroy()
        df = temp_df
    end subroutine df_reorder_columns

    !========================================================================
    ! HELPER SUBROUTINES (NOT PREFIXED WITH df_)
    !========================================================================

    !> Helper subroutine to copy filtered rows for a single column
    !!
    !! Internal helper used by filtering functions to copy selected rows
    !! from source data frame to target data frame for a specific column
    !!
    !! @param[in] source_df Source data frame to copy from
    !! @param[in,out] target_df Target data frame to copy to
    !! @param[in] col_index Index of the column to copy
    !! @param[in] selected_rows Array of row indices to copy
    subroutine copy_filtered_column(source_df, target_df, col_index, selected_rows)
        type(data_frame), intent(in) :: source_df
        type(data_frame), intent(inout) :: target_df
        integer, intent(in) :: col_index
        integer, dimension(:), intent(in) :: selected_rows

        integer :: i, n_selected
        real(rk), allocatable :: real_filtered(:)
        integer(ik), allocatable :: int_filtered(:)
        logical, allocatable :: logical_filtered(:)
        character(len=:), allocatable :: char_filtered(:)
        complex(rk), allocatable :: complex_filtered(:)
        type(column) :: col

        n_selected = size(selected_rows)
        col = source_df % get_data_col(col_index)

        select case (col % get_type())
        case (REAL_NUM)
            allocate (real_filtered(n_selected))
            do i = 1, n_selected
                real_filtered(i) = col % getr(selected_rows(i))
            end do
            if (source_df % get_with_headers()) then
                call df_append_real(target_df, real_filtered, source_df % header(col_index))
            else
                call df_append_real(target_df, real_filtered)
            end if
            deallocate (real_filtered)
        case (INTEGER_NUM)
            allocate (int_filtered(n_selected))
            do i = 1, n_selected
                int_filtered(i) = col % geti(selected_rows(i))
            end do
            if (source_df % get_with_headers()) then
                call df_append_integer(target_df, int_filtered, source_df % header(col_index))
            else
                call df_append_integer(target_df, int_filtered)
            end if
            deallocate (int_filtered)
        case (LOGICAL_NUM)
            allocate (logical_filtered(n_selected))
            do i = 1, n_selected
                logical_filtered(i) = col % getl(selected_rows(i))
            end do
            if (source_df % get_with_headers()) then
                call df_append_logical(target_df, logical_filtered, source_df % header(col_index))
            else
                call df_append_logical(target_df, logical_filtered)
            end if
            deallocate (logical_filtered)
        case (CHARACTER_NUM)
            allocate (character(len=len(col % getch(1))) :: char_filtered(n_selected))
            do i = 1, n_selected
                char_filtered(i) = col % getch(selected_rows(i))
            end do
            if (source_df % get_with_headers()) then
                call df_append_character(target_df, char_filtered, source_df % header(col_index))
            else
                call df_append_character(target_df, char_filtered)
            end if
            deallocate (char_filtered)
        case (COMPLEX_NUM)
            allocate (complex_filtered(n_selected))
            do i = 1, n_selected
                complex_filtered(i) = col % getc(selected_rows(i))
            end do
            if (source_df % get_with_headers()) then
                call df_append_complex(target_df, complex_filtered, source_df % header(col_index))
            else
                call df_append_complex(target_df, complex_filtered)
            end if
            deallocate (complex_filtered)
        end select
    end subroutine copy_filtered_column

end module datafort_manipulation
