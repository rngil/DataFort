!> DataFort Advanced Operations Module
!!
!! This module provides standalone advanced data operations for data frames.
!! All functions take a data_frame as the first argument instead of being type-bound procedures.
!!
!! ## Functions
!!
!! ### Unique Value Functions
!! - `df_unique_real(df, col_index)` - Get unique values from real column
!! - `df_unique_integer(df, col_index)` - Get unique values from integer column
!! - `df_unique_character(df, col_index)` - Get unique values from character column
!!
!! ### Value Counting Functions
!! - `df_value_counts_real(df, col_index)` - Count occurrences of each value (real)
!! - `df_value_counts_integer(df, col_index)` - Count occurrences of each value (integer)
!! - `df_value_counts_character(df, col_index)` - Count occurrences of each value (character)
!!
!! ### Concatenation Functions
!! - `df_concat(df1, df2, axis)` - Concatenate two data frames (axis=0: vertical, axis=1: horizontal)
!!
!! ### Duplicate Detection and Removal Functions
!! - `df_duplicated(df)` - Check which rows are duplicates
!! - `df_drop_duplicates(df)` - Remove duplicate rows
!! - `df_drop_duplicates_subset(df, col_indices)` - Remove duplicates based on specific columns
!!
!! ## Notes
!! - Unique functions return sorted unique values (except character which is unsorted)
!! - Value counts return a data frame with "Value" and "Count" columns
!! - Concat with axis=0 requires same number of columns; axis=1 requires same number of rows
!! - Duplicate detection compares all data types including NaN values
module datafort_advanced
    use precision
    use types
    use column_class
    use datafort_types
    use datafort_accessors
    use datafort_manipulation, only: df_copy
    use utilities
    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
    implicit none
    private

    ! Public unique value functions
    public :: df_unique_real
    public :: df_unique_integer
    public :: df_unique_character

    ! Public value counting functions
    public :: df_value_counts_real
    public :: df_value_counts_integer
    public :: df_value_counts_character

    ! Public concatenation functions
    public :: df_concat

    ! Public duplicate detection and removal functions
    public :: df_duplicated
    public :: df_drop_duplicates
    public :: df_drop_duplicates_subset

contains

    !========================================================================
    ! UNIQUE VALUE FUNCTIONS
    !========================================================================

    !> Get unique values from a real column
    !!
    !! Returns sorted array of unique values from the specified real column
    !!
    !! @param[in] df The data frame instance
    !! @param[in] col_index Column index (1-based)
    !! @return Array of unique values (sorted)
    function df_unique_real(df, col_index) result(unique_vals)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: col_index
        real(rk), dimension(:), allocatable :: unique_vals

        real(rk), dimension(:), allocatable :: col, temp_unique
        integer :: i, j, n_unique
        logical :: is_unique

        col = df_get_col_real(df, col_index)
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
    end function df_unique_real

    !> Get unique values from an integer column
    !!
    !! Returns sorted array of unique values from the specified integer column
    !!
    !! @param[in] df The data frame instance
    !! @param[in] col_index Column index (1-based)
    !! @return Array of unique values (sorted)
    function df_unique_integer(df, col_index) result(unique_vals)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: col_index
        integer(ik), dimension(:), allocatable :: unique_vals

        integer(ik), dimension(:), allocatable :: col, temp_unique
        integer :: i, j, n_unique
        logical :: is_unique

        col = df_get_col_integer(df, col_index)
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
    end function df_unique_integer

    !> Get unique values from a character column
    !!
    !! Returns array of unique values from the specified character column
    !!
    !! @param[in] df The data frame instance
    !! @param[in] col_index Column index (1-based)
    !! @return Array of unique values (unsorted)
    function df_unique_character(df, col_index) result(unique_vals)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: col_index
        character(len=:), allocatable :: unique_vals(:)

        character(len=:), allocatable :: col(:), temp_unique(:)
        integer :: i, j, n_unique
        logical :: is_unique

        col = df_get_col_character(df, col_index)
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
    end function df_unique_character

    !========================================================================
    ! VALUE COUNTING FUNCTIONS
    !========================================================================

    !> Count occurrences of each value in a real column
    !!
    !! Returns a data frame with two columns: unique values and their counts
    !!
    !! @param[in] df The data frame instance
    !! @param[in] col_index Column index (1-based)
    !! @return Data frame with columns "Value" and "Count"
    function df_value_counts_real(df, col_index) result(counts_df)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: col_index
        type(data_frame) :: counts_df

        real(rk), dimension(:), allocatable :: col, unique_vals
        integer(ik), dimension(:), allocatable :: counts
        integer :: i, j

        unique_vals = df_unique_real(df, col_index)
        allocate (counts(size(unique_vals)))
        counts = 0

        col = df_get_col_real(df, col_index)
        do i = 1, size(col)
            do j = 1, size(unique_vals)
                if (abs(col(i) - unique_vals(j)) < 1.0e-10_rk) then
                    counts(j) = counts(j) + 1
                    exit
                end if
            end do
        end do

        call counts_df % new()
        call df_append_real(counts_df, unique_vals, "Value")
        call df_append_integer(counts_df, counts, "Count")
    end function df_value_counts_real

    !> Count occurrences of each value in an integer column
    !!
    !! Returns a data frame with two columns: unique values and their counts
    !!
    !! @param[in] df The data frame instance
    !! @param[in] col_index Column index (1-based)
    !! @return Data frame with columns "Value" and "Count"
    function df_value_counts_integer(df, col_index) result(counts_df)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: col_index
        type(data_frame) :: counts_df

        integer(ik), dimension(:), allocatable :: col, unique_vals, counts
        integer :: i, j

        unique_vals = df_unique_integer(df, col_index)
        allocate (counts(size(unique_vals)))
        counts = 0

        col = df_get_col_integer(df, col_index)
        do i = 1, size(col)
            do j = 1, size(unique_vals)
                if (col(i) == unique_vals(j)) then
                    counts(j) = counts(j) + 1
                    exit
                end if
            end do
        end do

        call counts_df % new()
        call df_append_integer(counts_df, unique_vals, "Value")
        call df_append_integer(counts_df, counts, "Count")
    end function df_value_counts_integer

    !> Count occurrences of each value in a character column
    !!
    !! Returns a data frame with two columns: unique values and their counts
    !!
    !! @param[in] df The data frame instance
    !! @param[in] col_index Column index (1-based)
    !! @return Data frame with columns "Value" and "Count"
    function df_value_counts_character(df, col_index) result(counts_df)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: col_index
        type(data_frame) :: counts_df

        character(len=:), allocatable :: col(:), unique_vals(:)
        integer(ik), dimension(:), allocatable :: counts
        integer :: i, j

        unique_vals = df_unique_character(df, col_index)
        allocate (counts(size(unique_vals)))
        counts = 0

        col = df_get_col_character(df, col_index)
        do i = 1, size(col)
            do j = 1, size(unique_vals)
                if (trim(col(i)) == trim(unique_vals(j))) then
                    counts(j) = counts(j) + 1
                    exit
                end if
            end do
        end do

        call counts_df % new()
        call df_append_character(counts_df, unique_vals, "Value")
        call df_append_integer(counts_df, counts, "Count")
    end function df_value_counts_character

    !========================================================================
    ! CONCATENATION FUNCTIONS
    !========================================================================

    !> Concatenate two data frames vertically (rows) or horizontally (columns)
    !!
    !! @param[in] df1 First data frame
    !! @param[in] df2 Second data frame
    !! @param[in] axis 0 for vertical (stack rows), 1 for horizontal (add columns)
    !! @return Concatenated data frame
    !!
    !! @note For axis=0 (vertical), both data frames must have same number of columns
    !! @note For axis=1 (horizontal), both data frames must have same number of rows
    function df_concat(df1, df2, axis) result(result_df)
        type(data_frame), intent(in) :: df1, df2
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
            if (df1 % ncols() /= df2 % ncols()) then
                print *, "Error: DataFrames must have same number of columns"
                call result_df % new()
                return
            end if

            call result_df % new(max(df1 % get_max_char_len(), df2 % get_max_char_len()))

            do i = 1, df1 % ncols()
                dtype = df1 % dtype(i)

                select case (dtype)
                case (REAL_NUM)
                    real_col1 = df_get_col_real(df1, i)
                    real_col2 = df_get_col_real(df2, i)
                    allocate (real_combined(size(real_col1) + size(real_col2)))
                    real_combined(1:size(real_col1)) = real_col1
                    real_combined(size(real_col1) + 1:) = real_col2
                    if (df1 % get_with_headers()) then
                        header_name = df1 % header(i)
                        call df_append_real(result_df, real_combined, trim(header_name))
                    else
                        call df_append_real(result_df, real_combined)
                    end if
                    deallocate (real_col1, real_col2, real_combined)

                case (INTEGER_NUM)
                    int_col1 = df_get_col_integer(df1, i)
                    int_col2 = df_get_col_integer(df2, i)
                    allocate (int_combined(size(int_col1) + size(int_col2)))
                    int_combined(1:size(int_col1)) = int_col1
                    int_combined(size(int_col1) + 1:) = int_col2
                    if (df1 % get_with_headers()) then
                        header_name = df1 % header(i)
                        call df_append_integer(result_df, int_combined, trim(header_name))
                    else
                        call df_append_integer(result_df, int_combined)
                    end if
                    deallocate (int_col1, int_col2, int_combined)

                case (LOGICAL_NUM)
                    log_col1 = df_get_col_logical(df1, i)
                    log_col2 = df_get_col_logical(df2, i)
                    allocate (log_combined(size(log_col1) + size(log_col2)))
                    log_combined(1:size(log_col1)) = log_col1
                    log_combined(size(log_col1) + 1:) = log_col2
                    if (df1 % get_with_headers()) then
                        header_name = df1 % header(i)
                        call df_append_logical(result_df, log_combined, trim(header_name))
                    else
                        call df_append_logical(result_df, log_combined)
                    end if
                    deallocate (log_col1, log_col2, log_combined)

                case (CHARACTER_NUM)
                    char_col1 = df_get_col_character(df1, i)
                    char_col2 = df_get_col_character(df2, i)
       allocate (character(len=max(len(char_col1), len(char_col2))) :: char_combined(size(char_col1) + size(char_col2)))
                    char_combined(1:size(char_col1)) = char_col1
                    char_combined(size(char_col1) + 1:) = char_col2
                    if (df1 % get_with_headers()) then
                        header_name = df1 % header(i)
                        call df_append_character(result_df, char_combined, trim(header_name))
                    else
                        call df_append_character(result_df, char_combined)
                    end if
                    deallocate (char_col1, char_col2, char_combined)

                case (COMPLEX_NUM)
                    cmplx_col1 = df_get_col_complex(df1, i)
                    cmplx_col2 = df_get_col_complex(df2, i)
                    allocate (cmplx_combined(size(cmplx_col1) + size(cmplx_col2)))
                    cmplx_combined(1:size(cmplx_col1)) = cmplx_col1
                    cmplx_combined(size(cmplx_col1) + 1:) = cmplx_col2
                    if (df1 % get_with_headers()) then
                        header_name = df1 % header(i)
                        call df_append_complex(result_df, cmplx_combined, trim(header_name))
                    else
                        call df_append_complex(result_df, cmplx_combined)
                    end if
                    deallocate (cmplx_col1, cmplx_col2, cmplx_combined)
                end select
            end do

        else if (axis == 1) then
            ! Horizontal concatenation (add columns)
            if (df1 % nrows() /= df2 % nrows()) then
                print *, "Error: DataFrames must have same number of rows"
                call result_df % new()
                return
            end if

            result_df = df_copy(df1)

            ! Add columns from df2
            do i = 1, df2 % ncols()
                dtype = df2 % dtype(i)

                select case (dtype)
                case (REAL_NUM)
                    real_col = df_get_col_real(df2, i)
                    if (df2 % get_with_headers()) then
                        header_name = df2 % header(i)
                        call df_append_real(result_df, real_col, trim(header_name))
                    else
                        call df_append_real(result_df, real_col)
                    end if
                    deallocate (real_col)

                case (INTEGER_NUM)
                    int_col = df_get_col_integer(df2, i)
                    if (df2 % get_with_headers()) then
                        header_name = df2 % header(i)
                        call df_append_integer(result_df, int_col, trim(header_name))
                    else
                        call df_append_integer(result_df, int_col)
                    end if
                    deallocate (int_col)

                case (LOGICAL_NUM)
                    log_col = df_get_col_logical(df2, i)
                    if (df2 % get_with_headers()) then
                        header_name = df2 % header(i)
                        call df_append_logical(result_df, log_col, trim(header_name))
                    else
                        call df_append_logical(result_df, log_col)
                    end if
                    deallocate (log_col)

                case (CHARACTER_NUM)
                    char_col = df_get_col_character(df2, i)
                    if (df2 % get_with_headers()) then
                        header_name = df2 % header(i)
                        call df_append_character(result_df, char_col, trim(header_name))
                    else
                        call df_append_character(result_df, char_col)
                    end if
                    deallocate (char_col)

                case (COMPLEX_NUM)
                    cmplx_col = df_get_col_complex(df2, i)
                    if (df2 % get_with_headers()) then
                        header_name = df2 % header(i)
                        call df_append_complex(result_df, cmplx_col, trim(header_name))
                    else
                        call df_append_complex(result_df, cmplx_col)
                    end if
                    deallocate (cmplx_col)
                end select
            end do
        else
            print *, "Error: axis must be 0 (vertical) or 1 (horizontal)"
            call result_df % new()
        end if
    end function df_concat

    !========================================================================
    ! DUPLICATE DETECTION AND REMOVAL FUNCTIONS
    !========================================================================

    !> Check which rows are duplicates
    !!
    !! Returns a logical array where .true. indicates the row is a duplicate
    !! of a previous row. The first occurrence is not marked as duplicate.
    !!
    !! @param[in] df The data frame instance
    !! @return Logical array indicating duplicate rows
    function df_duplicated(df) result(is_dup)
        type(data_frame), intent(in) :: df
        logical, dimension(:), allocatable :: is_dup

        integer :: i, j, k, dtype
        logical :: rows_match

        allocate (is_dup(df % nrows()))
        is_dup = .false.

        do i = 2, df % nrows()
            do j = 1, i - 1
                rows_match = .true.

                ! Compare all columns
                do k = 1, df % ncols()
                    dtype = df % dtype(k)

                    select case (dtype)
                    case (REAL_NUM)
                        if (abs(df_get_val_real(df, i, k) - df_get_val_real(df, j, k)) >= 1.0e-10_rk) then
                            rows_match = .false.
                            exit
                        end if
                    case (INTEGER_NUM)
                        if (df_get_val_integer(df, i, k) /= df_get_val_integer(df, j, k)) then
                            rows_match = .false.
                            exit
                        end if
                    case (LOGICAL_NUM)
                        if (df_get_val_logical(df, i, k) .neqv. df_get_val_logical(df, j, k)) then
                            rows_match = .false.
                            exit
                        end if
                    case (CHARACTER_NUM)
                        if (trim(df_get_val_character(df, i, k)) /= trim(df_get_val_character(df, j, k))) then
                            rows_match = .false.
                            exit
                        end if
                    case (COMPLEX_NUM)
                        if (df_get_val_complex(df, i, k) /= df_get_val_complex(df, j, k)) then
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
    end function df_duplicated

    !> Remove duplicate rows
    !!
    !! Returns new data frame with duplicate rows removed (keeps first occurrence)
    !!
    !! @param[in] df The data frame instance
    !! @return Data frame with unique rows only
    function df_drop_duplicates(df) result(unique_df)
        type(data_frame), intent(in) :: df
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

        is_dup = df_duplicated(df)
        allocate (keep_mask(df % nrows()))
        keep_mask = .not. is_dup

        num_unique = count(keep_mask)

        if (num_unique == 0) then
            call unique_df % new()
            return
        end if

        ! Build index array
        allocate (unique_indices(num_unique))
        j = 0
        do i = 1, df % nrows()
            if (keep_mask(i)) then
                j = j + 1
                unique_indices(j) = i
            end if
        end do

        ! Build result data frame
        call unique_df % new(df % get_max_char_len())

        do i = 1, df % ncols()
            dtype = df % dtype(i)

            select case (dtype)
            case (REAL_NUM)
                real_col = df_get_col_real(df, i)
                allocate (real_unique_col(num_unique))
                do j = 1, num_unique
                    real_unique_col(j) = real_col(unique_indices(j))
                end do
                if (df % get_with_headers()) then
                    header_name = df % header(i)
                    call df_append_real(unique_df, real_unique_col, trim(header_name))
                else
                    call df_append_real(unique_df, real_unique_col)
                end if
                deallocate (real_unique_col)

            case (INTEGER_NUM)
                int_col = df_get_col_integer(df, i)
                allocate (int_unique_col(num_unique))
                do j = 1, num_unique
                    int_unique_col(j) = int_col(unique_indices(j))
                end do
                if (df % get_with_headers()) then
                    header_name = df % header(i)
                    call df_append_integer(unique_df, int_unique_col, trim(header_name))
                else
                    call df_append_integer(unique_df, int_unique_col)
                end if
                deallocate (int_unique_col)

            case (LOGICAL_NUM)
                log_col = df_get_col_logical(df, i)
                allocate (log_unique_col(num_unique))
                do j = 1, num_unique
                    log_unique_col(j) = log_col(unique_indices(j))
                end do
                if (df % get_with_headers()) then
                    header_name = df % header(i)
                    call df_append_logical(unique_df, log_unique_col, trim(header_name))
                else
                    call df_append_logical(unique_df, log_unique_col)
                end if
                deallocate (log_unique_col)

            case (CHARACTER_NUM)
                char_col = df_get_col_character(df, i)
                allocate (character(len=len(char_col)) :: char_unique_col(num_unique))
                do j = 1, num_unique
                    char_unique_col(j) = char_col(unique_indices(j))
                end do
                if (df % get_with_headers()) then
                    header_name = df % header(i)
                    call df_append_character(unique_df, char_unique_col, trim(header_name))
                else
                    call df_append_character(unique_df, char_unique_col)
                end if
                deallocate (char_unique_col)

            case (COMPLEX_NUM)
                cmplx_col = df_get_col_complex(df, i)
                allocate (cmplx_unique_col(num_unique))
                do j = 1, num_unique
                    cmplx_unique_col(j) = cmplx_col(unique_indices(j))
                end do
                if (df % get_with_headers()) then
                    header_name = df % header(i)
                    call df_append_complex(unique_df, cmplx_unique_col, trim(header_name))
                else
                    call df_append_complex(unique_df, cmplx_unique_col)
                end if
                deallocate (cmplx_unique_col)
            end select
        end do

        deallocate (is_dup, keep_mask, unique_indices)
    end function df_drop_duplicates

    !> Drop duplicate rows based on specific columns (subset)
    !!
    !! Returns new data frame with duplicate rows removed, where duplicates are
    !! determined by comparing only the specified columns. Keeps first occurrence.
    !!
    !! @param[in] df The data frame instance
    !! @param[in] col_indices Array of column indices to check for duplicates
    !! @return New data frame with duplicate rows removed
    function df_drop_duplicates_subset(df, col_indices) result(unique_df)
        type(data_frame), intent(in) :: df
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

        allocate (is_dup(df % nrows()))
        is_dup = .false.

        ! Check each row against all previous rows (only comparing subset columns)
        do row1 = 1, df % nrows()
            if (is_dup(row1)) cycle

            do row2 = row1 + 1, df % nrows()
                if (is_dup(row2)) cycle

                is_duplicate = .true.
                ! Only check columns in subset
                do k = 1, size(col_indices)
                    col_idx = col_indices(k)
                    dtype = df % dtype(col_idx)

                    select case (dtype)
                    case (REAL_NUM)
                        real_col = df_get_col_real(df, col_idx)
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
                        int_col = df_get_col_integer(df, col_idx)
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
                        log_col = df_get_col_logical(df, col_idx)
                        if (log_col(row1) .neqv. log_col(row2)) then
                            is_duplicate = .false.
                            exit
                        end if
                    case (CHARACTER_NUM)
                        char_col = df_get_col_character(df, col_idx)
                        if (char_col(row1) /= char_col(row2)) then
                            is_duplicate = .false.
                            exit
                        end if
                    case (COMPLEX_NUM)
                        cmplx_col = df_get_col_complex(df, col_idx)
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
        allocate (keep_mask(df % nrows()))
        keep_mask = .not. is_dup
        num_unique = count(keep_mask)

        ! Get indices of unique rows
        allocate (unique_indices(num_unique))
        j = 0
        do i = 1, df % nrows()
            if (keep_mask(i)) then
                j = j + 1
                unique_indices(j) = i
            end if
        end do

        ! Build result data frame with all columns
        call unique_df % new(df % get_max_char_len())

        do i = 1, df % ncols()
            dtype = df % dtype(i)

            select case (dtype)
            case (REAL_NUM)
                real_col = df_get_col_real(df, i)
                allocate (real_unique_col(num_unique))
                do j = 1, num_unique
                    real_unique_col(j) = real_col(unique_indices(j))
                end do
                if (df % get_with_headers()) then
                    header_name = df % header(i)
                    call df_append_real(unique_df, real_unique_col, trim(header_name))
                else
                    call df_append_real(unique_df, real_unique_col)
                end if
                deallocate (real_unique_col)

            case (INTEGER_NUM)
                int_col = df_get_col_integer(df, i)
                allocate (int_unique_col(num_unique))
                do j = 1, num_unique
                    int_unique_col(j) = int_col(unique_indices(j))
                end do
                if (df % get_with_headers()) then
                    header_name = df % header(i)
                    call df_append_integer(unique_df, int_unique_col, trim(header_name))
                else
                    call df_append_integer(unique_df, int_unique_col)
                end if
                deallocate (int_unique_col)

            case (LOGICAL_NUM)
                log_col = df_get_col_logical(df, i)
                allocate (log_unique_col(num_unique))
                do j = 1, num_unique
                    log_unique_col(j) = log_col(unique_indices(j))
                end do
                if (df % get_with_headers()) then
                    header_name = df % header(i)
                    call df_append_logical(unique_df, log_unique_col, trim(header_name))
                else
                    call df_append_logical(unique_df, log_unique_col)
                end if
                deallocate (log_unique_col)

            case (CHARACTER_NUM)
                char_col = df_get_col_character(df, i)
                allocate (character(len=len(char_col)) :: char_unique_col(num_unique))
                do j = 1, num_unique
                    char_unique_col(j) = char_col(unique_indices(j))
                end do
                if (df % get_with_headers()) then
                    header_name = df % header(i)
                    call df_append_character(unique_df, char_unique_col, trim(header_name))
                else
                    call df_append_character(unique_df, char_unique_col)
                end if
                deallocate (char_unique_col)

            case (COMPLEX_NUM)
                cmplx_col = df_get_col_complex(df, i)
                allocate (cmplx_unique_col(num_unique))
                do j = 1, num_unique
                    cmplx_unique_col(j) = cmplx_col(unique_indices(j))
                end do
                if (df % get_with_headers()) then
                    header_name = df % header(i)
                    call df_append_complex(unique_df, cmplx_unique_col, trim(header_name))
                else
                    call df_append_complex(unique_df, cmplx_unique_col)
                end if
                deallocate (cmplx_unique_col)
            end select
        end do

        deallocate (is_dup, keep_mask, unique_indices)
    end function df_drop_duplicates_subset

end module datafort_advanced
