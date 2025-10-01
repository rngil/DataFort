!> DataFort Joins Module
!!
!! This module provides standalone join and merge functions for data frames.
!! All functions take a data_frame as the first argument instead of being type-bound procedures.
!!
!! ## Functions
!!
!! ### Join Functions
!! - `df_inner_join(df1, df2, key_col1, key_col2)` - SQL-style inner join
!! - `df_left_join(df1, df2, key_col1, key_col2)` - SQL-style left join
!! - `df_right_join(df1, df2, key_col1, key_col2)` - SQL-style right join
!! - `df_outer_join(df1, df2, key_col1, key_col2)` - SQL-style outer join (full outer join)
!!
!! ### Merge Functions
!! - `df_merge(df1, df2, on_column, how)` - Merge using column names
!!
!! ## Notes
!! - All join functions return a new data frame
!! - Key columns must have the same data type (integer, real, or character)
!! - Right table columns are suffixed with "_right" to avoid name conflicts
!! - For merge, the 'how' parameter can be: "inner", "left", "right", or "outer"
module datafort_joins
    use precision
    use types
    use column_class
    use datafort_types
    use datafort_accessors
    implicit none
    private

    ! Public join functions
    public :: df_inner_join
    public :: df_left_join
    public :: df_right_join
    public :: df_outer_join
    public :: df_merge

    ! Private helper procedures
    private :: build_joined_dataframe

contains

    !========================================================================
    ! JOIN OPERATIONS
    !========================================================================

    !> Perform an inner join between two data frames
    !!
    !! Returns a new data frame containing only rows where the key columns match
    !! in both data frames. Similar to SQL INNER JOIN.
    !!
    !! @param[in] df1 The left data frame
    !! @param[in] df2 The right data frame
    !! @param[in] key_col1 Column index for join key in left data frame
    !! @param[in] key_col2 Column index for join key in right data frame
    !! @return A new data frame with matched rows from both tables
    !!
    !! @note Key columns must have the same data type (integer, real, or character)
    !! @note Right table columns are suffixed with "_right" to avoid name conflicts
    function df_inner_join(df1, df2, key_col1, key_col2) result(joined_df)
        type(data_frame), intent(in) :: df1, df2
        integer, intent(in) :: key_col1, key_col2
        type(data_frame) :: joined_df

        integer :: i, j, dtype1, dtype2, num_matches
        integer, dimension(:), allocatable :: match_indices_this, match_indices_other
        logical :: match_found

        ! Check if key columns are compatible types
        dtype1 = df1 % dtype(key_col1)
        dtype2 = df2 % dtype(key_col2)

        if (dtype1 /= dtype2) then
            print *, "Error: Key columns must have the same data type"
            call joined_df % new()
            return
        end if

        ! Count matches first
        num_matches = 0
        do i = 1, df1 % nrows()
            do j = 1, df2 % nrows()
                match_found = .false.

                select case (dtype1)
                case (INTEGER_NUM)
                    if (df_get_val_integer(df1, i, key_col1) == df_get_val_integer(df2, j, key_col2)) then
                        match_found = .true.
                    end if
                case (REAL_NUM)
                    if (abs(df_get_val_real(df1, i, key_col1) - df_get_val_real(df2, j, key_col2)) < 1.0e-10_rk) then
                        match_found = .true.
                    end if
                case (CHARACTER_NUM)
                    if (trim(df_get_val_character(df1, i, key_col1)) == trim(df_get_val_character(df2, j, key_col2))) then
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
        do i = 1, df1 % nrows()
            do j = 1, df2 % nrows()
                match_found = .false.

                select case (dtype1)
                case (INTEGER_NUM)
                    if (df_get_val_integer(df1, i, key_col1) == df_get_val_integer(df2, j, key_col2)) then
                        match_found = .true.
                    end if
                case (REAL_NUM)
                    if (abs(df_get_val_real(df1, i, key_col1) - df_get_val_real(df2, j, key_col2)) < 1.0e-10_rk) then
                        match_found = .true.
                    end if
                case (CHARACTER_NUM)
                    if (trim(df_get_val_character(df1, i, key_col1)) == trim(df_get_val_character(df2, j, key_col2))) then
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
        call build_joined_dataframe(df1, df2, match_indices_this, match_indices_other, &
                                    num_matches, joined_df)

        deallocate (match_indices_this)
        deallocate (match_indices_other)
    end function df_inner_join

    !> Perform a left join between two data frames
    !!
    !! Returns all rows from left df, with matching rows from right df (or NULL)
    !!
    !! @param[in] df1 The left data frame
    !! @param[in] df2 The right data frame
    !! @param[in] key_col1 Column index for join key in left data frame
    !! @param[in] key_col2 Column index for join key in right data frame
    !! @return A new data frame with all rows from left and matching rows from right
    function df_left_join(df1, df2, key_col1, key_col2) result(joined_df)
        type(data_frame), intent(in) :: df1, df2
        integer, intent(in) :: key_col1, key_col2
        type(data_frame) :: joined_df

        integer :: i, j, dtype1, dtype2
        integer, dimension(:), allocatable :: match_indices_this, match_indices_other
        logical :: match_found
        integer :: num_rows

        dtype1 = df1 % dtype(key_col1)
        dtype2 = df2 % dtype(key_col2)

        if (dtype1 /= dtype2) then
            print *, "Error: Key columns must have the same data type"
            call joined_df % new()
            return
        end if

        ! For left join, we need one row for each row in 'df1', possibly more if multiple matches
        num_rows = 0
        allocate (match_indices_this(df1 % nrows() * max(1, df2 % nrows())))
        allocate (match_indices_other(df1 % nrows() * max(1, df2 % nrows())))

        do i = 1, df1 % nrows()
            match_found = .false.

            do j = 1, df2 % nrows()
                select case (dtype1)
                case (INTEGER_NUM)
                    if (df_get_val_integer(df1, i, key_col1) == df_get_val_integer(df2, j, key_col2)) then
                        num_rows = num_rows + 1
                        match_indices_this(num_rows) = i
                        match_indices_other(num_rows) = j
                        match_found = .true.
                    end if
                case (REAL_NUM)
                    if (abs(df_get_val_real(df1, i, key_col1) - df_get_val_real(df2, j, key_col2)) < 1.0e-10_rk) then
                        num_rows = num_rows + 1
                        match_indices_this(num_rows) = i
                        match_indices_other(num_rows) = j
                        match_found = .true.
                    end if
                case (CHARACTER_NUM)
                    if (trim(df_get_val_character(df1, i, key_col1)) == trim(df_get_val_character(df2, j, key_col2))) then
                        num_rows = num_rows + 1
                        match_indices_this(num_rows) = i
                        match_indices_other(num_rows) = j
                        match_found = .true.
                    end if
                end select
            end do

            ! If no match found, still include the row from 'df1' with NULL for 'df2'
            if (.not. match_found) then
                num_rows = num_rows + 1
                match_indices_this(num_rows) = i
                match_indices_other(num_rows) = -1  ! -1 indicates no match
            end if
        end do

        call build_joined_dataframe(df1, df2, match_indices_this(1:num_rows), &
                                    match_indices_other(1:num_rows), num_rows, joined_df)

        deallocate (match_indices_this)
        deallocate (match_indices_other)
    end function df_left_join

    !> Perform a right join between two data frames
    !!
    !! Returns all rows from right df, with matching rows from left df (or NULL)
    !!
    !! @param[in] df1 The left data frame
    !! @param[in] df2 The right data frame
    !! @param[in] key_col1 Column index for join key in left data frame
    !! @param[in] key_col2 Column index for join key in right data frame
    !! @return A new data frame with all rows from right and matching rows from left
    function df_right_join(df1, df2, key_col1, key_col2) result(joined_df)
        type(data_frame), intent(in) :: df1, df2
        integer, intent(in) :: key_col1, key_col2
        type(data_frame) :: joined_df

        ! Right join is just a left join with arguments swapped
        joined_df = df_left_join(df2, df1, key_col2, key_col1)
    end function df_right_join

    !> Perform an outer join between two data frames
    !!
    !! Returns all rows from both dataframes
    !!
    !! @param[in] df1 The left data frame
    !! @param[in] df2 The right data frame
    !! @param[in] key_col1 Column index for join key in left data frame
    !! @param[in] key_col2 Column index for join key in right data frame
    !! @return A new data frame with all rows from both tables
    function df_outer_join(df1, df2, key_col1, key_col2) result(joined_df)
        type(data_frame), intent(in) :: df1, df2
        integer, intent(in) :: key_col1, key_col2
        type(data_frame) :: joined_df

        integer :: i, j, dtype1, dtype2
        integer, dimension(:), allocatable :: match_indices_this, match_indices_other
        logical, dimension(:), allocatable :: other_matched
        integer :: num_rows
        logical :: found_match

        dtype1 = df1 % dtype(key_col1)
        dtype2 = df2 % dtype(key_col2)

        if (dtype1 /= dtype2) then
            print *, "Error: Key columns must have the same data type"
            call joined_df % new()
            return
        end if

        allocate (other_matched(df2 % nrows()))
        other_matched = .false.

        num_rows = 0
        allocate (match_indices_this((df1 % nrows() + df2 % nrows()) * max(1, max(df1 % nrows(), df2 % nrows()))))
        allocate (match_indices_other((df1 % nrows() + df2 % nrows()) * max(1, max(df1 % nrows(), df2 % nrows()))))

        ! First pass: all rows from 'df1' with matches from 'df2'
        do i = 1, df1 % nrows()
            found_match = .false.

            do j = 1, df2 % nrows()
                select case (dtype1)
                case (INTEGER_NUM)
                    if (df_get_val_integer(df1, i, key_col1) == df_get_val_integer(df2, j, key_col2)) then
                        num_rows = num_rows + 1
                        match_indices_this(num_rows) = i
                        match_indices_other(num_rows) = j
                        other_matched(j) = .true.
                        found_match = .true.
                    end if
                case (REAL_NUM)
                    if (abs(df_get_val_real(df1, i, key_col1) - df_get_val_real(df2, j, key_col2)) < 1.0e-10_rk) then
                        num_rows = num_rows + 1
                        match_indices_this(num_rows) = i
                        match_indices_other(num_rows) = j
                        other_matched(j) = .true.
                        found_match = .true.
                    end if
                case (CHARACTER_NUM)
                    if (trim(df_get_val_character(df1, i, key_col1)) == trim(df_get_val_character(df2, j, key_col2))) then
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

        ! Second pass: unmatched rows from 'df2'
        do j = 1, df2 % nrows()
            if (.not. other_matched(j)) then
                num_rows = num_rows + 1
                match_indices_this(num_rows) = -1
                match_indices_other(num_rows) = j
            end if
        end do

        call build_joined_dataframe(df1, df2, match_indices_this(1:num_rows), &
                                    match_indices_other(1:num_rows), num_rows, joined_df)

        deallocate (match_indices_this)
        deallocate (match_indices_other)
        deallocate (other_matched)
    end function df_outer_join

    !> Merge two dataframes on column names (similar to SQL join but using names)
    !!
    !! @param[in] df1 Left dataframe
    !! @param[in] df2 Right dataframe
    !! @param[in] on_column Name of column to join on
    !! @param[in] how Type of join: "inner", "left", "right", "outer"
    !! @return Merged dataframe
    function df_merge(df1, df2, on_column, how) result(merged_df)
        type(data_frame), intent(in) :: df1, df2
        character(len=*), intent(in) :: on_column, how
        type(data_frame) :: merged_df

        integer :: col_idx1, col_idx2

        ! Find column indices
        col_idx1 = df1 % find_header_index(on_column)
        col_idx2 = df2 % find_header_index(on_column)

        if (col_idx1 == -1 .or. col_idx2 == -1) then
            print *, "Error: Column '", trim(on_column), "' not found in one or both dataframes"
            call merged_df % new()
            return
        end if

        ! Perform appropriate join based on 'how'
        select case (trim(how))
        case ("inner")
            merged_df = df_inner_join(df1, df2, col_idx1, col_idx2)
        case ("left")
            merged_df = df_left_join(df1, df2, col_idx1, col_idx2)
        case ("right")
            merged_df = df_right_join(df1, df2, col_idx1, col_idx2)
        case ("outer")
            merged_df = df_outer_join(df1, df2, col_idx1, col_idx2)
        case default
            print *, "Error: 'how' must be 'inner', 'left', 'right', or 'outer'"
            call merged_df % new()
        end select
    end function df_merge

    !========================================================================
    ! HELPER PROCEDURES
    !========================================================================

    !> Helper subroutine to build joined dataframe from match indices
    !!
    !! @param[in] df1 First dataframe
    !! @param[in] df2 Second dataframe
    !! @param[in] indices1 Row indices from df1 (-1 means no match)
    !! @param[in] indices2 Row indices from df2 (-1 means no match)
    !! @param[in] num_rows Number of rows in result
    !! @param[out] result_df Resulting joined dataframe
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

        call result_df % new(max(df1 % get_max_char_len(), df2 % get_max_char_len()))

        ! Add columns from df1
        do i = 1, df1 % ncols()
            dtype = df1 % dtype(i)

            select case (dtype)
            case (REAL_NUM)
                allocate (real_col(num_rows))
                do row = 1, num_rows
                    if (indices1(row) > 0) then
                        real_col(row) = df_get_val_real(df1, indices1(row), i)
                    else
                        real_col(row) = 0.0_rk  ! NULL value
                    end if
                end do
                if (df1 % get_with_headers()) then
                    header_name = df1 % header(i)
                    call df_append_real(result_df, real_col, trim(header_name))
                else
                    call df_append_real(result_df, real_col)
                end if
                deallocate (real_col)

            case (INTEGER_NUM)
                allocate (int_col(num_rows))
                do row = 1, num_rows
                    if (indices1(row) > 0) then
                        int_col(row) = df_get_val_integer(df1, indices1(row), i)
                    else
                        int_col(row) = 0_ik  ! NULL value
                    end if
                end do
                if (df1 % get_with_headers()) then
                    header_name = df1 % header(i)
                    call df_append_integer(result_df, int_col, trim(header_name))
                else
                    call df_append_integer(result_df, int_col)
                end if
                deallocate (int_col)

            case (LOGICAL_NUM)
                allocate (log_col(num_rows))
                do row = 1, num_rows
                    if (indices1(row) > 0) then
                        log_col(row) = df_get_val_logical(df1, indices1(row), i)
                    else
                        log_col(row) = .false.  ! NULL value
                    end if
                end do
                if (df1 % get_with_headers()) then
                    header_name = df1 % header(i)
                    call df_append_logical(result_df, log_col, trim(header_name))
                else
                    call df_append_logical(result_df, log_col)
                end if
                deallocate (log_col)

            case (CHARACTER_NUM)
                allocate (character(len=df1 % get_max_char_len()) :: char_col(num_rows))
                do row = 1, num_rows
                    if (indices1(row) > 0) then
                        char_col(row) = df_get_val_character(df1, indices1(row), i)
                    else
                        char_col(row) = "NULL"
                    end if
                end do
                if (df1 % get_with_headers()) then
                    header_name = df1 % header(i)
                    call df_append_character(result_df, char_col, trim(header_name))
                else
                    call df_append_character(result_df, char_col)
                end if
                deallocate (char_col)

            case (COMPLEX_NUM)
                allocate (cmplx_col(num_rows))
                do row = 1, num_rows
                    if (indices1(row) > 0) then
                        cmplx_col(row) = df_get_val_complex(df1, indices1(row), i)
                    else
                        cmplx_col(row) = cmplx(0.0_rk, 0.0_rk, rk)
                    end if
                end do
                if (df1 % get_with_headers()) then
                    header_name = df1 % header(i)
                    call df_append_complex(result_df, cmplx_col, trim(header_name))
                else
                    call df_append_complex(result_df, cmplx_col)
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
                        real_col(row) = df_get_val_real(df2, indices2(row), i)
                    else
                        real_col(row) = 0.0_rk
                    end if
                end do
                if (df2 % get_with_headers()) then
                    header_name = df2 % header(i)
                    call df_append_real(result_df, real_col, trim(header_name)//"_right")
                else
                    call df_append_real(result_df, real_col)
                end if
                deallocate (real_col)

            case (INTEGER_NUM)
                allocate (int_col(num_rows))
                do row = 1, num_rows
                    if (indices2(row) > 0) then
                        int_col(row) = df_get_val_integer(df2, indices2(row), i)
                    else
                        int_col(row) = 0_ik
                    end if
                end do
                if (df2 % get_with_headers()) then
                    header_name = df2 % header(i)
                    call df_append_integer(result_df, int_col, trim(header_name)//"_right")
                else
                    call df_append_integer(result_df, int_col)
                end if
                deallocate (int_col)

            case (LOGICAL_NUM)
                allocate (log_col(num_rows))
                do row = 1, num_rows
                    if (indices2(row) > 0) then
                        log_col(row) = df_get_val_logical(df2, indices2(row), i)
                    else
                        log_col(row) = .false.
                    end if
                end do
                if (df2 % get_with_headers()) then
                    header_name = df2 % header(i)
                    call df_append_logical(result_df, log_col, trim(header_name)//"_right")
                else
                    call df_append_logical(result_df, log_col)
                end if
                deallocate (log_col)

            case (CHARACTER_NUM)
                allocate (character(len=df2 % get_max_char_len()) :: char_col(num_rows))
                do row = 1, num_rows
                    if (indices2(row) > 0) then
                        char_col(row) = df_get_val_character(df2, indices2(row), i)
                    else
                        char_col(row) = "NULL"
                    end if
                end do
                if (df2 % get_with_headers()) then
                    header_name = df2 % header(i)
                    call df_append_character(result_df, char_col, trim(header_name)//"_right")
                else
                    call df_append_character(result_df, char_col)
                end if
                deallocate (char_col)

            case (COMPLEX_NUM)
                allocate (cmplx_col(num_rows))
                do row = 1, num_rows
                    if (indices2(row) > 0) then
                        cmplx_col(row) = df_get_val_complex(df2, indices2(row), i)
                    else
                        cmplx_col(row) = cmplx(0.0_rk, 0.0_rk, rk)
                    end if
                end do
                if (df2 % get_with_headers()) then
                    header_name = df2 % header(i)
                    call df_append_complex(result_df, cmplx_col, trim(header_name)//"_right")
                else
                    call df_append_complex(result_df, cmplx_col)
                end if
                deallocate (cmplx_col)
            end select
        end do
    end subroutine build_joined_dataframe

end module datafort_joins
