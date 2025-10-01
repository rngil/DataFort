!> DataFort Utilities Module
!!
!! This module provides utility procedures for data frame operations including:
!! - Data inspection: head, tail, shape, info, empty
!! - Data manipulation: clear, sample, shuffle
!! - Row operations: apply_to_row_real, apply_to_all_rows_real
!!
!! All procedures are standalone functions prefixed with `df_`.
module datafort_utilities
    use precision
    use types
    use datafort_types
    use datafort_accessors
    use datafort_manipulation, only: df_copy
    use datafort_sorting, only: df_sort_by_column
    use column_class
    implicit none
    private

    public :: df_head
    public :: df_tail
    public :: df_shape
    public :: df_info
    public :: df_empty
    public :: df_clear
    public :: df_sample
    public :: df_shuffle
    public :: df_apply_to_row_real
    public :: df_apply_to_all_rows_real
    public :: row_func_real
    public :: df_nlargest_real
    public :: df_nsmallest_real
    public :: df_nlargest_integer
    public :: df_nsmallest_integer

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

    !> Return first n rows as a new dataframe
    !!
    !! @param[in] df The data frame to extract from
    !! @param[in] n Optional number of rows (default: 6)
    !! @return New data frame containing first n rows
    function df_head(df, n) result(head_df)
        type(data_frame), intent(in) :: df
        integer, intent(in), optional :: n
        type(data_frame) :: head_df

        integer :: num_rows

        if (present(n)) then
            num_rows = min(n, df % nrows())
        else
            num_rows = min(6, df % nrows())  ! Default to 6 rows like pandas
        end if

        if (num_rows == 0 .or. df % nrows() == 0) then
            call head_df % new(df % get_max_char_len())
            return
        end if

        head_df = df_slice_rows(df, 1, num_rows)
    end function df_head

    !> Return last n rows as a new dataframe
    !!
    !! @param[in] df The data frame to extract from
    !! @param[in] n Optional number of rows (default: 6)
    !! @return New data frame containing last n rows
    function df_tail(df, n) result(tail_df)
        type(data_frame), intent(in) :: df
        integer, intent(in), optional :: n
        type(data_frame) :: tail_df

        integer :: num_rows, start_row

        if (present(n)) then
            num_rows = min(n, df % nrows())
        else
            num_rows = min(6, df % nrows())  ! Default to 6 rows like pandas
        end if

        if (num_rows == 0 .or. df % nrows() == 0) then
            call tail_df % new(df % get_max_char_len())
            return
        end if

        start_row = df % nrows() - num_rows + 1
        tail_df = df_slice_rows(df, start_row, df % nrows())
    end function df_tail

    !> Return shape as [nrows, ncols]
    !!
    !! @param[in] df The data frame
    !! @return Array of dimensions [nrows, ncols]
    function df_shape(df) result(dims)
        type(data_frame), intent(in) :: df
        integer, dimension(2) :: dims

        dims(1) = df % nrows()
        dims(2) = df % ncols()
    end function df_shape

    !> Print information about the dataframe
    !!
    !! @param[in] df The data frame
    !! @param[in] unit Optional output unit (default: stdout)
    subroutine df_info(df, unit)
        type(data_frame), intent(in) :: df
        integer, intent(in), optional :: unit

        integer :: out_unit, i, dtype
        character(len=20) :: type_name
        integer, dimension(2) :: dims

        if (present(unit)) then
            out_unit = unit
        else
            out_unit = 6  ! stdout
        end if

        dims = df_shape(df)

        write (out_unit, '(a)') repeat('=', 60)
        write (out_unit, '(a)') 'DataFrame Information'
        write (out_unit, '(a)') repeat('=', 60)
        write (out_unit, '(a,i0,a,i0,a)') 'Shape: (', dims(1), ' rows, ', dims(2), ' columns)'
        write (out_unit, '(a,l1)') 'Has headers: ', df % get_with_headers()

        if (df % ncols() > 0) then
            write (out_unit, '(a)') ''
            write (out_unit, '(a)') 'Columns:'
            write (out_unit, '(a)') repeat('-', 60)

            do i = 1, df % ncols()
                dtype = df % dtype(i)

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

                if (df % get_with_headers()) then
                    write (out_unit, '(i4,2a,t30,3a)') i, '. ', trim(df % header(i)), &
                        '(', trim(type_name), ')'
                else
                    write (out_unit, '(i4,2a,a)') i, '. Column (', trim(type_name), ')'
                end if
            end do
        end if

        write (out_unit, '(a)') repeat('=', 60)
    end subroutine df_info

    !> Check if dataframe is empty (no rows or no columns)
    !!
    !! @param[in] df The data frame
    !! @return True if empty, false otherwise
    pure function df_empty(df) result(is_empty)
        type(data_frame), intent(in) :: df
        logical :: is_empty

        is_empty = (df % nrows() == 0 .or. df % ncols() == 0)
    end function df_empty

    !> Clear the dataframe (destroy and re-initialize)
    !!
    !! @param[in,out] df The data frame to clear
    subroutine df_clear(df)
        type(data_frame), intent(inout) :: df
        integer :: saved_char_len

        saved_char_len = df % get_max_char_len()
        call df % destroy()
        call df % new(saved_char_len)
    end subroutine df_clear

    !> Get n random rows from the dataframe
    !!
    !! @param[in] df The data frame to sample from
    !! @param[in] n Number of rows to sample
    !! @param[in] seed Optional random seed
    !! @return New data frame with sampled rows
    function df_sample(df, n, seed) result(sampled_df)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: n
        integer, intent(in), optional :: seed
        type(data_frame) :: sampled_df

        integer, dimension(:), allocatable :: indices, selected_indices, seed_array
        integer :: i, j, temp, num_samples, seed_size
        real :: rand_val

        num_samples = min(n, df % nrows())

        if (num_samples == 0) then
            call sampled_df % new(df % get_max_char_len())
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
        allocate (indices(df % nrows()))
        do i = 1, df % nrows()
            indices(i) = i
        end do

        ! Fisher-Yates shuffle to get random sample
        do i = df % nrows(), 2, -1
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
        call sampled_df % new(df % get_max_char_len())
        do i = 1, df % ncols()
            call copy_filtered_column(df, sampled_df, i, selected_indices)
        end do

        deallocate (indices, selected_indices)
    end function df_sample

    !> Shuffle all rows randomly in place
    !!
    !! @param[in,out] df The data frame to shuffle
    !! @param[in] seed Optional random seed
    subroutine df_shuffle(df, seed)
        type(data_frame), intent(inout) :: df
        integer, intent(in), optional :: seed

        integer, dimension(:), allocatable :: indices, seed_array
        integer :: i, j, temp, seed_size
        real :: rand_val

        if (df % nrows() < 2) return

        ! Initialize random seed if provided
        if (present(seed)) then
            call random_seed(size=seed_size)
            allocate (seed_array(seed_size))
            seed_array = seed
            call random_seed(put=seed_array)
            deallocate (seed_array)
        end if

        ! Create array of indices
        allocate (indices(df % nrows()))
        do i = 1, df % nrows()
            indices(i) = i
        end do

        ! Fisher-Yates shuffle
        do i = df % nrows(), 2, -1
            call random_number(rand_val)
            j = int(rand_val * i) + 1
            temp = indices(i)
            indices(i) = indices(j)
            indices(j) = temp
        end do

        ! Reorder all columns according to shuffled indices
        call reorder_all_columns(df, indices)

        deallocate (indices)
    end subroutine df_shuffle

    !> Apply a function to a specific row
    !!
    !! Extracts all numeric values from a specified row and applies
    !! a user-defined function to them. Non-numeric columns are skipped.
    !!
    !! @param[in] df The data frame
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
    !! result = df_apply_to_row_real(df, 5, row_sum)
    !! ```
    function df_apply_to_row_real(df, row_idx, func) result(output)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: row_idx
        procedure(row_func_real) :: func
        real(rk) :: output

        real(rk), dimension(:), allocatable :: row_values
        integer :: i, n_numeric, col_type
        type(column) :: col

        ! Count numeric columns
        n_numeric = 0
        do i = 1, df % ncols()
            col = df % get_data_col(i)
            col_type = col % dtype
            if (col_type == REAL_NUM .or. col_type == INTEGER_NUM) then
                n_numeric = n_numeric + 1
            end if
        end do

        ! Allocate and fill row values
        allocate (row_values(n_numeric))
        n_numeric = 0
        do i = 1, df % ncols()
            col = df % get_data_col(i)
            col_type = col % dtype
            if (col_type == REAL_NUM) then
                n_numeric = n_numeric + 1
                row_values(n_numeric) = col % getr(row_idx)
            else if (col_type == INTEGER_NUM) then
                n_numeric = n_numeric + 1
                row_values(n_numeric) = real(col % geti(row_idx), rk)
            end if
        end do

        ! Apply function
        output = func(row_values, n_numeric)

        deallocate (row_values)
    end function df_apply_to_row_real

    !> Apply a function to all rows
    !!
    !! Applies a user-defined function to each row in the data frame,
    !! returning an array of results.
    !!
    !! @param[in] df The data frame
    !! @param[in] func Function to apply (must match row_func_real interface)
    !! @return Array of results, one per row
    !!
    !! ## Example
    !!
    !! ```fortran
    !! real(rk), dimension(:), allocatable :: row_sums
    !! row_sums = df_apply_to_all_rows_real(df, row_sum)
    !! ```
    function df_apply_to_all_rows_real(df, func) result(outputs)
        type(data_frame), intent(in) :: df
        procedure(row_func_real) :: func
        real(rk), dimension(:), allocatable :: outputs
        integer :: i, n_rows

        n_rows = df % nrows()
        allocate (outputs(n_rows))

        do i = 1, n_rows
            outputs(i) = df_apply_to_row_real(df, i, func)
        end do
    end function df_apply_to_all_rows_real

    ! ========================================================================
    ! INTERNAL HELPER PROCEDURES
    ! ========================================================================

    !> Internal: Slice rows to create a new data frame
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
            error stop "invalid row range for slicing"
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

    !> Internal: Copy filtered column from source to target
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

    !> Internal: Reorder all columns according to index array
    subroutine reorder_all_columns(df, indices)
        type(data_frame), intent(inout) :: df
        integer, dimension(:), intent(in) :: indices

        integer :: i, j
        real(rk), allocatable :: real_temp(:)
        integer(ik), allocatable :: int_temp(:)
        logical, allocatable :: logical_temp(:)
        character(len=:), allocatable :: char_temp(:)
        complex(rk), allocatable :: complex_temp(:)
        type(column) :: col

        do i = 1, df % ncols()
            col = df % get_data_col(i)

            select case (col % get_type())
            case (REAL_NUM)
                allocate (real_temp(df % nrows()))
                do j = 1, df % nrows()
                    real_temp(j) = col % getr(indices(j))
                end do
                call col % destroy()
                call col % new(real_temp)
                call df % set_data_col(i, col)
                deallocate (real_temp)
            case (INTEGER_NUM)
                allocate (int_temp(df % nrows()))
                do j = 1, df % nrows()
                    int_temp(j) = col % geti(indices(j))
                end do
                call col % destroy()
                call col % new(int_temp)
                call df % set_data_col(i, col)
                deallocate (int_temp)
            case (LOGICAL_NUM)
                allocate (logical_temp(df % nrows()))
                do j = 1, df % nrows()
                    logical_temp(j) = col % getl(indices(j))
                end do
                call col % destroy()
                call col % new(logical_temp)
                call df % set_data_col(i, col)
                deallocate (logical_temp)
            case (CHARACTER_NUM)
                allocate (character(len=len(col % getch(1))) :: char_temp(df % nrows()))
                do j = 1, df % nrows()
                    char_temp(j) = col % getch(indices(j))
                end do
                call col % destroy()
                call col % new(char_temp)
                call df % set_data_col(i, col)
                deallocate (char_temp)
            case (COMPLEX_NUM)
                allocate (complex_temp(df % nrows()))
                do j = 1, df % nrows()
                    complex_temp(j) = col % getc(indices(j))
                end do
                call col % destroy()
                call col % new(complex_temp)
                call df % set_data_col(i, col)
                deallocate (complex_temp)
            end select
        end do
    end subroutine reorder_all_columns

    !========================================================================
    ! NLARGEST / NSMALLEST FUNCTIONS
    !========================================================================

    !> Return dataframe with n largest values in a real column
    !!
    !! @param df The data frame
    !! @param n Number of largest values to return
    !! @param col_index Column index to sort by
    !! @return New dataframe with n rows containing largest values
    function df_nlargest_real(df, n, col_index) result(result_df)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: n
        integer, intent(in) :: col_index
        type(data_frame) :: result_df

        type(data_frame) :: sorted_df
        integer :: n_rows

        if (col_index < 1 .or. col_index > df%ncols()) error stop "column index out of range"
        if (df%dtype(col_index) /= REAL_NUM) error stop "column is not real type"

        ! Sort descending
        sorted_df = df_copy(df)
        call df_sort_by_column(sorted_df, col_index, .false.)

        ! Take first n rows
        n_rows = min(n, sorted_df%nrows())
        result_df = df_slice_rows(sorted_df, 1, n_rows)

        call sorted_df%destroy()
    end function df_nlargest_real

    !> Return dataframe with n smallest values in a real column
    function df_nsmallest_real(df, n, col_index) result(result_df)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: n
        integer, intent(in) :: col_index
        type(data_frame) :: result_df

        type(data_frame) :: sorted_df
        integer :: n_rows

        if (col_index < 1 .or. col_index > df%ncols()) error stop "column index out of range"
        if (df%dtype(col_index) /= REAL_NUM) error stop "column is not real type"

        ! Sort ascending
        sorted_df = df_copy(df)
        call df_sort_by_column(sorted_df, col_index, .true.)

        ! Take first n rows
        n_rows = min(n, sorted_df%nrows())
        result_df = df_slice_rows(sorted_df, 1, n_rows)

        call sorted_df%destroy()
    end function df_nsmallest_real

    !> Return dataframe with n largest values in an integer column
    function df_nlargest_integer(df, n, col_index) result(result_df)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: n
        integer, intent(in) :: col_index
        type(data_frame) :: result_df

        type(data_frame) :: sorted_df
        integer :: n_rows

        if (col_index < 1 .or. col_index > df%ncols()) error stop "column index out of range"
        if (df%dtype(col_index) /= INTEGER_NUM) error stop "column is not integer type"

        ! Sort descending
        sorted_df = df_copy(df)
        call df_sort_by_column(sorted_df, col_index, .false.)

        ! Take first n rows
        n_rows = min(n, sorted_df%nrows())
        result_df = df_slice_rows(sorted_df, 1, n_rows)

        call sorted_df%destroy()
    end function df_nlargest_integer

    !> Return dataframe with n smallest values in an integer column
    function df_nsmallest_integer(df, n, col_index) result(result_df)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: n
        integer, intent(in) :: col_index
        type(data_frame) :: result_df

        type(data_frame) :: sorted_df
        integer :: n_rows

        if (col_index < 1 .or. col_index > df%ncols()) error stop "column index out of range"
        if (df%dtype(col_index) /= INTEGER_NUM) error stop "column is not integer type"

        ! Sort ascending
        sorted_df = df_copy(df)
        call df_sort_by_column(sorted_df, col_index, .true.)

        ! Take first n rows
        n_rows = min(n, sorted_df%nrows())
        result_df = df_slice_rows(sorted_df, 1, n_rows)

        call sorted_df%destroy()
    end function df_nsmallest_integer

end module datafort_utilities
