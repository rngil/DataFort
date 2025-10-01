!> DataFort Sorting Module
!!
!! This module provides standalone functions for sorting and ranking operations on data frames.
!! All functions take a data_frame as the first argument instead of being type-bound procedures.
!!
!! ## Functions
!!
!! ### Sorting Functions
!! - `df_sort_by_column(df, col_index, ascending)` - Sort data frame by column values
!!
!! ### Sorting Check Functions
!! - `df_is_sorted_real(df, col_index, ascending)` - Check if real column is sorted
!! - `df_is_sorted_integer(df, col_index, ascending)` - Check if integer column is sorted
!!
!! ### Ranking Functions
!! - `df_rank_real(df, col_index, ascending)` - Get ranks for real column values
!! - `df_rank_integer(df, col_index, ascending)` - Get ranks for integer column values
module datafort_sorting
    use precision
    use types
    use column_class
    use datafort_types
    use utilities
    implicit none
    private

    ! Public sorting functions
    public :: df_sort_by_column

    ! Public sorting check functions
    public :: df_is_sorted_real, df_is_sorted_integer

    ! Public ranking functions
    public :: df_rank_real, df_rank_integer

contains

    !========================================================================
    ! SORTING FUNCTIONS
    !========================================================================

    !> Sort data frame by column values
    !!
    !! Sorts all rows of the data frame based on values in the specified column.
    !! Only real and integer columns can be used for sorting.
    !!
    !! @param[in,out] df The data frame instance
    !! @param[in] col_index Index of column to sort by
    !! @param[in] ascending Optional sort direction (default: .true. for ascending)
    !!
    !! @note This operation modifies the data frame in place
    subroutine df_sort_by_column(df, col_index, ascending)
        type(data_frame), intent(inout) :: df
        integer, intent(in) :: col_index
        logical, intent(in), optional :: ascending

        logical :: asc
        integer, allocatable :: indices(:)
        integer :: i
        type(column) :: col

        asc = .true.
        if (present(ascending)) asc = ascending

        if (col_index < 1 .or. col_index > df%ncols()) then
            error stop "Column index out of range in df_sort_by_column"
        end if

        allocate (indices(df%nrows()))
        do i = 1, df%nrows()
            indices(i) = i
        end do

        col = df%get_data_col(col_index)

        select case (col%get_type())
        case (REAL_NUM)
            call sort_indices_real(col%getr(), indices, asc)
        case (INTEGER_NUM)
            call sort_indices_integer(col%geti(), indices, asc)
        case default
            error stop "Sorting only supported for real and integer columns"
        end select

        call reorder_all_columns(df, indices)

        deallocate (indices)
    end subroutine df_sort_by_column

    !========================================================================
    ! SORTING CHECK FUNCTIONS
    !========================================================================

    !> Check if a real column is sorted
    !!
    !! Verifies whether the values in a real column are in sorted order
    !!
    !! @param[in] df The data frame instance
    !! @param[in] col_index Index of column to check
    !! @param[in] ascending Optional sort direction to check (default: .true.)
    !! @return .true. if column is sorted, .false. otherwise
    function df_is_sorted_real(df, col_index, ascending) result(sorted)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: col_index
        logical, intent(in), optional :: ascending
        logical :: sorted

        real(rk), dimension(:), allocatable :: col
        type(column) :: data_col
        logical :: asc
        integer :: i

        asc = .true.
        if (present(ascending)) asc = ascending

        data_col = df%get_data_col(col_index)
        col = data_col%getr()
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
    end function df_is_sorted_real

    !> Check if an integer column is sorted
    !!
    !! Verifies whether the values in an integer column are in sorted order
    !!
    !! @param[in] df The data frame instance
    !! @param[in] col_index Index of column to check
    !! @param[in] ascending Optional sort direction to check (default: .true.)
    !! @return .true. if column is sorted, .false. otherwise
    function df_is_sorted_integer(df, col_index, ascending) result(sorted)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: col_index
        logical, intent(in), optional :: ascending
        logical :: sorted

        integer(ik), dimension(:), allocatable :: col
        type(column) :: data_col
        logical :: asc
        integer :: i

        asc = .true.
        if (present(ascending)) asc = ascending

        data_col = df%get_data_col(col_index)
        col = data_col%geti()
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
    end function df_is_sorted_integer

    !========================================================================
    ! RANKING FUNCTIONS
    !========================================================================

    !> Rank values in a real column
    !!
    !! Assigns rank positions to each value in the column (1 = smallest/largest)
    !!
    !! @param[in] df The data frame instance
    !! @param[in] col_index Index of column to rank
    !! @param[in] ascending Optional ranking direction (default: .true. for ascending)
    !! @return Array of integer ranks corresponding to each row
    function df_rank_real(df, col_index, ascending) result(ranks)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: col_index
        logical, intent(in), optional :: ascending
        integer(ik), dimension(:), allocatable :: ranks

        real(rk), dimension(:), allocatable :: col, sorted_col
        type(column) :: data_col
        integer :: i, j, n
        logical :: asc

        asc = .true.
        if (present(ascending)) asc = ascending

        data_col = df%get_data_col(col_index)
        col = data_col%getr()
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
    end function df_rank_real

    !> Rank values in an integer column
    !!
    !! Assigns rank positions to each value in the column (1 = smallest/largest)
    !!
    !! @param[in] df The data frame instance
    !! @param[in] col_index Index of column to rank
    !! @param[in] ascending Optional ranking direction (default: .true. for ascending)
    !! @return Array of integer ranks corresponding to each row
    function df_rank_integer(df, col_index, ascending) result(ranks)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: col_index
        logical, intent(in), optional :: ascending
        integer(ik), dimension(:), allocatable :: ranks

        integer(ik), dimension(:), allocatable :: col, sorted_col
        type(column) :: data_col
        integer :: i, j, n
        logical :: asc

        asc = .true.
        if (present(ascending)) asc = ascending

        data_col = df%get_data_col(col_index)
        col = data_col%geti()
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
    end function df_rank_integer

    !========================================================================
    ! INTERNAL HELPER FUNCTIONS
    !========================================================================

    !> Sort indices array based on real values
    !!
    !! Internal helper that sorts an index array based on corresponding real values
    !!
    !! @param[in] values Real values to sort by
    !! @param[in,out] indices Index array to be sorted
    !! @param[in] ascending Sort direction
    subroutine sort_indices_real(values, indices, ascending)
        real(rk), dimension(:), intent(in) :: values
        integer, dimension(:), intent(inout) :: indices
        logical, intent(in) :: ascending

        if (size(values) > 0) then
            call quicksort_indices_real(values, indices, 1, size(values), ascending)
        end if
    end subroutine sort_indices_real

    !> Quicksort implementation for real value indices
    !!
    !! Recursive quicksort algorithm that sorts indices based on real values
    !!
    !! @param[in] values Real values to sort by
    !! @param[in,out] indices Index array to be sorted
    !! @param[in] low Lower bound of sorting range
    !! @param[in] high Upper bound of sorting range
    !! @param[in] ascending Sort direction
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

    !> Partition function for real value quicksort
    !!
    !! Partitions the indices array around a pivot for quicksort
    !!
    !! @param[in] values Real values to sort by
    !! @param[in,out] indices Index array to be partitioned
    !! @param[in] low Lower bound of partition range
    !! @param[in] high Upper bound of partition range (pivot position)
    !! @param[in] ascending Sort direction
    !! @return Index of the pivot after partitioning
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

    !> Sort indices array based on integer values
    !!
    !! Internal helper that sorts an index array based on corresponding integer values
    !!
    !! @param[in] values Integer values to sort by
    !! @param[in,out] indices Index array to be sorted
    !! @param[in] ascending Sort direction
    subroutine sort_indices_integer(values, indices, ascending)
        integer(ik), dimension(:), intent(in) :: values
        integer, dimension(:), intent(inout) :: indices
        logical, intent(in) :: ascending

        if (size(values) > 0) then
            call quicksort_indices_integer(values, indices, 1, size(values), ascending)
        end if
    end subroutine sort_indices_integer

    !> Quicksort implementation for integer value indices
    !!
    !! Recursive quicksort algorithm that sorts indices based on integer values
    !!
    !! @param[in] values Integer values to sort by
    !! @param[in,out] indices Index array to be sorted
    !! @param[in] low Lower bound of sorting range
    !! @param[in] high Upper bound of sorting range
    !! @param[in] ascending Sort direction
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

    !> Partition function for integer value quicksort
    !!
    !! Partitions the indices array around a pivot for quicksort
    !!
    !! @param[in] values Integer values to sort by
    !! @param[in,out] indices Index array to be partitioned
    !! @param[in] low Lower bound of partition range
    !! @param[in] high Upper bound of partition range (pivot position)
    !! @param[in] ascending Sort direction
    !! @return Index of the pivot after partitioning
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

    !> Reorder all columns based on index array
    !!
    !! Internal helper that reorders all data frame columns according to an index array
    !!
    !! @param[in,out] df The data frame instance
    !! @param[in] indices Array specifying the new row order
    subroutine reorder_all_columns(df, indices)
        type(data_frame), intent(inout) :: df
        integer, dimension(:), intent(in) :: indices

        integer :: i, j
        real(rk), allocatable :: real_temp(:)
        integer(ik), allocatable :: int_temp(:)
        logical, allocatable :: logical_temp(:)
        character(len=:), allocatable :: char_temp(:)
        complex(rk), allocatable :: complex_temp(:)
        type(column) :: col, new_col

        do i = 1, df%ncols()
            col = df%get_data_col(i)

            select case (col%get_type())
            case (REAL_NUM)
                allocate (real_temp(df%nrows()))
                do j = 1, df%nrows()
                    real_temp(j) = col%getr(indices(j))
                end do
                call col%destroy()
                call new_col%new(real_temp)
                call df%set_data_col(i, new_col)
                deallocate (real_temp)
            case (INTEGER_NUM)
                allocate (int_temp(df%nrows()))
                do j = 1, df%nrows()
                    int_temp(j) = col%geti(indices(j))
                end do
                call col%destroy()
                call new_col%new(int_temp)
                call df%set_data_col(i, new_col)
                deallocate (int_temp)
            case (LOGICAL_NUM)
                allocate (logical_temp(df%nrows()))
                do j = 1, df%nrows()
                    logical_temp(j) = col%getl(indices(j))
                end do
                call col%destroy()
                call new_col%new(logical_temp)
                call df%set_data_col(i, new_col)
                deallocate (logical_temp)
            case (CHARACTER_NUM)
                allocate (character(len=len(col%getch(1))) :: char_temp(df%nrows()))
                do j = 1, df%nrows()
                    char_temp(j) = col%getch(indices(j))
                end do
                call col%destroy()
                call new_col%new(char_temp)
                call df%set_data_col(i, new_col)
                deallocate (char_temp)
            case (COMPLEX_NUM)
                allocate (complex_temp(df%nrows()))
                do j = 1, df%nrows()
                    complex_temp(j) = col%getc(indices(j))
                end do
                call col%destroy()
                call new_col%new(complex_temp)
                call df%set_data_col(i, new_col)
                deallocate (complex_temp)
            end select
        end do
    end subroutine reorder_all_columns

end module datafort_sorting
