!> DataFort NaN Handling Module
!!
!! This module provides standalone NaN handling functions for data frames.
!! All functions take a data_frame as the first argument instead of being type-bound procedures.
!!
!! ## Functions
!!
!! ### NaN Detection Functions
!! - `df_isna_real(df, col_index)` - Check for NaN values in real column
!! - `df_isna_integer(df, col_index)` - Check for NaN values in integer column
!!
!! ### NaN Replacement Functions
!! - `df_fillna_real(df, col_index, fill_value)` - Replace NaN with fill value (real)
!! - `df_fillna_integer(df, col_index, fill_value)` - Replace NaN with fill value (integer)
!!
!! ### Row Removal Functions
!! - `df_dropna(df)` - Remove rows containing NaN values
!!
!! ## Notes
!! - For real columns, NaN is detected using IEEE floating-point NaN
!! - For integer columns, NaN is represented by a sentinel value (huge(1_ik))
!! - df_dropna returns a new data frame with NaN-containing rows removed
!! - df_fillna_* subroutines modify the data frame in-place
module datafort_nan
    use precision
    use types
    use column_class
    use datafort_types
    use datafort_accessors
    implicit none
    private

    ! Public NaN detection functions
    public :: df_isna_real, df_isna_integer

    ! Public NaN replacement functions
    public :: df_fillna_real, df_fillna_integer

    ! Public row removal functions
    public :: df_dropna

contains

    !========================================================================
    ! NaN DETECTION FUNCTIONS
    !========================================================================

    !> Check for NaN values in a real column
    !!
    !! Returns a logical array indicating which values are NaN
    !!
    !! @param[in] df The data frame instance
    !! @param[in] col_index Column index to check
    !! @return Logical array where .true. indicates NaN
    function df_isna_real(df, col_index) result(mask)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: col_index
        logical, dimension(:), allocatable :: mask

        real(rk), dimension(:), allocatable :: col
        integer :: i

        if (col_index < 1 .or. col_index > df%ncols()) error stop "column index out of range"

        col = df_get_col_real(df, col_index)
        allocate (mask(size(col)))

        do i = 1, size(col)
            mask(i) = is_nan_real(col(i))
        end do
    end function df_isna_real

    !> Check for NaN values in an integer column
    !!
    !! Returns a logical array indicating which values are NaN (sentinel value)
    !!
    !! @param[in] df The data frame instance
    !! @param[in] col_index Column index to check
    !! @return Logical array where .true. indicates NaN
    function df_isna_integer(df, col_index) result(mask)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: col_index
        logical, dimension(:), allocatable :: mask

        integer(ik), dimension(:), allocatable :: col
        integer :: i

        if (col_index < 1 .or. col_index > df%ncols()) error stop "column index out of range"

        col = df_get_col_integer(df, col_index)
        allocate (mask(size(col)))

        do i = 1, size(col)
            mask(i) = is_nan_integer(col(i))
        end do
    end function df_isna_integer

    !========================================================================
    ! NaN REPLACEMENT FUNCTIONS
    !========================================================================

    !> Replace NaN values in a real column with a fill value
    !!
    !! Replaces all NaN values in the specified column with the given value
    !!
    !! @param[in,out] df The data frame instance
    !! @param[in] col_index Column index to fill
    !! @param[in] fill_value Value to replace NaN with
    subroutine df_fillna_real(df, col_index, fill_value)
        type(data_frame), intent(inout) :: df
        integer, intent(in) :: col_index
        real(rk), intent(in) :: fill_value

        real(rk), dimension(:), allocatable :: col
        integer :: i

        if (col_index < 1 .or. col_index > df%ncols()) error stop "column index out of range"

        col = df_get_col_real(df, col_index)

        do i = 1, size(col)
            if (is_nan_real(col(i))) then
                col(i) = fill_value
            end if
        end do

        call df_set_col_real(df, col_index, col)
    end subroutine df_fillna_real

    !> Replace NaN values in an integer column with a fill value
    !!
    !! Replaces all NaN sentinel values in the specified column with the given value
    !!
    !! @param[in,out] df The data frame instance
    !! @param[in] col_index Column index to fill
    !! @param[in] fill_value Value to replace NaN with
    subroutine df_fillna_integer(df, col_index, fill_value)
        type(data_frame), intent(inout) :: df
        integer, intent(in) :: col_index
        integer(ik), intent(in) :: fill_value

        integer(ik), dimension(:), allocatable :: col
        integer :: i

        if (col_index < 1 .or. col_index > df%ncols()) error stop "column index out of range"

        col = df_get_col_integer(df, col_index)

        do i = 1, size(col)
            if (is_nan_integer(col(i))) then
                col(i) = fill_value
            end if
        end do

        call df_set_col_integer(df, col_index, col)
    end subroutine df_fillna_integer

    !========================================================================
    ! ROW REMOVAL FUNCTIONS
    !========================================================================

    !> Remove rows containing NaN values
    !!
    !! Returns a new data frame with all rows containing NaN in any column removed.
    !! Checks real and integer columns for NaN values.
    !!
    !! @param[in] df The data frame instance
    !! @return New data frame with NaN-containing rows removed
    function df_dropna(df) result(clean_df)
        type(data_frame), intent(in) :: df
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

        allocate (keep_mask(df%nrows()))
        keep_mask = .true.

        ! Check all columns for NaN
        do i = 1, df%ncols()
            dtype = df%dtype(i)

            if (dtype == REAL_NUM) then
                real_col = df_get_col_real(df, i)
                do j = 1, size(real_col)
                    if (is_nan_real(real_col(j))) then
                        keep_mask(j) = .false.
                    end if
                end do
                deallocate (real_col)
            else if (dtype == INTEGER_NUM) then
                int_col = df_get_col_integer(df, i)
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
            call clean_df%new(df%get_max_char_len())
            return
        end if

        ! Build index array of rows to keep
        allocate (clean_indices(num_clean_rows))
        j = 0
        do i = 1, df%nrows()
            if (keep_mask(i)) then
                j = j + 1
                clean_indices(j) = i
            end if
        end do

        ! Create new dataframe with clean rows
        call clean_df%new(df%get_max_char_len())

        do i = 1, df%ncols()
            dtype = df%dtype(i)

            select case (dtype)
            case (REAL_NUM)
                real_col = df_get_col_real(df, i)
                allocate (real_clean_col(num_clean_rows))
                do j = 1, num_clean_rows
                    real_clean_col(j) = real_col(clean_indices(j))
                end do
                if (df%get_with_headers()) then
                    header_name = df%header(i)
                    call df_append_real(clean_df, real_clean_col, trim(header_name))
                else
                    call df_append_real(clean_df, real_clean_col)
                end if
                deallocate (real_col)
                deallocate (real_clean_col)

            case (INTEGER_NUM)
                int_col = df_get_col_integer(df, i)
                allocate (int_clean_col(num_clean_rows))
                do j = 1, num_clean_rows
                    int_clean_col(j) = int_col(clean_indices(j))
                end do
                if (df%get_with_headers()) then
                    header_name = df%header(i)
                    call df_append_integer(clean_df, int_clean_col, trim(header_name))
                else
                    call df_append_integer(clean_df, int_clean_col)
                end if
                deallocate (int_col)
                deallocate (int_clean_col)

            case (LOGICAL_NUM)
                log_col = df_get_col_logical(df, i)
                allocate (log_clean_col(num_clean_rows))
                do j = 1, num_clean_rows
                    log_clean_col(j) = log_col(clean_indices(j))
                end do
                if (df%get_with_headers()) then
                    header_name = df%header(i)
                    call df_append_logical(clean_df, log_clean_col, trim(header_name))
                else
                    call df_append_logical(clean_df, log_clean_col)
                end if
                deallocate (log_col)
                deallocate (log_clean_col)

            case (CHARACTER_NUM)
                char_col = df_get_col_character(df, i)
                allocate (character(len=len(char_col)) :: char_clean_col(num_clean_rows))
                do j = 1, num_clean_rows
                    char_clean_col(j) = char_col(clean_indices(j))
                end do
                if (df%get_with_headers()) then
                    header_name = df%header(i)
                    call df_append_character(clean_df, char_clean_col, trim(header_name))
                else
                    call df_append_character(clean_df, char_clean_col)
                end if
                deallocate (char_col)
                deallocate (char_clean_col)

            case (COMPLEX_NUM)
                cmplx_col = df_get_col_complex(df, i)
                allocate (cmplx_clean_col(num_clean_rows))
                do j = 1, num_clean_rows
                    cmplx_clean_col(j) = cmplx_col(clean_indices(j))
                end do
                if (df%get_with_headers()) then
                    header_name = df%header(i)
                    call df_append_complex(clean_df, cmplx_clean_col, trim(header_name))
                else
                    call df_append_complex(clean_df, cmplx_clean_col)
                end if
                deallocate (cmplx_col)
                deallocate (cmplx_clean_col)
            end select
        end do

        deallocate (keep_mask)
        deallocate (clean_indices)
    end function df_dropna

end module datafort_nan
