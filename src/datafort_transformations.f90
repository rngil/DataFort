!> DataFort Transformations Module
!!
!! This module provides standalone functions for transforming data frame columns.
!! All functions take a data_frame as the first argument instead of being type-bound procedures.
!!
!! ## Functions
!!
!! ### Normalization/Standardization
!! - `df_normalize_column_real(df, col_index)` - Normalize to [0,1] range
!! - `df_standardize_column_real(df, col_index)` - Standardize to mean=0, std=1
!!
!! ### Absolute Value
!! - `df_abs_column_real(df, col_index)` - Absolute value for real column
!! - `df_abs_column_integer(df, col_index)` - Absolute value for integer column
!!
!! ### Cumulative Sum
!! - `df_cumsum_real(df, col_index)` - Cumulative sum for real column
!! - `df_cumsum_integer(df, col_index)` - Cumulative sum for integer column
!!
!! ### Differences
!! - `df_diff_real(df, col_index)` - Differences between consecutive rows (real)
!! - `df_diff_integer(df, col_index)` - Differences between consecutive rows (integer)
!!
!! ### Value Replacement
!! - `df_replace_value_real(df, col_index, old_value, new_value)` - Replace values in real column
!! - `df_replace_value_integer(df, col_index, old_value, new_value)` - Replace values in integer column
!!
!! ### Clipping
!! - `df_clip_real(df, col_index, min_val, max_val)` - Clip real values to range
!! - `df_clip_integer(df, col_index, min_val, max_val)` - Clip integer values to range
!!
!! ### Rounding
!! - `df_round_column(df, col_index, decimals)` - Round to decimal places
!!
!! ### Mathematical Functions
!! - `df_log_column(df, col_index)` - Natural logarithm
!! - `df_exp_column(df, col_index)` - Exponential
!! - `df_sqrt_column(df, col_index)` - Square root
!! - `df_pow_column(df, col_index, power)` - Raise to power
!!
!! ### Custom Transformation
!! - `df_apply_to_column(df, col_index, func)` - Apply custom function
module datafort_transformations
    use precision
    use types
    use column_class
    use datafort_types
    use datafort_accessors
    implicit none
    private

    ! Public normalization/standardization functions
    public :: df_normalize_column_real, df_standardize_column_real

    ! Public absolute value functions
    public :: df_abs_column_real, df_abs_column_integer

    ! Public cumulative sum functions
    public :: df_cumsum_real, df_cumsum_integer

    ! Public difference functions
    public :: df_diff_real, df_diff_integer

    ! Public value replacement functions
    public :: df_replace_value_real, df_replace_value_integer

    ! Public clipping functions
    public :: df_clip_real, df_clip_integer

    ! Public rounding function
    public :: df_round_column

    ! Public mathematical functions
    public :: df_log_column, df_exp_column, df_sqrt_column, df_pow_column

    ! Public custom transformation function
    public :: df_apply_to_column

    ! Public abstract interface for custom transformations
    public :: transform_func

    ! Abstract interface for apply function
    abstract interface
        pure function transform_func(x) result(y)
            import :: rk
            real(rk), intent(in) :: x
            real(rk) :: y
        end function transform_func
    end interface

contains

    !========================================================================
    ! HELPER FUNCTIONS FOR STATISTICS
    !========================================================================

    !> Calculate mean of a real column
    function calculate_mean_real(df, col_index) result(avg)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: col_index
        real(rk) :: avg

        real(rk), dimension(:), allocatable :: col

        col = df_get_col_real(df, col_index)
        avg = sum(col) / real(size(col), rk)
    end function calculate_mean_real

    !> Calculate standard deviation of a real column
    function calculate_std_real(df, col_index) result(stddev)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: col_index
        real(rk) :: stddev

        real(rk), dimension(:), allocatable :: col
        real(rk) :: avg

        col = df_get_col_real(df, col_index)
        avg = calculate_mean_real(df, col_index)
        stddev = sqrt(sum((col - avg)**2) / real(size(col) - 1, rk))
    end function calculate_std_real

    !> Calculate minimum of a real column
    function calculate_min_real(df, col_index) result(min_val)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: col_index
        real(rk) :: min_val

        real(rk), dimension(:), allocatable :: col

        col = df_get_col_real(df, col_index)
        min_val = minval(col)
    end function calculate_min_real

    !> Calculate maximum of a real column
    function calculate_max_real(df, col_index) result(max_val)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: col_index
        real(rk) :: max_val

        real(rk), dimension(:), allocatable :: col

        col = df_get_col_real(df, col_index)
        max_val = maxval(col)
    end function calculate_max_real

    !========================================================================
    ! NORMALIZATION AND STANDARDIZATION
    !========================================================================

    !> Normalize a real column to [0, 1] range
    !!
    !! Applies min-max normalization: (x - min) / (max - min)
    !!
    !! @param[in,out] df The data frame instance
    !! @param[in] col_index Index of the column to normalize
    !!
    !! @note If all values are the same (range = 0), the column is not modified
    subroutine df_normalize_column_real(df, col_index)
        type(data_frame), intent(inout) :: df
        integer, intent(in) :: col_index

        real(rk), dimension(:), allocatable :: col
        real(rk) :: min_val, max_val, range
        integer :: i

        col = df_get_col_real(df, col_index)
        min_val = calculate_min_real(df, col_index)
        max_val = calculate_max_real(df, col_index)
        range = max_val - min_val

        if (range > 0.0_rk) then
            do i = 1, size(col)
                col(i) = (col(i) - min_val) / range
            end do
            call df_set_col_real(df, col_index, col)
        end if
    end subroutine df_normalize_column_real

    !> Standardize a real column (z-score: mean=0, std=1)
    !!
    !! Applies z-score standardization: (x - mean) / std
    !!
    !! @param[in,out] df The data frame instance
    !! @param[in] col_index Index of the column to standardize
    !!
    !! @note If std = 0, the column is not modified
    subroutine df_standardize_column_real(df, col_index)
        type(data_frame), intent(inout) :: df
        integer, intent(in) :: col_index

        real(rk), dimension(:), allocatable :: col
        real(rk) :: mean_val, std_val
        integer :: i

        col = df_get_col_real(df, col_index)
        mean_val = calculate_mean_real(df, col_index)
        std_val = calculate_std_real(df, col_index)

        if (std_val > 0.0_rk) then
            do i = 1, size(col)
                col(i) = (col(i) - mean_val) / std_val
            end do
            call df_set_col_real(df, col_index, col)
        end if
    end subroutine df_standardize_column_real

    !========================================================================
    ! ABSOLUTE VALUE
    !========================================================================

    !> Take absolute value of all elements in a real column
    !!
    !! @param[in,out] df The data frame instance
    !! @param[in] col_index Index of the column
    subroutine df_abs_column_real(df, col_index)
        type(data_frame), intent(inout) :: df
        integer, intent(in) :: col_index

        real(rk), dimension(:), allocatable :: col

        col = df_get_col_real(df, col_index)
        col = abs(col)
        call df_set_col_real(df, col_index, col)
    end subroutine df_abs_column_real

    !> Take absolute value of all elements in an integer column
    !!
    !! @param[in,out] df The data frame instance
    !! @param[in] col_index Index of the column
    subroutine df_abs_column_integer(df, col_index)
        type(data_frame), intent(inout) :: df
        integer, intent(in) :: col_index

        integer(ik), dimension(:), allocatable :: col

        col = df_get_col_integer(df, col_index)
        col = abs(col)
        call df_set_col_integer(df, col_index, col)
    end subroutine df_abs_column_integer

    !========================================================================
    ! CUMULATIVE SUM
    !========================================================================

    !> Calculate cumulative sum for a real column (modifies in place)
    !!
    !! Each element becomes the sum of all elements up to and including that position
    !!
    !! @param[in,out] df The data frame instance
    !! @param[in] col_index Index of the column
    subroutine df_cumsum_real(df, col_index)
        type(data_frame), intent(inout) :: df
        integer, intent(in) :: col_index

        real(rk), dimension(:), allocatable :: col
        integer :: i

        col = df_get_col_real(df, col_index)
        do i = 2, size(col)
            col(i) = col(i) + col(i - 1)
        end do
        call df_set_col_real(df, col_index, col)
    end subroutine df_cumsum_real

    !> Calculate cumulative sum for an integer column (modifies in place)
    !!
    !! Each element becomes the sum of all elements up to and including that position
    !!
    !! @param[in,out] df The data frame instance
    !! @param[in] col_index Index of the column
    subroutine df_cumsum_integer(df, col_index)
        type(data_frame), intent(inout) :: df
        integer, intent(in) :: col_index

        integer(ik), dimension(:), allocatable :: col
        integer :: i

        col = df_get_col_integer(df, col_index)
        do i = 2, size(col)
            col(i) = col(i) + col(i - 1)
        end do
        call df_set_col_integer(df, col_index, col)
    end subroutine df_cumsum_integer

    !========================================================================
    ! DIFFERENCES
    !========================================================================

    !> Calculate differences between consecutive rows (result has n-1 elements)
    !!
    !! Returns an array where each element is the difference between consecutive values
    !!
    !! @param[in] df The data frame instance
    !! @param[in] col_index Index of the column
    !! @return Array of differences with n-1 elements
    function df_diff_real(df, col_index) result(differences)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: col_index
        real(rk), dimension(:), allocatable :: differences

        real(rk), dimension(:), allocatable :: col
        integer :: i, n

        col = df_get_col_real(df, col_index)
        n = size(col)

        if (n < 2) then
            allocate (differences(0))
            return
        end if

        allocate (differences(n - 1))
        do i = 1, n - 1
            differences(i) = col(i + 1) - col(i)
        end do
    end function df_diff_real

    !> Calculate differences between consecutive rows (result has n-1 elements)
    !!
    !! Returns an array where each element is the difference between consecutive values
    !!
    !! @param[in] df The data frame instance
    !! @param[in] col_index Index of the column
    !! @return Array of differences with n-1 elements
    function df_diff_integer(df, col_index) result(differences)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: col_index
        integer(ik), dimension(:), allocatable :: differences

        integer(ik), dimension(:), allocatable :: col
        integer :: i, n

        col = df_get_col_integer(df, col_index)
        n = size(col)

        if (n < 2) then
            allocate (differences(0))
            return
        end if

        allocate (differences(n - 1))
        do i = 1, n - 1
            differences(i) = col(i + 1) - col(i)
        end do
    end function df_diff_integer

    !========================================================================
    ! VALUE REPLACEMENT
    !========================================================================

    !> Replace all occurrences of a value in a real column
    !!
    !! Uses tolerance-based comparison for real values
    !!
    !! @param[in,out] df The data frame instance
    !! @param[in] col_index Index of the column
    !! @param[in] old_value Value to replace
    !! @param[in] new_value Replacement value
    subroutine df_replace_value_real(df, col_index, old_value, new_value)
        type(data_frame), intent(inout) :: df
        integer, intent(in) :: col_index
        real(rk), intent(in) :: old_value, new_value

        real(rk), dimension(:), allocatable :: col
        integer :: i
        real(rk), parameter :: tol = 1.0e-10_rk

        col = df_get_col_real(df, col_index)
        do i = 1, size(col)
            if (abs(col(i) - old_value) < tol) then
                col(i) = new_value
            end if
        end do
        call df_set_col_real(df, col_index, col)
    end subroutine df_replace_value_real

    !> Replace all occurrences of a value in an integer column
    !!
    !! @param[in,out] df The data frame instance
    !! @param[in] col_index Index of the column
    !! @param[in] old_value Value to replace
    !! @param[in] new_value Replacement value
    subroutine df_replace_value_integer(df, col_index, old_value, new_value)
        type(data_frame), intent(inout) :: df
        integer, intent(in) :: col_index
        integer(ik), intent(in) :: old_value, new_value

        integer(ik), dimension(:), allocatable :: col
        integer :: i

        col = df_get_col_integer(df, col_index)
        do i = 1, size(col)
            if (col(i) == old_value) then
                col(i) = new_value
            end if
        end do
        call df_set_col_integer(df, col_index, col)
    end subroutine df_replace_value_integer

    !========================================================================
    ! CLIPPING
    !========================================================================

    !> Clip (clamp) values in a real column to [min_val, max_val]
    !!
    !! Values below min_val are set to min_val, values above max_val are set to max_val
    !!
    !! @param[in,out] df The data frame instance
    !! @param[in] col_index Index of the column
    !! @param[in] min_val Minimum allowed value
    !! @param[in] max_val Maximum allowed value
    subroutine df_clip_real(df, col_index, min_val, max_val)
        type(data_frame), intent(inout) :: df
        integer, intent(in) :: col_index
        real(rk), intent(in) :: min_val, max_val

        real(rk), dimension(:), allocatable :: col
        integer :: i

        col = df_get_col_real(df, col_index)
        do i = 1, size(col)
            col(i) = max(min_val, min(max_val, col(i)))
        end do
        call df_set_col_real(df, col_index, col)
    end subroutine df_clip_real

    !> Clip (clamp) values in an integer column to [min_val, max_val]
    !!
    !! Values below min_val are set to min_val, values above max_val are set to max_val
    !!
    !! @param[in,out] df The data frame instance
    !! @param[in] col_index Index of the column
    !! @param[in] min_val Minimum allowed value
    !! @param[in] max_val Maximum allowed value
    subroutine df_clip_integer(df, col_index, min_val, max_val)
        type(data_frame), intent(inout) :: df
        integer, intent(in) :: col_index
        integer(ik), intent(in) :: min_val, max_val

        integer(ik), dimension(:), allocatable :: col
        integer :: i

        col = df_get_col_integer(df, col_index)
        do i = 1, size(col)
            col(i) = max(min_val, min(max_val, col(i)))
        end do
        call df_set_col_integer(df, col_index, col)
    end subroutine df_clip_integer

    !========================================================================
    ! ROUNDING
    !========================================================================

    !> Round real column to specified number of decimal places
    !!
    !! @param[in,out] df The data frame instance
    !! @param[in] col_index Index of the column
    !! @param[in] decimals Number of decimal places
    subroutine df_round_column(df, col_index, decimals)
        type(data_frame), intent(inout) :: df
        integer, intent(in) :: col_index
        integer, intent(in) :: decimals

        real(rk), dimension(:), allocatable :: col
        real(rk) :: multiplier
        integer :: i

        col = df_get_col_real(df, col_index)
        multiplier = 10.0_rk**decimals

        do i = 1, size(col)
            col(i) = nint(col(i) * multiplier) / multiplier
        end do
        call df_set_col_real(df, col_index, col)
    end subroutine df_round_column

    !========================================================================
    ! MATHEMATICAL FUNCTIONS
    !========================================================================

    !> Apply natural logarithm to column
    !!
    !! @param[in,out] df The data frame instance
    !! @param[in] col_index Index of the column
    !!
    !! @warning Values must be positive
    subroutine df_log_column(df, col_index)
        type(data_frame), intent(inout) :: df
        integer, intent(in) :: col_index

        real(rk), dimension(:), allocatable :: col

        col = df_get_col_real(df, col_index)
        col = log(col)
        call df_set_col_real(df, col_index, col)
    end subroutine df_log_column

    !> Apply exponential to column
    !!
    !! @param[in,out] df The data frame instance
    !! @param[in] col_index Index of the column
    subroutine df_exp_column(df, col_index)
        type(data_frame), intent(inout) :: df
        integer, intent(in) :: col_index

        real(rk), dimension(:), allocatable :: col

        col = df_get_col_real(df, col_index)
        col = exp(col)
        call df_set_col_real(df, col_index, col)
    end subroutine df_exp_column

    !> Apply square root to column
    !!
    !! @param[in,out] df The data frame instance
    !! @param[in] col_index Index of the column
    !!
    !! @warning Values must be non-negative
    subroutine df_sqrt_column(df, col_index)
        type(data_frame), intent(inout) :: df
        integer, intent(in) :: col_index

        real(rk), dimension(:), allocatable :: col

        col = df_get_col_real(df, col_index)
        col = sqrt(col)
        call df_set_col_real(df, col_index, col)
    end subroutine df_sqrt_column

    !> Raise column to a power
    !!
    !! @param[in,out] df The data frame instance
    !! @param[in] col_index Index of the column
    !! @param[in] power The exponent to raise each value to
    subroutine df_pow_column(df, col_index, power)
        type(data_frame), intent(inout) :: df
        integer, intent(in) :: col_index
        real(rk), intent(in) :: power

        real(rk), dimension(:), allocatable :: col

        col = df_get_col_real(df, col_index)
        col = col**power
        call df_set_col_real(df, col_index, col)
    end subroutine df_pow_column

    !========================================================================
    ! CUSTOM TRANSFORMATION
    !========================================================================

    !> Apply custom function to column
    !!
    !! Applies a user-defined transformation function to each element in the column
    !!
    !! @param[in,out] df The data frame instance
    !! @param[in] col_index Index of the column
    !! @param[in] func Function to apply (must match transform_func interface)
    subroutine df_apply_to_column(df, col_index, func)
        type(data_frame), intent(inout) :: df
        integer, intent(in) :: col_index
        procedure(transform_func) :: func

        real(rk), dimension(:), allocatable :: col
        integer :: i

        col = df_get_col_real(df, col_index)
        do i = 1, size(col)
            col(i) = func(col(i))
        end do
        call df_set_col_real(df, col_index, col)
    end subroutine df_apply_to_column

end module datafort_transformations
