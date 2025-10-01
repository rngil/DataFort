!> DataFort Statistics Module
!!
!! This module provides standalone statistical functions for data frame analysis.
!! All functions take a data_frame as the first argument instead of being type-bound procedures.
!!
!! ## Functions
!!
!! ### Sum Functions
!! - `df_sum_real(df, col_index)` - Calculate sum of real column
!! - `df_sum_integer(df, col_index)` - Calculate sum of integer column
!!
!! ### Mean Functions
!! - `df_mean_real(df, col_index)` - Calculate mean of real column
!! - `df_mean_integer(df, col_index)` - Calculate mean of integer column
!!
!! ### Standard Deviation Functions
!! - `df_std_real(df, col_index)` - Calculate standard deviation of real column
!! - `df_std_integer(df, col_index)` - Calculate standard deviation of integer column
!!
!! ### Median Functions
!! - `df_median_real(df, col_index)` - Calculate median of real column
!! - `df_median_integer(df, col_index)` - Calculate median of integer column
!!
!! ### Percentile Functions
!! - `df_percentile_real(df, col_index, percentile)` - Calculate percentile of real column
!! - `df_percentile_integer(df, col_index, percentile)` - Calculate percentile of integer column
!!
!! ### Variance Functions
!! - `df_variance_real(df, col_index)` - Calculate variance of real column
!! - `df_variance_integer(df, col_index)` - Calculate variance of integer column
!!
!! ### Min/Max Functions
!! - `df_min_real(df, col_index)` - Find minimum value in real column
!! - `df_min_integer(df, col_index)` - Find minimum value in integer column
!! - `df_max_real(df, col_index)` - Find maximum value in real column
!! - `df_max_integer(df, col_index)` - Find maximum value in integer column
!!
!! ### Correlation Functions
!! - `df_correlation_real(df, col_index1, col_index2)` - Calculate Pearson correlation
!!
!! ### Summary Functions
!! - `df_describe_numeric(df, unit)` - Print summary statistics for all numeric columns
module datafort_statistics
    use precision
    use types
    use column_class
    use datafort_types
    use utilities
    implicit none
    private

    ! Public sum functions
    public :: df_sum_real, df_sum_integer

    ! Public mean functions
    public :: df_mean_real, df_mean_integer

    ! Public standard deviation functions
    public :: df_std_real, df_std_integer

    ! Public median functions
    public :: df_median_real, df_median_integer

    ! Public percentile functions
    public :: df_percentile_real, df_percentile_integer

    ! Public variance functions
    public :: df_variance_real, df_variance_integer

    ! Public min/max functions
    public :: df_min_real, df_min_integer
    public :: df_max_real, df_max_integer

    ! Public correlation functions
    public :: df_correlation_real

    ! Public summary functions
    public :: df_describe_numeric

    ! Public shape/distribution functions
    public :: df_skewness_real, df_skewness_integer
    public :: df_kurtosis_real, df_kurtosis_integer

contains

    !========================================================================
    ! SUM FUNCTIONS
    !========================================================================

    !> Calculate sum of real column
    function df_sum_real(df, col_index) result(total)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: col_index
        real(rk) :: total

        real(rk), dimension(:), allocatable :: col
        type(column) :: data_col

        if (col_index < 1 .or. col_index > df%ncols()) error stop "column index out of range"

        data_col = df%get_data_col(col_index)
        if (data_col%get_type() /= REAL_NUM) error stop "column is not real type"

        col = data_col%getr()
        total = sum(col)
    end function df_sum_real

    !> Calculate sum of integer column
    function df_sum_integer(df, col_index) result(total)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: col_index
        integer(ik) :: total

        integer(ik), dimension(:), allocatable :: col
        type(column) :: data_col

        if (col_index < 1 .or. col_index > df%ncols()) error stop "column index out of range"

        data_col = df%get_data_col(col_index)
        if (data_col%get_type() /= INTEGER_NUM) error stop "column is not integer type"

        col = data_col%geti()
        total = sum(col)
    end function df_sum_integer

    !========================================================================
    ! MEAN FUNCTIONS
    !========================================================================

    !> Calculate the arithmetic mean of a real column
    !!
    !! Computes the average value of all elements in a real-valued column
    !!
    !! @param[in] df The data frame instance
    !! @param[in] col_index Index of the column (1-based)
    !! @return Mean value of the column
    function df_mean_real(df, col_index) result(avg)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: col_index
        real(rk) :: avg

        real(rk), dimension(:), allocatable :: col
        type(column) :: data_col

        if (col_index < 1 .or. col_index > df%ncols()) error stop "column index out of range"

        data_col = df%get_data_col(col_index)
        if (data_col%get_type() /= REAL_NUM) error stop "column is not real type"

        col = data_col%getr()
        avg = sum(col) / real(size(col), rk)
    end function df_mean_real

    !> Calculate the arithmetic mean of an integer column
    function df_mean_integer(df, col_index) result(avg)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: col_index
        real(rk) :: avg

        integer(ik), dimension(:), allocatable :: col
        type(column) :: data_col

        if (col_index < 1 .or. col_index > df%ncols()) error stop "column index out of range"

        data_col = df%get_data_col(col_index)
        if (data_col%get_type() /= INTEGER_NUM) error stop "column is not integer type"

        col = data_col%geti()
        avg = real(sum(col), rk) / real(size(col), rk)
    end function df_mean_integer

    !========================================================================
    ! STANDARD DEVIATION FUNCTIONS
    !========================================================================

    !> Calculate standard deviation of real column
    function df_std_real(df, col_index) result(stddev)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: col_index
        real(rk) :: stddev

        real(rk), dimension(:), allocatable :: col
        real(rk) :: avg
        type(column) :: data_col

        if (col_index < 1 .or. col_index > df%ncols()) error stop "column index out of range"

        data_col = df%get_data_col(col_index)
        if (data_col%get_type() /= REAL_NUM) error stop "column is not real type"

        col = data_col%getr()
        avg = df_mean_real(df, col_index)
        stddev = sqrt(sum((col - avg)**2) / real(size(col) - 1, rk))
    end function df_std_real

    !> Calculate standard deviation of integer column
    function df_std_integer(df, col_index) result(stddev)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: col_index
        real(rk) :: stddev

        integer(ik), dimension(:), allocatable :: col
        real(rk) :: avg
        type(column) :: data_col

        if (col_index < 1 .or. col_index > df%ncols()) error stop "column index out of range"

        data_col = df%get_data_col(col_index)
        if (data_col%get_type() /= INTEGER_NUM) error stop "column is not integer type"

        col = data_col%geti()
        avg = df_mean_integer(df, col_index)
        stddev = sqrt(sum((real(col, rk) - avg)**2) / real(size(col) - 1, rk))
    end function df_std_integer

    !========================================================================
    ! MEDIAN FUNCTIONS
    !========================================================================

    !> Calculate median of real column
    function df_median_real(df, col_index) result(med)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: col_index
        real(rk) :: med

        real(rk), dimension(:), allocatable :: col, sorted_col
        integer :: n, mid
        type(column) :: data_col

        if (col_index < 1 .or. col_index > df%ncols()) error stop "column index out of range"

        data_col = df%get_data_col(col_index)
        if (data_col%get_type() /= REAL_NUM) error stop "column is not real type"

        col = data_col%getr()
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
    end function df_median_real

    !> Calculate median of integer column
    function df_median_integer(df, col_index) result(med)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: col_index
        real(rk) :: med

        integer(ik), dimension(:), allocatable :: col, sorted_col
        integer :: n, mid
        type(column) :: data_col

        if (col_index < 1 .or. col_index > df%ncols()) error stop "column index out of range"

        data_col = df%get_data_col(col_index)
        if (data_col%get_type() /= INTEGER_NUM) error stop "column is not integer type"

        col = data_col%geti()
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
    end function df_median_integer

    !========================================================================
    ! PERCENTILE FUNCTIONS
    !========================================================================

    !> Calculate percentile of real column
    function df_percentile_real(df, col_index, percentile) result(perc)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: col_index
        real(rk), intent(in) :: percentile
        real(rk) :: perc

        real(rk), dimension(:), allocatable :: col, sorted_col
        integer :: n, idx
        real(rk) :: pos
        type(column) :: data_col

        if (percentile < 0.0_rk .or. percentile > 100.0_rk) error stop "percentile must be between 0 and 100"

        if (col_index < 1 .or. col_index > df%ncols()) error stop "column index out of range"

        data_col = df%get_data_col(col_index)
        if (data_col%get_type() /= REAL_NUM) error stop "column is not real type"

        col = data_col%getr()
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
    end function df_percentile_real

    !> Calculate percentile of integer column
    function df_percentile_integer(df, col_index, percentile) result(perc)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: col_index
        real(rk), intent(in) :: percentile
        real(rk) :: perc

        integer(ik), dimension(:), allocatable :: col, sorted_col
        integer :: n, idx
        real(rk) :: pos
        type(column) :: data_col

        if (percentile < 0.0_rk .or. percentile > 100.0_rk) error stop "percentile must be between 0 and 100"

        if (col_index < 1 .or. col_index > df%ncols()) error stop "column index out of range"

        data_col = df%get_data_col(col_index)
        if (data_col%get_type() /= INTEGER_NUM) error stop "column is not integer type"

        col = data_col%geti()
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
    end function df_percentile_integer

    !========================================================================
    ! VARIANCE FUNCTIONS
    !========================================================================

    !> Calculate variance of real column
    function df_variance_real(df, col_index) result(var)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: col_index
        real(rk) :: var

        real(rk), dimension(:), allocatable :: col
        real(rk) :: avg
        type(column) :: data_col

        if (col_index < 1 .or. col_index > df%ncols()) error stop "column index out of range"

        data_col = df%get_data_col(col_index)
        if (data_col%get_type() /= REAL_NUM) error stop "column is not real type"

        col = data_col%getr()
        avg = df_mean_real(df, col_index)
        var = sum((col - avg)**2) / real(size(col) - 1, rk)
    end function df_variance_real

    !> Calculate variance of integer column
    function df_variance_integer(df, col_index) result(var)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: col_index
        real(rk) :: var

        integer(ik), dimension(:), allocatable :: col
        real(rk) :: avg
        type(column) :: data_col

        if (col_index < 1 .or. col_index > df%ncols()) error stop "column index out of range"

        data_col = df%get_data_col(col_index)
        if (data_col%get_type() /= INTEGER_NUM) error stop "column is not integer type"

        col = data_col%geti()
        avg = df_mean_integer(df, col_index)
        var = sum((real(col, rk) - avg)**2) / real(size(col) - 1, rk)
    end function df_variance_integer

    !========================================================================
    ! MIN/MAX FUNCTIONS
    !========================================================================

    !> Find minimum value in real column
    function df_min_real(df, col_index) result(min_val)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: col_index
        real(rk) :: min_val

        real(rk), dimension(:), allocatable :: col
        type(column) :: data_col

        if (col_index < 1 .or. col_index > df%ncols()) error stop "column index out of range"

        data_col = df%get_data_col(col_index)
        if (data_col%get_type() /= REAL_NUM) error stop "column is not real type"

        col = data_col%getr()
        min_val = minval(col)
    end function df_min_real

    !> Find minimum value in integer column
    function df_min_integer(df, col_index) result(min_val)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: col_index
        integer(ik) :: min_val

        integer(ik), dimension(:), allocatable :: col
        type(column) :: data_col

        if (col_index < 1 .or. col_index > df%ncols()) error stop "column index out of range"

        data_col = df%get_data_col(col_index)
        if (data_col%get_type() /= INTEGER_NUM) error stop "column is not integer type"

        col = data_col%geti()
        min_val = minval(col)
    end function df_min_integer

    !> Find maximum value in real column
    function df_max_real(df, col_index) result(max_val)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: col_index
        real(rk) :: max_val

        real(rk), dimension(:), allocatable :: col
        type(column) :: data_col

        if (col_index < 1 .or. col_index > df%ncols()) error stop "column index out of range"

        data_col = df%get_data_col(col_index)
        if (data_col%get_type() /= REAL_NUM) error stop "column is not real type"

        col = data_col%getr()
        max_val = maxval(col)
    end function df_max_real

    !> Find maximum value in integer column
    function df_max_integer(df, col_index) result(max_val)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: col_index
        integer(ik) :: max_val

        integer(ik), dimension(:), allocatable :: col
        type(column) :: data_col

        if (col_index < 1 .or. col_index > df%ncols()) error stop "column index out of range"

        data_col = df%get_data_col(col_index)
        if (data_col%get_type() /= INTEGER_NUM) error stop "column is not integer type"

        col = data_col%geti()
        max_val = maxval(col)
    end function df_max_integer

    !========================================================================
    ! CORRELATION FUNCTIONS
    !========================================================================

    !> Calculate Pearson correlation coefficient between two real columns
    function df_correlation_real(df, col_index1, col_index2) result(corr)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: col_index1, col_index2
        real(rk) :: corr

        real(rk), dimension(:), allocatable :: col1, col2
        real(rk) :: mean1, mean2, std1, std2, covariance
        integer :: i, n
        type(column) :: data_col1, data_col2

        if (col_index1 < 1 .or. col_index1 > df%ncols()) error stop "column index 1 out of range"
        if (col_index2 < 1 .or. col_index2 > df%ncols()) error stop "column index 2 out of range"

        data_col1 = df%get_data_col(col_index1)
        data_col2 = df%get_data_col(col_index2)

        if (data_col1%get_type() /= REAL_NUM) error stop "column 1 is not real type"
        if (data_col2%get_type() /= REAL_NUM) error stop "column 2 is not real type"

        col1 = data_col1%getr()
        col2 = data_col2%getr()
        n = size(col1)

        if (n /= size(col2)) error stop "columns must have same length"

        mean1 = df_mean_real(df, col_index1)
        mean2 = df_mean_real(df, col_index2)
        std1 = df_std_real(df, col_index1)
        std2 = df_std_real(df, col_index2)

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
    end function df_correlation_real

    !========================================================================
    ! SUMMARY FUNCTIONS
    !========================================================================

    !> Print summary statistics for all numeric columns
    subroutine df_describe_numeric(df, unit)
        type(data_frame), intent(in) :: df
        integer, intent(in), optional :: unit

        integer :: out_unit, i, dtype
        character(len=25) :: col_name
        type(column) :: data_col

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

        do i = 1, df%ncols()
            data_col = df%get_data_col(i)
            dtype = data_col%get_type()

            ! Only process real and integer columns
            if (dtype == REAL_NUM .or. dtype == INTEGER_NUM) then
                if (df%get_with_headers()) then
                    col_name = trim(df%header(i))
                else
                    write (col_name, '(a,i0)') "Column ", i
                end if

                write (out_unit, '(a)') trim(col_name)
                write (out_unit, '(a)') repeat('-', len(trim(col_name)))

                if (dtype == REAL_NUM) then
                    write (out_unit, '(a,f12.4)') "  Count:       ", real(df%nrows(), rk)
                    write (out_unit, '(a,f12.4)') "  Mean:        ", df_mean_real(df, i)
                    write (out_unit, '(a,f12.4)') "  Std Dev:     ", df_std_real(df, i)
                    write (out_unit, '(a,f12.4)') "  Min:         ", df_min_real(df, i)
                    write (out_unit, '(a,f12.4)') "  25%:         ", df_percentile_real(df, i, 25.0_rk)
                    write (out_unit, '(a,f12.4)') "  Median (50%):", df_median_real(df, i)
                    write (out_unit, '(a,f12.4)') "  75%:         ", df_percentile_real(df, i, 75.0_rk)
                    write (out_unit, '(a,f12.4)') "  Max:         ", df_max_real(df, i)
                else ! INTEGER_NUM
                    write (out_unit, '(a,i12)') "  Count:       ", df%nrows()
                    write (out_unit, '(a,f12.2)') "  Mean:        ", df_mean_integer(df, i)
                    write (out_unit, '(a,f12.2)') "  Std Dev:     ", df_std_integer(df, i)
                    write (out_unit, '(a,i12)') "  Min:         ", df_min_integer(df, i)
                    write (out_unit, '(a,f12.2)') "  25%:         ", df_percentile_integer(df, i, 25.0_rk)
                    write (out_unit, '(a,f12.2)') "  Median (50%):", df_median_integer(df, i)
                    write (out_unit, '(a,f12.2)') "  75%:         ", df_percentile_integer(df, i, 75.0_rk)
                    write (out_unit, '(a,i12)') "  Max:         ", df_max_integer(df, i)
                end if

                write (out_unit, '(a)') ""
            end if
        end do

        write (out_unit, '(a)') repeat('=', 80)
    end subroutine df_describe_numeric

    !========================================================================
    ! SKEWNESS FUNCTIONS
    !========================================================================

    !> Calculate skewness (measure of asymmetry) of real column
    !!
    !! Skewness measures the asymmetry of the distribution:
    !! - skewness = 0: symmetric distribution
    !! - skewness > 0: right-skewed (tail on the right)
    !! - skewness < 0: left-skewed (tail on the left)
    !!
    !! Uses the sample skewness formula with bias correction (Fisher's moment coefficient)
    function df_skewness_real(df, col_index) result(skew)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: col_index
        real(rk) :: skew

        real(rk), dimension(:), allocatable :: col
        type(column) :: data_col
        real(rk) :: avg, std_dev, m3
        integer :: n, i

        if (col_index < 1 .or. col_index > df%ncols()) error stop "column index out of range"

        data_col = df%get_data_col(col_index)
        if (data_col%get_type() /= REAL_NUM) error stop "column is not real type"

        col = data_col%getr()
        n = size(col)

        if (n < 3) then
            skew = 0.0_rk
            return
        end if

        ! Calculate mean and standard deviation
        avg = sum(col) / real(n, rk)
        std_dev = sqrt(sum((col - avg)**2) / real(n - 1, rk))

        if (std_dev < epsilon(1.0_rk)) then
            skew = 0.0_rk
            return
        end if

        ! Calculate third moment
        m3 = sum(((col - avg) / std_dev)**3) / real(n, rk)

        ! Apply bias correction
        skew = m3 * sqrt(real(n * (n - 1), rk)) / real(n - 2, rk)
    end function df_skewness_real

    !> Calculate skewness of integer column
    function df_skewness_integer(df, col_index) result(skew)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: col_index
        real(rk) :: skew

        integer(ik), dimension(:), allocatable :: col
        type(column) :: data_col
        real(rk) :: avg, std_dev, m3
        integer :: n, i

        if (col_index < 1 .or. col_index > df%ncols()) error stop "column index out of range"

        data_col = df%get_data_col(col_index)
        if (data_col%get_type() /= INTEGER_NUM) error stop "column is not integer type"

        col = data_col%geti()
        n = size(col)

        if (n < 3) then
            skew = 0.0_rk
            return
        end if

        ! Calculate mean and standard deviation
        avg = sum(real(col, rk)) / real(n, rk)
        std_dev = sqrt(sum((real(col, rk) - avg)**2) / real(n - 1, rk))

        if (std_dev < epsilon(1.0_rk)) then
            skew = 0.0_rk
            return
        end if

        ! Calculate third moment
        m3 = sum(((real(col, rk) - avg) / std_dev)**3) / real(n, rk)

        ! Apply bias correction
        skew = m3 * sqrt(real(n * (n - 1), rk)) / real(n - 2, rk)
    end function df_skewness_integer

    !========================================================================
    ! KURTOSIS FUNCTIONS
    !========================================================================

    !> Calculate kurtosis (measure of tailedness) of real column
    !!
    !! Kurtosis measures the "tailedness" of the distribution:
    !! - kurtosis = 3: normal distribution (mesokurtic)
    !! - kurtosis > 3: heavy tails (leptokurtic)
    !! - kurtosis < 3: light tails (platykurtic)
    !!
    !! Returns excess kurtosis (kurtosis - 3) for easier interpretation
    !! Uses the sample kurtosis formula with bias correction
    function df_kurtosis_real(df, col_index) result(kurt)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: col_index
        real(rk) :: kurt

        real(rk), dimension(:), allocatable :: col
        type(column) :: data_col
        real(rk) :: avg, std_dev, m4
        integer :: n, i

        if (col_index < 1 .or. col_index > df%ncols()) error stop "column index out of range"

        data_col = df%get_data_col(col_index)
        if (data_col%get_type() /= REAL_NUM) error stop "column is not real type"

        col = data_col%getr()
        n = size(col)

        if (n < 4) then
            kurt = 0.0_rk
            return
        end if

        ! Calculate mean and standard deviation
        avg = sum(col) / real(n, rk)
        std_dev = sqrt(sum((col - avg)**2) / real(n - 1, rk))

        if (std_dev < epsilon(1.0_rk)) then
            kurt = 0.0_rk
            return
        end if

        ! Calculate fourth moment
        m4 = sum(((col - avg) / std_dev)**4) / real(n, rk)

        ! Apply bias correction and return excess kurtosis
        kurt = (real(n * (n + 1), rk) * m4 - 3.0_rk * real((n - 1)**2, rk)) / &
               real((n - 2) * (n - 3), rk)
    end function df_kurtosis_real

    !> Calculate kurtosis of integer column
    function df_kurtosis_integer(df, col_index) result(kurt)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: col_index
        real(rk) :: kurt

        integer(ik), dimension(:), allocatable :: col
        type(column) :: data_col
        real(rk) :: avg, std_dev, m4
        integer :: n, i

        if (col_index < 1 .or. col_index > df%ncols()) error stop "column index out of range"

        data_col = df%get_data_col(col_index)
        if (data_col%get_type() /= INTEGER_NUM) error stop "column is not integer type"

        col = data_col%geti()
        n = size(col)

        if (n < 4) then
            kurt = 0.0_rk
            return
        end if

        ! Calculate mean and standard deviation
        avg = sum(real(col, rk)) / real(n, rk)
        std_dev = sqrt(sum((real(col, rk) - avg)**2) / real(n - 1, rk))

        if (std_dev < epsilon(1.0_rk)) then
            kurt = 0.0_rk
            return
        end if

        ! Calculate fourth moment
        m4 = sum(((real(col, rk) - avg) / std_dev)**4) / real(n, rk)

        ! Apply bias correction and return excess kurtosis
        kurt = (real(n * (n + 1), rk) * m4 - 3.0_rk * real((n - 1)**2, rk)) / &
               real((n - 2) * (n - 3), rk)
    end function df_kurtosis_integer

end module datafort_statistics
