program test_row_functions
    use datafort
    use precision
    implicit none

    type(data_frame) :: df
    real(rk), dimension(5) :: col1 = [1.0_rk, 2.0_rk, 3.0_rk, 4.0_rk, 5.0_rk]
    real(rk), dimension(5) :: col2 = [10.0_rk, 20.0_rk, 30.0_rk, 40.0_rk, 50.0_rk]
    integer(ik), dimension(5) :: col3 = [100_ik, 200_ik, 300_ik, 400_ik, 500_ik]
    real(rk) :: row_result
    real(rk), dimension(:), allocatable :: all_results
    integer :: i

    ! Create dataframe
    call df % new()
    call df % append(col1, "A")
    call df % append(col2, "B")
    call df % append(col3, "C")

    print *, "Original DataFrame:"
    call df % write_console()
    print *

    ! Apply function to single row
    print *, "Row 3 sum:", df % apply_to_row_real(3, row_sum)
    print *, "Row 3 mean:", df % apply_to_row_real(3, row_mean)
    print *, "Row 3 max:", df % apply_to_row_real(3, row_max)
    print *

    ! Apply function to all rows
    print *, "All row sums:"
    all_results = df % apply_to_all_rows_real(row_sum)
    do i = 1, size(all_results)
        print *, "  Row", i, ":", all_results(i)
    end do
    print *

    print *, "All row means:"
    all_results = df % apply_to_all_rows_real(row_mean)
    do i = 1, size(all_results)
        print *, "  Row", i, ":", all_results(i)
    end do

    call df % destroy()

contains

    ! Example function: sum of row values
    function row_sum(values, n) result(s)
        real(rk), dimension(:), intent(in) :: values
        integer, intent(in) :: n
        real(rk) :: s
        s = sum(values(1:n))
    end function row_sum

    ! Example function: mean of row values
    function row_mean(values, n) result(m)
        real(rk), dimension(:), intent(in) :: values
        integer, intent(in) :: n
        real(rk) :: m
        m = sum(values(1:n)) / real(n, rk)
    end function row_mean

    ! Example function: max of row values
    function row_max(values, n) result(mx)
        real(rk), dimension(:), intent(in) :: values
        integer, intent(in) :: n
        real(rk) :: mx
        mx = maxval(values(1:n))
    end function row_max

end program test_row_functions
