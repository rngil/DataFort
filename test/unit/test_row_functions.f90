program test_row_functions
    use datafort
    use precision
    implicit none

    type(data_frame) :: df
    real(rk), dimension(5) :: col1 = [1.0_rk, 2.0_rk, 3.0_rk, 4.0_rk, 5.0_rk]
    real(rk), dimension(5) :: col2 = [10.0_rk, 20.0_rk, 30.0_rk, 40.0_rk, 50.0_rk]
    integer(ik), dimension(5) :: col3 = [100_ik, 200_ik, 300_ik, 400_ik, 500_ik]
    real(rk) :: row_result, tolerance
    real(rk), dimension(:), allocatable :: all_results
    integer :: i, num_failed

    tolerance = 1.0e-6_rk
    num_failed = 0

    ! Create dataframe
    call df % new()
    call df % append(col1, "A")
    call df % append(col2, "B")
    call df % append(col3, "C")

    print *, ""
    print *, "Testing Row Functions"
    print *, "====================="
    print *, ""
    print *, "Original DataFrame:"
    call df % write_console()
    print *

    ! Apply function to single row
    print *, "Test 1: Apply to single row"
    print *, "----------------------------"
    row_result = df % apply_to_row_real(3, row_sum)
    print *, "Row 3 sum:", row_result
    call assert_real(row_result, 333.0_rk, "Row 3 sum", num_failed)

    row_result = df % apply_to_row_real(3, row_mean)
    print *, "Row 3 mean:", row_result
    call assert_real(row_result, 111.0_rk, "Row 3 mean", num_failed)

    row_result = df % apply_to_row_real(3, row_max)
    print *, "Row 3 max:", row_result
    call assert_real(row_result, 300.0_rk, "Row 3 max", num_failed)
    print *

    ! Apply function to all rows
    print *, "Test 2: Apply to all rows (sum)"
    print *, "--------------------------------"
    all_results = df % apply_to_all_rows_real(row_sum)
    do i = 1, size(all_results)
        print *, "  Row", i, ":", all_results(i)
    end do
    call assert_real(all_results(1), 111.0_rk, "All rows sum - row 1", num_failed)
    call assert_real(all_results(2), 222.0_rk, "All rows sum - row 2", num_failed)
    call assert_real(all_results(3), 333.0_rk, "All rows sum - row 3", num_failed)
    call assert_real(all_results(4), 444.0_rk, "All rows sum - row 4", num_failed)
    call assert_real(all_results(5), 555.0_rk, "All rows sum - row 5", num_failed)
    print *

    print *, "Test 3: Apply to all rows (mean)"
    print *, "---------------------------------"
    all_results = df % apply_to_all_rows_real(row_mean)
    do i = 1, size(all_results)
        print *, "  Row", i, ":", all_results(i)
    end do
    call assert_real(all_results(1), 37.0_rk, "All rows mean - row 1", num_failed)
    call assert_real(all_results(2), 74.0_rk, "All rows mean - row 2", num_failed)
    call assert_real(all_results(3), 111.0_rk, "All rows mean - row 3", num_failed)
    call assert_real(all_results(4), 148.0_rk, "All rows mean - row 4", num_failed)
    call assert_real(all_results(5), 185.0_rk, "All rows mean - row 5", num_failed)

    call df % destroy()

    print *
    print *, "====================="
    if (num_failed == 0) then
        print *, "All tests PASSED!"
    else
        print *, "FAILED:", num_failed, "test(s) failed"
        error stop 1
    end if
    print *

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

    ! Assert helper for real values
    subroutine assert_real(actual, expected, test_name, num_failed)
        real(rk), intent(in) :: actual, expected
        character(len=*), intent(in) :: test_name
        integer, intent(inout) :: num_failed

        if (abs(actual - expected) <= tolerance) then
            print *, "   PASS:", trim(test_name)
        else
            print *, "   FAIL:", trim(test_name), "- Got", actual, "Expected", expected
            num_failed = num_failed + 1
        end if
    end subroutine assert_real

end program test_row_functions
