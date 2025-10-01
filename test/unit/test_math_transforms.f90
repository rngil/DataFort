program test_math_transforms
    use datafort
    use precision
    implicit none

    type(data_frame) :: df
    real(rk), dimension(10) :: values, expected_real
    integer(ik), dimension(10) :: int_values, expected_int
    integer :: i, num_failed
    logical :: sorted
    real(rk) :: val, tolerance
    integer(ik) :: ival

    tolerance = 1.0e-6_rk
    num_failed = 0

    write (*, '(a)') ""
    write (*, '(a)') "Testing Mathematical Transformation Features"
    write (*, '(a)') "============================================"

    ! Create test data
    values = [1.234_rk, -2.567_rk, 3.891_rk, -4.123_rk, 5.678_rk, &
              6.234_rk, 7.890_rk, 8.123_rk, 9.456_rk, 10.789_rk]
    int_values = [1_ik, 5_ik, 3_ik, 7_ik, 2_ik, 9_ik, 4_ik, 6_ik, 8_ik, 10_ik]

    call df % new()
    call df_append_real(df, values, "Values")
    call df_append_integer(df, int_values, "IntValues")

    write (*, '(a)') ""
    write (*, '(a)') "Original data:"
    call df_write_console(df)

    ! Test clip
    write (*, '(a)') ""
    write (*, '(a)') "Test 1: Clipping real values to [-3, 7]"
    call df_clip_real(df, 1, -3.0_rk, 7.0_rk)
    call df_write_console(df)
    expected_real = [1.234_rk, -2.567_rk, 3.891_rk, -3.0_rk, 5.678_rk, &
                     6.234_rk, 7.0_rk, 7.0_rk, 7.0_rk, 7.0_rk]
    call assert_real_column(df, 1, expected_real, "clip_real", num_failed)

    ! Test clip integer
    write (*, '(a)') ""
    write (*, '(a)') "Test 2: Clipping integer values to [3, 8]"
    call df_clip_integer(df, 2, 3_ik, 8_ik)
    call df_write_console(df)
    expected_int = [3_ik, 5_ik, 3_ik, 7_ik, 3_ik, 8_ik, 4_ik, 6_ik, 8_ik, 8_ik]
    call assert_int_column(df, 2, expected_int, "clip_integer", num_failed)

    ! Test is_sorted
    write (*, '(a)') ""
    write (*, '(a)') "Test 3: Check if integer column is sorted"
    sorted = df_is_sorted_integer(df, 2, .true.)
    write (*, '(a,l1)') "   Is sorted (ascending): ", sorted
    if (.not. sorted) then
        write (*, '(a)') "   PASS: Column is not sorted as expected"
    else
        write (*, '(a)') "   FAIL: Column should not be sorted"
        num_failed = num_failed + 1
    end if

    ! Test round
    write (*, '(a)') ""
    write (*, '(a)') "Test 4: Round real values to 1 decimal place"
    call df_round_column(df, 1, 1)
    call df_write_console(df)
    expected_real = [1.2_rk, -2.6_rk, 3.9_rk, -3.0_rk, 5.7_rk, &
                     6.2_rk, 7.0_rk, 7.0_rk, 7.0_rk, 7.0_rk]
    call assert_real_column(df, 1, expected_real, "round_column", num_failed)

    ! Reset data for math operations
    call df % destroy()
    call df % new()
    values = [1.0_rk, 2.0_rk, 3.0_rk, 4.0_rk, 5.0_rk, &
              6.0_rk, 7.0_rk, 8.0_rk, 9.0_rk, 10.0_rk]
    call df_append_real(df, values, "Values")

    ! Test log
    write (*, '(a)') ""
    write (*, '(a)') "Test 5: Natural logarithm"
    call df_log_column(df, 1)
    call df_write_console(df)
    expected_real = [log(1.0_rk), log(2.0_rk), log(3.0_rk), log(4.0_rk), log(5.0_rk), &
                     log(6.0_rk), log(7.0_rk), log(8.0_rk), log(9.0_rk), log(10.0_rk)]
    call assert_real_column(df, 1, expected_real, "log_column", num_failed)

    ! Reset for exp
    call df % destroy()
    call df % new()
    values = [0.0_rk, 0.5_rk, 1.0_rk, 1.5_rk, 2.0_rk, &
              2.5_rk, 3.0_rk, 3.5_rk, 4.0_rk, 4.5_rk]
    call df_append_real(df, values, "Values")

    ! Test exp
    write (*, '(a)') ""
    write (*, '(a)') "Test 6: Exponential"
    call df_exp_column(df, 1)
    call df_write_console(df)
    expected_real = [exp(0.0_rk), exp(0.5_rk), exp(1.0_rk), exp(1.5_rk), exp(2.0_rk), &
                     exp(2.5_rk), exp(3.0_rk), exp(3.5_rk), exp(4.0_rk), exp(4.5_rk)]
    call assert_real_column(df, 1, expected_real, "exp_column", num_failed)

    ! Reset for sqrt
    call df % destroy()
    call df % new()
    values = [1.0_rk, 4.0_rk, 9.0_rk, 16.0_rk, 25.0_rk, &
              36.0_rk, 49.0_rk, 64.0_rk, 81.0_rk, 100.0_rk]
    call df_append_real(df, values, "Values")

    ! Test sqrt
    write (*, '(a)') ""
    write (*, '(a)') "Test 7: Square root"
    call df_sqrt_column(df, 1)
    call df_write_console(df)
    expected_real = [1.0_rk, 2.0_rk, 3.0_rk, 4.0_rk, 5.0_rk, &
                     6.0_rk, 7.0_rk, 8.0_rk, 9.0_rk, 10.0_rk]
    call assert_real_column(df, 1, expected_real, "sqrt_column", num_failed)

    ! Reset for power
    call df % destroy()
    call df % new()
    values = [1.0_rk, 2.0_rk, 3.0_rk, 4.0_rk, 5.0_rk, &
              6.0_rk, 7.0_rk, 8.0_rk, 9.0_rk, 10.0_rk]
    call df_append_real(df, values, "Values")

    ! Test pow
    write (*, '(a)') ""
    write (*, '(a)') "Test 8: Raise to power 2"
    call df_pow_column(df, 1, 2.0_rk)
    call df_write_console(df)
    expected_real = [1.0_rk, 4.0_rk, 9.0_rk, 16.0_rk, 25.0_rk, &
                     36.0_rk, 49.0_rk, 64.0_rk, 81.0_rk, 100.0_rk]
    call assert_real_column(df, 1, expected_real, "pow_column", num_failed)

    ! Reset for apply_to_column
    call df % destroy()
    call df % new()
    values = [1.0_rk, 2.0_rk, 3.0_rk, 4.0_rk, 5.0_rk, &
              6.0_rk, 7.0_rk, 8.0_rk, 9.0_rk, 10.0_rk]
    call df_append_real(df, values, "Values")

    ! Test apply_to_column
    write (*, '(a)') ""
    write (*, '(a)') "Test 9: Apply custom function (x^2 + 2*x + 1)"
    call df_apply_to_column(df, 1, quadratic_func)
    call df_write_console(df)
    expected_real = [4.0_rk, 9.0_rk, 16.0_rk, 25.0_rk, 36.0_rk, &
                     49.0_rk, 64.0_rk, 81.0_rk, 100.0_rk, 121.0_rk]
    call assert_real_column(df, 1, expected_real, "apply_to_column", num_failed)

    call df % destroy()

    write (*, '(a)') ""
    write (*, '(a)') "============================================"
    if (num_failed == 0) then
        write (*, '(a)') "All tests PASSED!"
    else
        write (*, '(a,i0,a)') "FAILED: ", num_failed, " test(s) failed"
        error stop 1
    end if
    write (*, '(a)') ""

contains

    ! Custom function for apply_to_column test
    pure function quadratic_func(x) result(y)
        real(rk), intent(in) :: x
        real(rk) :: y
        y = x**2 + 2.0_rk*x + 1.0_rk
    end function quadratic_func

    ! Assert helper for real columns
    subroutine assert_real_column(df, col_idx, expected, test_name, num_failed)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: col_idx
        real(rk), dimension(:), intent(in) :: expected
        character(len=*), intent(in) :: test_name
        integer, intent(inout) :: num_failed

        real(rk) :: val
        integer :: i
        logical :: passed

        passed = .true.
        do i = 1, size(expected)
            val = df_get_val_real(df, i, col_idx)
            if (abs(val - expected(i)) > tolerance) then
                write (*, '(a,a,a,i0,a,f12.6,a,f12.6)') &
                    "   FAIL [", trim(test_name), "] Row ", i, ": Got ", val, ", Expected ", expected(i)
                passed = .false.
            end if
        end do

        if (passed) then
            write (*, '(a,a,a)') "   PASS: ", trim(test_name), " produced correct results"
        else
            num_failed = num_failed + 1
        end if
    end subroutine assert_real_column

    ! Assert helper for integer columns
    subroutine assert_int_column(df, col_idx, expected, test_name, num_failed)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: col_idx
        integer(ik), dimension(:), intent(in) :: expected
        character(len=*), intent(in) :: test_name
        integer, intent(inout) :: num_failed

        integer(ik) :: val
        integer :: i
        logical :: passed

        passed = .true.
        do i = 1, size(expected)
            val = df_get_val_integer(df, i, col_idx)
            if (val /= expected(i)) then
                write (*, '(a,a,a,i0,a,i0,a,i0)') &
                    "   FAIL [", trim(test_name), "] Row ", i, ": Got ", val, ", Expected ", expected(i)
                passed = .false.
            end if
        end do

        if (passed) then
            write (*, '(a,a,a)') "   PASS: ", trim(test_name), " produced correct results"
        else
            num_failed = num_failed + 1
        end if
    end subroutine assert_int_column

end program test_math_transforms
