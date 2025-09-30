program test_math_transforms
    use datafort
    use precision
    implicit none

    type(data_frame) :: df
    real(rk), dimension(10) :: values
    integer(ik), dimension(10) :: int_values
    integer :: i
    logical :: sorted

    write (*, '(a)') ""
    write (*, '(a)') "Testing Mathematical Transformation Features"
    write (*, '(a)') "============================================"

    ! Create test data
    values = [1.234_rk, -2.567_rk, 3.891_rk, -4.123_rk, 5.678_rk, &
              6.234_rk, 7.890_rk, 8.123_rk, 9.456_rk, 10.789_rk]
    int_values = [1_ik, 5_ik, 3_ik, 7_ik, 2_ik, 9_ik, 4_ik, 6_ik, 8_ik, 10_ik]

    call df % new()
    call df % append(values, "Values")
    call df % append(int_values, "IntValues")

    write (*, '(a)') ""
    write (*, '(a)') "Original data:"
    call df % write_console()

    ! Test clip
    write (*, '(a)') ""
    write (*, '(a)') "Test 1: Clipping real values to [-3, 7]"
    call df % clip_real(1, -3.0_rk, 7.0_rk)
    call df % write_console()

    ! Test clip integer
    write (*, '(a)') ""
    write (*, '(a)') "Test 2: Clipping integer values to [3, 8]"
    call df % clip_integer(2, 3_ik, 8_ik)
    call df % write_console()

    ! Test is_sorted
    write (*, '(a)') ""
    write (*, '(a)') "Test 3: Check if integer column is sorted"
    sorted = df % is_sorted_integer(2, .true.)
    write (*, '(a,l1)') "   Is sorted (ascending): ", sorted

    ! Test round
    write (*, '(a)') ""
    write (*, '(a)') "Test 4: Round real values to 1 decimal place"
    call df % round_column(1, 1)
    call df % write_console()

    ! Reset data for math operations
    call df % destroy()
    call df % new()
    values = [1.0_rk, 2.0_rk, 3.0_rk, 4.0_rk, 5.0_rk, &
              6.0_rk, 7.0_rk, 8.0_rk, 9.0_rk, 10.0_rk]
    call df % append(values, "Values")

    ! Test log
    write (*, '(a)') ""
    write (*, '(a)') "Test 5: Natural logarithm"
    call df % log_column(1)
    call df % write_console()

    ! Reset for exp
    call df % destroy()
    call df % new()
    values = [0.0_rk, 0.5_rk, 1.0_rk, 1.5_rk, 2.0_rk, &
              2.5_rk, 3.0_rk, 3.5_rk, 4.0_rk, 4.5_rk]
    call df % append(values, "Values")

    ! Test exp
    write (*, '(a)') ""
    write (*, '(a)') "Test 6: Exponential"
    call df % exp_column(1)
    call df % write_console()

    ! Reset for sqrt
    call df % destroy()
    call df % new()
    values = [1.0_rk, 4.0_rk, 9.0_rk, 16.0_rk, 25.0_rk, &
              36.0_rk, 49.0_rk, 64.0_rk, 81.0_rk, 100.0_rk]
    call df % append(values, "Values")

    ! Test sqrt
    write (*, '(a)') ""
    write (*, '(a)') "Test 7: Square root"
    call df % sqrt_column(1)
    call df % write_console()

    ! Reset for power
    call df % destroy()
    call df % new()
    values = [1.0_rk, 2.0_rk, 3.0_rk, 4.0_rk, 5.0_rk, &
              6.0_rk, 7.0_rk, 8.0_rk, 9.0_rk, 10.0_rk]
    call df % append(values, "Values")

    ! Test pow
    write (*, '(a)') ""
    write (*, '(a)') "Test 8: Raise to power 2"
    call df % pow_column(1, 2.0_rk)
    call df % write_console()

    call df % destroy()

    write (*, '(a)') ""
    write (*, '(a)') "All mathematical transformation tests completed!"
    write (*, '(a)') ""

end program test_math_transforms
