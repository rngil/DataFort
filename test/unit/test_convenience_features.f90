program test_convenience_features
    use datafort
    use precision
    implicit none

    logical :: all_tests_passed = .true.

    write (*, *) "Testing Convenience Features"
    write (*, *) "============================="

    call test_to_array()
    call test_equals()
    call test_pipe()
    call test_isin()
    call test_insert_column()

    if (all_tests_passed) then
        write (*, *) "All convenience feature tests passed!"
    else
        write (*, *) "Some convenience feature tests failed!"
        stop 1
    end if

contains

    subroutine assert_true(condition, test_name)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: test_name

        if (condition) then
            write (*, *) "PASS: ", test_name
        else
            write (*, *) "FAIL: ", test_name
            all_tests_passed = .false.
        end if
    end subroutine

    subroutine assert_approx_equal(val1, val2, tolerance, test_name)
        real(rk), intent(in) :: val1, val2, tolerance
        character(len=*), intent(in) :: test_name

        if (abs(val1 - val2) < tolerance) then
            write (*, *) "PASS: ", test_name
        else
            write (*, *) "FAIL: ", test_name, " Expected:", val2, " Got:", val1
            all_tests_passed = .false.
        end if
    end subroutine

    subroutine test_to_array()
        type(data_frame) :: df
        real(rk), dimension(3) :: temps = [20.0_rk, 25.0_rk, 30.0_rk]
        integer(ik), dimension(3) :: counts = [10_ik, 15_ik, 20_ik]
        real(rk), dimension(:, :), allocatable :: array

        write (*, *) "Testing to_array() function..."

        call df%new()
        call df_append_real(df, temps, "Temperature")
        call df_append_integer(df, counts, "Count")

        array = df_to_array_real(df)

        call assert_true(size(array, 1) == 3, "Array rows")
        call assert_true(size(array, 2) == 2, "Array cols")
        call assert_approx_equal(array(1, 1), 20.0_rk, 0.01_rk, "Array[1,1]")
        call assert_approx_equal(array(2, 1), 25.0_rk, 0.01_rk, "Array[2,1]")
        call assert_approx_equal(array(1, 2), 10.0_rk, 0.01_rk, "Array[1,2] (int converted to real)")

        deallocate (array)
        call df%destroy()
    end subroutine

    subroutine test_equals()
        type(data_frame) :: df1, df2, df3
        real(rk), dimension(3) :: temps = [20.0_rk, 25.0_rk, 30.0_rk]
        integer(ik), dimension(3) :: counts = [10_ik, 15_ik, 20_ik]

        write (*, *) "Testing equals() function..."

        ! Create first dataframe
        call df1%new()
        call df_append_real(df1, temps, "Temperature")
        call df_append_integer(df1, counts, "Count")

        ! Create identical dataframe
        call df2%new()
        call df_append_real(df2, temps, "Temperature")
        call df_append_integer(df2, counts, "Count")

        ! Create different dataframe
        call df3%new()
        call df_append_real(df3, temps, "Temperature")
        call df_append_integer(df3, [10_ik, 15_ik, 21_ik], "Count")  ! Different value

        call assert_true(df_equals(df1, df2), "Identical dataframes")
        call assert_true(.not. df_equals(df1, df3), "Different dataframes")

        call df1%destroy()
        call df2%destroy()
        call df3%destroy()
    end subroutine

    subroutine test_pipe()
        type(data_frame) :: df, result_df
        real(rk), dimension(5) :: values = [1.0_rk, 2.0_rk, 3.0_rk, 4.0_rk, 5.0_rk]

        write (*, *) "Testing pipe() function..."

        call df%new()
        call df_append_real(df, values, "Values")

        ! Use pipe to get head(3)
        result_df = df_pipe(df, head_wrapper)

        call assert_true(result_df%nrows() == 3, "Pipe result rows")
        call assert_approx_equal(df_get_val_real(result_df, 1, 1), 1.0_rk, 0.01_rk, "Pipe result value")

        call df%destroy()
        call result_df%destroy()
    end subroutine

    ! Helper function for pipe test
    function head_wrapper(df_in) result(df_out)
        type(data_frame), intent(in) :: df_in
        type(data_frame) :: df_out
        df_out = df_head(df_in, 3)
    end function

    subroutine test_isin()
        type(data_frame) :: df
        real(rk), dimension(5) :: temps = [20.0_rk, 25.0_rk, 30.0_rk, 25.0_rk, 35.0_rk]
        integer(ik), dimension(5) :: counts = [10_ik, 15_ik, 20_ik, 15_ik, 25_ik]
        character(len=10), dimension(5) :: cities = ["New York  ", "London    ", "Tokyo     ", &
                                                      "London    ", "Paris     "]
        logical, dimension(:), allocatable :: mask

        write (*, *) "Testing isin() functions..."

        call df%new()
        call df_append_real(df, temps, "Temperature")
        call df_append_integer(df, counts, "Count")
        call df_append_character(df, cities, "City")

        ! Test isin_real
        mask = df_isin_real(df, 1, [25.0_rk, 35.0_rk])
        call assert_true(count(mask) == 3, "Isin real count")  ! rows 2, 4, 5
        call assert_true(mask(2), "Isin real - row 2")
        call assert_true(mask(4), "Isin real - row 4")
        call assert_true(mask(5), "Isin real - row 5")
        deallocate (mask)

        ! Test isin_integer
        mask = df_isin_integer(df, 2, [15_ik, 25_ik])
        call assert_true(count(mask) == 3, "Isin integer count")  ! rows 2, 4, 5
        call assert_true(mask(2), "Isin integer - row 2")
        deallocate (mask)

        ! Test isin_character
        mask = df_isin_character(df, 3, ["London", "Paris "])
        call assert_true(count(mask) == 3, "Isin character count")  ! rows 2, 4, 5
        call assert_true(mask(2), "Isin character - row 2 (London)")
        call assert_true(mask(5), "Isin character - row 5 (Paris)")
        deallocate (mask)

        call df%destroy()
    end subroutine

    subroutine test_insert_column()
        type(data_frame) :: df
        real(rk), dimension(3) :: temps = [20.0_rk, 25.0_rk, 30.0_rk]
        integer(ik), dimension(3) :: new_ints = [100_ik, 200_ik, 300_ik]

        write (*, *) "Testing insert_column() functions..."

        ! Create initial dataframe
        call df%new()
        call df_append_real(df, temps, "Temperature")

        ! Insert integer column at position 1 (before Temperature)
        call df_insert_column_integer(df, new_ints, 1, "ID")

        call assert_true(df%ncols() == 2, "Insert column count")
        call assert_true(df%header(1) == "ID", "Insert first header")
        call assert_true(df%header(2) == "Temperature", "Insert second header")
        call assert_true(df_get_val_integer(df, 1, 1) == 100_ik, "Insert value check")

        ! Insert another column at position 2 (between ID and Temperature)
        call df_insert_column_real(df, [1.5_rk, 2.5_rk, 3.5_rk], 2, "Factor")

        call assert_true(df%ncols() == 3, "Insert second column count")
        call assert_true(df%header(2) == "Factor", "Insert middle header")
        call assert_approx_equal(df_get_val_real(df, 1, 2), 1.5_rk, 0.01_rk, "Insert middle value")

        call df%destroy()
    end subroutine

end program test_convenience_features
