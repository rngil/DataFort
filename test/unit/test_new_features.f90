program test_new_features
    use datafort
    use precision
    implicit none

    logical :: all_tests_passed = .true.

    write (*, *) "Testing New DataFort Features"
    write (*, *) "==============================="

    call test_min_max_functions()
    call test_column_selection()
    call test_row_slicing()
    call test_filtering()
    call test_sorting()
    call test_copying()

    if (all_tests_passed) then
        write (*, *) "All new feature tests passed!"
    else
        write (*, *) "Some new feature tests failed!"
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

    subroutine test_min_max_functions()
        type(data_frame) :: df
        real(rk), dimension(5) :: temps = [15.5_rk, 22.3_rk, 18.7_rk, 25.1_rk, 19.9_rk]
        integer(ik), dimension(5) :: scores = [85_ik, 92_ik, 78_ik, 96_ik, 88_ik]

        write (*, *) "Testing min/max functions..."

        call df % new()
        call df_append_real(df, temps, "Temperature")
        call df_append_integer(df, scores, "Score")

        call assert_true(abs(df_min_real(df, 1) - 15.5_rk) < 1e-6_rk, "Min real value")
        call assert_true(abs(df_max_real(df, 1) - 25.1_rk) < 1e-6_rk, "Max real value")
        call assert_true(df_min_integer(df, 2) == 78_ik, "Min integer value")
        call assert_true(df_max_integer(df, 2) == 96_ik, "Max integer value")

        call df % destroy()
    end subroutine

    subroutine test_column_selection()
        type(data_frame) :: df, selected_df
        real(rk), dimension(3) :: temps = [20.0_rk, 22.5_rk, 25.0_rk]
        integer(ik), dimension(3) :: pressures = [1010_ik, 1015_ik, 1020_ik]
        logical, dimension(3) :: sunny = [.true., .false., .true.]
        integer, dimension(2) :: selected_cols = [1, 3]

        write (*, *) "Testing column selection..."

        call df % new()
        call df_append_real(df, temps, "Temperature")
        call df_append_integer(df, pressures, "Pressure")
        call df_append_logical(df, sunny, "Sunny")

        selected_df = df_select_columns(df, selected_cols)

        call assert_true(selected_df % ncols() == 2, "Selected columns count")
        call assert_true(selected_df % nrows() == 3, "Selected rows count")
        call assert_true(selected_df % header(1) == "Temperature", "First selected header")
        call assert_true(selected_df % header(2) == "Sunny", "Second selected header")

        call df % destroy()
        call selected_df % destroy()
    end subroutine

    subroutine test_row_slicing()
        type(data_frame) :: df, sliced_df
        real(rk), dimension(5) :: temps = [20.0_rk, 22.5_rk, 25.0_rk, 18.0_rk, 24.0_rk]
        integer(ik), dimension(5) :: pressures = [1010_ik, 1015_ik, 1020_ik, 1005_ik, 1018_ik]

        write (*, *) "Testing row slicing..."

        call df % new()
        call df_append_real(df, temps, "Temperature")
        call df_append_integer(df, pressures, "Pressure")

        sliced_df = df_slice_rows(df, 2, 4)

        call assert_true(sliced_df % ncols() == 2, "Sliced columns count")
        call assert_true(sliced_df % nrows() == 3, "Sliced rows count")
        call assert_true(abs(df_get_val_real(sliced_df, 1, 1) - 22.5_rk) < 1e-6_rk, "First sliced value")
        call assert_true(df_get_val_integer(sliced_df, 3, 2) == 1005_ik, "Last sliced value")

        call df % destroy()
        call sliced_df % destroy()
    end subroutine

    subroutine test_filtering()
        type(data_frame) :: df, filtered_df
        real(rk), dimension(4) :: temps = [20.0_rk, 25.0_rk, 18.0_rk, 23.0_rk]
        logical, dimension(4) :: hot_days = [.false., .true., .false., .true.]

        write (*, *) "Testing logical filtering..."

        call df % new()
        call df_append_real(df, temps, "Temperature")
        call df_append_logical(df, hot_days, "HotDay")

        filtered_df = df_filter_rows_logical(df, 2)

        call assert_true(filtered_df % ncols() == 2, "Filtered columns count")
        call assert_true(filtered_df % nrows() == 2, "Filtered rows count")
        call assert_true(abs(df_get_val_real(filtered_df, 1, 1) - 25.0_rk) < 1e-6_rk, "First filtered temp")
        call assert_true(abs(df_get_val_real(filtered_df, 2, 1) - 23.0_rk) < 1e-6_rk, "Second filtered temp")

        call df % destroy()
        call filtered_df % destroy()
    end subroutine

    subroutine test_sorting()
        type(data_frame) :: df
        real(rk), dimension(4) :: temps = [25.0_rk, 18.0_rk, 23.0_rk, 20.0_rk]
        character(len=8), dimension(4) :: cities = ["Austin  ", "Boston  ", "Chicago ", "Denver  "]

        write (*, *) "Testing sorting..."

        call df % new()
        call df_append_real(df, temps, "Temperature")
        call df_append_character(df, cities, "City")

        ! Sort by temperature (ascending)
        call df_sort_by_column(df, 1, .true.)

        call assert_true(abs(df_get_val_real(df, 1, 1) - 18.0_rk) < 1e-6_rk, "Sorted first temp")
        call assert_true(abs(df_get_val_real(df, 4, 1) - 25.0_rk) < 1e-6_rk, "Sorted last temp")
        call assert_true(trim(df_get_val_character(df, 1, 2)) == "Boston", "Sorted first city")
        call assert_true(trim(df_get_val_character(df, 4, 2)) == "Austin", "Sorted last city")

        call df % destroy()
    end subroutine

    subroutine test_copying()
        type(data_frame) :: df, df_copied
        real(rk), dimension(3) :: temps = [20.0_rk, 22.5_rk, 25.0_rk]
        integer(ik), dimension(3) :: scores = [85_ik, 90_ik, 95_ik]

        write (*, *) "Testing data frame copying..."

        call df % new()
        call df_append_real(df, temps, "Temperature")
        call df_append_integer(df, scores, "Score")

        df_copied = df_copy(df)

        call assert_true(df_copied % ncols() == df % ncols(), "Copy columns count")
        call assert_true(df_copied % nrows() == df % nrows(), "Copy rows count")
        call assert_true(df_copied % header(1) == df % header(1), "Copy first header")
        call assert_true(df_copied % header(2) == df % header(2), "Copy second header")

        ! Test that they're independent
        call df_set_val_real(df, 1, 1, 99.0_rk)
        call assert_true(abs(df_get_val_real(df_copied, 1, 1) - 20.0_rk) < 1e-6_rk, "Copy independence")

        call df % destroy()
        call df_copied % destroy()
    end subroutine

end program test_new_features
