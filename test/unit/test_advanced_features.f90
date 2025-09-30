program test_advanced_features
    use datafort
    use precision
    implicit none

    logical :: all_tests_passed = .true.

    write (*, *) "Testing Advanced DataFort Features"
    write (*, *) "==================================="

    call test_statistical_functions()
    call test_column_manipulation()
    call test_advanced_filtering()
    call test_transpose()

    if (all_tests_passed) then
        write (*, *) "All advanced feature tests passed!"
    else
        write (*, *) "Some advanced feature tests failed!"
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

    subroutine test_statistical_functions()
        type(data_frame) :: df
        real(rk), dimension(5) :: temps = [15.0_rk, 20.0_rk, 25.0_rk, 30.0_rk, 35.0_rk]
        integer(ik), dimension(5) :: scores = [60_ik, 70_ik, 80_ik, 90_ik, 100_ik]

        write (*, *) "Testing statistical functions..."

        call df % new()
        call df % append(temps, "Temperature")
        call df % append(scores, "Score")

        ! Test median functions
        call assert_approx_equal(df % median_real(1), 25.0_rk, 0.01_rk, "Real median")
        call assert_approx_equal(df % median_integer(2), 80.0_rk, 0.01_rk, "Integer median")

        ! Test percentile functions
        call assert_approx_equal(df % percentile_real(1, 50.0_rk), 25.0_rk, 0.01_rk, "Real 50th percentile")
        call assert_approx_equal(df % percentile_real(1, 25.0_rk), 20.0_rk, 1.0_rk, "Real 25th percentile")
        call assert_approx_equal(df % percentile_integer(2, 75.0_rk), 90.0_rk, 1.0_rk, "Integer 75th percentile")

        ! Test variance functions
        call assert_approx_equal(df % variance_real(1), 62.5_rk, 1.0_rk, "Real variance")
        call assert_approx_equal(df % variance_integer(2), 250.0_rk, 1.0_rk, "Integer variance")

        call df % destroy()
    end subroutine

    subroutine test_column_manipulation()
        type(data_frame) :: df
        real(rk), dimension(3) :: temps = [20.0_rk, 25.0_rk, 30.0_rk]
        integer(ik), dimension(3) :: humidity = [60_ik, 70_ik, 80_ik]
        character(len=8), dimension(3) :: locations = ["New York", "London  ", "Tokyo   "]
        integer, dimension(3) :: new_order = [2, 1, 3]

        write (*, *) "Testing column manipulation..."

        call df % new()
        call df % append(temps, "Temperature")
        call df % append(humidity, "Humidity")
        call df % append(locations, "Location")

        ! Test renaming
        call df % rename_column(1, "Temp_C")
        call assert_true(df % header(1) == "Temp_C", "Column rename")

        ! Test reordering
        call assert_true(df % header(1) == "Temp_C", "Before reorder - first column")
        call assert_true(df % header(2) == "Humidity", "Before reorder - second column")

        call df % reorder_columns(new_order)
        call assert_true(df % header(1) == "Humidity", "After reorder - first column")
        call assert_true(df % header(2) == "Temp_C", "After reorder - second column")

        ! Test dropping
        call df % drop_column(1)  ! Remove humidity (now first column)
        call assert_true(df % ncols() == 2, "Column count after drop")
        call assert_true(df % header(1) == "Temp_C", "First column after drop")

        call df % destroy()
    end subroutine

    subroutine test_advanced_filtering()
        type(data_frame) :: df, filtered_df
        real(rk), dimension(5) :: temps = [15.0_rk, 20.0_rk, 25.0_rk, 30.0_rk, 35.0_rk]
        integer(ik), dimension(5) :: scores = [60_ik, 70_ik, 80_ik, 90_ik, 100_ik]
        character(len=8), dimension(5) :: cities = ["Boston  ", "New York", "Chicago ", "Miami   ", "Phoenix "]

        write (*, *) "Testing advanced filtering..."

        call df % new()
        call df % append(temps, "Temperature")
        call df % append(scores, "Score")
        call df % append(cities, "City")

        ! Test real range filtering
        filtered_df = df % filter_rows_real_range(1, 20.0_rk, 30.0_rk)
        call assert_true(filtered_df % nrows() == 3, "Real range filter row count")
        call assert_approx_equal(filtered_df % get_val_real(1, 1), 20.0_rk, 0.01_rk, "Real range filter first value")

        call filtered_df % destroy()

        ! Test integer range filtering
        filtered_df = df % filter_rows_integer_range(2, 75_ik, 95_ik)
        call assert_true(filtered_df % nrows() == 2, "Integer range filter row count")
        call assert_true(filtered_df % get_val_integer(2, 1) == 80_ik, "Integer range filter first value")

        call filtered_df % destroy()

        ! Test string pattern filtering
        filtered_df = df % filter_rows_string_pattern(3, "i")  ! Cities containing "i"
        call assert_true(filtered_df % nrows() == 3, "String pattern filter row count")  ! Chicago, Miami, Phoenix

        call filtered_df % destroy()
        call df % destroy()
    end subroutine

    subroutine test_transpose()
        type(data_frame) :: df, transposed_df
        real(rk), dimension(2) :: temps = [20.0_rk, 25.0_rk]
        integer(ik), dimension(2) :: humidity = [60_ik, 70_ik]

        write (*, *) "Testing transpose..."

        call df % new()
        call df % append(temps, "Temperature")
        call df % append(humidity, "Humidity")

        transposed_df = df % transpose()

        ! In transpose: columns become rows, so we should have 3 columns (Headers, Row_1, Row_2)
        ! and 2 rows (Temperature, Humidity)
        call assert_true(transposed_df % ncols() == 3, "Transposed columns count")
        call assert_true(transposed_df % nrows() == 2, "Transposed rows count")

        call df % destroy()
        call transposed_df % destroy()
    end subroutine

end program test_advanced_features
