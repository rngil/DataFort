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
    call test_new_statistical_functions()

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
        call df_append_real(df, temps, "Temperature")
        call df_append_integer(df, scores, "Score")

        ! Test median functions
        call assert_approx_equal(df_median_real(df, 1), 25.0_rk, 0.01_rk, "Real median")
        call assert_approx_equal(df_median_integer(df, 2), 80.0_rk, 0.01_rk, "Integer median")

        ! Test percentile functions
        call assert_approx_equal(df_percentile_real(df, 1, 50.0_rk), 25.0_rk, 0.01_rk, "Real 50th percentile")
        call assert_approx_equal(df_percentile_real(df, 1, 25.0_rk), 20.0_rk, 1.0_rk, "Real 25th percentile")
        call assert_approx_equal(df_percentile_integer(df, 2, 75.0_rk), 90.0_rk, 1.0_rk, "Integer 75th percentile")

        ! Test variance functions
        call assert_approx_equal(df_variance_real(df, 1), 62.5_rk, 1.0_rk, "Real variance")
        call assert_approx_equal(df_variance_integer(df, 2), 250.0_rk, 1.0_rk, "Integer variance")

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
        call df_append_real(df, temps, "Temperature")
        call df_append_integer(df, humidity, "Humidity")
        call df_append_character(df, locations, "Location")

        ! Test renaming
        call df_rename_column(df, 1, "Temp_C")
        call assert_true(df % header(1) == "Temp_C", "Column rename")

        ! Test reordering
        call assert_true(df % header(1) == "Temp_C", "Before reorder - first column")
        call assert_true(df % header(2) == "Humidity", "Before reorder - second column")

        call df_reorder_columns(df, new_order)
        call assert_true(df % header(1) == "Humidity", "After reorder - first column")
        call assert_true(df % header(2) == "Temp_C", "After reorder - second column")

        ! Test dropping
        call df_drop_column(df, 1)  ! Remove humidity (now first column)
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
        call df_append_real(df, temps, "Temperature")
        call df_append_integer(df, scores, "Score")
        call df_append_character(df, cities, "City")

        ! Test real range filtering
        filtered_df = df_filter_rows_real_range(df, 1, 20.0_rk, 30.0_rk)
        call assert_true(filtered_df % nrows() == 3, "Real range filter row count")
        call assert_approx_equal(df_get_val_real(filtered_df, 1, 1), 20.0_rk, 0.01_rk, "Real range filter first value")

        call filtered_df % destroy()

        ! Test integer range filtering
        filtered_df = df_filter_rows_integer_range(df, 2, 75_ik, 95_ik)
        call assert_true(filtered_df % nrows() == 2, "Integer range filter row count")
        call assert_true(df_get_val_integer(filtered_df, 1, 2) == 80_ik, "Integer range filter first value")

        call filtered_df % destroy()

        ! Test string pattern filtering
        filtered_df = df_filter_rows_string_pattern(df, 3, "i")  ! Cities containing "i"
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
        call df_append_real(df, temps, "Temperature")
        call df_append_integer(df, humidity, "Humidity")

        transposed_df = df_transpose(df)

        ! In transpose: columns become rows, so we should have 3 columns (Headers, Row_1, Row_2)
        ! and 2 rows (Temperature, Humidity)
        call assert_true(transposed_df % ncols() == 3, "Transposed columns count")
        call assert_true(transposed_df % nrows() == 2, "Transposed rows count")

        call df % destroy()
        call transposed_df % destroy()
    end subroutine

    subroutine test_new_statistical_functions()
        type(data_frame) :: df, nlargest_df, nsmallest_df
        real(rk), dimension(10) :: values = [1.0_rk, 2.0_rk, 3.0_rk, 4.0_rk, 5.0_rk, &
                                              6.0_rk, 7.0_rk, 8.0_rk, 9.0_rk, 10.0_rk]
        integer(ik), dimension(10) :: counts = [10_ik, 20_ik, 30_ik, 40_ik, 50_ik, &
                                                 60_ik, 70_ik, 80_ik, 90_ik, 100_ik]
        real(rk), dimension(7) :: skewed = [-10.0_rk, -5.0_rk, 0.0_rk, 1.0_rk, 2.0_rk, 3.0_rk, 20.0_rk]
        integer(ik), dimension(7) :: skewed_int = [-10_ik, -5_ik, 0_ik, 1_ik, 2_ik, 3_ik, 20_ik]
        character(len=10), dimension(5) :: names = ["Alice     ", "Bob       ", "Alice     ", &
                                                     "Charlie   ", "Bob       "]
        real(rk) :: skew_val, kurt_val
        integer :: n_unique

        write (*, *) "Testing new statistical functions..."

        ! Test skewness on symmetric data (should be near 0)
        call df % new()
        call df_append_real(df, values, "Values")
        call df_append_integer(df, counts, "Counts")

        skew_val = df_skewness_real(df, 1)
        call assert_approx_equal(skew_val, 0.0_rk, 0.1_rk, "Real skewness (symmetric)")

        skew_val = df_skewness_integer(df, 2)
        call assert_approx_equal(skew_val, 0.0_rk, 0.1_rk, "Integer skewness (symmetric)")

        call df % destroy()

        ! Test skewness on skewed data (should be positive for right skew)
        call df % new()
        call df_append_real(df, skewed, "Skewed")
        call df_append_integer(df, skewed_int, "SkewedInt")

        skew_val = df_skewness_real(df, 1)
        call assert_true(skew_val > 0.0_rk, "Real skewness (right skewed)")

        skew_val = df_skewness_integer(df, 2)
        call assert_true(skew_val > 0.0_rk, "Integer skewness (right skewed)")

        call df % destroy()

        ! Test kurtosis on symmetric data
        call df % new()
        call df_append_real(df, values, "Values")
        call df_append_integer(df, counts, "Counts")

        kurt_val = df_kurtosis_real(df, 1)
        ! Excess kurtosis for uniform-like distribution should be negative
        call assert_true(kurt_val < 0.0_rk, "Real kurtosis (platykurtic)")

        kurt_val = df_kurtosis_integer(df, 2)
        call assert_true(kurt_val < 0.0_rk, "Integer kurtosis (platykurtic)")

        call df % destroy()

        ! Test nunique functions
        call df % new()
        call df_append_real(df, [1.0_rk, 2.0_rk, 1.0_rk, 3.0_rk, 2.0_rk], "Real")
        call df_append_integer(df, [10_ik, 20_ik, 10_ik, 30_ik, 30_ik], "Integer")
        call df_append_character(df, names, "Names")

        n_unique = df_nunique_real(df, 1)
        call assert_true(n_unique == 3, "Real nunique count")

        n_unique = df_nunique_integer(df, 2)
        call assert_true(n_unique == 3, "Integer nunique count")

        n_unique = df_nunique_character(df, 3)
        call assert_true(n_unique == 3, "Character nunique count")

        call df % destroy()

        ! Test nlargest/nsmallest functions
        call df % new()
        call df_append_real(df, [5.0_rk, 2.0_rk, 8.0_rk, 1.0_rk, 9.0_rk], "Values")
        call df_append_integer(df, [50_ik, 20_ik, 80_ik, 10_ik, 90_ik], "Counts")

        ! Test nlargest for real
        nlargest_df = df_nlargest_real(df, 3, 1)
        call assert_true(nlargest_df % nrows() == 3, "Nlargest real row count")
        call assert_approx_equal(df_get_val_real(nlargest_df, 1, 1), 9.0_rk, 0.01_rk, &
                                  "Nlargest real first value")
        call assert_approx_equal(df_get_val_real(nlargest_df, 2, 1), 8.0_rk, 0.01_rk, &
                                  "Nlargest real second value")
        call nlargest_df % destroy()

        ! Test nsmallest for real
        nsmallest_df = df_nsmallest_real(df, 2, 1)
        call assert_true(nsmallest_df % nrows() == 2, "Nsmallest real row count")
        call assert_approx_equal(df_get_val_real(nsmallest_df, 1, 1), 1.0_rk, 0.01_rk, &
                                  "Nsmallest real first value")
        call nsmallest_df % destroy()

        ! Test nlargest for integer
        nlargest_df = df_nlargest_integer(df, 3, 2)
        call assert_true(nlargest_df % nrows() == 3, "Nlargest integer row count")
        call assert_true(df_get_val_integer(nlargest_df, 1, 2) == 90_ik, &
                         "Nlargest integer first value")
        call nlargest_df % destroy()

        ! Test nsmallest for integer
        nsmallest_df = df_nsmallest_integer(df, 2, 2)
        call assert_true(nsmallest_df % nrows() == 2, "Nsmallest integer row count")
        call assert_true(df_get_val_integer(nsmallest_df, 1, 2) == 10_ik, &
                         "Nsmallest integer first value")
        call nsmallest_df % destroy()

        call df % destroy()
    end subroutine

end program test_advanced_features
