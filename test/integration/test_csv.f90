program test_csv
    use datafort
    use precision
    implicit none

    type(data_frame) :: df_read
    logical :: all_tests_passed = .true.

    write (*, *) "Testing CSV I/O functionality"
    write (*, *) "==============================="

    ! Test reading the CSV file we just created
    call df_read_csv(df_read, "test/fixtures/advanced_weather_data.csv", .true.)

    write (*, *) "CSV data loaded successfully!"
    write (*, *) "Columns:", df_read % ncols(), " Rows:", df_read % nrows()
    write (*, *)

    call df_write_console(df_read)
    write (*, *)

    ! Verify some data
    call assert_true(df_read % ncols() == 6, "Number of columns")
    call assert_true(df_read % nrows() == 6, "Number of rows")
    call assert_true(df_read % header(1) == "Temperature", "First header")
    call assert_true(df_read % header(6) == "Category", "Last header")

    ! Test data values
    call assert_true(abs(df_get_val_real(df_read, 1, 1) - 15.8_rk) < 0.1_rk, "First temperature")
    call assert_true(df_get_val_integer(df_read, 1, 2) == 80_ik, "First humidity")
    call assert_true(df_get_val_logical(df_read, 1, 4) .eqv. .false., "First sunny value")
    call assert_true(trim(df_get_val_character(df_read, 1, 5)) == "Seattle", "First location")
    call assert_true(trim(df_get_val_character(df_read, 1, 6)) == "Cold", "First category")

    call df_read % destroy()

    if (all_tests_passed) then
        write (*, *) "All CSV tests passed!"
    else
        write (*, *) "Some CSV tests failed!"
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

end program test_csv
