program test_nan
    use datafort
    use precision
    implicit none

    type(data_frame) :: df, clean_df
    real(rk), dimension(6) :: temps
    integer(ik), dimension(6) :: counts
    character(len=10), dimension(6) :: labels
    logical, dimension(:), allocatable :: nan_mask
    integer :: i, num_failed

    num_failed = 0

    write (*, '(a)') ""
    write (*, '(a)') "Testing NaN Handling"
    write (*, '(a)') "===================="

    ! Initialize NaN constant
    call init_nan()

    ! Create data with NaN values
    temps(1) = 20.5_rk
    temps(2) = NaN_rk
    temps(3) = 22.3_rk
    temps(4) = 25.1_rk
    temps(5) = NaN_rk
    temps(6) = 21.8_rk

    counts(1) = 10_ik
    counts(2) = 15_ik
    counts(3) = NaN_ik
    counts(4) = 20_ik
    counts(5) = 25_ik
    counts(6) = NaN_ik

    labels = ["A     ", "B     ", "C     ", "D     ", "E     ", "F     "]

    call df % new()
    call df_append_real(df, temps, "Temperature")
    call df_append_integer(df, counts, "Count")
    call df_append_character(df, labels, "Label")

    write (*, '(a)') ""
    write (*, '(a)') "Original data with NaN values:"
    call df_write_console(df)

    ! Test isna_real
    write (*, '(a)') ""
    write (*, '(a)') "Test 1: Check which temperature values are NaN"
    nan_mask = df_isna_real(df, 1)
    write (*, '(a)') "   NaN mask for Temperature column:"
    do i = 1, size(nan_mask)
        write (*, '(a,i0,a,l1)') "   Row ", i, ": ", nan_mask(i)
    end do
    ! Check expected NaN positions (rows 2 and 5)
    call assert_logical(nan_mask(1), .false., "isna_real row 1", num_failed)
    call assert_logical(nan_mask(2), .true., "isna_real row 2", num_failed)
    call assert_logical(nan_mask(3), .false., "isna_real row 3", num_failed)
    call assert_logical(nan_mask(4), .false., "isna_real row 4", num_failed)
    call assert_logical(nan_mask(5), .true., "isna_real row 5", num_failed)
    call assert_logical(nan_mask(6), .false., "isna_real row 6", num_failed)

    ! Test isna_integer
    write (*, '(a)') ""
    write (*, '(a)') "Test 2: Check which count values are NaN"
    nan_mask = df_isna_integer(df, 2)
    write (*, '(a)') "   NaN mask for Count column:"
    do i = 1, size(nan_mask)
        write (*, '(a,i0,a,l1)') "   Row ", i, ": ", nan_mask(i)
    end do
    ! Check expected NaN positions (rows 3 and 6)
    call assert_logical(nan_mask(1), .false., "isna_integer row 1", num_failed)
    call assert_logical(nan_mask(2), .false., "isna_integer row 2", num_failed)
    call assert_logical(nan_mask(3), .true., "isna_integer row 3", num_failed)
    call assert_logical(nan_mask(4), .false., "isna_integer row 4", num_failed)
    call assert_logical(nan_mask(5), .false., "isna_integer row 5", num_failed)
    call assert_logical(nan_mask(6), .true., "isna_integer row 6", num_failed)

    ! Test fillna_real
    write (*, '(a)') ""
    write (*, '(a)') "Test 3: Fill NaN in Temperature with 0.0"
    call df_fillna_real(df, 1, 0.0_rk)
    call df_write_console(df)
    ! Verify NaN values were filled
    nan_mask = df_isna_real(df, 1)
    call assert_logical(all(.not. nan_mask), .true., "fillna_real removed all NaN", num_failed)
    call assert_real_equal(df_get_val_real(df, 2, 1), 0.0_rk, "fillna_real row 2 value", num_failed)
    call assert_real_equal(df_get_val_real(df, 5, 1), 0.0_rk, "fillna_real row 5 value", num_failed)

    ! Test fillna_integer
    write (*, '(a)') ""
    write (*, '(a)') "Test 4: Fill NaN in Count with 0"
    call df_fillna_integer(df, 2, 0_ik)
    call df_write_console(df)
    ! Verify NaN values were filled
    nan_mask = df_isna_integer(df, 2)
    call assert_logical(all(.not. nan_mask), .true., "fillna_integer removed all NaN", num_failed)
    call assert_int_equal(df_get_val_integer(df, 3, 2), 0_ik, "fillna_integer row 3 value", num_failed)
    call assert_int_equal(df_get_val_integer(df, 6, 2), 0_ik, "fillna_integer row 6 value", num_failed)

    ! Recreate data with NaN for dropna test
    call df % destroy()
    temps(1) = 20.5_rk
    temps(2) = NaN_rk
    temps(3) = 22.3_rk
    temps(4) = 25.1_rk
    temps(5) = NaN_rk
    temps(6) = 21.8_rk

    counts(1) = 10_ik
    counts(2) = 15_ik
    counts(3) = NaN_ik
    counts(4) = 20_ik
    counts(5) = 25_ik
    counts(6) = NaN_ik

    call df % new()
    call df_append_real(df, temps, "Temperature")
    call df_append_integer(df, counts, "Count")
    call df_append_character(df, labels, "Label")

    write (*, '(a)') ""
    write (*, '(a)') "Test 5: Drop all rows containing NaN"
    write (*, '(a)') "Original:"
    call df_write_console(df)

    clean_df = df_dropna(df)
    write (*, '(a)') ""
    write (*, '(a)') "After dropna():"
    call df_write_console(clean_df)
    write (*, '(a,i0,a,i0)') "   Dropped ", df % nrows() - clean_df % nrows(), " rows out of ", df % nrows()
    ! Verify correct number of rows dropped (rows 2, 3, 5, 6 have NaN)
    call assert_int_equal(clean_df % nrows(), 2, "dropna() result row count", num_failed)
    ! Verify remaining rows have no NaN
    nan_mask = df_isna_real(clean_df, 1)
    call assert_logical(all(.not. nan_mask), .true., "dropna() removed all real NaN", num_failed)
    nan_mask = df_isna_integer(clean_df, 2)
    call assert_logical(all(.not. nan_mask), .true., "dropna() removed all integer NaN", num_failed)

    ! Clean up
    call df % destroy()
    call clean_df % destroy()

    write (*, '(a)') ""
    write (*, '(a)') "===================="
    if (num_failed == 0) then
        write (*, '(a)') "All tests PASSED!"
    else
        write (*, '(a,i0,a)') "FAILED: ", num_failed, " test(s) failed"
        error stop 1
    end if
    write (*, '(a)') ""

contains

    subroutine assert_logical(actual, expected, test_name, num_failed)
        logical, intent(in) :: actual, expected
        character(len=*), intent(in) :: test_name
        integer, intent(inout) :: num_failed

        if (actual .eqv. expected) then
            write (*, '(a,a)') "   PASS: ", trim(test_name)
        else
            write (*, '(a,a,a,l1,a,l1)') "   FAIL: ", trim(test_name), " - Got ", actual, " Expected ", expected
            num_failed = num_failed + 1
        end if
    end subroutine assert_logical

    subroutine assert_real_equal(actual, expected, test_name, num_failed)
        real(rk), intent(in) :: actual, expected
        character(len=*), intent(in) :: test_name
        integer, intent(inout) :: num_failed
        real(rk) :: tolerance = 1.0e-6_rk

        if (abs(actual - expected) <= tolerance) then
            write (*, '(a,a)') "   PASS: ", trim(test_name)
        else
            write (*, '(a,a,a,f12.6,a,f12.6)') "   FAIL: ", trim(test_name), " - Got ", actual, " Expected ", expected
            num_failed = num_failed + 1
        end if
    end subroutine assert_real_equal

    subroutine assert_int_equal(actual, expected, test_name, num_failed)
        integer(ik), intent(in) :: actual, expected
        character(len=*), intent(in) :: test_name
        integer, intent(inout) :: num_failed

        if (actual == expected) then
            write (*, '(a,a)') "   PASS: ", trim(test_name)
        else
            write (*, '(a,a,a,i0,a,i0)') "   FAIL: ", trim(test_name), " - Got ", actual, " Expected ", expected
            num_failed = num_failed + 1
        end if
    end subroutine assert_int_equal

end program test_nan
