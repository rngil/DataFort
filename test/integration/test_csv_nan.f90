program test_csv_nan
    use datafort
    use precision
    implicit none

    type(data_frame) :: df, clean_df
    logical, dimension(:), allocatable :: nan_mask_temp, nan_mask_press
    integer :: i

    write (*, '(a)') ""
    write (*, '(a)') "Testing CSV Reading with NaN Values"
    write (*, '(a)') "===================================="

    ! Read CSV file with NaN values
    call df_read_csv(df, "test/fixtures/test_data_with_nan.csv", .true.)

    write (*, '(a)') ""
    write (*, '(a)') "Data loaded from CSV:"
    call df_write_console(df)

    ! Check for NaN in Temperature column
    write (*, '(a)') ""
    write (*, '(a)') "NaN values in Temperature column:"
    nan_mask_temp = df_isna_real(df, 1)
    do i = 1, size(nan_mask_temp)
        if (nan_mask_temp(i)) then
            write (*, '(a,i0)') "   Row ", i, " has NaN"
        end if
    end do

    ! Check for NaN in Pressure column
    write (*, '(a)') ""
    write (*, '(a)') "NaN values in Pressure column:"
    nan_mask_press = df_isna_integer(df, 2)
    do i = 1, size(nan_mask_press)
        if (nan_mask_press(i)) then
            write (*, '(a,i0)') "   Row ", i, " has NaN"
        end if
    end do

    ! Test fillna
    write (*, '(a)') ""
    write (*, '(a)') "Filling NaN values:"
    write (*, '(a)') "  Temperature NaN -> 0.0"
    write (*, '(a)') "  Pressure NaN -> 1000"
    call df_fillna_real(df, 1, 0.0_rk)
    call df_fillna_integer(df, 2, 1000_ik)
    call df_write_console(df)

    ! Test dropna with original data
    call df % destroy()
    call df_read_csv(df, "test/fixtures/test_data_with_nan.csv", .true.)

    write (*, '(a)') ""
    write (*, '(a)') "Using dropna() to remove rows with any NaN:"
    clean_df = df_dropna(df)
    call df_write_console(clean_df)
    write (*, '(a,i0,a,i0,a)') "Kept ", clean_df % nrows(), " out of ", df % nrows(), " rows"

    ! Clean up
    call df % destroy()
    call clean_df % destroy()

    write (*, '(a)') ""
    write (*, '(a)') "CSV NaN handling test completed!"
    write (*, '(a)') ""

end program test_csv_nan
