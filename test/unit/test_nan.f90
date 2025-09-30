program test_nan
    use datafort
    use precision
    implicit none

    type(data_frame) :: df, clean_df
    real(rk), dimension(6) :: temps
    integer(ik), dimension(6) :: counts
    character(len=10), dimension(6) :: labels
    logical, dimension(:), allocatable :: nan_mask
    integer :: i

    write(*,'(a)') ""
    write(*,'(a)') "Testing NaN Handling"
    write(*,'(a)') "===================="

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

    call df%new()
    call df%append(temps, "Temperature")
    call df%append(counts, "Count")
    call df%append(labels, "Label")

    write(*,'(a)') ""
    write(*,'(a)') "Original data with NaN values:"
    call df%write_console()

    ! Test isna_real
    write(*,'(a)') ""
    write(*,'(a)') "Test 1: Check which temperature values are NaN"
    nan_mask = df%isna_real(1)
    write(*,'(a)') "   NaN mask for Temperature column:"
    do i = 1, size(nan_mask)
        write(*,'(a,i0,a,l1)') "   Row ", i, ": ", nan_mask(i)
    end do

    ! Test isna_integer
    write(*,'(a)') ""
    write(*,'(a)') "Test 2: Check which count values are NaN"
    nan_mask = df%isna_integer(2)
    write(*,'(a)') "   NaN mask for Count column:"
    do i = 1, size(nan_mask)
        write(*,'(a,i0,a,l1)') "   Row ", i, ": ", nan_mask(i)
    end do

    ! Test fillna_real
    write(*,'(a)') ""
    write(*,'(a)') "Test 3: Fill NaN in Temperature with 0.0"
    call df%fillna_real(1, 0.0_rk)
    call df%write_console()

    ! Test fillna_integer
    write(*,'(a)') ""
    write(*,'(a)') "Test 4: Fill NaN in Count with 0"
    call df%fillna_integer(2, 0_ik)
    call df%write_console()

    ! Recreate data with NaN for dropna test
    call df%destroy()
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

    call df%new()
    call df%append(temps, "Temperature")
    call df%append(counts, "Count")
    call df%append(labels, "Label")

    write(*,'(a)') ""
    write(*,'(a)') "Test 5: Drop all rows containing NaN"
    write(*,'(a)') "Original:"
    call df%write_console()

    clean_df = df%dropna()
    write(*,'(a)') ""
    write(*,'(a)') "After dropna():"
    call clean_df%write_console()
    write(*,'(a,i0,a,i0)') "   Dropped ", df%nrows() - clean_df%nrows(), " rows out of ", df%nrows()

    ! Clean up
    call df%destroy()
    call clean_df%destroy()

    write(*,'(a)') ""
    write(*,'(a)') "All NaN handling tests completed!"
    write(*,'(a)') ""

end program test_nan