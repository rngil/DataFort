program main
    use datafort
    use precision
    implicit none

    type(data_frame) :: df
    real(rk), dimension(5) :: temperatures = [23.1_rk, 25.3_rk, 24.8_rk, 22.9_rk, 26.2_rk]
    integer(ik), dimension(5) :: pressures = [1013_ik, 1015_ik, 1012_ik, 1010_ik, 1018_ik]
    logical, dimension(5) :: is_sunny = [.true., .false., .true., .false., .true.]
    character(len=10), dimension(5) :: locations = ["New York  ", "London    ", "Tokyo     ", "Sydney    ", "Berlin    "]

    write(*,*) "==================================="
    write(*,*) "  DataFort Complete Example"
    write(*,*) "==================================="
    write(*,*)

    ! Create data frame
    call df%new()
    write(*,*) "1. Creating data frame and adding columns..."

    ! Add columns with headers
    call df%append(temperatures, "Temperature")
    call df%append(pressures, "Pressure")
    call df%append(is_sunny, "Sunny")
    call df%append(locations, "Location")

    write(*,*) "   Added", df%ncols(), "columns with", df%nrows(), "rows"
    write(*,*)

    ! Display the data frame
    write(*,*) "2. Data frame contents:"
    call df%write_console()
    write(*,*)

    ! Demonstrate mathematical operations
    write(*,*) "3. Mathematical operations:"
    write(*,'(a,f8.2)') "   Average temperature: ", df%mean_real(1)
    write(*,'(a,f8.2)') "   Temperature std dev: ", df%std_real(1)
    write(*,'(a,f8.2)') "   Total temperature:   ", df%sum_real(1)
    write(*,*)
    write(*,'(a,f8.2)') "   Average pressure:    ", df%mean_integer(2)
    write(*,'(a,i0)')   "   Total pressure:      ", df%sum_integer(2)
    write(*,*)

    ! Demonstrate single value access
    write(*,*) "4. Individual value access:"
    write(*,'(a,f6.1)') "   Temperature in row 3: ", df%get_val_real(1, 3)
    write(*,'(a,i0)')   "   Pressure in row 2:    ", df%get_val_integer(2, 2)
    write(*,'(a,l1)')   "   Sunny in row 1:       ", df%get_val_logical(3, 1)
    write(*,'(a,a)')    "   Location in row 4:    ", trim(df%get_val_character(4, 4))
    write(*,*)

    ! Modify some values
    write(*,*) "5. Modifying values:"
    call df%set_val_real(1, 1, 30.0_rk)
    call df%set_val_logical(3, 2, .true.)
    write(*,*) "   Changed temperature in row 1 to 30.0"
    write(*,*) "   Changed sunny status in row 2 to True"
    write(*,*)

    ! Display updated data frame
    write(*,*) "6. Updated data frame:"
    call df%write_console()
    write(*,*)

    ! Save to CSV
    write(*,*) "7. Saving to CSV file..."
    call df%write_csv("weather_data.csv")
    write(*,*) "   Saved to 'weather_data.csv'"
    write(*,*)

    ! Clean up
    call df%destroy()
    write(*,*) "8. Data frame cleaned up successfully!"
    write(*,*)

    write(*,*) "==================================="
    write(*,*) "  Example completed successfully!"
    write(*,*) "==================================="

end program main