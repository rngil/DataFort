program advanced_example
    use datafort
    use precision
    implicit none

    type(data_frame) :: df, filtered_df, selected_df, sliced_df

    write(*,*) "======================================="
    write(*,*) "  DataFort Advanced Features Demo"
    write(*,*) "======================================="
    write(*,*)

    ! Create sample dataset
    call create_sample_data(df)

    write(*,*) "1. Original dataset:"
    call df%write_console()
    write(*,*)

    ! Statistical analysis
    write(*,*) "2. Statistical Summary:"
    write(*,'(a,f6.2,a,f6.2,a,f6.2)') "   Temperature: Min=", df%min_real(1), " Max=", df%max_real(1), " Mean=", df%mean_real(1)
    write(*,'(a,i0,a,i0,a,f6.2)') "   Humidity: Min=", df%min_integer(2), " Max=", df%max_integer(2), " Mean=", df%mean_integer(2)
    write(*,'(a,i0,a,i0,a,i0)') "   Pressure: Min=", df%min_integer(3), " Max=", df%max_integer(3), " Sum=", df%sum_integer(3)
    write(*,*)

    ! Column selection
    write(*,*) "3. Selecting Temperature and Location columns:"
    selected_df = df%select_columns([1, 5])
    call selected_df%write_console()
    write(*,*)

    ! Row slicing
    write(*,*) "4. Slicing rows 2-4:"
    sliced_df = df%slice_rows(2, 4)
    call sliced_df%write_console()
    write(*,*)

    ! Filtering
    write(*,*) "5. Filtering for sunny days only:"
    filtered_df = df%filter_rows_logical(4)
    call filtered_df%write_console()
    write(*,*)

    ! Sorting
    write(*,*) "6. Sorting by temperature (ascending):"
    call df%sort_by_column(1, .true.)
    call df%write_console()
    write(*,*)

    ! Data manipulation example
    write(*,*) "7. Creating temperature categories:"
    call add_temperature_categories(df)
    call df%write_console()
    write(*,*)

    ! Export results
    write(*,*) "8. Exporting sorted data to CSV..."
    call df%write_csv("advanced_weather_data.csv")
    write(*,*) "   Saved to 'advanced_weather_data.csv'"
    write(*,*)

    ! Clean up
    call df%destroy()
    call filtered_df%destroy()
    call selected_df%destroy()
    call sliced_df%destroy()

    write(*,*) "======================================="
    write(*,*) "  Advanced demo completed!"
    write(*,*) "======================================="

contains

    subroutine create_sample_data(df)
        type(data_frame), intent(inout) :: df
        real(rk), dimension(6) :: temperatures = [18.5_rk, 24.2_rk, 31.1_rk, 15.8_rk, 28.7_rk, 22.3_rk]
        integer(ik), dimension(6) :: humidity = [65_ik, 70_ik, 45_ik, 80_ik, 55_ik, 68_ik]
        integer(ik), dimension(6) :: pressure = [1015_ik, 1018_ik, 1022_ik, 1008_ik, 1020_ik, 1012_ik]
        logical, dimension(6) :: sunny = [.false., .true., .true., .false., .true., .false.]
        character(len=10), dimension(6) :: locations = ["London    ", "Madrid    ", "Phoenix   ", "Seattle   ", "Rome      ", "Paris     "]

        call df%new()
        call df%append(temperatures, "Temperature")
        call df%append(humidity, "Humidity")
        call df%append(pressure, "Pressure")
        call df%append(sunny, "Sunny")
        call df%append(locations, "Location")
    end subroutine

    subroutine add_temperature_categories(df)
        type(data_frame), intent(inout) :: df
        character(len=8), dimension(6) :: categories
        integer :: i

        ! Classify temperatures
        do i = 1, df%nrows()
            if (df%get_val_real(1, i) < 20.0_rk) then
                categories(i) = "Cold    "
            else if (df%get_val_real(1, i) < 25.0_rk) then
                categories(i) = "Mild    "
            else
                categories(i) = "Hot     "
            end if
        end do

        call df%append(categories, "Category")
    end subroutine

end program advanced_example