program new_features_demo
    use datafort
    use precision
    implicit none

    type(data_frame) :: df, filtered_df, transposed_df
    integer :: i

    write(*,*) "======================================="
    write(*,*) "  DataFort New Features Demo"
    write(*,*) "======================================="
    write(*,*)

    ! Create sample dataset with more variety
    call create_extended_sample_data(df)

    write(*,*) "1. Original extended dataset:"
    call df%write_console()
    write(*,*)

    ! Enhanced statistical analysis
    write(*,*) "2. Enhanced Statistical Summary:"
    write(*,'(a,f8.2)') "   Temperature - Mean: ", df%mean_real(1)
    write(*,'(a,f8.2)') "   Temperature - Median: ", df%median_real(1)
    write(*,'(a,f8.2)') "   Temperature - Std Dev: ", df%std_real(1)
    write(*,'(a,f8.2)') "   Temperature - Variance: ", df%variance_real(1)
    write(*,'(a,f8.2)') "   Temperature - 25th percentile: ", df%percentile_real(1, 25.0_rk)
    write(*,'(a,f8.2)') "   Temperature - 75th percentile: ", df%percentile_real(1, 75.0_rk)
    write(*,*)

    write(*,'(a,f8.2)') "   Humidity - Mean: ", df%mean_integer(2)
    write(*,'(a,f8.2)') "   Humidity - Median: ", df%median_integer(2)
    write(*,'(a,f8.2)') "   Humidity - Variance: ", df%variance_integer(2)
    write(*,*)

    ! Column manipulation demonstrations
    write(*,*) "3. Column Manipulation:"
    write(*,*) "   Renaming Temperature to Temp_C..."
    call df%rename_column(1, "Temp_C")
    write(*,'(a,a)') "   New first column name: ", df%header(1)
    write(*,*)

    write(*,*) "   Reordering columns: [Location, Temp_C, Humidity, Pressure, Sunny]"
    call df%reorder_columns([5, 1, 2, 3, 4])
    write(*,*) "   Columns after reordering:"
    do i = 1, df%ncols()
        write(*,'(a,i0,a,a)') "   ", i, ": ", df%header(i)
    end do
    write(*,*)

    ! Advanced filtering demonstrations
    write(*,*) "4. Advanced Filtering Examples:"

    write(*,*) "   a) Temperature range filter (20-30°C):"
    filtered_df = df%filter_rows_real_range(2, 20.0_rk, 30.0_rk)  ! Temp_C is now column 2
    call filtered_df%write_console()
    write(*,*)
    call filtered_df%destroy()

    write(*,*) "   b) Humidity range filter (60-80%):"
    filtered_df = df%filter_rows_integer_range(3, 60_ik, 80_ik)  ! Humidity is now column 3
    call filtered_df%write_console()
    write(*,*)
    call filtered_df%destroy()

    write(*,*) "   c) Location pattern filter (contains 'o'):"
    filtered_df = df%filter_rows_string_pattern(1, "o")  ! Location is now column 1
    call filtered_df%write_console()
    write(*,*)
    call filtered_df%destroy()

    ! Transpose demonstration
    write(*,*) "5. Data Transpose:"
    write(*,*) "   Creating smaller dataset for transpose demo..."

    call df%destroy()
    call create_small_sample_for_transpose(df)

    write(*,*) "   Original small dataset:"
    call df%write_console()
    write(*,*)

    transposed_df = df%transpose()
    write(*,*) "   Transposed version:"
    call transposed_df%write_console()
    write(*,*)

    ! Clean up
    call df%destroy()
    call transposed_df%destroy()

    write(*,*) "======================================="
    write(*,*) "  New features demo completed!"
    write(*,*) "======================================="

contains

    subroutine create_extended_sample_data(df)
        type(data_frame), intent(inout) :: df
        real(rk), dimension(8) :: temperatures = [15.2_rk, 22.8_rk, 28.5_rk, 18.7_rk, 31.2_rk, 25.1_rk, 19.8_rk, 26.4_rk]
        integer(ik), dimension(8) :: humidity = [75_ik, 65_ik, 45_ik, 82_ik, 38_ik, 58_ik, 71_ik, 63_ik]
        integer(ik), dimension(8) :: pressure = [1012_ik, 1018_ik, 1025_ik, 1008_ik, 1022_ik, 1015_ik, 1010_ik, 1020_ik]
        logical, dimension(8) :: sunny = [.false., .true., .true., .false., .true., .true., .false., .true.]
        character(len=12), dimension(8) :: locations = ["Boston      ", "London      ", "Phoenix     ", "Seattle     ", &
                                                       "Rome        ", "Tokyo       ", "Moscow      ", "Cairo       "]

        call df%new()
        call df%append(temperatures, "Temperature")
        call df%append(humidity, "Humidity")
        call df%append(pressure, "Pressure")
        call df%append(sunny, "Sunny")
        call df%append(locations, "Location")
    end subroutine

    subroutine create_small_sample_for_transpose(df)
        type(data_frame), intent(inout) :: df
        real(rk), dimension(3) :: temps = [20.0_rk, 25.0_rk, 30.0_rk]
        integer(ik), dimension(3) :: humidity = [60_ik, 70_ik, 80_ik]
        character(len=8), dimension(3) :: cities = ["NYC     ", "London  ", "Tokyo   "]

        call df%new()
        call df%append(temps, "Temp")
        call df%append(humidity, "Humidity")
        call df%append(cities, "City")
    end subroutine

end program new_features_demo