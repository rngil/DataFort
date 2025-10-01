program check
    use datafort
    use precision
    implicit none

    type(data_frame) :: df
    real(rk), dimension(3) :: temps = [20.0_rk, 22.5_rk, 25.0_rk]

    print *, "Simple DataFort check..."

    call df % new()
    call df_append_real(df, temps, "Temperature")

    print *, "Data frame created with", df % ncols(), "columns and", df % nrows(), "rows"
    print *, "Average temperature:", df_mean_real(df, 1)

    call df % destroy()
    print *, "Check completed successfully!"

end program check
