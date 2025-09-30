!> Precision and NaN handling module
!!
!! Defines precision parameters and NaN (Not-a-Number) constants for the library
module precision
    use, intrinsic :: iso_fortran_env, only: r64 => real64, i32 => int32
    use, intrinsic :: ieee_arithmetic
    implicit none

    ! Define kind parameters
    integer, parameter :: rk = r64, &
                          ik = i32

    ! NaN sentinel value for integers (use extreme negative value)
    integer(ik), parameter :: NaN_ik = -huge(0_ik) - 1_ik

    ! NaN for reals - must be created at runtime using ieee_value
    real(rk) :: NaN_rk

    logical, save :: nan_initialized = .false.

contains

    !> Initialize the NaN_rk constant
    !!
    !! Must be called before using NaN_rk. Automatically called by other functions.
    subroutine init_nan()
        if (.not. nan_initialized) then
            NaN_rk = ieee_value(0.0_rk, ieee_quiet_nan)
            nan_initialized = .true.
        end if
    end subroutine init_nan

    !> Check if a real value is NaN
    !!
    !! @param[in] x Real value to check
    !! @return True if x is NaN, false otherwise
    elemental function is_nan_real(x) result(is_nan)
        real(rk), intent(in) :: x
        logical :: is_nan
        is_nan = ieee_is_nan(x)
    end function is_nan_real

    !> Check if an integer value is NaN (sentinel)
    !!
    !! @param[in] x Integer value to check
    !! @return True if x equals the integer NaN sentinel, false otherwise
    elemental function is_nan_integer(x) result(is_nan)
        integer(ik), intent(in) :: x
        logical :: is_nan
        is_nan = (x == NaN_ik)
    end function is_nan_integer

end module precision
