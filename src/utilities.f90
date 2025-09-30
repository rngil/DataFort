module utilities
    use precision
    use types
    use column_class
    implicit none
    private

    public :: quick_sort_real, quick_sort_integer

contains

    ! Quick sort utility for median/percentile calculations
    recursive subroutine quick_sort_real(arr, low, high)
        real(rk), intent(inout) :: arr(:)
        integer, intent(in) :: low, high
        integer :: pivot_index

        if (low < high) then
            pivot_index = partition_real(arr, low, high)
            call quick_sort_real(arr, low, pivot_index - 1)
            call quick_sort_real(arr, pivot_index + 1, high)
        end if
    end subroutine quick_sort_real

    function partition_real(arr, low, high) result(pivot_index)
        real(rk), intent(inout) :: arr(:)
        integer, intent(in) :: low, high
        integer :: pivot_index
        real(rk) :: pivot, temp
        integer :: i, j

        pivot = arr(high)
        i = low - 1

        do j = low, high - 1
            if (arr(j) <= pivot) then
                i = i + 1
                temp = arr(i)
                arr(i) = arr(j)
                arr(j) = temp
            end if
        end do

        temp = arr(i + 1)
        arr(i + 1) = arr(high)
        arr(high) = temp
        pivot_index = i + 1
    end function partition_real

    recursive subroutine quick_sort_integer(arr, low, high)
        integer(ik), intent(inout) :: arr(:)
        integer, intent(in) :: low, high
        integer :: pivot_index

        if (low < high) then
            pivot_index = partition_integer(arr, low, high)
            call quick_sort_integer(arr, low, pivot_index - 1)
            call quick_sort_integer(arr, pivot_index + 1, high)
        end if
    end subroutine quick_sort_integer

    function partition_integer(arr, low, high) result(pivot_index)
        integer(ik), intent(inout) :: arr(:)
        integer, intent(in) :: low, high
        integer :: pivot_index
        integer(ik) :: pivot, temp
        integer :: i, j

        pivot = arr(high)
        i = low - 1

        do j = low, high - 1
            if (arr(j) <= pivot) then
                i = i + 1
                temp = arr(i)
                arr(i) = arr(j)
                arr(j) = temp
            end if
        end do

        temp = arr(i + 1)
        arr(i + 1) = arr(high)
        arr(high) = temp
        pivot_index = i + 1
    end function partition_integer

end module utilities