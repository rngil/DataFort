!> DataFort Accessors Module
!!
!! This module provides standalone functions for accessing and modifying data frame columns and values.
!! All functions take a data_frame as the first argument instead of being type-bound procedures.
!!
!! ## Functions
!!
!! ### Column Append Functions
!! - `df_append_real/integer/logical/character/complex(df, col, header)` - Add column
!!
!! ### Get Column Functions
!! - `df_get_col_real/integer/logical/character/complex(df, col_index)` - Get entire column
!!
!! ### Get Value Functions
!! - `df_get_val_real/integer/logical/character/complex(df, row, col)` - Get single value
!!
!! ### Set Column Functions
!! - `df_set_col_real/integer/logical/character/complex(df, col_index, col)` - Set entire column
!!
!! ### Set Value Functions
!! - `df_set_val_real/integer/logical/character/complex(df, row, col, val)` - Set single value
module datafort_accessors
    use precision
    use types
    use column_class
    use datafort_types
    implicit none
    private

    ! Public append functions
    public :: df_append_real, df_append_integer, df_append_logical
    public :: df_append_character, df_append_complex

    ! Public get column functions
    public :: df_get_col_real, df_get_col_integer, df_get_col_logical
    public :: df_get_col_character, df_get_col_complex

    ! Public get value functions
    public :: df_get_val_real, df_get_val_integer, df_get_val_logical
    public :: df_get_val_character, df_get_val_complex

    ! Public set column functions
    public :: df_set_col_real, df_set_col_integer, df_set_col_logical
    public :: df_set_col_character, df_set_col_complex

    ! Public set value functions
    public :: df_set_val_real, df_set_val_integer, df_set_val_logical
    public :: df_set_val_character, df_set_val_complex

contains

    !========================================================================
    ! APPEND COLUMN FUNCTIONS
    !========================================================================

    !> Append a real-valued column to the data frame
    !!
    !! Adds a new column of real numbers to the data frame with optional header
    !!
    !! @param[in,out] df The data frame instance
    !! @param[in] col Array of real values to append
    !! @param[in] header Optional column name (if not provided, no header is set)
    !!
    !! @warning All columns must have the same number of rows
    subroutine df_append_real(df, col, header)
        type(data_frame), intent(inout) :: df
        real(rk), dimension(:), intent(in) :: col
        character(len=*), intent(in), optional :: header

        type(column) :: new_col
        integer :: new_index

        call df%validate_column_addition(header, size(col))
        call df%resize_storage()

        call df%increment_num_cols()
        new_index = df%ncols()
        call new_col%new(col)
        call df%set_data_col(new_index, new_col)

        if (present(header)) then
            call df%set_header_at_index(new_index, header)
        end if
    end subroutine df_append_real

    !> Append an integer-valued column to the data frame
    subroutine df_append_integer(df, col, header)
        type(data_frame), intent(inout) :: df
        integer(ik), dimension(:), intent(in) :: col
        character(len=*), intent(in), optional :: header

        type(column) :: new_col
        integer :: new_index

        call df%validate_column_addition(header, size(col))
        call df%resize_storage()

        call df%increment_num_cols()
        new_index = df%ncols()
        call new_col%new(col)
        call df%set_data_col(new_index, new_col)

        if (present(header)) then
            call df%set_header_at_index(new_index, header)
        end if
    end subroutine df_append_integer

    !> Append a logical-valued column to the data frame
    subroutine df_append_logical(df, col, header)
        type(data_frame), intent(inout) :: df
        logical, dimension(:), intent(in) :: col
        character(len=*), intent(in), optional :: header

        type(column) :: new_col
        integer :: new_index

        call df%validate_column_addition(header, size(col))
        call df%resize_storage()

        call df%increment_num_cols()
        new_index = df%ncols()
        call new_col%new(col)
        call df%set_data_col(new_index, new_col)

        if (present(header)) then
            call df%set_header_at_index(new_index, header)
        end if
    end subroutine df_append_logical

    !> Append a character-valued column to the data frame
    subroutine df_append_character(df, col, header)
        type(data_frame), intent(inout) :: df
        character(len=*), dimension(:), intent(in) :: col
        character(len=*), intent(in), optional :: header

        type(column) :: new_col
        integer :: new_index

        call df%validate_column_addition(header, size(col))
        call df%resize_storage()

        call df%increment_num_cols()
        new_index = df%ncols()
        call new_col%new(col)
        call df%set_data_col(new_index, new_col)

        if (present(header)) then
            call df%set_header_at_index(new_index, header)
        end if
    end subroutine df_append_character

    !> Append a complex-valued column to the data frame
    subroutine df_append_complex(df, col, header)
        type(data_frame), intent(inout) :: df
        complex(rk), dimension(:), intent(in) :: col
        character(len=*), intent(in), optional :: header

        type(column) :: new_col
        integer :: new_index

        call df%validate_column_addition(header, size(col))
        call df%resize_storage()

        call df%increment_num_cols()
        new_index = df%ncols()
        call new_col%new(col)
        call df%set_data_col(new_index, new_col)

        if (present(header)) then
            call df%set_header_at_index(new_index, header)
        end if
    end subroutine df_append_complex

    !========================================================================
    ! GET COLUMN FUNCTIONS
    !========================================================================

    !> Get real column by index
    function df_get_col_real(df, index) result(col)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: index
        real(rk), dimension(:), allocatable :: col

        type(column) :: data_col

        if (index < 1 .or. index > df%ncols()) error stop "column index out of range"

        data_col = df%get_data_col(index)
        if (data_col%get_type() /= REAL_NUM) error stop "column is not real type"

        col = data_col%getr()
    end function df_get_col_real

    !> Get integer column by index
    function df_get_col_integer(df, index) result(col)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: index
        integer(ik), dimension(:), allocatable :: col

        type(column) :: data_col

        if (index < 1 .or. index > df%ncols()) error stop "column index out of range"

        data_col = df%get_data_col(index)
        if (data_col%get_type() /= INTEGER_NUM) error stop "column is not integer type"

        col = data_col%geti()
    end function df_get_col_integer

    !> Get logical column by index
    function df_get_col_logical(df, index) result(col)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: index
        logical, dimension(:), allocatable :: col

        type(column) :: data_col

        if (index < 1 .or. index > df%ncols()) error stop "column index out of range"

        data_col = df%get_data_col(index)
        if (data_col%get_type() /= LOGICAL_NUM) error stop "column is not logical type"

        col = data_col%getl()
    end function df_get_col_logical

    !> Get character column by index
    function df_get_col_character(df, index) result(col)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: index
        character(len=:), dimension(:), allocatable :: col

        type(column) :: data_col

        if (index < 1 .or. index > df%ncols()) error stop "column index out of range"

        data_col = df%get_data_col(index)
        if (data_col%get_type() /= CHARACTER_NUM) error stop "column is not character type"

        col = data_col%getch()
    end function df_get_col_character

    !> Get complex column by index
    function df_get_col_complex(df, index) result(col)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: index
        complex(rk), dimension(:), allocatable :: col

        type(column) :: data_col

        if (index < 1 .or. index > df%ncols()) error stop "column index out of range"

        data_col = df%get_data_col(index)
        if (data_col%get_type() /= COMPLEX_NUM) error stop "column is not complex type"

        col = data_col%getc()
    end function df_get_col_complex

    !========================================================================
    ! GET VALUE FUNCTIONS
    !========================================================================

    !> Get single real value from data frame
    function df_get_val_real(df, row_index, col_index) result(val)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: row_index, col_index
        real(rk) :: val

        type(column) :: data_col

        if (col_index < 1 .or. col_index > df%ncols()) error stop "column index out of range"

        data_col = df%get_data_col(col_index)
        if (data_col%get_type() /= REAL_NUM) error stop "column is not real type"

        val = data_col%getr(row_index)
    end function df_get_val_real

    !> Get single integer value from data frame
    function df_get_val_integer(df, row_index, col_index) result(val)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: row_index, col_index
        integer(ik) :: val

        type(column) :: data_col

        if (col_index < 1 .or. col_index > df%ncols()) error stop "column index out of range"

        data_col = df%get_data_col(col_index)
        if (data_col%get_type() /= INTEGER_NUM) error stop "column is not integer type"

        val = data_col%geti(row_index)
    end function df_get_val_integer

    !> Get single logical value from data frame
    function df_get_val_logical(df, row_index, col_index) result(val)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: row_index, col_index
        logical :: val

        type(column) :: data_col

        if (col_index < 1 .or. col_index > df%ncols()) error stop "column index out of range"

        data_col = df%get_data_col(col_index)
        if (data_col%get_type() /= LOGICAL_NUM) error stop "column is not logical type"

        val = data_col%getl(row_index)
    end function df_get_val_logical

    !> Get single character value from data frame
    function df_get_val_character(df, row_index, col_index) result(val)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: row_index, col_index
        character(len=:), allocatable :: val

        type(column) :: data_col

        if (col_index < 1 .or. col_index > df%ncols()) error stop "column index out of range"

        data_col = df%get_data_col(col_index)
        if (data_col%get_type() /= CHARACTER_NUM) error stop "column is not character type"

        val = data_col%getch(row_index)
    end function df_get_val_character

    !> Get single complex value from data frame
    function df_get_val_complex(df, row_index, col_index) result(val)
        type(data_frame), intent(in) :: df
        integer, intent(in) :: row_index, col_index
        complex(rk) :: val

        type(column) :: data_col

        if (col_index < 1 .or. col_index > df%ncols()) error stop "column index out of range"

        data_col = df%get_data_col(col_index)
        if (data_col%get_type() /= COMPLEX_NUM) error stop "column is not complex type"

        val = data_col%getc(row_index)
    end function df_get_val_complex

    !========================================================================
    ! SET COLUMN FUNCTIONS
    !========================================================================

    !> Set entire real column by index
    subroutine df_set_col_real(df, col_index, col)
        type(data_frame), intent(inout) :: df
        integer, intent(in) :: col_index
        real(rk), dimension(:), intent(in) :: col

        type(column) :: data_col

        if (col_index < 1 .or. col_index > df%ncols()) error stop "column index out of range"

        data_col = df%get_data_col(col_index)
        if (data_col%get_type() /= REAL_NUM) error stop "column is not real type"
        if (size(col) /= df%nrows()) error stop "column size mismatch"

        call data_col%destroy()
        call data_col%new(col)
        call df%set_data_col(col_index, data_col)
    end subroutine df_set_col_real

    !> Set entire integer column by index
    subroutine df_set_col_integer(df, col_index, col)
        type(data_frame), intent(inout) :: df
        integer, intent(in) :: col_index
        integer(ik), dimension(:), intent(in) :: col

        type(column) :: data_col

        if (col_index < 1 .or. col_index > df%ncols()) error stop "column index out of range"

        data_col = df%get_data_col(col_index)
        if (data_col%get_type() /= INTEGER_NUM) error stop "column is not integer type"
        if (size(col) /= df%nrows()) error stop "column size mismatch"

        call data_col%destroy()
        call data_col%new(col)
        call df%set_data_col(col_index, data_col)
    end subroutine df_set_col_integer

    !> Set entire logical column by index
    subroutine df_set_col_logical(df, col_index, col)
        type(data_frame), intent(inout) :: df
        integer, intent(in) :: col_index
        logical, dimension(:), intent(in) :: col

        type(column) :: data_col

        if (col_index < 1 .or. col_index > df%ncols()) error stop "column index out of range"

        data_col = df%get_data_col(col_index)
        if (data_col%get_type() /= LOGICAL_NUM) error stop "column is not logical type"
        if (size(col) /= df%nrows()) error stop "column size mismatch"

        call data_col%destroy()
        call data_col%new(col)
        call df%set_data_col(col_index, data_col)
    end subroutine df_set_col_logical

    !> Set entire character column by index
    subroutine df_set_col_character(df, col_index, col)
        type(data_frame), intent(inout) :: df
        integer, intent(in) :: col_index
        character(len=*), dimension(:), intent(in) :: col

        type(column) :: data_col

        if (col_index < 1 .or. col_index > df%ncols()) error stop "column index out of range"

        data_col = df%get_data_col(col_index)
        if (data_col%get_type() /= CHARACTER_NUM) error stop "column is not character type"
        if (size(col) /= df%nrows()) error stop "column size mismatch"

        call data_col%destroy()
        call data_col%new(col)
        call df%set_data_col(col_index, data_col)
    end subroutine df_set_col_character

    !> Set entire complex column by index
    subroutine df_set_col_complex(df, col_index, col)
        type(data_frame), intent(inout) :: df
        integer, intent(in) :: col_index
        complex(rk), dimension(:), intent(in) :: col

        type(column) :: data_col

        if (col_index < 1 .or. col_index > df%ncols()) error stop "column index out of range"

        data_col = df%get_data_col(col_index)
        if (data_col%get_type() /= COMPLEX_NUM) error stop "column is not complex type"
        if (size(col) /= df%nrows()) error stop "column size mismatch"

        call data_col%destroy()
        call data_col%new(col)
        call df%set_data_col(col_index, data_col)
    end subroutine df_set_col_complex

    !========================================================================
    ! SET VALUE FUNCTIONS
    !========================================================================

    !> Set single real value in data frame
    subroutine df_set_val_real(df, row_index, col_index, val)
        type(data_frame), intent(inout) :: df
        integer, intent(in) :: row_index, col_index
        real(rk), intent(in) :: val

        type(column) :: data_col

        if (col_index < 1 .or. col_index > df%ncols()) error stop "column index out of range"

        data_col = df%get_data_col(col_index)
        if (data_col%get_type() /= REAL_NUM) error stop "column is not real type"

        call data_col%changer(row_index, val)
        call df%set_data_col(col_index, data_col)
    end subroutine df_set_val_real

    !> Set single integer value in data frame
    subroutine df_set_val_integer(df, row_index, col_index, val)
        type(data_frame), intent(inout) :: df
        integer, intent(in) :: row_index, col_index
        integer(ik), intent(in) :: val

        type(column) :: data_col

        if (col_index < 1 .or. col_index > df%ncols()) error stop "column index out of range"

        data_col = df%get_data_col(col_index)
        if (data_col%get_type() /= INTEGER_NUM) error stop "column is not integer type"

        call data_col%changei(row_index, val)
        call df%set_data_col(col_index, data_col)
    end subroutine df_set_val_integer

    !> Set single logical value in data frame
    subroutine df_set_val_logical(df, row_index, col_index, val)
        type(data_frame), intent(inout) :: df
        integer, intent(in) :: row_index, col_index
        logical, intent(in) :: val

        type(column) :: data_col

        if (col_index < 1 .or. col_index > df%ncols()) error stop "column index out of range"

        data_col = df%get_data_col(col_index)
        if (data_col%get_type() /= LOGICAL_NUM) error stop "column is not logical type"

        call data_col%changel(row_index, val)
        call df%set_data_col(col_index, data_col)
    end subroutine df_set_val_logical

    !> Set single character value in data frame
    subroutine df_set_val_character(df, row_index, col_index, val)
        type(data_frame), intent(inout) :: df
        integer, intent(in) :: row_index, col_index
        character(len=*), intent(in) :: val

        type(column) :: data_col

        if (col_index < 1 .or. col_index > df%ncols()) error stop "column index out of range"

        data_col = df%get_data_col(col_index)
        if (data_col%get_type() /= CHARACTER_NUM) error stop "column is not character type"

        call data_col%changech(row_index, val)
        call df%set_data_col(col_index, data_col)
    end subroutine df_set_val_character

    !> Set single complex value in data frame
    subroutine df_set_val_complex(df, row_index, col_index, val)
        type(data_frame), intent(inout) :: df
        integer, intent(in) :: row_index, col_index
        complex(rk), intent(in) :: val

        type(column) :: data_col

        if (col_index < 1 .or. col_index > df%ncols()) error stop "column index out of range"

        data_col = df%get_data_col(col_index)
        if (data_col%get_type() /= COMPLEX_NUM) error stop "column is not complex type"

        call data_col%changec(row_index, val)
        call df%set_data_col(col_index, data_col)
    end subroutine df_set_val_complex

end module datafort_accessors
