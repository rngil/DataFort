!> DataFort Types Module
!!
!! This module provides the core data_frame type definition and essential operations.
!! It contains the type definition, constructor/destructor, and basic getter functions.
!!
!! All data manipulation operations are provided as standalone functions in separate modules.
module datafort_types
    use precision
    use types
    use column_class
    implicit none
    private

    public :: data_frame
    public :: MAX_CHAR_LEN_DEFAULT

    integer, parameter :: MAX_CHAR_LEN_DEFAULT = 100

    !> Main data frame type for storing heterogeneous tabular data
    !!
    !! A data frame consists of columns of potentially different types,
    !! similar to a spreadsheet or database table. Each column must have
    !! the same number of rows.
    !!
    !! ## Type-bound Procedures
    !!
    !! ### Constructor/Destructor
    !! - `new([char_len])` - Initialize data frame
    !! - `destroy()` - Free memory
    !!
    !! ### Basic Info
    !! - `ncols()` - Get number of columns
    !! - `nrows()` - Get number of rows
    !! - `get_max_char_len()` - Get maximum character length
    !! - `header(index)` - Get column header by index
    !! - `dtype(index or header)` - Get column data type
    !! - `is_initialized()` - Check if data frame is initialized
    type :: data_frame
        private

        integer :: num_cols = 0, max_char_len = MAX_CHAR_LEN_DEFAULT
        logical :: with_headers = .false.
        character(len=:), dimension(:), allocatable :: headers
        type(column), dimension(:), allocatable :: data_cols
        logical :: initialized = .false.

    contains
        private

        ! Constructor/Destructor
        procedure, public :: new => df_constructor
        procedure, public :: destroy => df_destructor
        procedure, public :: is_initialized => df_is_initialized

        ! Basic info
        procedure, public :: ncols => df_get_num_cols
        procedure, public :: nrows => df_get_num_rows
        procedure, public :: get_max_char_len => df_get_max_char_len
        procedure, public :: header => get_header
        procedure :: df_get_col_type_header, df_get_col_type_index
        generic, public :: dtype => df_get_col_type_header, df_get_col_type_index

        ! Internal utility procedures (public for use by other datafort modules)
        procedure, public :: already_header
        procedure, public :: resize_storage
        procedure, public :: validate_column_addition
        procedure, public :: find_header_index
        procedure, public :: get_data_col  ! Access to internal column object
        procedure, public :: set_data_col  ! Set internal column object
        procedure, public :: get_with_headers  ! Check if has headers
        procedure, public :: set_with_headers  ! Set headers flag
        procedure, public :: set_header_at_index  ! Set header at specific index
        procedure, public :: increment_num_cols  ! Increment column count
    end type data_frame

contains

    !> Initialize a new data frame
    !!
    !! Creates a new empty data frame with optional character length specification
    !!
    !! @param[in,out] this The data frame instance
    !! @param[in] char_len Optional maximum character length for string columns (default: 100)
    !!
    !! @note If the data frame is already initialized, it will be destroyed first
    subroutine df_constructor(this, char_len)
        class(data_frame), intent(inout) :: this
        integer, intent(in), optional :: char_len

        if (this % initialized) call this % destroy()

        this % num_cols = 0
        this % with_headers = .false.

        if (present(char_len)) then
            this % max_char_len = char_len
        else
            this % max_char_len = MAX_CHAR_LEN_DEFAULT
        end if

        this % initialized = .true.
    end subroutine df_constructor

    !> Destroy a data frame and free all memory
    !!
    !! Deallocates all columns and resets the data frame to uninitialized state
    !!
    !! @param[in,out] this The data frame instance
    subroutine df_destructor(this)
        class(data_frame), intent(inout) :: this

        integer :: i

        if (allocated(this % data_cols)) then
            do i = 1, size(this % data_cols)
                call this % data_cols(i) % destroy()
            end do
            deallocate (this % data_cols)
        end if

        if (allocated(this % headers)) deallocate (this % headers)

        this % num_cols = 0
        this % with_headers = .false.
        this % initialized = .false.
    end subroutine df_destructor

    ! Check if initialized
    pure function df_is_initialized(this) result(is_init)
        class(data_frame), intent(in) :: this
        logical :: is_init
        is_init = this % initialized
    end function df_is_initialized

    ! Get number of columns
    pure function df_get_num_cols(this) result(num_cols)
        class(data_frame), intent(in) :: this
        integer :: num_cols
        num_cols = this % num_cols
    end function df_get_num_cols

    ! Get number of rows
    pure function df_get_num_rows(this) result(num_rows)
        class(data_frame), intent(in) :: this
        integer :: num_rows

        if (this % num_cols == 0) then
            num_rows = 0
        else
            num_rows = this % data_cols(1) % n
        end if
    end function df_get_num_rows

    ! Get max char length
    pure function df_get_max_char_len(this) result(max_len)
        class(data_frame), intent(in) :: this
        integer :: max_len
        max_len = this % max_char_len
    end function df_get_max_char_len

    ! Get header by index
    function get_header(this, index) result(header)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: index
        character(len=:), allocatable :: header

        if (.not. this % with_headers) error stop "data frame has no headers"
        if (index < 1 .or. index > this % num_cols) error stop "header index out of range"

        header = trim(this % headers(index))
    end function get_header

    ! Get column type by header
    pure function df_get_col_type_header(this, header) result(dtype)
        class(data_frame), intent(in) :: this
        character(len=*), intent(in) :: header
        integer :: dtype

        integer :: ind
        ind = this % find_header_index(header)
        dtype = this % data_cols(ind) % get_type()
    end function df_get_col_type_header

    ! Get column type by index
    pure function df_get_col_type_index(this, index) result(dtype)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: index
        integer :: dtype

        if (index < 1 .or. index > this % num_cols) error stop "column index out of range"
        dtype = this % data_cols(index) % get_type()
    end function df_get_col_type_index

    ! Find header index
    pure function find_header_index(this, header) result(index)
        class(data_frame), intent(in) :: this
        character(len=*), intent(in) :: header
        integer :: index

        integer :: i
        character(len=:), allocatable :: trimmed_header

        if (.not. this % with_headers) error stop "data frame has no headers"

        trimmed_header = trim(adjustl(header))
        index = -1

        do i = 1, this % num_cols
            if (trim(adjustl(this % headers(i))) == trimmed_header) then
                index = i
                exit
            end if
        end do

        if (index == -1) error stop "header not found"
    end function find_header_index

    ! Check if header already exists
    function already_header(this, header) result(exists)
        class(data_frame), intent(in) :: this
        character(len=*), intent(in) :: header
        logical :: exists

        integer :: i
        character(len=:), allocatable :: trimmed_header

        exists = .false.
        if (.not. this % with_headers) return

        trimmed_header = trim(adjustl(header))

        do i = 1, this % num_cols
            if (trim(adjustl(this % headers(i))) == trimmed_header) then
                exists = .true.
                exit
            end if
        end do
    end function already_header

    ! Resize storage arrays
    subroutine resize_storage(this)
        class(data_frame), intent(inout) :: this

        type(column), dimension(:), allocatable :: temp_cols
        character(len=:), dimension(:), allocatable :: temp_headers
        integer :: new_size, i

        new_size = this % num_cols + 1

        ! Resize columns array
        if (allocated(this % data_cols)) then
            allocate (temp_cols(size(this % data_cols)))
            do i = 1, size(this % data_cols)
                temp_cols(i) = this % data_cols(i)
            end do
            deallocate (this % data_cols)
        end if

        allocate (this % data_cols(new_size))

        if (allocated(temp_cols)) then
            do i = 1, size(temp_cols)
                this % data_cols(i) = temp_cols(i)
            end do
            deallocate (temp_cols)
        end if

        ! Resize headers array if needed
        if (this % with_headers) then
            if (allocated(this % headers)) then
                allocate (character(len=this % max_char_len) :: temp_headers(size(this % headers)))
                temp_headers = this % headers
                deallocate (this % headers)
            end if

            allocate (character(len=this % max_char_len) :: this % headers(new_size))

            if (allocated(temp_headers)) then
                this % headers(1:size(temp_headers)) = temp_headers
                deallocate (temp_headers)
            end if
        end if
    end subroutine resize_storage

    ! Validate column addition
    subroutine validate_column_addition(this, header, col_size)
        class(data_frame), intent(inout) :: this
        character(len=*), intent(in), optional :: header
        integer, intent(in) :: col_size

        ! Check row size consistency
        if (this % num_cols > 0 .and. col_size /= this % nrows()) then
            error stop "column size must match existing columns"
        end if

        ! Handle headers
        if (present(header)) then
            if (this % num_cols == 0) then
                this % with_headers = .true.
                allocate (character(len=this % max_char_len) :: this % headers(0))
            else if (.not. this % with_headers) then
                error stop "cannot add header to data frame without headers"
            end if

            if (this % already_header(header)) then
                error stop "header already exists"
            end if
        else
            if (this % with_headers) then
                error stop "cannot add column without header to data frame with headers"
            end if
        end if
    end subroutine validate_column_addition

    ! Get access to internal column object (for use by other datafort modules)
    function get_data_col(this, index) result(col)
        class(data_frame), intent(in) :: this
        integer, intent(in) :: index
        type(column) :: col

        if (index < 1 .or. index > this % num_cols) error stop "column index out of range"
        col = this % data_cols(index)
    end function get_data_col

    ! Set internal column object (for use by other datafort modules)
    subroutine set_data_col(this, index, col)
        class(data_frame), intent(inout) :: this
        integer, intent(in) :: index
        type(column), intent(in) :: col

        if (index < 1 .or. index > this % num_cols) error stop "column index out of range"
        this % data_cols(index) = col
    end subroutine set_data_col

    ! Check if data frame has headers
    pure function get_with_headers(this) result(has_headers)
        class(data_frame), intent(in) :: this
        logical :: has_headers
        has_headers = this % with_headers
    end function get_with_headers

    ! Set headers flag (for use by other datafort modules)
    subroutine set_with_headers(this, has_headers)
        class(data_frame), intent(inout) :: this
        logical, intent(in) :: has_headers
        this % with_headers = has_headers
    end subroutine set_with_headers

    ! Set header at specific index (for use by other datafort modules)
    subroutine set_header_at_index(this, index, header)
        class(data_frame), intent(inout) :: this
        integer, intent(in) :: index
        character(len=*), intent(in) :: header

        if (.not. this % with_headers) error stop "data frame has no headers"
        if (index < 1 .or. index > this % num_cols) error stop "header index out of range"

        this % headers(index) = header
    end subroutine set_header_at_index

    ! Increment column count (for use by other datafort modules)
    subroutine increment_num_cols(this)
        class(data_frame), intent(inout) :: this
        this % num_cols = this % num_cols + 1
    end subroutine increment_num_cols

end module datafort_types
