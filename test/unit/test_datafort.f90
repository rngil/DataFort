program test_suite
    use datafort
    use column_class
    use types
    use precision
    implicit none

    logical :: all_tests_passed = .true.

    write(*,*) "Running DataFort Test Suite"
    write(*,*) "=============================="

    ! Test column class first
    call test_column_functionality()

    ! Test data frame basic functionality
    call test_data_frame_basic()

    ! Test data frame with headers
    call test_data_frame_headers()

    ! Test mixed data types
    call test_mixed_data_types()

    ! Test edge cases
    call test_edge_cases()

    if (all_tests_passed) then
        write(*,*) "All tests passed!"
    else
        write(*,*) "Some tests failed!"
        stop 1
    end if

contains

    subroutine assert_true(condition, test_name)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: test_name

        if (condition) then
            write(*,*) "PASS: ", test_name
        else
            write(*,*) "FAIL: ", test_name
            all_tests_passed = .false.
        end if
    end subroutine

    subroutine test_column_functionality()
        type(column) :: col
        real(rk), dimension(3) :: test_data = [1.0_rk, 2.0_rk, 3.0_rk]
        real(rk), dimension(3) :: retrieved_data

        write(*,*) "Testing column class..."

        ! Test real column creation and retrieval
        call col%new(test_data)
        call assert_true(col%get_type() == REAL_NUM, "Column type detection")
        call assert_true(col%n == 3, "Column size detection")

        retrieved_data = col%getr()
        call assert_true(all(abs(retrieved_data - test_data) < 1e-6_rk), "Column data retrieval")

        ! Test single element access
        call assert_true(abs(col%getr(2) - 2.0_rk) < 1e-6_rk, "Single element access")

        ! Test element modification
        call col%changer(2, 5.0_rk)
        call assert_true(abs(col%getr(2) - 5.0_rk) < 1e-6_rk, "Element modification")

        call col%destroy()
    end subroutine

    subroutine test_data_frame_basic()
        type(data_frame) :: df
        real(rk), dimension(3) :: real_col = [1.0_rk, 2.0_rk, 3.0_rk]
        integer(ik), dimension(3) :: int_col = [10_ik, 20_ik, 30_ik]

        write(*,*) "Testing data frame basic functionality..."

        call df%new()
        call assert_true(df%is_initialized(), "Data frame initialization")
        call assert_true(df%ncols() == 0, "Initial column count")
        call assert_true(df%nrows() == 0, "Initial row count")

        ! Add columns
        call df%append(real_col)
        call assert_true(df%ncols() == 1, "Column addition")
        call assert_true(df%nrows() == 3, "Row count after first column")

        call df%append(int_col)
        call assert_true(df%ncols() == 2, "Second column addition")
        call assert_true(df%nrows() == 3, "Row count after second column")

        call df%destroy()
    end subroutine

    subroutine test_data_frame_headers()
        type(data_frame) :: df
        real(rk), dimension(3) :: real_col = [1.0_rk, 2.0_rk, 3.0_rk]
        integer(ik), dimension(3) :: int_col = [10_ik, 20_ik, 30_ik]

        write(*,*) "Testing data frame with headers..."

        call df%new()

        ! Add columns with headers
        call df%append(real_col, "real_data")
        call df%append(int_col, "int_data")

        call assert_true(df%header(1) == "real_data", "First header retrieval")
        call assert_true(df%header(2) == "int_data", "Second header retrieval")

        call df%destroy()
    end subroutine

    subroutine test_mixed_data_types()
        type(data_frame) :: df
        real(rk), dimension(2) :: real_col = [1.5_rk, 2.5_rk]
        integer(ik), dimension(2) :: int_col = [10_ik, 20_ik]
        logical, dimension(2) :: logical_col = [.true., .false.]
        character(len=5), dimension(2) :: char_col = ["hello", "world"]

        write(*,*) "Testing mixed data types..."

        call df%new()

        call df%append(real_col, "reals")
        call df%append(int_col, "ints")
        call df%append(logical_col, "logicals")
        call df%append(char_col, "chars")

        call assert_true(df%ncols() == 4, "Mixed type column count")
        call assert_true(df%dtype("reals") == REAL_NUM, "Real column type")
        call assert_true(df%dtype("ints") == INTEGER_NUM, "Integer column type")
        call assert_true(df%dtype("logicals") == LOGICAL_NUM, "Logical column type")
        call assert_true(df%dtype("chars") == CHARACTER_NUM, "Character column type")

        call df%destroy()
    end subroutine

    subroutine test_edge_cases()
        type(data_frame) :: df

        write(*,*) "Testing edge cases..."

        call df%new()

        ! Test empty data frame operations
        call assert_true(df%ncols() == 0, "Empty data frame column count")
        call assert_true(df%nrows() == 0, "Empty data frame row count")

        call df%destroy()
    end subroutine

end program test_suite