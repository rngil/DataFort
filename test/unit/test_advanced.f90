program test_advanced
    use datafort
    use precision
    implicit none

    type(data_frame) :: df, df2, result, counts, students, courses
    real(rk), dimension(10) :: scores
    real(rk), dimension(3) :: more_scores
    real(rk), dimension(4) :: course_scores
    integer(ik), dimension(10) :: grades, ranks
    integer(ik), dimension(3) :: more_grades
    integer(ik), dimension(4) :: student_ids, course_student_ids
    character(len=10), dimension(10) :: names
    character(len=10), dimension(3) :: more_names
    character(len=10), dimension(4) :: student_names, course_names
    logical, dimension(:), allocatable :: dup_mask
    logical, dimension(10) :: passed
    real(rk), dimension(:), allocatable :: unique_scores
    integer(ik), dimension(:), allocatable :: unique_grades
    character(len=:), allocatable :: unique_names(:)
    integer :: i, num_failed

    num_failed = 0

    write(*,'(a)') ""
    write(*,'(a)') "Testing Advanced Data Operations"
    write(*,'(a)') "================================="

    ! Create test data with some duplicates
    scores = [85.5_rk, 92.3_rk, 78.0_rk, 92.3_rk, 88.5_rk, &
              85.5_rk, 90.0_rk, 78.0_rk, 95.2_rk, 88.5_rk]
    grades = [3_ik, 4_ik, 2_ik, 4_ik, 3_ik, 3_ik, 4_ik, 2_ik, 4_ik, 3_ik]
    names = ["Alice  ", "Bob    ", "Charlie", "Bob    ", "Diana  ", &
             "Alice  ", "Eve    ", "Charlie", "Frank  ", "Diana  "]

    call df%new()
    call df_append_character(df, names, "Name")
    call df_append_real(df, scores, "Score")
    call df_append_integer(df, grades, "Grade")

    write(*,'(a)') ""
    write(*,'(a)') "Original data:"
    call df_write_console(df)

    ! Test unique()
    write(*,'(a)') ""
    write(*,'(a)') "Test 1: Unique values"
    write(*,'(a)') "---------------------"
    unique_scores = df_unique_real(df, 2)
    write(*,'(a)') "Unique scores:"
    do i = 1, size(unique_scores)
        write(*,'(a,f8.2)') "  ", unique_scores(i)
    end do
    call assert_int_equal(size(unique_scores), 6, "unique_real count", num_failed)

    unique_grades = df_unique_integer(df, 3)
    write(*,'(a)') "Unique grades:"
    do i = 1, size(unique_grades)
        write(*,'(a,i0)') "  ", unique_grades(i)
    end do
    call assert_int_equal(size(unique_grades), 3, "unique_integer count", num_failed)

    ! Test value_counts()
    write(*,'(a)') ""
    write(*,'(a)') "Test 2: Value counts"
    write(*,'(a)') "--------------------"
    counts = df_value_counts_integer(df, 3)
    write(*,'(a)') "Grade value counts:"
    call df_write_console(counts)
    call assert_int_equal(counts%nrows(), 3, "value_counts rows", num_failed)
    call assert_int_equal(counts%ncols(), 2, "value_counts cols", num_failed)

    ! Test duplicated() and drop_duplicates()
    write(*,'(a)') ""
    write(*,'(a)') "Test 3: Duplicates"
    write(*,'(a)') "-------------------"
    dup_mask = df_duplicated(df)
    write(*,'(a)') "Duplicate rows:"
    do i = 1, size(dup_mask)
        if (dup_mask(i)) then
            write(*,'(a,i0)') "  Row ", i
        end if
    end do
    call assert_int_equal(count(dup_mask), 4, "duplicated count", num_failed)

    result = df_drop_duplicates(df)
    write(*,'(a)') ""
    write(*,'(a)') "After drop_duplicates():"
    call df_write_console(result)
    write(*,'(a,i0,a,i0)') "Kept ", result%nrows(), " unique rows out of ", df%nrows()
    call assert_int_equal(result%nrows(), 6, "drop_duplicates result rows", num_failed)

    ! Test drop_duplicates with subset
    write(*,'(a)') ""
    write(*,'(a)') "Test subset: drop_duplicates on Name column only:"
    result = df_drop_duplicates_subset(df, [1])  ! Column 1 is Name
    call df_write_console(result)
    write(*,'(a,i0,a,i0)') "Kept ", result%nrows(), " unique names out of ", df%nrows()
    call assert_int_equal(result%nrows(), 6, "drop_duplicates_subset result rows", num_failed)

    ! Test rank()
    write(*,'(a)') ""
    write(*,'(a)') "Test 4: Ranking"
    write(*,'(a)') "---------------"
    ranks = df_rank_real(df, 2, .true.)
    write(*,'(a)') "Ranks of scores (ascending):"
    do i = 1, min(10, size(ranks))
        write(*,'(a,i0,a,f8.2,a,i0)') "  Row ", i, ": Score ", scores(i), " -> Rank ", ranks(i)
    end do
    call assert_int_equal(size(ranks), 10, "rank result size", num_failed)
    ! Lowest score (78.0) should have rank 1 or 2
    call assert_true(ranks(3) <= 2, "rank for low score", num_failed)

    ! Test concat() - vertical
    write(*,'(a)') ""
    write(*,'(a)') "Test 5: Concat (vertical)"
    write(*,'(a)') "--------------------------"

    more_scores = [82.0_rk, 91.5_rk, 87.3_rk]
    more_grades = [3_ik, 4_ik, 3_ik]
    more_names = ["Grace", "Henry", "Iris "]

    call df2%new()
    call df_append_character(df2, more_names, "Name")
    call df_append_real(df2, more_scores, "Score")
    call df_append_integer(df2, more_grades, "Grade")

    result = df_concat(df, df2, 0)
    write(*,'(a)') "Combined dataframe (vertical stack):"
    call df_write_console(result)
    call assert_int_equal(result%nrows(), 13, "concat vertical rows", num_failed)
    call assert_int_equal(result%ncols(), 3, "concat vertical cols", num_failed)

    ! Test concat() - horizontal
    write(*,'(a)') ""
    write(*,'(a)') "Test 6: Concat (horizontal)"
    write(*,'(a)') "----------------------------"
    passed = [.true., .true., .false., .true., .true., &
              .true., .true., .false., .true., .true.]

    call df2%destroy()
    call df2%new()
    call df_append_logical(df2, passed, "Passed")

    result = df_concat(df, df2, 1)
    write(*,'(a)') "Dataframe with additional column:"
    call df_write_console(result)
    call assert_int_equal(result%nrows(), 10, "concat horizontal rows", num_failed)
    call assert_int_equal(result%ncols(), 4, "concat horizontal cols", num_failed)

    ! Test merge()
    write(*,'(a)') ""
    write(*,'(a)') "Test 7: Merge"
    write(*,'(a)') "-------------"

    student_names = ["Alice  ", "Bob    ", "Charlie", "Diana  "]
    student_ids = [1_ik, 2_ik, 3_ik, 4_ik]

    call students%new()
    call df_append_integer(students, student_ids, "ID")
    call df_append_character(students, student_names, "Name")

    course_student_ids = [1_ik, 2_ik, 1_ik, 3_ik]
    course_names = ["Math   ", "Math   ", "Physics", "Math   "]
    course_scores = [90.0_rk, 85.0_rk, 88.0_rk, 92.0_rk]

    call courses%new()
    call df_append_integer(courses, course_student_ids, "ID")
    call df_append_character(courses, course_names, "Course")
    call df_append_real(courses, course_scores, "Score")

    write(*,'(a)') "Students:"
    call df_write_console(students)
    write(*,'(a)') ""
    write(*,'(a)') "Courses:"
    call df_write_console(courses)

    result = df_merge(students, courses, "ID", "inner")
    write(*,'(a)') ""
    write(*,'(a)') "Merged (inner join on ID):"
    call df_write_console(result)
    call assert_int_equal(result%nrows(), 4, "merge result rows", num_failed)
    call assert_int_equal(result%ncols(), 5, "merge result cols", num_failed)

    ! Clean up
    call df%destroy()
    call df2%destroy()
    call result%destroy()
    call counts%destroy()
    call students%destroy()
    call courses%destroy()

    write(*,'(a)') ""
    write(*,'(a)') "================================="
    if (num_failed == 0) then
        write(*,'(a)') "All tests PASSED!"
    else
        write(*,'(a,i0,a)') "FAILED: ", num_failed, " test(s) failed"
        error stop 1
    end if
    write(*,'(a)') ""

contains

    subroutine assert_int_equal(actual, expected, test_name, num_failed)
        integer, intent(in) :: actual, expected
        character(len=*), intent(in) :: test_name
        integer, intent(inout) :: num_failed

        if (actual == expected) then
            write(*,'(a,a)') "   PASS: ", trim(test_name)
        else
            write(*,'(a,a,a,i0,a,i0)') "   FAIL: ", trim(test_name), " - Got ", actual, " Expected ", expected
            num_failed = num_failed + 1
        end if
    end subroutine assert_int_equal

    subroutine assert_true(condition, test_name, num_failed)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: test_name
        integer, intent(inout) :: num_failed

        if (condition) then
            write(*,'(a,a)') "   PASS: ", trim(test_name)
        else
            write(*,'(a,a)') "   FAIL: ", trim(test_name)
            num_failed = num_failed + 1
        end if
    end subroutine assert_true

end program test_advanced