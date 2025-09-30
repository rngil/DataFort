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
    integer :: i

    write(*,'(a)') ""
    write(*,'(a)') "Testing Advanced Data Operations"
    write(*,'(a)') "================================="

    ! Create test data with some duplicates
    scores = [85.5_rk, 92.3_rk, 78.0_rk, 92.3_rk, 88.5_rk, &
              85.5_rk, 90.0_rk, 78.0_rk, 95.2_rk, 88.5_rk]
    grades = [3_ik, 4_ik, 2_ik, 4_ik, 3_ik, 3_ik, 4_ik, 2_ik, 4_ik, 3_ik]
    names = ["Alice     ", "Bob       ", "Charlie   ", "Bob       ", "Diana     ", &
             "Alice     ", "Eve       ", "Charlie   ", "Frank     ", "Diana     "]

    call df%new()
    call df%append(names, "Name")
    call df%append(scores, "Score")
    call df%append(grades, "Grade")

    write(*,'(a)') ""
    write(*,'(a)') "Original data:"
    call df%write_console()

    ! Test unique()
    write(*,'(a)') ""
    write(*,'(a)') "Test 1: Unique values"
    write(*,'(a)') "---------------------"
    unique_scores = df%unique_real(2)
    write(*,'(a)') "Unique scores:"
    do i = 1, size(unique_scores)
        write(*,'(a,f8.2)') "  ", unique_scores(i)
    end do

    unique_grades = df%unique_integer(3)
    write(*,'(a)') "Unique grades:"
    do i = 1, size(unique_grades)
        write(*,'(a,i0)') "  ", unique_grades(i)
    end do

    ! Test value_counts()
    write(*,'(a)') ""
    write(*,'(a)') "Test 2: Value counts"
    write(*,'(a)') "--------------------"
    counts = df%value_counts_integer(3)
    write(*,'(a)') "Grade value counts:"
    call counts%write_console()

    ! Test duplicated() and drop_duplicates()
    write(*,'(a)') ""
    write(*,'(a)') "Test 3: Duplicates"
    write(*,'(a)') "-------------------"
    dup_mask = df%duplicated()
    write(*,'(a)') "Duplicate rows:"
    do i = 1, size(dup_mask)
        if (dup_mask(i)) then
            write(*,'(a,i0)') "  Row ", i
        end if
    end do

    result = df%drop_duplicates()
    write(*,'(a)') ""
    write(*,'(a)') "After drop_duplicates():"
    call result%write_console()
    write(*,'(a,i0,a,i0)') "Kept ", result%nrows(), " unique rows out of ", df%nrows()

    ! Test drop_duplicates with subset
    write(*,'(a)') ""
    write(*,'(a)') "Test subset: drop_duplicates on Name column only:"
    result = df%drop_duplicates_subset([1])  ! Column 1 is Name
    call result%write_console()
    write(*,'(a,i0,a,i0)') "Kept ", result%nrows(), " unique names out of ", df%nrows()

    ! Test rank()
    write(*,'(a)') ""
    write(*,'(a)') "Test 4: Ranking"
    write(*,'(a)') "---------------"
    ranks = df%rank_real(2, .true.)
    write(*,'(a)') "Ranks of scores (ascending):"
    do i = 1, min(10, size(ranks))
        write(*,'(a,i0,a,f8.2,a,i0)') "  Row ", i, ": Score ", scores(i), " -> Rank ", ranks(i)
    end do

    ! Test concat() - vertical
    write(*,'(a)') ""
    write(*,'(a)') "Test 5: Concat (vertical)"
    write(*,'(a)') "--------------------------"

    more_scores = [82.0_rk, 91.5_rk, 87.3_rk]
    more_grades = [3_ik, 4_ik, 3_ik]
    more_names = ["Grace     ", "Henry     ", "Iris      "]

    call df2%new()
    call df2%append(more_names, "Name")
    call df2%append(more_scores, "Score")
    call df2%append(more_grades, "Grade")

    result = df%concat(df2, 0)
    write(*,'(a)') "Combined dataframe (vertical stack):"
    call result%write_console()

    ! Test concat() - horizontal
    write(*,'(a)') ""
    write(*,'(a)') "Test 6: Concat (horizontal)"
    write(*,'(a)') "----------------------------"
    passed = [.true., .true., .false., .true., .true., &
              .true., .true., .false., .true., .true.]

    call df2%destroy()
    call df2%new()
    call df2%append(passed, "Passed")

    result = df%concat(df2, 1)
    write(*,'(a)') "Dataframe with additional column:"
    call result%write_console()

    ! Test merge()
    write(*,'(a)') ""
    write(*,'(a)') "Test 7: Merge"
    write(*,'(a)') "-------------"

    student_names = ["Alice     ", "Bob       ", "Charlie   ", "Diana     "]
    student_ids = [1_ik, 2_ik, 3_ik, 4_ik]

    call students%new()
    call students%append(student_ids, "ID")
    call students%append(student_names, "Name")

    course_student_ids = [1_ik, 2_ik, 1_ik, 3_ik]
    course_names = ["Math      ", "Math      ", "Physics   ", "Math      "]
    course_scores = [90.0_rk, 85.0_rk, 88.0_rk, 92.0_rk]

    call courses%new()
    call courses%append(course_student_ids, "ID")
    call courses%append(course_names, "Course")
    call courses%append(course_scores, "Score")

    write(*,'(a)') "Students:"
    call students%write_console()
    write(*,'(a)') ""
    write(*,'(a)') "Courses:"
    call courses%write_console()

    result = students%merge(courses, "ID", "inner")
    write(*,'(a)') ""
    write(*,'(a)') "Merged (inner join on ID):"
    call result%write_console()

    ! Clean up
    call df%destroy()
    call df2%destroy()
    call result%destroy()
    call counts%destroy()
    call students%destroy()
    call courses%destroy()

    write(*,'(a)') ""
    write(*,'(a)') "All advanced operation tests completed!"
    write(*,'(a)') ""

end program test_advanced