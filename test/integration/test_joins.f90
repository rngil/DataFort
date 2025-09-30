program test_joins
    use datafort
    use precision
    implicit none

    type(data_frame) :: employees, departments, inner_result, left_result, right_result, outer_result
    type(data_frame) :: products, suppliers, product_join
    integer(ik), dimension(6) :: emp_ids, dept_ids_emp
    character(len=20), dimension(6) :: emp_names
    integer(ik), dimension(4) :: dept_ids
    character(len=20), dimension(4) :: dept_names
    character(len=10), dimension(5) :: product_codes, supplier_codes_prod
    character(len=20), dimension(5) :: product_names
    real(rk), dimension(5) :: prices
    character(len=10), dimension(3) :: supplier_codes
    character(len=20), dimension(3) :: supplier_names

    write(*,'(a)') ""
    write(*,'(a)') "Testing Join Operations"
    write(*,'(a)') "======================="

    ! Create employees dataframe
    emp_ids = [101_ik, 102_ik, 103_ik, 104_ik, 105_ik, 106_ik]
    emp_names = ["Alice   ", "Bob     ", "Charlie ", "Diana   ", "Eve     ", "Frank   "]
    dept_ids_emp = [1_ik, 2_ik, 1_ik, 3_ik, 2_ik, 5_ik]  ! Note: Frank is in dept 5 which doesn't exist

    call employees%new()
    call employees%append(emp_ids, "EmpID")
    call employees%append(emp_names, "Name")
    call employees%append(dept_ids_emp, "DeptID")

    write(*,'(a)') ""
    write(*,'(a)') "Employees Table:"
    call employees%write_console()

    ! Create departments dataframe
    dept_ids = [1_ik, 2_ik, 3_ik, 4_ik]  ! Note: dept 4 has no employees, dept 5 is missing
    dept_names = ["Engineering ", "Sales       ", "HR          ", "Marketing   "]

    call departments%new()
    call departments%append(dept_ids, "DeptID")
    call departments%append(dept_names, "DeptName")

    write(*,'(a)') ""
    write(*,'(a)') "Departments Table:"
    call departments%write_console()

    ! Test INNER JOIN
    write(*,'(a)') ""
    write(*,'(a)') "================================================"
    write(*,'(a)') "Test 1: INNER JOIN (only matching rows)"
    write(*,'(a)') "================================================"
    inner_result = employees%inner_join(departments, 3, 1)
    call inner_result%write_console()
    write(*,'(a,i0,a)') "Result has ", inner_result%nrows(), " rows"

    ! Test LEFT JOIN
    write(*,'(a)') ""
    write(*,'(a)') "================================================"
    write(*,'(a)') "Test 2: LEFT JOIN (all employees + matching depts)"
    write(*,'(a)') "================================================"
    left_result = employees%left_join(departments, 3, 1)
    call left_result%write_console()
    write(*,'(a,i0,a)') "Result has ", left_result%nrows(), " rows"
    write(*,'(a)') "Note: Frank (dept 5) has NULL for department name"

    ! Test RIGHT JOIN
    write(*,'(a)') ""
    write(*,'(a)') "================================================"
    write(*,'(a)') "Test 3: RIGHT JOIN (all depts + matching employees)"
    write(*,'(a)') "================================================"
    right_result = employees%right_join(departments, 3, 1)
    call right_result%write_console()
    write(*,'(a,i0,a)') "Result has ", right_result%nrows(), " rows"
    write(*,'(a)') "Note: Marketing dept has NULL for employee info"

    ! Test OUTER JOIN
    write(*,'(a)') ""
    write(*,'(a)') "================================================"
    write(*,'(a)') "Test 4: OUTER JOIN (all rows from both tables)"
    write(*,'(a)') "================================================"
    outer_result = employees%outer_join(departments, 3, 1)
    call outer_result%write_console()
    write(*,'(a,i0,a)') "Result has ", outer_result%nrows(), " rows"
    write(*,'(a)') "Note: Shows Frank (no dept) AND Marketing (no employees)"

    ! Test with different key types - using character keys
    write(*,'(a)') ""
    write(*,'(a)') "================================================"
    write(*,'(a)') "Test 5: JOIN with CHARACTER keys"
    write(*,'(a)') "================================================"

    product_codes = ["P001  ", "P002  ", "P003  ", "P004  ", "P005  "]
    product_names = ["Widget A    ", "Widget B    ", "Gadget X    ", "Gadget Y    ", "Tool Z      "]
    prices = [19.99_rk, 29.99_rk, 39.99_rk, 49.99_rk, 59.99_rk]
    supplier_codes_prod = ["S001  ", "S002  ", "S001  ", "S003  ", "S002  "]

    call products%new()
    call products%append(product_codes, "ProductCode")
    call products%append(product_names, "ProductName")
    call products%append(prices, "Price")
    call products%append(supplier_codes_prod, "SupplierCode")

    write(*,'(a)') ""
    write(*,'(a)') "Products Table:"
    call products%write_console()

    supplier_codes = ["S001  ", "S002  ", "S003  "]
    supplier_names = ["Acme Corp       ", "Global Supply   ", "Best Parts      "]

    call suppliers%new()
    call suppliers%append(supplier_codes, "SupplierCode")
    call suppliers%append(supplier_names, "SupplierName")

    write(*,'(a)') ""
    write(*,'(a)') "Suppliers Table:"
    call suppliers%write_console()

    write(*,'(a)') ""
    write(*,'(a)') "INNER JOIN on SupplierCode:"
    product_join = products%inner_join(suppliers, 4, 1)
    call product_join%write_console()

    ! Clean up
    call employees%destroy()
    call departments%destroy()
    call inner_result%destroy()
    call left_result%destroy()
    call right_result%destroy()
    call outer_result%destroy()
    call products%destroy()
    call suppliers%destroy()
    call product_join%destroy()

    write(*,'(a)') ""
    write(*,'(a)') "All join tests completed successfully!"
    write(*,'(a)') ""

end program test_joins