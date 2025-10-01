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

    write (*, '(a)') ""
    write (*, '(a)') "Testing Join Operations"
    write (*, '(a)') "======================="

    ! Create employees dataframe
    emp_ids = [101_ik, 102_ik, 103_ik, 104_ik, 105_ik, 106_ik]
    emp_names = ["Alice   ", "Bob     ", "Charlie ", "Diana   ", "Eve     ", "Frank   "]
    dept_ids_emp = [1_ik, 2_ik, 1_ik, 3_ik, 2_ik, 5_ik]  ! Note: Frank is in dept 5 which doesn't exist

    call employees % new()
    call df_append_integer(employees, emp_ids, "EmpID")
    call df_append_character(employees, emp_names, "Name")
    call df_append_integer(employees, dept_ids_emp, "DeptID")

    write (*, '(a)') ""
    write (*, '(a)') "Employees Table:"
    call df_write_console(employees)

    ! Create departments dataframe
    dept_ids = [1_ik, 2_ik, 3_ik, 4_ik]  ! Note: dept 4 has no employees, dept 5 is missing
    dept_names = ["Engineering ", "Sales       ", "HR          ", "Marketing   "]

    call departments % new()
    call df_append_integer(departments, dept_ids, "DeptID")
    call df_append_character(departments, dept_names, "DeptName")

    write (*, '(a)') ""
    write (*, '(a)') "Departments Table:"
    call df_write_console(departments)

    ! Test INNER JOIN
    write (*, '(a)') ""
    write (*, '(a)') "================================================"
    write (*, '(a)') "Test 1: INNER JOIN (only matching rows)"
    write (*, '(a)') "================================================"
    inner_result = df_inner_join(employees, departments, 3, 1)
    call df_write_console(inner_result)
    write (*, '(a,i0,a)') "Result has ", inner_result % nrows(), " rows"

    ! Test LEFT JOIN
    write (*, '(a)') ""
    write (*, '(a)') "================================================"
    write (*, '(a)') "Test 2: LEFT JOIN (all employees + matching depts)"
    write (*, '(a)') "================================================"
    left_result = df_left_join(employees, departments, 3, 1)
    call df_write_console(left_result)
    write (*, '(a,i0,a)') "Result has ", left_result % nrows(), " rows"
    write (*, '(a)') "Note: Frank (dept 5) has NULL for department name"

    ! Test RIGHT JOIN
    write (*, '(a)') ""
    write (*, '(a)') "================================================"
    write (*, '(a)') "Test 3: RIGHT JOIN (all depts + matching employees)"
    write (*, '(a)') "================================================"
    right_result = df_right_join(employees, departments, 3, 1)
    call df_write_console(right_result)
    write (*, '(a,i0,a)') "Result has ", right_result % nrows(), " rows"
    write (*, '(a)') "Note: Marketing dept has NULL for employee info"

    ! Test OUTER JOIN
    write (*, '(a)') ""
    write (*, '(a)') "================================================"
    write (*, '(a)') "Test 4: OUTER JOIN (all rows from both tables)"
    write (*, '(a)') "================================================"
    outer_result = df_outer_join(employees, departments, 3, 1)
    call df_write_console(outer_result)
    write (*, '(a,i0,a)') "Result has ", outer_result % nrows(), " rows"
    write (*, '(a)') "Note: Shows Frank (no dept) AND Marketing (no employees)"

    ! Test with different key types - using character keys
    write (*, '(a)') ""
    write (*, '(a)') "================================================"
    write (*, '(a)') "Test 5: JOIN with CHARACTER keys"
    write (*, '(a)') "================================================"

    product_codes = ["P001  ", "P002  ", "P003  ", "P004  ", "P005  "]
    product_names = ["Widget A    ", "Widget B    ", "Gadget X    ", "Gadget Y    ", "Tool Z      "]
    prices = [19.99_rk, 29.99_rk, 39.99_rk, 49.99_rk, 59.99_rk]
    supplier_codes_prod = ["S001  ", "S002  ", "S001  ", "S003  ", "S002  "]

    call products % new()
    call df_append_character(products, product_codes, "ProductCode")
    call df_append_character(products, product_names, "ProductName")
    call df_append_real(products, prices, "Price")
    call df_append_character(products, supplier_codes_prod, "SupplierCode")

    write (*, '(a)') ""
    write (*, '(a)') "Products Table:"
    call df_write_console(products)

    supplier_codes = ["S001  ", "S002  ", "S003  "]
    supplier_names = ["Acme Corp       ", "Global Supply   ", "Best Parts      "]

    call suppliers % new()
    call df_append_character(suppliers, supplier_codes, "SupplierCode")
    call df_append_character(suppliers, supplier_names, "SupplierName")

    write (*, '(a)') ""
    write (*, '(a)') "Suppliers Table:"
    call df_write_console(suppliers)

    write (*, '(a)') ""
    write (*, '(a)') "INNER JOIN on SupplierCode:"
    product_join = df_inner_join(products, suppliers, 4, 1)
    call df_write_console(product_join)

    ! Clean up
    call employees % destroy()
    call departments % destroy()
    call inner_result % destroy()
    call left_result % destroy()
    call right_result % destroy()
    call outer_result % destroy()
    call products % destroy()
    call suppliers % destroy()
    call product_join % destroy()

    write (*, '(a)') ""
    write (*, '(a)') "All join tests completed successfully!"
    write (*, '(a)') ""

end program test_joins
