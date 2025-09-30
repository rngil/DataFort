---
title: Documentation
---

<script>
document.addEventListener('DOMContentLoaded', function() {
    // Create sidebar
    var sidebar = document.createElement('div');
    sidebar.id = 'guide-sidebar';
    sidebar.innerHTML = '<h4>Contents</h4><ul id="sidebar-nav"></ul>';

    // Find all h2 headings in the main content
    var headings = document.querySelectorAll('#text h2');
    var navList = sidebar.querySelector('#sidebar-nav');

    headings.forEach(function(heading) {
        var li = document.createElement('li');
        var a = document.createElement('a');

        // Create or use existing anchor
        if (!heading.id) {
            heading.id = heading.textContent.toLowerCase().replace(/\s+/g, '-').replace(/[^\w-]/g, '');
        }

        a.href = '#' + heading.id;
        a.textContent = heading.textContent;
        li.appendChild(a);
        navList.appendChild(li);

        // Smooth scroll
        a.addEventListener('click', function(e) {
            e.preventDefault();
            heading.scrollIntoView({ behavior: 'smooth', block: 'start' });
        });
    });

    // Append sidebar to body
    document.body.appendChild(sidebar);
});
</script>

# DataFort Guide

---

## Reading and Writing Data

### Load CSV with Headers
```fortran
type(data_frame) :: df
call df%read_csv("data.csv", .true.)  ! .true. = has headers
call df%write_console()
```

### Write to CSV
```fortran
call df%write_csv("output.csv")
```

---

## Data Exploration

### Display First/Last Rows
```fortran
type(data_frame) :: head_df, tail_df
head_df = df%head(5)     ! First 5 rows
tail_df = df%tail(10)    ! Last 10 rows
call head_df%write_console()
```

### Get DataFrame Info
```fortran
call df%info()           ! Print structure and types
call df%describe_numeric()  ! Statistical summary

print*, "Shape:", df%shape()
print*, "Rows:", df%nrows()
print*, "Cols:", df%ncols()
```

### Access Individual Elements

#### Get a Single Value
```fortran
real(rk) :: value_r
integer(ik) :: value_i
character(len=:), allocatable :: value_c
logical :: value_l

! Get value at row i, column j (by index)
call df%get_val_real(j, i, value_r)       ! For real columns
call df%get_val_integer(j, i, value_i)    ! For integer columns
call df%get_val_character(j, i, value_c)  ! For character columns
call df%get_val_logical(j, i, value_l)    ! For logical columns

! Example: Get value at row 5, column 2
call df%get_val_real(2, 5, value_r)
print*, "Value at [5,2]:", value_r
```

#### Get Value by Column Name
```fortran
character(len=:), allocatable :: col_name
real(rk) :: temperature
integer :: row_idx, col_idx

row_idx = 10
col_name = "Temperature"

! First find column index
col_idx = df%find_header_index(col_name)

! Then get the value
call df%get_val_real(col_idx, row_idx, temperature)
print*, "Temperature at row", row_idx, ":", temperature
```

#### Set a Single Value
```fortran
! Set value at row i, column j
call df%set_val_real(j, i, 42.5_rk)        ! For real columns
call df%set_val_integer(j, i, 100_ik)      ! For integer columns
call df%set_val_character(j, i, "NewVal")  ! For character columns
call df%set_val_logical(j, i, .true.)      ! For logical columns

! Example: Set value at row 3, column 1
call df%set_val_real(1, 3, 99.9_rk)
```

#### Access by Header Name
```fortran
character(len=:), allocatable :: header
integer :: col_index

! Get column index from header name
header = "Age"
col_index = df%find_header_index(header)

if (col_index > 0) then
    ! Use the column index to access data
    call df%get_val_integer(col_index, 5, value_i)
    print*, "Age at row 5:", value_i
else
    print*, "Column not found"
end if
```

---

## Data Selection and Filtering

### Select Columns
```fortran
type(data_frame) :: subset
integer, dimension(3) :: cols = [1, 3, 5]
subset = df%select_columns(cols)
```

### Slice Rows
```fortran
type(data_frame) :: sliced
sliced = df%slice_rows(10, 20)  ! Rows 10 to 20
```

### Filter by Condition
```fortran
type(data_frame) :: filtered
logical, dimension(:), allocatable :: mask

! Filter numeric range
filtered = df%filter_rows_real_range(1, 20.0_rk, 30.0_rk)

! Filter by string pattern
filtered = df%filter_rows_string_pattern(2, "Alice")

! Custom logical mask
allocate(mask(df%nrows()))
mask = temperatures > 25.0_rk
filtered = df%filter_rows_logical(mask)
```

---

## Statistics and Aggregation

### Basic Statistics
```fortran
real(rk) :: avg, std_dev, var
integer(ik) :: total

! Column 1 statistics
avg = df%mean_real(1)
std_dev = df%std_real(1)
var = df%variance_real(1)
total = df%sum_integer(2)

print*, "Mean:", avg
print*, "Std Dev:", std_dev
```

### Percentiles and Quantiles
```fortran
real(rk) :: median, q25, q75

median = df%median_real(1)
q25 = df%percentile_real(1, 25.0_rk)
q75 = df%percentile_real(1, 75.0_rk)

print*, "Median:", median
print*, "IQR:", q75 - q25
```

### Correlation
```fortran
real(rk) :: corr
corr = df%correlation_real(1, 2)  ! Between columns 1 and 2
print*, "Correlation:", corr
```

---

## Data Transformation

### Normalize Data
```fortran
call df%normalize_column_real(1)  ! Scale to [0, 1]
```

### Standardize Data
```fortran
call df%standardize_column_real(1)  ! z-score normalization
```

### Mathematical Operations
```fortran
! Apply functions to entire columns
call df%abs_column_real(1)
call df%log_column(1)
call df%exp_column(1)
call df%sqrt_column(1)
call df%pow_column(1, 2.0_rk)  ! Raise to power
call df%round_column(1, 2)     ! Round to 2 decimals
```

### Cumulative and Differencing
```fortran
real(rk), dimension(:), allocatable :: cumulative, differences

cumulative = df%cumsum_real(1)
differences = df%diff_real(1)
```

---

## Sorting and Ranking

### Sort by Column
```fortran
call df%sort_by_column(1, ascending=.true.)
```

### Get Ranks
```fortran
real(rk), dimension(:), allocatable :: ranks
ranks = df%rank_real(1)
```

### Check if Sorted
```fortran
logical :: sorted
sorted = df%is_sorted_real(1)
```

---

## Missing Data (NaN) Handling

### Check for Missing Values
```fortran
logical, dimension(:), allocatable :: na_mask
na_mask = df%isna_real(1)

! Count missing values
print*, "Missing values:", count(na_mask)
```

### Fill Missing Values
```fortran
call df%fillna_real(1, 0.0_rk)  ! Fill with 0
```

### Drop Missing Values
```fortran
type(data_frame) :: cleaned
cleaned = df%dropna()
```

---

## Column Operations

### Add Column
```fortran
real(rk), dimension(100) :: new_data
call df%append(new_data, "NewColumn")
```

### Drop Column
```fortran
call df%drop_column(3)  ! Drop column 3
```

### Rename Column
```fortran
call df%rename_column(1, "Temperature_C")
```

### Reorder Columns
```fortran
integer, dimension(4) :: new_order = [3, 1, 4, 2]
call df%reorder_columns(new_order)
```

### Get Column Type
```fortran
integer :: col_type
col_type = df%get_col_type(1)
! Returns: 1=Real, 2=Integer, 3=Logical, 4=Character, 5=Complex
```

---

## Joining and Merging

### Inner Join
```fortran
type(data_frame) :: df1, df2, result
result = df1%inner_join(df2, 1, 1)  ! Join on column 1 of both
```

### Left Join
```fortran
result = df1%left_join(df2, 1, 1)
```

### Right Join
```fortran
result = df1%right_join(df2, 1, 1)
```

### Outer Join
```fortran
result = df1%outer_join(df2, 1, 1)
```

### Concatenate DataFrames
```fortran
type(data_frame) :: combined
combined = df1%concat(df2)  ! Stack vertically
```

---

## Duplicates

### Find Duplicates
```fortran
logical, dimension(:), allocatable :: is_dup
is_dup = df%duplicated(1)  ! Check column 1
```

### Drop Duplicates
```fortran
type(data_frame) :: unique_df
unique_df = df%drop_duplicates(1)  ! Based on column 1
```

### Get Unique Values
```fortran
real(rk), dimension(:), allocatable :: unique_vals
unique_vals = df%unique_real(1)
```

### Value Counts
```fortran
type(data_frame) :: counts
counts = df%value_counts_real(1)
call counts%write_console()
```

---

## Row Operations

### Apply Function to Row
```fortran
! Define a function that operates on a row
function row_sum(row_values, num_cols) result(output)
    use precision
    real(rk), dimension(:), intent(in) :: row_values
    integer, intent(in) :: num_cols
    real(rk) :: output
    output = sum(row_values)
end function row_sum

! Apply to a single row
real(rk) :: result
result = df%apply_to_row_real(5, row_sum)  ! Apply to row 5
```

### Apply Function to All Rows
```fortran
real(rk), dimension(:), allocatable :: results
results = df%apply_to_all_rows_real(row_sum)  ! Apply to all rows
```

---

## Other Operations

### Transpose
```fortran
type(data_frame) :: transposed
transposed = df%transpose()
```

### Sample Rows
```fortran
type(data_frame) :: sample_df
sample_df = df%sample(10)  ! Random 10 rows
```

### Shuffle
```fortran
call df%shuffle()  ! Randomize row order
```

### Copy
```fortran
type(data_frame) :: df_copy
df_copy = df%copy()
```

### Clear
```fortran
call df%clear()  ! Empty the dataframe
```

---

## Complete Example

```fortran
program datafort_example
    use datafort
    use precision
    implicit none

    type(data_frame) :: df, filtered, stats
    real(rk), dimension(5) :: temps = [23.1_rk, 25.3_rk, 24.8_rk, 22.5_rk, 26.0_rk]
    integer(ik), dimension(5) :: ids = [1_ik, 2_ik, 3_ik, 4_ik, 5_ik]

    ! Create dataframe
    call df%new()
    call df%append(ids, "ID")
    call df%append(temps, "Temperature")

    ! Basic info
    call df%info()
    call df%describe_numeric()

    ! Statistics
    print*, "Mean temp:", df%mean_real(2)
    print*, "Max temp:", df%max_real(2)

    ! Filter
    filtered = df%filter_rows_real_range(2, 24.0_rk, 26.0_rk)
    call filtered%write_console()

    ! Transform
    call df%normalize_column_real(2)

    ! Export
    call df%write_csv("output.csv")

    ! Cleanup
    call df%destroy()
    call filtered%destroy()

end program datafort_example
```