# DataFort Refactoring Plan

## Overview
Refactor the monolithic `datafort.f90` (4579 lines) into separate, focused modules to improve maintainability and organization.

## Already Extracted Modules

### 1. `datafort_types.f90` ✓
- Type definition for `data_frame`
- Constructor/Destructor (`new`, `destroy`, `is_initialized`)
- Basic getters (`ncols`, `nrows`, `get_max_char_len`, `header`, `dtype`)
- Internal utility procedures (`already_header`, `resize_storage`, `validate_column_addition`, `find_header_index`, etc.)

### 2. `datafort_accessors.f90` ✓
- Column append functions (`df_append_real/integer/logical/character/complex`)
- Get column functions (`df_get_col_*`)
- Get value functions (`df_get_val_*`)
- Set column functions (`df_set_col_*`)
- Set value functions (`df_set_val_*`)

## Modules to Create

### 3. `datafort_statistics.f90`
**Purpose:** Statistical operations and aggregations

**Procedures (18):**
- `df_sum_real` - Sum of real column
- `df_sum_integer` - Sum of integer column
- `df_mean_real` - Arithmetic mean of real column
- `df_mean_integer` - Arithmetic mean of integer column
- `df_std_real` - Standard deviation of real column
- `df_std_integer` - Standard deviation of integer column
- `df_median_real` - Median of real column
- `df_median_integer` - Median of integer column
- `df_percentile_real` - Percentile calculation for real column
- `df_percentile_integer` - Percentile calculation for integer column
- `df_variance_real` - Variance of real column
- `df_variance_integer` - Variance of integer column
- `df_min_real` - Minimum value in real column
- `df_min_integer` - Minimum value in integer column
- `df_max_real` - Maximum value in real column
- `df_max_integer` - Maximum value in integer column
- `df_correlation_real` - Pearson correlation between two real columns
- `df_describe_numeric` - Print summary statistics for all numeric columns

---

### 4. `datafort_transformations.f90`
**Purpose:** Mathematical transformations and data modifications

**Procedures (18):**
- `df_normalize_column_real` - Normalize to [0,1] range (min-max scaling)
- `df_standardize_column_real` - Standardize (z-score: mean=0, std=1)
- `df_abs_column_real` - Absolute value of real column
- `df_abs_column_integer` - Absolute value of integer column
- `df_cumsum_real` - Cumulative sum for real column
- `df_cumsum_integer` - Cumulative sum for integer column
- `df_diff_real` - Differences between consecutive rows (real)
- `df_diff_integer` - Differences between consecutive rows (integer)
- `df_replace_value_real` - Replace all occurrences of a value (real)
- `df_replace_value_integer` - Replace all occurrences of a value (integer)
- `df_clip_real` - Clamp values to [min, max] range (real)
- `df_clip_integer` - Clamp values to [min, max] range (integer)
- `df_round_column` - Round real column to decimal places
- `df_log_column` - Apply natural logarithm to column
- `df_exp_column` - Apply exponential to column
- `df_sqrt_column` - Apply square root to column
- `df_pow_column` - Raise column to a power
- `df_apply_to_column` - Apply custom function to column

---

### 5. `datafort_manipulation.f90`
**Purpose:** Data selection and manipulation

**Procedures (11):**
- `df_select_columns` - Extract subset of columns
- `df_slice_rows` - Extract row range
- `df_filter_rows_logical` - Filter rows by logical column
- `df_filter_rows_real_range` - Filter rows by real value range
- `df_filter_rows_integer_range` - Filter rows by integer value range
- `df_filter_rows_string_pattern` - Filter rows by string pattern
- `df_copy` - Deep copy of data frame
- `df_transpose` - Transpose data frame
- `df_rename_column` - Rename a column
- `df_drop_column` - Remove a column
- `df_reorder_columns` - Reorder columns

**Helper Procedures:**
- `copy_filtered_column` - Copy filtered column data to target data frame

---

### 6. `datafort_sorting.f90`
**Purpose:** Sorting and ranking operations

**Procedures (11):**
- `df_sort_by_column` - Sort data frame by column values
- `df_is_sorted_real` - Check if real column is sorted
- `df_is_sorted_integer` - Check if integer column is sorted
- `df_rank_real` - Rank values in real column
- `df_rank_integer` - Rank values in integer column

**Helper Procedures:**
- `sort_indices_real` - Create sorted indices for real array
- `sort_indices_integer` - Create sorted indices for integer array
- `quicksort_indices_real` - Quicksort implementation for real indices
- `quicksort_indices_integer` - Quicksort implementation for integer indices
- `partition_indices_real` - Partition for quicksort (real)
- `partition_indices_integer` - Partition for quicksort (integer)
- `reorder_all_columns` - Reorder all columns by index array

---

### 7. `datafort_io.f90`
**Purpose:** Input/output operations

**Procedures (5):**
- `df_write_csv` - Export data frame to CSV file
- `df_read_csv` - Import data frame from CSV file
- `df_write_console` - Display data frame in terminal

**Helper Procedures:**
- `parse_csv_line` - Parse a CSV line into fields
- `add_csv_column` - Add column with automatic type detection

---

### 8. `datafort_joins.f90`
**Purpose:** Join operations

**Procedures (6):**
- `df_inner_join` - SQL-style inner join
- `df_left_join` - SQL-style left join
- `df_right_join` - SQL-style right join
- `df_outer_join` - SQL-style outer join (full outer join)
- `df_merge` - Merge using column names

**Helper Procedures:**
- `build_joined_dataframe` - Build result from join indices

---

### 9. `datafort_nan.f90`
**Purpose:** NaN handling

**Procedures (5):**
- `df_isna_real` - Check for NaN values in real column
- `df_isna_integer` - Check for NaN values in integer column
- `df_fillna_real` - Replace NaN with fill value (real)
- `df_fillna_integer` - Replace NaN with fill value (integer)
- `df_dropna` - Remove rows containing NaN values

---

### 10. `datafort_advanced.f90`
**Purpose:** Advanced data operations

**Procedures (9):**
- `df_unique_real` - Get unique values from real column
- `df_unique_integer` - Get unique values from integer column
- `df_unique_character` - Get unique values from character column
- `df_value_counts_real` - Count occurrences of each value (real)
- `df_value_counts_integer` - Count occurrences of each value (integer)
- `df_value_counts_character` - Count occurrences of each value (character)
- `df_concat` - Concatenate two data frames (vertical/horizontal)
- `df_duplicated` - Check which rows are duplicates
- `df_drop_duplicates` - Remove duplicate rows
- `df_drop_duplicates_subset` - Remove duplicates based on specific columns

---

### 11. `datafort_utilities.f90`
**Purpose:** Convenience functions

**Procedures (10):**
- `df_head` - Return first n rows
- `df_tail` - Return last n rows
- `df_shape` - Return [nrows, ncols] dimensions
- `df_info` - Print data frame information
- `df_empty` - Check if data frame is empty
- `df_clear` - Clear data frame (destroy and re-initialize)
- `df_sample` - Get n random rows
- `df_shuffle` - Shuffle all rows randomly
- `df_apply_to_row_real` - Apply function to single row
- `df_apply_to_all_rows_real` - Apply function to all rows

---

## Final Module Structure

```
src/
├── datafort_types.f90              # ✓ Type definitions, constructors, getters
├── datafort_accessors.f90          # ✓ Append, get_col, get_val, set_col, set_val
├── datafort_statistics.f90         # Statistics & aggregations (18 procedures)
├── datafort_transformations.f90    # Mathematical transformations (18 procedures)
├── datafort_manipulation.f90       # Data selection & manipulation (11 procedures)
├── datafort_sorting.f90            # Sorting & ranking (11 procedures)
├── datafort_io.f90                 # Input/output operations (5 procedures)
├── datafort_joins.f90              # Join operations (6 procedures)
├── datafort_nan.f90                # NaN handling (5 procedures)
├── datafort_advanced.f90           # Advanced data operations (9 procedures)
├── datafort_utilities.f90          # Convenience functions (10 procedures)
└── datafort.f90                    # Main module: re-exports everything
```

## Implementation Strategy

1. **Function Signatures:** All extracted procedures will be standalone functions/subroutines that take `type(data_frame)` as the first argument, following the pattern used in `datafort_accessors.f90`

2. **Naming Convention:** All public procedures will be prefixed with `df_` to maintain consistency and avoid naming conflicts

3. **Dependencies:** Modules will import from:
   - `datafort_types` - for the `data_frame` type
   - `precision` - for `rk`, `ik` kinds
   - `types` - for type constants
   - `column_class` - for column operations
   - Other datafort modules as needed

4. **Main Module:** The final `datafort.f90` will:
   - Import all submodules
   - Re-export all public procedures
   - Provide a clean unified interface

5. **Testing:** After each module is created, verify compilation and run existing tests

## Total Procedure Count
- **Already extracted:** ~50 procedures (types + accessors)
- **To extract:** ~115 procedures
- **Total:** ~165 procedures

## Benefits

1. **Maintainability:** Smaller, focused files are easier to understand and modify
2. **Compilation speed:** Changes to one module won't require recompiling everything
3. **Code organization:** Related functionality is grouped logically
4. **Testing:** Easier to write targeted tests for specific functionality
5. **Collaboration:** Multiple developers can work on different modules simultaneously
