!> DataFort - Modern Data Frame Library for Fortran
!!
!! This module provides a comprehensive data frame implementation for Fortran,
!! offering pandas-like functionality for scientific computing and data analysis.
!!
!! ## Features
!!
!! - **Mixed-type columns**: real, integer, logical, character, complex
!! - **Statistical operations**: mean, std, median, percentile, variance, correlation
!! - **Data manipulation**: filter, sort, slice, transpose, join
!! - **Mathematical functions**: cumsum, diff, normalize, log, exp, sqrt
!! - **I/O operations**: CSV import/export, console display
!! - **Convenience methods**: head, tail, info, describe, sample, shuffle
!!
!! ## Example Usage
!!
!! ```fortran
!! use datafort
!! use precision
!! type(data_frame) :: df
!! real(rk) :: temps(3) = [20.0_rk, 25.0_rk, 22.0_rk]
!!
!! call df%new()
!! call df%append(temps, "Temperature")
!! call df%write_console()
!! print*, "Mean:", df%mean_real(1)
!! call df%destroy()
!! ```
!!
!! ## Module Organization
!!
!! This module serves as the main entry point and re-exports all functionality
!! from specialized submodules:
!!
!! - `datafort_types` - Core data_frame type definition
!! - `datafort_accessors` - Append, get, and set operations
!! - `datafort_statistics` - Statistical operations
!! - `datafort_transformations` - Mathematical transformations
!! - `datafort_manipulation` - Data selection and manipulation
!! - `datafort_sorting` - Sorting and ranking operations
!! - `datafort_io` - Input/output operations
!! - `datafort_utilities` - Convenience functions
!! - `datafort_joins` - Join operations
!! - `datafort_nan` - NaN handling
!! - `datafort_advanced` - Advanced data operations
module datafort
    use precision
    use types
    use column_class

    ! Import core type and utilities
    use datafort_types, only: data_frame, MAX_CHAR_LEN_DEFAULT

    ! Import accessor operations
    use datafort_accessors, only: &
        df_append_real, df_append_integer, df_append_logical, &
        df_append_character, df_append_complex, &
        df_get_col_real, df_get_col_integer, df_get_col_logical, &
        df_get_col_character, df_get_col_complex, &
        df_get_val_real, df_get_val_integer, df_get_val_logical, &
        df_get_val_character, df_get_val_complex, &
        df_set_col_real, df_set_col_integer, df_set_col_logical, &
        df_set_col_character, df_set_col_complex, &
        df_set_val_real, df_set_val_integer, df_set_val_logical, &
        df_set_val_character, df_set_val_complex

    ! Import statistical operations
    use datafort_statistics, only: &
        df_sum_real, df_sum_integer, &
        df_mean_real, df_mean_integer, &
        df_std_real, df_std_integer, &
        df_median_real, df_median_integer, &
        df_percentile_real, df_percentile_integer, &
        df_variance_real, df_variance_integer, &
        df_min_real, df_min_integer, &
        df_max_real, df_max_integer, &
        df_correlation_real, &
        df_describe_numeric, &
        df_skewness_real, df_skewness_integer, &
        df_kurtosis_real, df_kurtosis_integer

    ! Import transformation operations
    use datafort_transformations, only: &
        df_normalize_column_real, df_standardize_column_real, &
        df_abs_column_real, df_abs_column_integer, &
        df_cumsum_real, df_cumsum_integer, &
        df_diff_real, df_diff_integer, &
        df_replace_value_real, df_replace_value_integer, &
        df_clip_real, df_clip_integer, &
        df_round_column, &
        df_log_column, df_exp_column, df_sqrt_column, df_pow_column, &
        df_apply_to_column, &
        transform_func

    ! Import manipulation operations
    use datafort_manipulation, only: &
        df_select_columns, &
        df_slice_rows, &
        df_filter_rows_logical, &
        df_filter_rows_real_range, &
        df_filter_rows_integer_range, &
        df_filter_rows_string_pattern, &
        df_copy, &
        df_transpose, &
        df_rename_column, &
        df_drop_column, &
        df_reorder_columns, &
        df_isin_real, df_isin_integer, df_isin_character, &
        df_insert_column_real, df_insert_column_integer, df_insert_column_logical, &
        df_insert_column_character, df_insert_column_complex

    ! Import sorting operations
    use datafort_sorting, only: &
        df_sort_by_column, &
        df_is_sorted_real, df_is_sorted_integer, &
        df_rank_real, df_rank_integer

    ! Import I/O operations
    use datafort_io, only: &
        df_write_csv, df_read_csv, df_write_console

    ! Import utility functions
    use datafort_utilities, only: &
        df_head, df_tail, df_shape, df_info, df_empty, df_clear, &
        df_sample, df_shuffle, &
        df_apply_to_row_real, df_apply_to_all_rows_real, &
        row_func_real, &
        df_nlargest_real, df_nsmallest_real, &
        df_nlargest_integer, df_nsmallest_integer, &
        df_to_array_real, df_equals, df_pipe, pipe_func

    ! Import join operations
    use datafort_joins, only: &
        df_inner_join, df_left_join, df_right_join, df_outer_join, &
        df_merge

    ! Import NaN handling
    use datafort_nan, only: &
        df_isna_real, df_isna_integer, &
        df_fillna_real, df_fillna_integer, &
        df_dropna

    ! Import advanced operations
    use datafort_advanced, only: &
        df_unique_real, df_unique_integer, df_unique_character, &
        df_value_counts_real, df_value_counts_integer, df_value_counts_character, &
        df_concat, &
        df_duplicated, df_drop_duplicates, df_drop_duplicates_subset, &
        df_nunique_real, df_nunique_integer, df_nunique_character

    implicit none
    private

    ! Re-export the data_frame type and constant
    public :: data_frame
    public :: MAX_CHAR_LEN_DEFAULT

    ! Re-export accessor operations
    public :: df_append_real, df_append_integer, df_append_logical
    public :: df_append_character, df_append_complex
    public :: df_get_col_real, df_get_col_integer, df_get_col_logical
    public :: df_get_col_character, df_get_col_complex
    public :: df_get_val_real, df_get_val_integer, df_get_val_logical
    public :: df_get_val_character, df_get_val_complex
    public :: df_set_col_real, df_set_col_integer, df_set_col_logical
    public :: df_set_col_character, df_set_col_complex
    public :: df_set_val_real, df_set_val_integer, df_set_val_logical
    public :: df_set_val_character, df_set_val_complex

    ! Re-export statistical operations
    public :: df_sum_real, df_sum_integer
    public :: df_mean_real, df_mean_integer
    public :: df_std_real, df_std_integer
    public :: df_median_real, df_median_integer
    public :: df_percentile_real, df_percentile_integer
    public :: df_variance_real, df_variance_integer
    public :: df_min_real, df_min_integer
    public :: df_max_real, df_max_integer
    public :: df_correlation_real
    public :: df_describe_numeric
    public :: df_skewness_real, df_skewness_integer
    public :: df_kurtosis_real, df_kurtosis_integer

    ! Re-export transformation operations
    public :: df_normalize_column_real, df_standardize_column_real
    public :: df_abs_column_real, df_abs_column_integer
    public :: df_cumsum_real, df_cumsum_integer
    public :: df_diff_real, df_diff_integer
    public :: df_replace_value_real, df_replace_value_integer
    public :: df_clip_real, df_clip_integer
    public :: df_round_column
    public :: df_log_column, df_exp_column, df_sqrt_column, df_pow_column
    public :: df_apply_to_column
    public :: transform_func

    ! Re-export manipulation operations
    public :: df_select_columns
    public :: df_slice_rows
    public :: df_filter_rows_logical
    public :: df_filter_rows_real_range
    public :: df_filter_rows_integer_range
    public :: df_filter_rows_string_pattern
    public :: df_copy
    public :: df_transpose
    public :: df_rename_column
    public :: df_drop_column
    public :: df_reorder_columns
    public :: df_isin_real, df_isin_integer, df_isin_character
    public :: df_insert_column_real, df_insert_column_integer, df_insert_column_logical
    public :: df_insert_column_character, df_insert_column_complex

    ! Re-export sorting operations
    public :: df_sort_by_column
    public :: df_is_sorted_real, df_is_sorted_integer
    public :: df_rank_real, df_rank_integer

    ! Re-export I/O operations
    public :: df_write_csv, df_read_csv, df_write_console

    ! Re-export utility functions
    public :: df_head, df_tail, df_shape, df_info, df_empty, df_clear
    public :: df_sample, df_shuffle
    public :: df_apply_to_row_real, df_apply_to_all_rows_real
    public :: row_func_real
    public :: df_nlargest_real, df_nsmallest_real
    public :: df_nlargest_integer, df_nsmallest_integer
    public :: df_to_array_real, df_equals, df_pipe, pipe_func

    ! Re-export join operations
    public :: df_inner_join, df_left_join, df_right_join, df_outer_join
    public :: df_merge

    ! Re-export NaN handling
    public :: df_isna_real, df_isna_integer
    public :: df_fillna_real, df_fillna_integer
    public :: df_dropna

    ! Re-export advanced operations
    public :: df_unique_real, df_unique_integer, df_unique_character
    public :: df_value_counts_real, df_value_counts_integer, df_value_counts_character
    public :: df_concat
    public :: df_duplicated, df_drop_duplicates, df_drop_duplicates_subset
    public :: df_nunique_real, df_nunique_integer, df_nunique_character

end module datafort
