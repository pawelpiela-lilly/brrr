#' Get metadata for a given dataset
#'
#' This function calculates metadata for a given dataset, including the number 
#' of unique categories, minimum and maximum values, whether the data is reversed, 
#' and the number of subcategories. This metadata is used internally by plot_br().
#'
#' @param data A data frame containing the clinical trial data. Must contain
#'   columns for value, lower, and upper bounds.
#' @param split_axis_by_col Character. The column name to split the data by on the x-axis.
#' @param axis_labels_col Character. The column name containing the axis labels.
#' @param split_box_by_col Character. The column name to split the data by in each box.
#' @param vline_col Character or NULL. The column name for vertical reference line values.
#'
#' @return A data frame containing the calculated metadata with columns:
#' \describe{
#'   \item{ncats}{Number of unique categories}
#'   \item{nsubcats}{Number of subcategories}
#'   \item{minval}{Minimum value across value, lower, upper}
#'   \item{maxval}{Maximum value across value, lower, upper}
#'   \item{reversed}{Whether axis should be reversed}
#'   \item{vertical_line}{Vertical line position if specified}
#'   \item{logscale}{Whether to use log scale (if column exists)}
#'   \item{logbase}{Log base to use (if column exists)}
#' }
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   endpoint = c("A", "A", "B", "B"),
#'   estimator = c("HR", "HR", "OR", "OR"),
#'   value = c(0.8, 1.2, 0.9, 1.1),
#'   lower = c(0.5, 0.9, 0.6, 0.8),
#'   upper = c(1.1, 1.5, 1.2, 1.4)
#' )
#' 
#' meta <- get_metadata(data, "estimator", "estimator", "endpoint", NULL)
#' }
#'
#' @importFrom dplyr group_by mutate across n select any_of distinct ungroup filter arrange
#' @importFrom rlang .data !! :=
#' @export
get_metadata <- function(data, split_axis_by_col, axis_labels_col, split_box_by_col, vline_col) {
  
  # Input validation
  if (!is.data.frame(data)) {
    stop("data must be a data frame", call. = FALSE)
  }
  
  required_cols <- c("value", "lower", "upper")
  missing_cols <- setdiff(required_cols, colnames(data))
  if (length(missing_cols) > 0) {
    stop(sprintf("data is missing required columns: %s", 
                 paste(missing_cols, collapse = ", ")), call. = FALSE)
  }
  
  # Check that grouping columns exist
  grouping_cols <- c(split_axis_by_col, axis_labels_col, split_box_by_col)
  missing_group_cols <- setdiff(grouping_cols, colnames(data))
  if (length(missing_group_cols) > 0) {
    stop(sprintf("data is missing specified grouping columns: %s", 
                 paste(missing_group_cols, collapse = ", ")), call. = FALSE)
  }
  
  # Check vline_col if specified
  if (!is.null(vline_col) && !vline_col %in% colnames(data)) {
    warning(sprintf("vline_col '%s' not found in data; ignoring", vline_col))
    vline_col <- NULL
  }
  
  # Add reversed column if not present
  if (!"reversed" %in% colnames(data)) {
    data$reversed <- FALSE
  }
  
  # Safe max function handling all NAs
  safe_max <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NA_real_)
    max(x)
  }
  
  # Safe min function handling all NAs
  safe_min <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NA_real_)
    min(x)
  }
  
  # Build metadata - using modern dplyr syntax
  result <- data
  
  # Group by split_axis_by_col and calculate group-level stats
  result <- result |>
    dplyr::group_by(dplyr::across(dplyr::all_of(split_axis_by_col)))
  
  # Count unique categories in split_box_by_col
  result <- result |>
    dplyr::mutate(
      ncats = length(unique(.data[[split_box_by_col]]))
    )
  
  # Add vertical line value if column specified
  if (!is.null(vline_col)) {
    result <- result |>
      dplyr::mutate(
        vertical_line = safe_max(.data[[vline_col]])
      )
  } else {
    result <- result |>
      dplyr::mutate(vertical_line = NA_real_)
  }
  
  # Calculate min/max values including vertical line
  result <- result |>
    dplyr::mutate(
      minval = safe_min(c(.data$value, .data$lower, .data$upper, 
                          if (!all(is.na(.data$vertical_line))) as.numeric(.data$vertical_line) else NULL)),
      maxval = safe_max(c(.data$value, .data$lower, .data$upper,
                          if (!all(is.na(.data$vertical_line))) as.numeric(.data$vertical_line) else NULL)),
      reversed = any(.data$reversed, na.rm = TRUE)
    )
  
  # Ungroup temporarily to regroup for subcategory count
  result <- result |>
    dplyr::ungroup()
  
  # Count subcategories within each combination
  result <- result |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(split_box_by_col, split_axis_by_col)))) |>
    dplyr::mutate(nsubcats = dplyr::n()) |>
    dplyr::ungroup()
  
  # Select relevant columns
  select_cols <- c(split_axis_by_col, axis_labels_col, split_box_by_col, 
                   'vertical_line', 'ncats', 'nsubcats', 'minval', 'maxval', 'reversed')
  
  # Add optional columns if they exist
  if ("logscale" %in% colnames(result)) {
    select_cols <- c(select_cols, "logscale")
  }
  if ("logbase" %in% colnames(result)) {
    select_cols <- c(select_cols, "logbase")
  }
  
  result <- result |>
    dplyr::select(dplyr::any_of(select_cols)) |>
    dplyr::distinct()
  
  return(result)
}
