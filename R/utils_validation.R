#' @title Input Validation Utilities
#' @description Internal functions for validating function inputs
#' @name validation
#' @keywords internal
NULL

#' Check if value is a positive numeric
#' @param x Value to check
#' @param name Name of parameter for error message
#' @keywords internal
check_positive_numeric <- function(x, name = "value") {
  if (!is.numeric(x) || length(x) != 1 || x <= 0) {
    stop(sprintf("'%s' must be a single positive numeric value", name), call. = FALSE)
  }
  invisible(TRUE)
}

#' Check if value is a non-negative numeric
#' @param x Value to check
#' @param name Name of parameter for error message
#' @keywords internal
check_non_negative_numeric <- function(x, name = "value") {
  if (!is.numeric(x) || length(x) != 1 || x < 0) {
    stop(sprintf("'%s' must be a single non-negative numeric value", name), call. = FALSE)
  }
  invisible(TRUE)
}

#' Check if value is in range [0, 1]
#' @param x Value to check
#' @param name Name of parameter for error message
#' @keywords internal
check_proportion <- function(x, name = "value") {
  if (!is.numeric(x) || length(x) != 1 || x < 0 || x > 1) {
    stop(sprintf("'%s' must be a single numeric value between 0 and 1", name), call. = FALSE)
  }
  invisible(TRUE)
}

#' Check if value is a character string
#' @param x Value to check
#' @param name Name of parameter for error message
#' @param allow_null Allow NULL values
#' @keywords internal
check_character <- function(x, name = "value", allow_null = FALSE) {
  if (allow_null && is.null(x)) return(invisible(TRUE))
  if (!is.character(x) || length(x) != 1) {
    stop(sprintf("'%s' must be a single character string", name), call. = FALSE)
  }
  invisible(TRUE)
}

#' Check if value is a logical
#' @param x Value to check
#' @param name Name of parameter for error message
#' @keywords internal
check_logical <- function(x, name = "value") {
  if (!is.logical(x) || length(x) != 1) {
    stop(sprintf("'%s' must be a single logical value (TRUE/FALSE)", name), call. = FALSE)
  }
  invisible(TRUE)
}

#' Check if value is a numeric vector
#' @param x Value to check
#' @param name Name of parameter for error message
#' @param min_length Minimum required length
#' @keywords internal
check_numeric_vector <- function(x, name = "value", min_length = 1) {
  if (!is.numeric(x) || length(x) < min_length) {
    stop(sprintf("'%s' must be a numeric vector with at least %d element(s)", 
                 name, min_length), call. = FALSE)
  }
  invisible(TRUE)
}

#' Check if value is a character vector
#' @param x Value to check
#' @param name Name of parameter for error message
#' @param min_length Minimum required length
#' @param allow_null Allow NULL values
#' @keywords internal
check_character_vector <- function(x, name = "value", min_length = 1, allow_null = FALSE) {
  if (allow_null && is.null(x)) return(invisible(TRUE))
  if (!is.character(x) || length(x) < min_length) {
    stop(sprintf("'%s' must be a character vector with at least %d element(s)", 
                 name, min_length), call. = FALSE)
  }
  invisible(TRUE)
}

#' Check if data frame contains required columns
#' @param data Data frame to check
#' @param required_cols Character vector of required column names
#' @param data_name Name of data frame for error message
#' @keywords internal
check_required_columns <- function(data, required_cols, data_name = "data") {
  if (!is.data.frame(data)) {
    stop(sprintf("'%s' must be a data frame", data_name), call. = FALSE)
  }
  missing_cols <- setdiff(required_cols, colnames(data))
  if (length(missing_cols) > 0) {
    stop(sprintf("'%s' is missing required columns: %s", 
                 data_name, paste(missing_cols, collapse = ", ")), call. = FALSE)
  }
  invisible(TRUE)
}

#' Check if object is a page_options instance
#' @param x Object to check
#' @param name Name of parameter for error message
#' @keywords internal
check_page_options <- function(x, name = "options") {
  if (!inherits(x, "page_options")) {
    stop(sprintf("'%s' must be a page_options object", name), call. = FALSE)
  }
  invisible(TRUE)
}

#' Safely get color from palette
#' @param palette Color palette
#' @param index Index to retrieve
#' @param default Default color if index out of bounds
#' @keywords internal
safe_get_color <- function(palette, index, default = "black") {
  if (is.null(index) || is.na(index) || index < 1 || index > length(palette)) {
    return(default)
  }
  palette[index]
}

# ============================================================================
# Viewport Bounds Checking Utilities
# ============================================================================

#' Check if a value is within viewport bounds
#' @param value Numeric value to check
#' @param min_bound Minimum allowed value (default: 0)
#' @param max_bound Maximum allowed value (default: 1)
#' @param name Name of the parameter for error messages
#' @return TRUE if within bounds, FALSE otherwise
#' @keywords internal
check_viewport_bounds <- function(value, min_bound = 0, max_bound = 1, name = "value") {
  if (is.null(value) || !is.numeric(value)) return(TRUE)
  value <- as.numeric(value)
  if (any(is.na(value))) return(TRUE)
  
  out_of_bounds <- value < min_bound | value > max_bound
  !any(out_of_bounds)
}

#' Clamp a value to viewport bounds
#' @param value Numeric value to clamp
#' @param min_bound Minimum allowed value (default: 0)
#' @param max_bound Maximum allowed value (default: 1)
#' @param warn Whether to warn when clamping (default: TRUE)
#' @param name Name of the parameter for warning messages
#' @return Clamped value
#' @keywords internal
clamp_to_bounds <- function(value, min_bound = 0, max_bound = 1, warn = TRUE, name = "value") {
  if (is.null(value) || !is.numeric(value)) return(value)
  
  original <- value
  value <- pmax(pmin(value, max_bound), min_bound)
  
  if (warn && any(original != value, na.rm = TRUE)) {
    warning(sprintf("%s was clamped to viewport bounds [%.3f, %.3f]. Content may be truncated.", 
                    name, min_bound, max_bound), call. = FALSE)
  }
  value
}

#' Calculate required height for visualization
#' @param n_boxes Number of boxes
#' @param categories_per_box Vector of category counts per box
#' @param category_height Height per category (numeric in npc or unit object)
#' @param box_spacing Spacing between boxes
#' @param header_height Header height
#' @param top_margin Top margin
#' @param bottom_margin Bottom margin
#' @param axis_height Additional height for axis labels
#' @return List with required_height, available_height, fits, and overflow
#' @keywords internal
calculate_required_height <- function(n_boxes, categories_per_box, category_height,
                                      box_spacing, header_height, top_margin, 
                                      bottom_margin, axis_height = 0.05) {
  # Convert category_height to numeric if it's a unit
  if (inherits(category_height, "unit")) {
    # Convert mm to approximate npc (assuming ~200mm page height)
    cat_h <- as.numeric(category_height) / 200
  } else {
    cat_h <- as.numeric(category_height)
  }
  
  total_categories <- sum(categories_per_box)
  content_height <- total_categories * cat_h
  spacing_height <- max(0, n_boxes - 1) * box_spacing
  axis_total_height <- n_boxes * axis_height
  
  required <- header_height + content_height + spacing_height + axis_total_height
  available <- 1 - top_margin - bottom_margin
  
  list(
    required_height = required,
    available_height = available,
    fits = required <= available,
    overflow = max(0, required - available),
    scale_factor = if (required > 0) min(1, available / required) else 1
  )
}

#' Calculate auto-scaled parameters to fit content
#' @param height_info Result from calculate_required_height
#' @param current_category_height Current category height
#' @param current_spacing Current box spacing
#' @param n_boxes Number of boxes
#' @return List with adjusted parameters
#' @keywords internal
auto_scale_to_fit <- function(height_info, current_category_height, current_spacing, n_boxes) {
  if (height_info$fits) {
    return(list(
      needs_scaling = FALSE,
      category_height = current_category_height,
      box_spacing = current_spacing,
      scale_factor = 1
    ))
  }
  
  # Wrap everything in tryCatch to prevent crashes
  tryCatch({
    scale <- height_info$scale_factor * 0.95  # 95% to leave small margin
    
    # Scale category height
    if (inherits(current_category_height, "unit")) {
      # Get the numeric value and scale it, then create new unit with "mm"
      # (the default unit type used throughout the package)
      numeric_val <- as.numeric(current_category_height)
      new_cat_height <- grid::unit(numeric_val * scale, "mm")
    } else if (is.numeric(current_category_height)) {
      new_cat_height <- current_category_height * scale
    } else {
      # Fallback: just return original
      new_cat_height <- current_category_height
    }
    
    # Scale spacing
    new_spacing <- current_spacing * scale
    
    list(
      needs_scaling = TRUE,
      category_height = new_cat_height,
      box_spacing = new_spacing,
      scale_factor = scale
    )
  }, error = function(e) {
    # If anything goes wrong, just return the original values without scaling
    warning("auto_scale_to_fit encountered an error: ", e$message, ". Using original values.")
    list(
      needs_scaling = FALSE,
      category_height = current_category_height,
      box_spacing = current_spacing,
      scale_factor = 1
    )
  })
}

#' Validate and adjust y position to stay within bounds
#' @param ypos Y position to validate
#' @param height Height of element being placed
#' @param bottom_margin Minimum y value (bottom margin)
#' @param top_bound Maximum y value (usually 1 - top_margin)
#' @param element_name Name of element for warning messages
#' @return Adjusted y position
#' @keywords internal
validate_y_position <- function(ypos, height = 0, bottom_margin = 0.05, 
                                 top_bound = 0.95, element_name = "element") {
  # Convert unit to numeric if needed
  if (inherits(ypos, "unit")) {
    ypos_num <- as.numeric(ypos)
  } else {
    ypos_num <- as.numeric(ypos)
  }
  
  # Check if bottom of element goes below margin
  element_bottom <- ypos_num - height
  

  if (element_bottom < bottom_margin) {
    warning(sprintf(
      "Visualization content overflow: %s would extend below visible area (y=%.3f). Consider reducing content, using smaller category heights, or increasing page size.",
      element_name, element_bottom
    ), call. = FALSE)
    # Clamp to bottom margin
    ypos_num <- bottom_margin + height
  }
  
  # Check if top goes above bound
  if (ypos_num > top_bound) {
    ypos_num <- top_bound
  }
  
  ypos_num
}

#' Check if layout will fit and provide recommendations
#' @param data Data frame being plotted
#' @param split_axis_by_col Column for axis splitting
#' @param split_box_by_col Column for box splitting
#' @param options page_options object
#' @return List with fit status and recommendations
#' @keywords internal
check_layout_fit <- function(data, split_axis_by_col, split_box_by_col, options) {
  # Count axes and categories
  if (!is.null(split_axis_by_col) && split_axis_by_col %in% names(data)) {
    n_axes <- length(unique(data[[split_axis_by_col]]))
  } else {
    n_axes <- 1
  }
  
  if (!is.null(split_box_by_col) && split_box_by_col %in% names(data)) {
    categories_per_axis <- as.numeric(table(data[[split_axis_by_col]]))
  } else {
    categories_per_axis <- nrow(data)
  }
  
  # Get dimensions from options
  cat_height <- options$box.category.height
  spacing <- options$get_box_spacing()
  header_h <- if (!is.null(options$HEADER_HEIGHT)) options$HEADER_HEIGHT else 0.1
  top_m <- options$PAGE_TOP_MARGIN
  bottom_m <- options$PAGE_BOTTOM_MARGIN
  
  height_info <- calculate_required_height(
    n_boxes = n_axes,
    categories_per_box = categories_per_axis,
    category_height = cat_height,
    box_spacing = spacing,
    header_height = header_h,
    top_margin = top_m,
    bottom_margin = bottom_m
  )
  
  if (!height_info$fits) {
    message(sprintf(
      "Note: Content may not fit in available space (needs %.1f%%, have %.1f%%). Consider:\n  - Reducing box.category.height\n  - Reducing box_spacing\n  - Reducing margins\n  - Using a taller output device",
      height_info$required_height * 100,
      height_info$available_height * 100
    ))
  }
  
  height_info
}
