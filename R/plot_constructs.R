#' Plot a horizontal segment on a box plot
#'
#' This function plots a horizontal segment (line) on a box plot, typically used
#' for confidence intervals in forest plots.
#'
#' @param box The box object containing axis and position information.
#' @param x Numeric vector of length 2. The x-coordinates (start and end) of the segment.
#' @param n1 Integer. The row/category index within the box.
#' @param n2 Integer. The sub-row position within the category. Default is 1.
#' @param N2 Integer. The total number of sub-rows in the category. Default is 1.
#' @param lty Integer. The line type. Default is 1 (solid).
#' @param lwd Numeric. The line width. Default is 3.
#' @param col Character. The line color. Default is 'black'.
#'
#' @return Invisible NULL. Draws on the current graphics device.
#'
#' @examples
#' \dontrun{
#' # After creating a box object
#' plot_horizontal_seg(box, x = c(0.5, 1.5), n1 = 1, col = 'blue')
#' }
#'
#' @import grid
#' @export
plot_horizontal_seg <- function(box, x, n1, n2 = 1, N2 = 1, lty = 1, lwd = 3, col = 'black') {
  
  # Local helper to clamp values
  clamp_value <- function(x, min_val = 0, max_val = 1) {
    pmax(pmin(x, max_val), min_val)
  }
  
  # Input validation
  if (is.null(box$axis) || is.null(box$axis$axis_function)) {
    stop("box must contain a valid axis object with axis_function", call. = FALSE)
  }
  if (!is.numeric(x) || length(x) != 2) {
    stop("x must be a numeric vector of length 2", call. = FALSE)
  }
  if (!is.numeric(n1) || n1 < 1 || n1 >= length(box$y_pos)) {
    warning(sprintf("n1 (%d) is out of range (1-%d), element may not be visible", 
                    n1, length(box$y_pos) - 1), call. = FALSE)
    return(invisible(NULL))
  }
  
  # Transform x coordinates using axis function
  xvec <- sapply(x, function(xi) box$axis$axis_function(xi))
  
  # Calculate y position
  yvec <- box$y_pos[n1] + (box$y_pos[n1 + 1] - box$y_pos[n1]) * n2 / (N2 + 1)
  
  # Check if y position is within viewport - clamp without error
  if (yvec < 0 || yvec > 1) {
    warning(sprintf("Element y position (%.3f) is outside viewport, element will be clipped", 
                    yvec), call. = FALSE)
    yvec <- clamp_value(yvec, 0.01, 0.99)
  }
  
  grid.lines(
    x = xvec, 
    y = rep(yvec, 2), 
    gp = gpar(lty = lty, lwd = lwd, col = col)
  )
  
  invisible(NULL)
}


#' Plot a long rectangle on a box plot
#'
#' This function plots a horizontal rectangle on a box plot, used as an alternative
#' to line segments for representing ranges.
#'
#' @param box The box object containing axis and position information.
#' @param x Numeric vector of length 2. The x-coordinates (start and end) of the rectangle.
#' @param n1 Integer. The row/category index within the box.
#' @param n2 Integer. The sub-row position within the category. Default is 1.
#' @param N2 Integer. The total number of sub-rows in the category. Default is 1.
#' @param height Numeric. The height of the rectangle. Default is 0.015.
#' @param lty Integer. The line type for the border. Default is 1 (solid).
#' @param lwd Numeric. The line width for the border. Default is 3.
#' @param col Character. The color of the rectangle. Default is 'black'.
#'
#' @return Invisible NULL. Draws on the current graphics device.
#'
#' @examples
#' \dontrun{
#' # After creating a box object
#' plot_long_rectangle(box, x = c(0.5, 1.5), n1 = 1, height = 0.02, col = 'red')
#' }
#'
#' @import grid
#' @export
plot_long_rectangle <- function(box, x, n1, n2 = 1, N2 = 1, height = 0.015, 
                                lty = 1, lwd = 3, col = 'black') {
  
  # Local helper to clamp values
  clamp_value <- function(x, min_val = 0, max_val = 1) {
    pmax(pmin(x, max_val), min_val)
  }
  
  # Input validation
  if (is.null(box$axis) || is.null(box$axis$axis_function)) {
    stop("box must contain a valid axis object with axis_function", call. = FALSE)
  }
  if (!is.numeric(x) || length(x) != 2) {
    stop("x must be a numeric vector of length 2", call. = FALSE)
  }
  if (!is.numeric(n1) || n1 < 1 || n1 >= length(box$y_pos)) {
    warning(sprintf("n1 (%d) is out of range (1-%d), element may not be visible", 
                    n1, length(box$y_pos) - 1), call. = FALSE)
    return(invisible(NULL))
  }
  
  # Transform x coordinates
  xvec <- sapply(x, function(xi) box$axis$axis_function(xi))
  
  # Calculate y position
  yvec <- box$y_pos[n1] + (box$y_pos[n1 + 1] - box$y_pos[n1]) * n2 / (N2 + 1)
  
  # Check if y position is within viewport - clamp without error
  if (yvec < 0 || yvec > 1) {
    warning(sprintf("Element y position (%.3f) is outside viewport, element will be clipped", 
                    yvec), call. = FALSE)
    yvec <- clamp_value(yvec, 0.01, 0.99)
  }
  
  # Calculate height (default to 1/5 of category height if NULL)
  if (is.null(height)) {
    height <- (box$y_pos[n1 + 1] - box$y_pos[n1]) / 5
  }
  
  grid.rect(
    x = unit(xvec[1], 'npc'), 
    y = unit(yvec, 'npc'), 
    width = unit(diff(xvec), 'npc'), 
    height = unit(height, 'npc'),
    just = c('left', 'center'),
    gp = gpar(lty = lty, lwd = lwd, col = col)
  )
  
  invisible(NULL)
}


#' Plot dots on a box
#'
#' This function plots point markers on a box, typically used for point estimates
#' in forest plots.
#'
#' @param box The box object containing axis and position information.
#' @param x Numeric. The x-coordinate of the dot (on the data scale).
#' @param n1 Integer. The row/category index within the box.
#' @param n2 Integer. The sub-row position within the category. Default is 1.
#' @param N2 Integer. The total number of sub-rows in the category. Default is 1.
#' @param pch Integer. The point character to use. Default is NULL (auto-select).
#'   If N2 > 1, cycles through filled shapes (21-26).
#' @param size A unit object. The size of the point. Default is unit(1, 'char').
#' @param col Character. The color of the point. Default is 'black'.
#'
#' @return Invisible NULL. Draws on the current graphics device.
#'
#' @examples
#' \dontrun{
#' # After creating a box object
#' plot_dot(box, x = 1.0, n1 = 1, col = 'purple')
#' 
#' # Multiple points in same category
#' plot_dot(box, x = 0.8, n1 = 1, n2 = 1, N2 = 2, col = 'blue')
#' plot_dot(box, x = 1.2, n1 = 1, n2 = 2, N2 = 2, col = 'red')
#' }
#'
#' @import grid
#' @export
plot_dot <- function(box, x, n1, n2 = 1, N2 = 1, pch = NULL, 
                     size = unit(1, 'char'), col = 'black') {
  
  # Local helper to clamp values
  clamp_value <- function(x, min_val = 0, max_val = 1) {
    pmax(pmin(x, max_val), min_val)
  }
  
  # Input validation
  if (is.null(box$axis) || is.null(box$axis$axis_function)) {
    stop("box must contain a valid axis object with axis_function", call. = FALSE)
  }
  if (!is.numeric(x)) {
    stop("x must be numeric", call. = FALSE)
  }
  if (!is.numeric(n1) || n1 < 1 || n1 >= length(box$y_pos)) {
    warning(sprintf("n1 (%d) is out of range (1-%d), element may not be visible", 
                    n1, length(box$y_pos) - 1), call. = FALSE)
    return(invisible(NULL))
  }
  
  # Transform x coordinate
  xvec <- sapply(x, function(xi) box$axis$axis_function(xi))
  
  # Calculate y position
  yvec <- box$y_pos[n1] + (box$y_pos[n1 + 1] - box$y_pos[n1]) * n2 / (N2 + 1)
  
  # Check if y position is within viewport - clamp without error
  if (yvec < 0 || yvec > 1) {
    warning(sprintf("Element y position (%.3f) is outside viewport, element will be clipped", 
                    yvec), call. = FALSE)
    yvec <- clamp_value(yvec, 0.01, 0.99)
  }
  
  # Determine point character
  if (is.null(pch) && N2 > 1) {
    pch <- 21 + (n2 - 1) %% 6
  } else if (is.null(pch)) {
    pch <- 21
  } else {
    pch <- 21 + (pch - 1) %% 6
  }
  
  grid.points(
    x = unit(xvec, 'npc'), 
    y = unit(yvec, 'npc'),
    pch = pch, 
    size = size, 
    gp = gpar(col = col, fill = col)
  )
  
  invisible(NULL)
}


#' Plot Forest Tree
#'
#' This function plots a forest plot element (confidence interval line with point estimate)
#' on a box plot.
#'
#' @param box The box object containing axis and position information.
#' @param x_lower Numeric. The lower bound of the confidence interval.
#' @param x_upper Numeric. The upper bound of the confidence interval.
#' @param x_dot Numeric. The point estimate.
#' @param n1 Integer. The row/category index within the box.
#' @param n2 Integer. The sub-row position within the category. Default is 1.
#' @param N2 Integer. The total number of sub-rows in the category. Default is 1.
#' @param col Character. The color of the forest tree element. Default is '#663399' (purple).
#' @param pch Integer. The point character. Default is NULL (auto-select).
#' @param userect Logical. Whether to use rectangle instead of line. Default is FALSE.
#' @param height Numeric. Rectangle height if userect is TRUE. Default is NULL.
#' @param options A page_options object for styling. Default creates new page_options.
#'
#' @return Invisible NULL. Draws on the current graphics device.
#'
#' @examples
#' \dontrun{
#' # After creating a box object
#' plot_forest_tree(box, x_lower = 0.5, x_upper = 1.5, x_dot = 0.9, n1 = 1)
#' 
#' # Multiple treatment groups
#' plot_forest_tree(box, 0.4, 1.2, 0.7, n1 = 1, n2 = 1, N2 = 2, col = 'blue')
#' plot_forest_tree(box, 0.6, 1.8, 1.1, n1 = 1, n2 = 2, N2 = 2, col = 'red')
#' }
#'
#' @import grid
#' @export
plot_forest_tree <- function(box, x_lower, x_upper, x_dot, n1, n2 = 1, N2 = 1, 
                             col = '#663399', pch = NULL, userect = FALSE, 
                             height = NULL, options = page_options$new()) {
  
  # Get styling from options
  br_palette <- options$get_palette()
  lty_ <- options$forest.line.type
  lwd_ <- options$forest.line.width
  
  # Determine color
  if (is.null(col) && N2 > 1) {
    col <- safe_get_color(br_palette, n2, "black")
  } else if (!is.null(col) && N2 > 1 && length(col) == N2) {
    col <- col[n2]
  } else if (is.null(col) && N2 == 1) {
    col <- safe_get_color(br_palette, 1, "black")
  }
  
  # Draw confidence interval (line or rectangle)
  if (!userect) {
    plot_horizontal_seg(box, c(x_lower, x_upper), n1, n2, N2, lty = lty_, lwd = lwd_, col = col)
  } else {
    plot_long_rectangle(box, c(x_lower, x_upper), n1, n2, N2, lty = lty_, lwd = lwd_, col = col, height = height)
  }
  
  # Draw point estimate
  plot_dot(box, x_dot, n1, n2, N2, col = col, pch = pch)
  
  invisible(NULL)
}


#' Draw Diagonal DRAFT Watermark
#'
#' This function draws the word "DRAFT" diagonally across the current graphics device.
#' It automatically adjusts the font size and rotation angle to fit the dimensions 
#' of the plotting area.
#'
#' @return Invisible NULL. Draws on the current graphics device.
#'
#' @examples
#' \dontrun{
#' grid.newpage()
#' draw_draft_watermark()
#' }
#'
#' @import grid
#' @keywords internal
draw_draft_watermark <- function() {
  # Get device dimensions
  width_in <- convertWidth(unit(1, "npc"), "inches", valueOnly = TRUE)
  height_in <- convertHeight(unit(1, "npc"), "inches", valueOnly = TRUE)
  
  diag_in <- sqrt(width_in^2 + height_in^2)
  
  # Measure text width at reference font size
  ref_grob <- textGrob("DRAFT", gp = gpar(fontsize = 1))
  width_per_point <- convertWidth(grobWidth(ref_grob), "inches", valueOnly = TRUE)
  
  # Calculate optimal font size
  scaling_factor <- 0.9
  optimal_fontsize <- (diag_in * scaling_factor) / width_per_point
  
  # Calculate rotation angle (in degrees)
  angle <- atan(height_in / width_in) * 180 / pi
  
  grid.text(
    label = "DRAFT", 
    x = 0.5, 
    y = 0.5, 
    rot = angle,
    gp = gpar(
      col = 'gray85',
      fontsize = optimal_fontsize,
      fontface = "bold"
    )
  )
  
  invisible(NULL)
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
