#' Plot Box
#'
#' This function plots a box at the specified position with the specified dimensions,
#' including an axis with ticks.
#'
#' @param xpos Numeric. The x-coordinate of the box's position (0-1 scale).
#' @param ypos Numeric. The y-coordinate of the box's position (0-1 scale).
#' @param xlength Numeric. The length of the box along the x-axis.
#' @param n_categories Integer. The number of categories in the box (must be >= 1).
#' @param single_category_height Numeric or unit. The height of each category.
#' @param neutral_pos Integer. The position of the neutral line within the box.
#' @param n_ticks Integer. The number of ticks on the axis.
#' @param from Numeric. The starting value of the axis range.
#' @param to Numeric. The ending value of the axis range.
#' @param label Character or expression. The label for the axis. Default is NULL.
#' @param logscale Logical. Whether to use logarithmic scale. Default is FALSE.
#' @param b Numeric. The base for logarithmic scale. Default is 10.
#' @param show_axis Logical. Whether to show the axis. Default is TRUE.
#' @param vline Numeric. Position of an optional vertical reference line. Default is NULL.
#' @param options A page_options object. Default creates new page_options.
#'
#' @return A list containing:
#' \describe{
#'   \item{axis}{The axis object}
#'   \item{name}{Character string "box" identifying the object type}
#'   \item{height}{The total height of the box}
#'   \item{y_pos}{Vector of y-positions for each category}
#' }
#'
#' @examples
#' \dontrun{
#' grid.newpage()
#' box1 <- plot_box(0.5, 0.5, 0.4, 3, 0.05, 3, 5, 0, 1, "Effect Size")
#' }
#' 
#' @import grid
#' @keywords internal
plot_box <- function(xpos, ypos, xlength, n_categories, single_category_height, 
                     neutral_pos, n_ticks, from, to, label = NULL, logscale = FALSE, 
                     b = 10, show_axis = TRUE, vline = NULL, options = page_options$new()) {
  
  # Local helper to clamp values to bounds
  clamp_value <- function(x, min_val = 0, max_val = 1) {
    pmax(pmin(x, max_val), min_val)
  }
  
  # Input validation - xpos (clamp with warning, don't error)
  if (!is.numeric(xpos)) {
    stop("xpos must be a numeric value", call. = FALSE)
  }
  if (xpos < 0 || xpos > 1) {
    warning(sprintf("xpos (%.3f) is outside viewport bounds [0, 1], clamping.", xpos), call. = FALSE)
    xpos <- clamp_value(xpos, 0, 1)
  }
  
 # Input validation - ypos (clamp with warning, don't error)
  if (!is.numeric(ypos)) {
    stop("ypos must be a numeric value", call. = FALSE)
  }
  original_ypos <- ypos
  if (ypos < 0 || ypos > 1) {
    warning(sprintf(
      "Box ypos (%.3f) is outside viewport bounds [0, 1]. Content will be clipped. Consider reducing content or using a taller output device.",
      ypos
    ), call. = FALSE)
    # Clamp to valid range - but allow some content to be visible
    ypos <- clamp_value(ypos, 0.01, 0.99)
  }
  
  if (!is.numeric(xlength) || xlength <= 0) {
    stop("xlength must be a positive numeric value", call. = FALSE)
  }
  if (!is.numeric(n_categories) || n_categories < 1) {
    stop("n_categories must be a positive integer", call. = FALSE)
  }
  
  # Convert single_category_height to npc if it's a unit
  if (is.unit(single_category_height)) { 
    single_category_height <- convertY(single_category_height, unitTo = 'npc', valueOnly = TRUE)
  } else {
    single_category_height <- as.numeric(single_category_height)
  }
  
  if (single_category_height <= 0) {
    stop("single_category_height must be positive", call. = FALSE)
  }
  
  box_height <- n_categories * single_category_height
  
  # Check if box bottom would be out of bounds
  box_bottom <- ypos - box_height
  if (box_bottom < 0) {
    warning(sprintf(
      "Box content extends below viewport (bottom at y=%.3f). Content will be clipped. Consider:\n  - Reducing category height\n  - Reducing number of categories\n  - Using a taller output device",
      box_bottom
    ), call. = FALSE)
    # Clamp the box bottom but still draw what we can
    box_bottom <- max(0.02, box_bottom)
  }
  
  # Plot the x-axis (with clamped position)
  axis_ypos <- max(0.02, ypos - box_height)  # Clamp axis position
  axis <- plot_axis(
    xlength = xlength, 
    xpos = xpos, 
    ypos = axis_ypos, 
    from = from, 
    to = to, 
    n_ticks = n_ticks, 
    neutral_pos = neutral_pos, 
    label = label, 
    logscale = logscale, 
    b = b, 
    show_axis = show_axis, 
    options = options
  )
  
  # Plot the box (clamp to viewport)
  draw_ypos <- min(0.98, ypos)
  draw_height <- min(box_height, draw_ypos - 0.02)  # Don't extend below y=0.02
  
  grid.rect(
    x = xpos, 
    y = draw_ypos, 
    width = axis$length, 
    height = draw_height, 
    just = c('left', 'top'), 
    gp = gpar(fill = options$box.fill.color, lty = 1, lwd = 1)
  )
  
  # Separate each category with a horizontal line
  if (n_categories > 1) {
    for (i in 1:(n_categories - 1)) {
      grid.lines(
        x = c(xpos, xpos + axis$length), 
        y = c(ypos - i * single_category_height, ypos - i * single_category_height), 
        gp = gpar(lty = 3, lwd = 1)
      )
    }
  }
  
  # Plot vertical line at point of no difference
  axis_transform_fun <- axis$axis_function
  neutral_value <- ifelse(logscale, 1, 0)
  
  grid.lines(
    x = rep(axis_transform_fun(neutral_value), 2),
    y = c(ypos, ypos - n_categories * single_category_height), 
    gp = gpar(lty = 2, lwd = 1)
  )
  
  # Plot optional reference vertical line
  if (!is.null(vline) && is.numeric(vline)) {
    grid.lines(
      x = rep(axis_transform_fun(vline), 2),
      y = c(ypos, ypos - n_categories * single_category_height),
      gp = gpar(lty = 3, lwd = 1)
    )
  }
  
  # Return box information
  return(list(
    axis = axis,
    name = 'box', 
    height = box_height,
    y_pos = seq(ypos, ypos - n_categories * single_category_height, -single_category_height)
  ))
}


#' Add a box to a plot
#'
#' This function adds a box to a plot. The box can be connected to either a header 
#' or another box, creating stacked visualizations.
#'
#' @param obj The object to which the box will be added (header or box object).
#' @param spacing Numeric. The spacing between the box and the object above it.
#' @param n_categories Integer. The number of categories in the box.
#' @param single_category_height Numeric or unit. The height of each category.
#' @param neutral_pos Integer. The position of the neutral point.
#' @param n_ticks Integer. The number of ticks on the axis.
#' @param from Numeric. The starting value of the axis range.
#' @param to Numeric. The ending value of the axis range.
#' @param label Character or expression. The label for the axis. Default is NULL.
#' @param logscale Logical. Whether to use logarithmic scale. Default is FALSE.
#' @param b Numeric. The base for logarithmic scale. Default is 10.
#' @param show_axis Logical. Whether to show the axis. Default is TRUE.
#' @param arrow_labels Character vector of length 2. Labels for benefit arrows. 
#'   Default is NULL (no arrows). Set to add directional arrows.
#' @param direction Character. Direction of benefit arrows ('up' or 'down'). Default is 'up'.
#' @param userect Logical. Whether to use rectangles instead of lines. Default is FALSE.
#' @param vline Numeric. Position of an optional vertical reference line. Default is NULL.
#' @param colbreaks Numeric vector. Column break positions for separation lines. Default is NULL.
#'
#' @return A list containing:
#' \describe{
#'   \item{label_fun}{Function to add labels to the box}
#'   \item{box}{The box object}
#'   \item{name}{Character string "box" identifying the object type}
#'   \item{userect}{Logical indicating rectangle usage}
#'   \item{header}{Reference to the header object}
#'   \item{y_pos}{The y-position of the bottom of the box}
#' }
#'
#' @examples
#' \dontrun{
#' grid.newpage()
#' 
#' # Create header
#' my_header <- create_header(c(0.2, 0.3, 0.5), c("Endpoint", "Values", "Comparison"))
#' 
#' # Add a box
#' box1 <- add_box(my_header, 0, 3, 0.05, 3, 5, 0, 1, "Effect Size",
#'                 arrow_labels = c("Favors Treatment", "Favors Control"))
#' 
#' # Add labels
#' box1$label_fun("Endpoint 1", 1, 1)
#' box1$label_fun("Value 1", 2, 1)
#' }
#' 
#' @import grid
#' @export
add_box <- function(obj, spacing, n_categories, single_category_height, 
                    neutral_pos, n_ticks, from, to, label = NULL, logscale = FALSE, 
                    b = 10, show_axis = TRUE, arrow_labels = NULL, direction = 'up', 
                    userect = FALSE, vline = NULL, colbreaks = NULL) {
  
  # Determine object type and get options
  options <- NULL
  if (!is.null(obj$name) && obj$name == 'header') {
    options <- obj$options
  } else if (!is.null(obj$name) && obj$name == 'box') {
    options <- obj$header$options
  } else {
    stop("obj must be a 'header' or 'box' object (unknown object type provided)", 
         call. = FALSE)
  }
  
  # Validate spacing
  if (!is.numeric(spacing) || spacing < 0) {
    stop("spacing must be a non-negative numeric value", call. = FALSE)
  }
  
  # Get page parameters
  HEADER_HEIGHT <- options$get_page_parameter('HEADER_HEIGHT')
  PAGE_TOP_MARGIN <- options$get_page_parameter('PAGE_TOP_MARGIN')
  PAGE_BOTTOM_MARGIN <- options$get_page_parameter('PAGE_BOTTOM_MARGIN')
  
  current_y <- NULL
  header <- NULL
  
  if (obj$name == 'header') {
    header <- obj
    current_y <- 1 - (HEADER_HEIGHT + PAGE_TOP_MARGIN) - spacing
    
    # Add benefit arrows if specified
    if (!is.null(arrow_labels)) {
      if (length(arrow_labels) != 2) {
        stop("arrow_labels must have exactly 2 elements", call. = FALSE)
      }
      add_benefit_arrows(header, neutral_pos / n_ticks, direction = direction, labels = arrow_labels)
    }
    
  } else if (obj$name == 'box') {
    header <- obj$header
    current_y <- obj$y_pos - spacing
  }
  
  # Calculate expected box height to check bounds
  cat_height <- if (is.unit(single_category_height)) {
    tryCatch(
      convertY(single_category_height, unitTo = 'npc', valueOnly = TRUE),
      error = function(e) as.numeric(single_category_height) / 200  # fallback
    )
  } else {
    as.numeric(single_category_height)
  }
  
  expected_bottom <- current_y - (n_categories * cat_height)
  
  # Check if this box will fit
  if (expected_bottom < PAGE_BOTTOM_MARGIN) {
    warning(sprintf(
      "Box will extend below page bottom margin (expected y=%.3f, margin=%.3f). Consider reducing category height, box spacing, or using a taller output device.",
      expected_bottom, PAGE_BOTTOM_MARGIN
    ), call. = FALSE)
  }
  
  # Calculate box position
  global_box_x <- rev(header$breaks_positions)[2]
  global_box_width <- rev(header$breaks_positions)[1] - global_box_x
  
  # Create the box
  box1 <- plot_box(
    xpos = global_box_x, 
    ypos = current_y, 
    xlength = global_box_width, 
    n_categories = n_categories, 
    single_category_height = single_category_height, 
    neutral_pos = neutral_pos, 
    n_ticks = n_ticks, 
    from = from, 
    to = to, 
    label = label, 
    logscale = logscale, 
    b = b, 
    show_axis = show_axis, 
    vline = vline, 
    options = options
  )
  
  # Add separation lines if requested
  if (options$label.use.separation.line && !is.null(colbreaks) && 
      length(colbreaks) == length(header$breaks_positions) - 2) {
    
    # Convert height if needed
    cat_height <- if (is.unit(single_category_height)) {
      convertY(single_category_height, unitTo = 'npc', valueOnly = TRUE)
    } else {
      as.numeric(single_category_height)
    }
    
    for (i in seq_len(length(header$breaks_positions) - 3)) {
      if (colbreaks[i] >= 0) {
        y_start <- current_y
        y_end <- current_y - n_categories * cat_height
        
        grid.lines(
          x = c(header$breaks_positions[i + 1], header$breaks_positions[i + 1]), 
          y = c(y_start, y_end), 
          gp = gpar(lty = 1, lwd = 0.2, col = 'lightgray')
        )
      }
    }
  }
  
  # Calculate label positions
  breaks_positions <- header$breaks_positions
  breaks_positions <- breaks_positions[seq_len(length(breaks_positions) - 1)]
  breaks_positions <- breaks_positions[-length(breaks_positions)] + diff(breaks_positions) / 2
  
  # Define label function
  add_label <- function(label, collevel, rowlevel, n = 1, N = 1, 
                        fontsize = NULL, fontface = 'bold', col = 'black', isglobal = FALSE) {
    
    if (is.null(fontsize)) {
      fontsize <- options$get_label_font_size()
    }
    
    y_pos <- box1$y_pos
    
    # Validate indices
    if (collevel < 1 || collevel > length(breaks_positions)) {
      warning(sprintf("collevel %d is out of range (1-%d)", collevel, length(breaks_positions)))
      return(invisible(NULL))
    }
    if (rowlevel < 1 || rowlevel >= length(y_pos)) {
      warning(sprintf("rowlevel %d is out of range (1-%d)", rowlevel, length(y_pos) - 1))
      return(invisible(NULL))
    }
    
    xvec <- breaks_positions[collevel]
    yvec <- y_pos[rowlevel] + (y_pos[rowlevel + 1] - y_pos[rowlevel]) * n / (N + 1)
    yvec <- ifelse(isglobal, (y_pos[1] + y_pos[length(y_pos)]) / 2, yvec)
    
    grid.text(
      label = label, 
      x = unit(xvec, 'npc'), 
      y = unit(yvec, 'npc'), 
      just = c('center', 'center'),
      gp = gpar(fontsize = fontsize, fontface = fontface, col = col)
    )
    
    invisible(NULL)
  }
  
  return(list(
    label_fun = add_label, 
    box = box1, 
    name = 'box', 
    userect = userect,
    header = header, 
    y_pos = min(box1$y_pos)
  ))
}
