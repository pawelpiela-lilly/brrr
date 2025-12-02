#' Add a legend to a plot
#'
#' This function adds a customizable legend to a benefit-risk plot. The legend 
#' supports multiple types of items including lines, rectangles, and points.
#'
#' @param legend_items A list of legend items. Each item is a list with:
#'   \describe{
#'     \item{label}{Character. The label text for the legend item}
#'     \item{type}{Character. The type of legend item: "line", "rectangle", or "text"}
#'     \item{pch}{Integer. The point character index (for points)}
#'     \item{col}{Integer or character. Color index or color value}
#'     \item{col_txt}{Integer or character. Text color index or value}
#'     \item{lty}{Integer. Line type (1=solid, 2=dashed, etc.)}
#'     \item{lwd}{Numeric. Line width}
#'   }
#' @param xpos Numeric or unit. The x position of the legend box (left edge).
#' @param ypos Numeric or unit. The y position of the legend box (top edge).
#' @param width Numeric or unit. The width of the legend box.
#' @param height Numeric or unit. The height of the legend box (excluding header).
#' @param n_rows Integer. Number of rows in the legend grid. Default is NULL (auto).
#' @param n_cols Integer. Number of columns in the legend grid. Default is NULL (auto).
#' @param byrow Logical. Arrange items by row (TRUE) or column (FALSE). Default is TRUE.
#' @param label Character. The legend title. Default is 'Legend'. Set to NULL for no title.
#' @param options A page_options object for styling. Default creates new page_options.
#'
#' @return Invisible NULL. Draws on the current graphics device.
#'
#' @examples
#' \dontrun{
#' # Define legend items
#' legend_items <- list(
#'   list(type = 'line', pch = 1, col = 1, label = 'Treatment A'),
#'   list(type = 'line', pch = 2, col = 2, label = 'Treatment B'),
#'   list(type = 'rectangle', pch = 1, col = 3, label = 'Treatment C')
#' )
#' 
#' # Add legend to plot
#' add_legend(
#'   legend_items = legend_items,
#'   xpos = unit(0.05, 'npc'),
#'   ypos = unit(0.2, 'npc'),
#'   width = unit(0.9, 'npc'),
#'   height = unit(0.08, 'npc'),
#'   n_cols = 3
#' )
#' }
#'
#' @import grid
#' @export
add_legend <- function(legend_items, xpos, ypos, width, height,
                       n_rows = NULL, n_cols = NULL, byrow = TRUE, 
                       label = 'Legend', options = page_options$new()) {
  
  # Input validation
  if (!is.list(legend_items) || length(legend_items) < 1) {
    stop("legend_items must be a non-empty list", call. = FALSE)
  }
  
  if (!inherits(options, "page_options")) {
    stop("options must be a page_options object", call. = FALSE)
  }
  
  # Calculate grid dimensions
  n_items <- length(legend_items)
  
  if (xor(is.null(n_rows), is.null(n_cols))) {
    if (is.null(n_rows)) {
      n_rows <- ceiling(n_items / n_cols)
    } else {
      n_cols <- ceiling(n_items / n_rows)
    }
  } else if (is.null(n_rows) && is.null(n_cols)) {
    if (!byrow) {
      n_rows <- n_items
      n_cols <- 1
    } else {
      n_rows <- 1
      n_cols <- n_items
    }
  }
  
  # Ensure we have enough cells
  if (n_rows * n_cols < n_items) {
    warning(sprintf("Grid size (%d x %d = %d) is smaller than number of items (%d). Some items may not display.",
                    n_rows, n_cols, n_rows * n_cols, n_items))
  }
  
  # Convert dimensions
  legend_width <- width / n_cols
  legend_height <- height / n_rows
  
  # Handle xpos and ypos conversion
  if (!is.unit(xpos)) {
    xpos <- unit(xpos, 'npc')
  }
  if (!is.unit(ypos)) {
    ypos <- unit(ypos, 'npc')
  }
  
  # Draw legend title if provided
  if (!is.null(label)) {
    legend_label_font_size <- options$legend.header.font.size
    legend_label_font_size_unit <- unit(legend_label_font_size, 'points')
    
    # Convert to npc for positioning
    legend_label_font_size_npc <- convertHeight(legend_label_font_size_unit, 'npc')
    
    # Draw title box border
    grid.rect(
      x = convertX(xpos, 'npc'), 
      y = ypos, 
      width = width, 
      height = legend_label_font_size_npc * 1.5, 
      just = c('left', 'top'),
      gp = gpar(fill = "transparent", lty = 1, lwd = 1)
    )
    
    # Draw title text
    grid.text(
      label = label, 
      x = xpos + convertX(unit(1.5, 'mm'), 'npc'), 
      y = ypos - legend_label_font_size_npc * 0.75, 
      just = c('left', 'center'), 
      gp = gpar(
        fontsize = legend_label_font_size_unit, 
        fontface = 'bold', 
        col = 'black'
      )
    )
    
    # Adjust ypos for content area
    ypos <- ypos - legend_label_font_size_npc * 1.5
    ypos <- unit(ypos, 'npc')
  }
  
  # Draw legend background box
  grid.rect(
    x = xpos, 
    y = ypos, 
    width = width, 
    height = height, 
    just = c('left', 'top'),
    gp = gpar(fill = options$legend.fill.color, lty = 1, lwd = 1)
  )
  
  # Helper function for defaults
  get_default <- function(x, default) {
    if (is.null(x)) default else x
  }
  
  # Helper function for palette colors
  get_default_color <- function(x, palette) {
    if (is.null(x)) {
      safe_get_color(palette, 1, "black")
    } else if (is.numeric(x) && x >= 1) {
      safe_get_color(palette, x, "black")
    } else {
      x
    }
  }
  
  # Maximum width for symbol area
  max_symbol_width <- convertWidth(unit(20, 'mm'), 'npc', valueOnly = TRUE)
  max_symbol_width <- unit(min(max_symbol_width, 0.25 * legend_width), 'npc')
  
  # Draw each legend item
  for (item_idx in seq_along(legend_items)) {
    # Calculate position in grid
    if (byrow) {
      col_num <- (item_idx - 1) %% n_cols
      row_num <- (item_idx - 1) %/% n_cols
    } else {
      col_num <- (item_idx - 1) %/% n_rows
      row_num <- (item_idx - 1) %% n_rows
    }
    
    # Skip if outside grid
    if (row_num >= n_rows || col_num >= n_cols) next
    
    # Extract item properties
    item <- legend_items[[item_idx]]
    legend_label <- item$label
    legend_type <- item$type
    legend_point <- item$pch
    legend_lwd <- get_default(item$lwd, options$forest.line.width)
    legend_lty <- get_default(item$lty, options$forest.line.type)
    legend_color <- get_default_color(item$col, options$br_palette)
    legend_text_color <- if (options$label.font.usecolors) {
      get_default_color(item$col_txt, options$br_palette)
    } else {
      'black'
    }
    
    # Calculate item position
    item_x <- xpos + col_num * legend_width
    item_y <- ypos - row_num * legend_height - legend_height / 2
    
    # Draw symbol based on type
    if (!is.null(legend_type)) {
      if (legend_type == 'line') {
        grid.lines(
          x = c(item_x + legend_width * 0.05, item_x + max_symbol_width),
          y = rep(item_y, 2),
          gp = gpar(col = legend_color, lty = legend_lty, lwd = legend_lwd)
        )
      } else if (legend_type == 'rectangle') {
        grid.rect(
          x = item_x + legend_width * 0.05, 
          y = item_y, 
          width = max_symbol_width - legend_width * 0.05, 
          height = 0.015, 
          just = c('left', 'center'),
          gp = gpar(col = legend_color, lty = legend_lty, lwd = legend_lwd)
        )
      }
    }
    
    # Draw point marker
    if (!is.null(legend_point)) {
      point_pch <- 21 + (legend_point - 1) %% 6
      grid.points(
        x = item_x + (legend_width * 0.05 + max_symbol_width) / 2, 
        y = item_y, 
        pch = point_pch, 
        gp = gpar(col = legend_color, fill = legend_color)
      )
    }
    
    # Draw label text
    if (!is.null(legend_label)) {
      txt_offset <- ifelse(!is.null(legend_type) && legend_type == 'text', 0.05, 1.2)
      grid.text(
        label = legend_label, 
        x = item_x + max_symbol_width * txt_offset, 
        y = item_y, 
        just = 'left', 
        gp = gpar(
          fontsize = options$legend.label.font.size, 
          col = legend_text_color
        )
      )
    }
  }
  
  invisible(NULL)
}


#' Safely get color from palette (internal)
#' @keywords internal
safe_get_color <- function(palette, index, default = "black") {
  if (is.null(index) || is.na(index) || !is.numeric(index) || 
      index < 1 || index > length(palette)) {
    return(default)
  }
  palette[index]
}
