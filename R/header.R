#' Create a header for a page
#'
#' This function creates a header for a benefit-risk plot, using the specified 
#' break widths, labels, and options.
#'
#' @param breaks_widths Numeric vector. The widths of the breaks relative to total width.
#'   Positive values create visible separators, negative values create invisible gaps.
#' @param labels Character vector. The labels for the breaks. Default is NULL.
#'   If provided, must have the same length as breaks_widths.
#' @param header_text_size Numeric vector. Scaling factors for header text sizes.
#'   Default is NULL (use default sizes). Must match length of labels if provided.
#' @param options A page_options object. Default creates new page_options.
#'
#' @return A list containing:
#' \describe{
#'   \item{options}{The page_options object used}
#'   \item{breaks}{The actual break positions (0-1 scale)}
#'   \item{breaks_positions}{The break positions in absolute coordinates}
#'   \item{name}{Character string "header" identifying the object type}
#' }
#'
#' @examples
#' \dontrun{
#' grid.newpage()
#' 
#' # Simple header with three columns
#' create_header(c(1, 2, 3), c("A", "B", "C"))
#' 
#' # Header with custom options
#' opts <- page_options$new()
#' create_header(c(0.2, 0.3, 0.5), c("Left", "Middle", "Right"), options = opts)
#' 
#' # Header with invisible gap (negative width)
#' create_header(c(0.2, -0.1, 0.2), c("Left", "Gap", "Right"))
#' }
#' 
#' @import grid
#' @export
create_header <- function(breaks_widths, labels = NULL, header_text_size = NULL, 
                          options = page_options$new()) {
  
  # Input validation
  if (!is.numeric(breaks_widths) || length(breaks_widths) < 1) {
    stop("breaks_widths must be a numeric vector with at least one element", call. = FALSE)
  }
  
  if (sum(abs(breaks_widths)) == 0) {
    stop("breaks_widths must have at least one non-zero value", call. = FALSE)
  }
  
  if (!is.null(labels) && length(labels) != length(breaks_widths)) {
    stop("Length of labels must be the same as length of breaks_widths", call. = FALSE)
  }
  
  if (!is.null(header_text_size) && length(header_text_size) != length(breaks_widths)) {
    stop("Length of header_text_size must be the same as length of breaks_widths", call. = FALSE)
  }
  
  if (!inherits(options, "page_options")) {
    stop("options must be a page_options object", call. = FALSE)
  }
  
  # Calculate cumulative breaks
  breaks <- cumsum(abs(breaks_widths))
  
  # Get page parameters
  PAGE_BOTTOM_MARGIN <- options$get_page_parameter('PAGE_BOTTOM_MARGIN')
  PAGE_TOP_MARGIN <- options$get_page_parameter('PAGE_TOP_MARGIN')
  PAGE_LEFT_MARGIN <- options$get_page_parameter('PAGE_LEFT_MARGIN')
  PAGE_RIGHT_MARGIN <- options$get_page_parameter('PAGE_RIGHT_MARGIN')
  HEADER_HEIGHT <- options$get_page_parameter('HEADER_HEIGHT')
  HEADER_WIDTH <- options$get_page_parameter('HEADER_WIDTH')
  
  # Draw the header rectangle
  grid.rect(
    x = PAGE_LEFT_MARGIN, 
    y = 1 - PAGE_TOP_MARGIN,
    width = 1 - PAGE_LEFT_MARGIN - PAGE_RIGHT_MARGIN,
    height = HEADER_HEIGHT, 
    gp = gpar(fill = "lightblue"), 
    just = c('left', 'top')
  )
  
  # Draw vertical separators (only for positive widths)
  for (i in seq_along(breaks)) {
    if (breaks_widths[i] < 0) next
    
    grid.lines(
      x = c(breaks[i], breaks[i]) * HEADER_WIDTH + PAGE_LEFT_MARGIN, 
      y = c(1 - PAGE_TOP_MARGIN, 1 - PAGE_TOP_MARGIN - HEADER_HEIGHT),
      gp = gpar(col = "black")
    )
  }
  
  # Calculate actual break positions
  actual_breaks <- c(0, breaks, 1)
  
  # Add labels to segments
  if (!is.null(labels)) {
    for (i in seq_len(length(actual_breaks) - 2)) {
      # Get text size
      text_size <- options$get_header_font_size()
      if (!is.null(header_text_size) && !is.null(header_text_size[i])) {
        text_size <- text_size * header_text_size[i]
      }
      
      # Calculate label position (center of segment)
      label_x <- PAGE_LEFT_MARGIN + HEADER_WIDTH * (actual_breaks[i] + actual_breaks[i + 1]) / 2
      label_y <- 1 - PAGE_TOP_MARGIN - HEADER_HEIGHT / 2
      
      grid.text(
        label = labels[i], 
        x = label_x, 
        y = label_y,
        just = c('center', 'center'),
        gp = gpar(
          fontsize = text_size, 
          fontface = "bold", 
          col = options$header.label.color
        )
      )
    }
  }
  
  # Return header information
  return(list(
    options = options,
    breaks = actual_breaks,
    breaks_positions = actual_breaks * HEADER_WIDTH + PAGE_LEFT_MARGIN,
    name = 'header'
  ))
}


#' Add benefit arrows to the header
#'
#' This function adds benefit arrows to the header of a plot. The arrows indicate 
#' the direction of benefit (favoring treatment vs. placebo/control).
#'
#' @param obj A header object created by \code{\link{create_header}}.
#' @param neutral_relative_x Numeric. The relative x-coordinate of the neutral position (0-1).
#' @param direction Character. The direction of arrows. Either 'up' or 'down'. Default is 'up'.
#' @param labels Character vector of length 2. Labels for the arrows. 
#'   Default is c('Favors LY', 'Favors Placebo').
#' @param col Character. Color of the arrows. Default is '#043099'.
#'
#' @return Invisible NULL. The function draws on the current graphics device.
#'
#' @examples
#' \dontrun{
#' grid.newpage()
#' 
#' # Create a header and add arrows
#' obj <- create_header(c(0.3, 0.3, 0.4), c("Endpoint", "Values", "Comparison"))
#' add_benefit_arrows(obj, neutral_relative_x = 0.5, direction = 'up',
#'                    labels = c('Favors Treatment', 'Favors Placebo'))
#' }
#'
#' @import grid
#' @export
add_benefit_arrows <- function(obj, neutral_relative_x, direction = 'up',
                               labels = c('Favors LY', 'Favors Placebo'), 
                               col = '#043099') {
  
  # Input validation
  if (is.null(obj$name) || obj$name != 'header') {
    stop("obj must be a header object created by create_header()", call. = FALSE)
  }
  
  if (!is.numeric(neutral_relative_x) || neutral_relative_x < 0 || neutral_relative_x > 1) {
    stop("neutral_relative_x must be a numeric value between 0 and 1", call. = FALSE)
  }
  
  if (!direction %in% c('up', 'down')) {
    stop("direction must be either 'up' or 'down'", call. = FALSE)
  }
  
  if (!is.character(labels) || length(labels) != 2) {
    stop("labels must be a character vector of length 2", call. = FALSE)
  }
  
  breaks <- obj$breaks
  
  # Get page parameters
  HEADER_HEIGHT <- obj$options$get_page_parameter('HEADER_HEIGHT')
  PAGE_TOP_MARGIN <- obj$options$get_page_parameter('PAGE_TOP_MARGIN')
  PAGE_LEFT_MARGIN <- obj$options$get_page_parameter('PAGE_LEFT_MARGIN')
  HEADER_WIDTH <- obj$options$get_page_parameter('HEADER_WIDTH')
  
  label_font_size <- obj$options$get_header_font_size()
  
  # Get last two breaks (the comparison column)
  last_breaks <- breaks[(length(breaks) - 1):length(breaks)]
  
  # Calculate relative x position within header
  header_relative_x <- last_breaks[2] * neutral_relative_x + last_breaks[1] * (1 - neutral_relative_x)
  
  # Transform to absolute x position
  x <- PAGE_LEFT_MARGIN + HEADER_WIDTH * header_relative_x
  
  # Calculate distance to nearest break
  min_distance_to_breaks <- min(abs(header_relative_x - last_breaks))
  distance_to_breaks <- min_distance_to_breaks * HEADER_WIDTH - 0.01
  
  # Create arrow graphics
  b_arrow <- arrow(type = "closed", angle = 10, length = unit(0.02, "npc"))
  
  # Y positions for arrows and labels
  arrow_y <- 1 - PAGE_TOP_MARGIN - HEADER_HEIGHT * 0.9
  label_y <- 1 - PAGE_TOP_MARGIN - HEADER_HEIGHT * 0.5
  
  # Right arrow (favors treatment or placebo depending on direction)
  grid.lines(
    x = c(x + 0.01, x + distance_to_breaks), 
    y = c(arrow_y, arrow_y),
    arrow = b_arrow, 
    gp = gpar(fill = col, col = col)
  )
  
  # Label for right arrow
  grid.text(
    label = ifelse(direction == 'up', labels[1], labels[2]), 
    x = unit(x + distance_to_breaks / 2, 'npc'), 
    y = unit(label_y, 'npc'),
    just = c('center', 'center'), 
    gp = gpar(
      fontsize = label_font_size, 
      fontface = "bold", 
      col = obj$options$header.label.color
    )
  )
  
  # Left arrow
  grid.lines(
    x = c(x - 0.01, x - distance_to_breaks),
    y = c(arrow_y, arrow_y),
    arrow = b_arrow, 
    gp = gpar(fill = col, col = col)
  )
  
  # Label for left arrow
  grid.text(
    label = ifelse(direction == 'up', labels[2], labels[1]),
    x = x - distance_to_breaks / 2, 
    y = label_y,
    just = c('center', 'center'), 
    gp = gpar(
      fontsize = label_font_size, 
      fontface = "bold", 
      col = obj$options$header.label.color
    )
  )
  
  invisible(NULL)
}
