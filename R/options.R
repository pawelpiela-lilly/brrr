#' Define a class for page options
#'
#' This class represents the options for a page layout in benefit-risk plots.
#' It includes properties for page margins, header dimensions, fonts, colors, 
#' and various styling parameters.
#' 
#' @field PAGE_TOP_MARGIN Numeric. The top margin of the page (0-1 scale).
#' @field PAGE_BOTTOM_MARGIN Numeric. The bottom margin of the page (0-1 scale).
#' @field PAGE_LEFT_MARGIN Numeric. The left margin of the page (0-1 scale).
#' @field PAGE_RIGHT_MARGIN Numeric. The right margin of the page (0-1 scale).
#' @field HEADER_HEIGHT Numeric. The height of the header (0-1 scale).
#' @field HEADER_WIDTH Numeric. The width of the header (0-1 scale).
#' @field row.label.font.size Numeric. The font size of the row labels in points.
#' @field header.label.font.size Numeric. The font size of the header labels in points.
#' @field header.label.color Character. The color of the header label text.
#' @field axis.label.font.size Numeric. The font size of the axis labels in points.
#' @field axis.ticks.font.size Numeric. The font size of the axis ticks in points.
#' @field axis.ticks.font.rotation Numeric. The rotation of the axis ticks in degrees.
#' @field axis.tick.len.ratio Numeric. The ratio of tick length to axis length.
#' @field axis.ticks.label.nice Logical. Whether to use nice labels for log scale ticks.
#' @field box.spacing Numeric. The spacing between boxes.
#' @field box.category.height Unit. Height of a single category in the box.
#' @field box.fill.color Character. The fill color of the box.
#' @field br_palette Character vector. The color palette for the page.
#' @field label.use.separation.line Logical. Whether to use separation lines between labels.
#' @field label.font.size Numeric. The font size of labels in points.
#' @field label.font.usecolors Logical. Whether to use colors for label fonts.
#' @field legend.label.font.size Numeric. The font size of the legend labels in points.
#' @field legend.header.font.size Numeric. The font size of the legend header in points.
#' @field legend.fill.color Character. The fill color of the legend.
#' @field forest.line.type Numeric. The line type for the forest plot.
#' @field forest.line.width Numeric. The line width for the forest plot.
#' @field forest.pch.shift Numeric. Shift for the point character in the forest plot.
#' 
#' @format An object of class \code{page_options}
#' @import R6
#' @import grid
#' @export
#' 
#' @examples
#' # Create default options
#' opts <- page_options$new()
#' 
#' # Get page parameters
#' opts$get_page_parameters()
#' 
#' # Modify margins
#' opts$set_page_parameter('PAGE_TOP_MARGIN', 0.1)
#' 
#' # Change color palette
#' opts$set_palette(c("blue", "red", "green"))
page_options <- R6::R6Class(
  "page_options",
  public = list(
    PAGE_TOP_MARGIN = 0.05,
    PAGE_BOTTOM_MARGIN = 0.05,
    PAGE_LEFT_MARGIN = 0.05,
    PAGE_RIGHT_MARGIN = 0.05,
    HEADER_HEIGHT = 0.05,
    HEADER_WIDTH = 0.9,
    br_palette = c(
      "black", 
      "rebeccapurple" = "#663399", 
      "cornflowerblue" = "#6495ED", 
      "mediumseagreen" = "#3CB371", 
      "tomato" = "#FF6347", 
      "peachpuff" = "#FFDAB9", 
      "lightsalmon" = "#FFA07A"
    ),
    row.label.font.size = NULL,
    label.use.separation.line = FALSE,
    label.font.size = NULL,
    label.font.usecolors = TRUE,
    header.label.font.size = NULL,
    header.label.color = "#043099",
    axis.label.font.size = NULL,
    axis.ticks.font.size = NULL,
    axis.ticks.font.rotation = 0,
    axis.tick.len.ratio = 0.02,
    axis.ticks.label.nice = TRUE,
    box.spacing = NULL,
    box.category.height = NULL,
    box.fill.color = "#ded8db43",
    legend.label.font.size = NULL,
    legend.header.font.size = NULL,
    legend.fill.color = "#fffdd0",
    forest.line.type = 1,
    forest.line.width = 3,
    forest.pch.shift = 0,
    
    #' @description Initialize the page options.
    #' @param PAGE_TOP_MARGIN The top margin of the page (0-1 scale).
    #' @param PAGE_BOTTOM_MARGIN The bottom margin of the page (0-1 scale).
    #' @param PAGE_LEFT_MARGIN The left margin of the page (0-1 scale).
    #' @param PAGE_RIGHT_MARGIN The right margin of the page (0-1 scale).
    #' @param HEADER_HEIGHT The height of the header. If NULL, calculated from 13mm.
    #' @return A new `page_options` object.
    initialize = function(PAGE_TOP_MARGIN = 0.05,
                          PAGE_BOTTOM_MARGIN = 0.05,
                          PAGE_LEFT_MARGIN = 0.05,
                          PAGE_RIGHT_MARGIN = 0.05,
                          HEADER_HEIGHT = NULL) {
      
      # Validate inputs
      if (!is.numeric(PAGE_TOP_MARGIN) || PAGE_TOP_MARGIN < 0 || PAGE_TOP_MARGIN > 1) {
        stop("PAGE_TOP_MARGIN must be a numeric value between 0 and 1", call. = FALSE)
      }
      if (!is.numeric(PAGE_BOTTOM_MARGIN) || PAGE_BOTTOM_MARGIN < 0 || PAGE_BOTTOM_MARGIN > 1) {
        stop("PAGE_BOTTOM_MARGIN must be a numeric value between 0 and 1", call. = FALSE)
      }
      if (!is.numeric(PAGE_LEFT_MARGIN) || PAGE_LEFT_MARGIN < 0 || PAGE_LEFT_MARGIN > 1) {
        stop("PAGE_LEFT_MARGIN must be a numeric value between 0 and 1", call. = FALSE)
      }
      if (!is.numeric(PAGE_RIGHT_MARGIN) || PAGE_RIGHT_MARGIN < 0 || PAGE_RIGHT_MARGIN > 1) {
        stop("PAGE_RIGHT_MARGIN must be a numeric value between 0 and 1", call. = FALSE)
      }
      
      self$PAGE_TOP_MARGIN <- PAGE_TOP_MARGIN
      self$PAGE_BOTTOM_MARGIN <- PAGE_BOTTOM_MARGIN
      self$PAGE_LEFT_MARGIN <- PAGE_LEFT_MARGIN
      self$PAGE_RIGHT_MARGIN <- PAGE_RIGHT_MARGIN
      
      # Calculate HEADER_HEIGHT if not provided (requires grid device)
      if (is.null(HEADER_HEIGHT)) {
        self$HEADER_HEIGHT <- private$safe_convert_y(unit(13, 'mm'), 'npc', 0.05)
      } else {
        self$HEADER_HEIGHT <- HEADER_HEIGHT
      }
      
      self$HEADER_WIDTH <- 1 - PAGE_LEFT_MARGIN - PAGE_RIGHT_MARGIN
      
      # Initialize font sizes with safe conversion
      self$row.label.font.size <- private$safe_convert_unit(unit(3, 'mm'), 'points', 8.5)
      self$label.font.size <- private$safe_convert_unit(unit(3, 'mm'), 'points', 8.5)
      self$header.label.font.size <- private$safe_convert_unit(unit(3, 'mm'), 'points', 8.5)
      self$axis.label.font.size <- private$safe_convert_unit(unit(4, 'mm'), 'points', 11.3)
      self$axis.ticks.font.size <- private$safe_convert_unit(unit(3, 'mm'), 'points', 8.5)
      self$legend.label.font.size <- private$safe_convert_unit(unit(3, 'mm'), 'points', 8.5)
      self$legend.header.font.size <- private$safe_convert_unit(unit(3, 'mm'), 'points', 8.5)
      
      # Initialize spacing and height
      self$box.spacing <- private$safe_convert_unit(unit(20, 'mm'), 'npc', 0.05)
      self$box.category.height <- unit(15, 'mm')
    },
    
    #' @description Get the color palette for the page.
    #' @return Character vector of colors.
    get_palette = function() {
      return(self$br_palette)
    },
    
    #' @description Set the color palette for the page.
    #' @param palette Character vector of colors.
    #' @return Invisible self for method chaining.
    set_palette = function(palette) {
      if (!is.character(palette) || length(palette) < 1) {
        stop("palette must be a character vector with at least one color", call. = FALSE)
      }
      self$br_palette <- palette
      invisible(self)
    },
    
    #' @description Get the page parameters.
    #' @return A named list of page parameters.
    get_page_parameters = function() {
      return(list(
        PAGE_TOP_MARGIN = self$PAGE_TOP_MARGIN,
        PAGE_BOTTOM_MARGIN = self$PAGE_BOTTOM_MARGIN,
        PAGE_LEFT_MARGIN = self$PAGE_LEFT_MARGIN,
        PAGE_RIGHT_MARGIN = self$PAGE_RIGHT_MARGIN,
        HEADER_HEIGHT = self$HEADER_HEIGHT,
        HEADER_WIDTH = self$HEADER_WIDTH
      ))
    },
    
    #' @description Get a specific page parameter.
    #' @param parameter Character string naming the parameter.
    #' @return The value of the parameter.
    get_page_parameter = function(parameter) {
      params <- self$get_page_parameters()
      if (!parameter %in% names(params)) {
        stop(sprintf("Unknown parameter: '%s'. Available parameters: %s", 
                     parameter, paste(names(params), collapse = ", ")), call. = FALSE)
      }
      return(params[[parameter]])
    },
    
    #' @description Set a specific page parameter.
    #' @param parameter Character string naming the parameter.
    #' @param value The value to set.
    #' @return Invisible self for method chaining.
    set_page_parameter = function(parameter, value) {
      valid_params <- names(self$get_page_parameters())
      if (!parameter %in% valid_params) {
        stop(sprintf("Unknown parameter: '%s'. Valid parameters: %s", 
                     parameter, paste(valid_params, collapse = ", ")), call. = FALSE)
      }
      if (!is.numeric(value)) {
        stop(sprintf("Value for '%s' must be numeric", parameter), call. = FALSE)
      }
      self[[parameter]] <- value
      invisible(self)
    },
    
    #' @description Get the font size of the row labels.
    #' @return Numeric font size in points.
    get_label_font_size = function() {
      return(self$row.label.font.size)
    },
    
    #' @description Set the font size of the row labels.
    #' @param size Numeric font size in points.
    #' @return Invisible self for method chaining.
    set_label_font_size = function(size) {
      if (!is.numeric(size) || size <= 0) {
        stop("size must be a positive numeric value", call. = FALSE)
      }
      self$row.label.font.size <- size
      invisible(self)
    },
    
    #' @description Get the font size of the header labels.
    #' @return Numeric font size in points.
    get_header_font_size = function() {
      return(self$header.label.font.size)
    },
    
    #' @description Set the font size of the header labels.
    #' @param size Numeric font size in points.
    #' @return Invisible self for method chaining.
    set_header_font_size = function(size) {
      if (!is.numeric(size) || size <= 0) {
        stop("size must be a positive numeric value", call. = FALSE)
      }
      self$header.label.font.size <- size
      invisible(self)
    },
    
    #' @description Get the font size of the axis labels.
    #' @return Numeric font size in points.
    get_axis_label_font_size = function() {
      return(self$axis.label.font.size)
    },
    
    #' @description Set the font size of the axis labels.
    #' @param size Numeric font size in points.
    #' @return Invisible self for method chaining.
    set_axis_label_font_size = function(size) {
      if (!is.numeric(size) || size <= 0) {
        stop("size must be a positive numeric value", call. = FALSE)
      }
      self$axis.label.font.size <- size
      invisible(self)
    },
    
    #' @description Get the font size of the axis ticks.
    #' @return Numeric font size in points.
    get_axis_ticks_font_size = function() {
      return(self$axis.ticks.font.size)
    },
    
    #' @description Set the font size of the axis ticks.
    #' @param size Numeric font size in points.
    #' @return Invisible self for method chaining.
    set_axis_ticks_font_size = function(size) {
      if (!is.numeric(size) || size <= 0) {
        stop("size must be a positive numeric value", call. = FALSE)
      }
      self$axis.ticks.font.size <- size
      invisible(self)
    },
    
    #' @description Get the box spacing.
    #' @return Numeric box spacing value.
    get_box_spacing = function() {
      return(self$box.spacing)
    },
    
    #' @description Set the box spacing.
    #' @param spacing Numeric spacing value.
    #' @return Invisible self for method chaining.
    set_box_spacing = function(spacing) {
      if (!is.numeric(spacing) || spacing < 0) {
        stop("spacing must be a non-negative numeric value", call. = FALSE)
      }
      self$box.spacing <- spacing
      invisible(self)
    },
    
    #' @description Auto-adjust parameters to fit content within viewport.
    #' @param n_boxes Integer. Number of boxes to fit.
    #' @param total_categories Integer. Total number of category rows across all boxes.
    #' @param verbose Logical. Print adjustment information. Default is TRUE.
    #' @return Invisible self for method chaining.
    auto_fit_for_content = function(n_boxes, total_categories, verbose = TRUE) {
      if (!is.numeric(n_boxes) || n_boxes < 1) {
        stop("n_boxes must be a positive integer", call. = FALSE)
      }
      if (!is.numeric(total_categories) || total_categories < 1) {
        stop("total_categories must be a positive integer", call. = FALSE)
      }
      
      # Calculate available space
      available <- 1 - self$PAGE_TOP_MARGIN - self$PAGE_BOTTOM_MARGIN - 
                   self$HEADER_HEIGHT - 0.1  # 0.1 for axis labels
      
      # Calculate required height with current settings
      cat_height <- private$safe_convert_y(self$box.category.height, 'npc', 0.075)
      spacing_total <- max(0, n_boxes - 1) * self$box.spacing
      axis_total <- n_boxes * 0.05  # approximate axis height
      
      required <- total_categories * cat_height + spacing_total + axis_total
      
      if (required <= available) {
        if (verbose) message("Content fits within available space.")
        return(invisible(self))
      }
      
      # Calculate scale factor
      scale_factor <- (available / required) * 0.95  # 5% margin
      
      # Apply scaling
      new_cat_height <- as.numeric(self$box.category.height) * scale_factor
      new_spacing <- self$box.spacing * scale_factor
      
      if (inherits(self$box.category.height, "unit")) {
        unit_type <- attr(self$box.category.height, "unit")
        self$box.category.height <- grid::unit(new_cat_height, unit_type)
      } else {
        self$box.category.height <- new_cat_height
      }
      self$box.spacing <- new_spacing
      
      if (verbose) {
        message(sprintf(
          "Auto-fit applied: scaled to %.0f%% to fit content.\n  New category height: %.2f\n  New box spacing: %.3f",
          scale_factor * 100, new_cat_height, new_spacing
        ))
      }
      
      invisible(self)
    }
  ),
  
  private = list(
    
    # Safely convert units with fallback
    # @param x Unit object to convert
    # @param unit_to Target unit
    # @param default Default value if conversion fails
    safe_convert_unit = function(x, unit_to, default) {
      tryCatch({
        grid::convertUnit(x, unit_to, valueOnly = TRUE)
      }, error = function(e) {
        default
      })
    },
    
    # Safely convert Y units with fallback
    # @param x Unit object to convert
    # @param unit_to Target unit
    # @param default Default value if conversion fails
    safe_convert_y = function(x, unit_to, default) {
      tryCatch({
        grid::convertY(x, unitTo = unit_to, valueOnly = TRUE)
      }, error = function(e) {
        default
      })
    }
  )
)
