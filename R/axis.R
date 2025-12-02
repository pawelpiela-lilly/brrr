#' Plot Axis
#'
#' This function plots an axis with ticks and labels on a given plot.
#'
#' @param xlength Numeric. The length of the axis line (0-1 scale).
#' @param xpos Numeric. The x-coordinate of the starting position of the axis line (0-1).
#' @param ypos Numeric. The y-coordinate of the starting position of the axis line (0-1).
#' @param from Numeric. The starting value of the axis range.
#' @param to Numeric. The ending value of the axis range.
#' @param n_ticks Integer. The number of ticks on the axis (must be >= 2).
#' @param neutral_pos Integer. The position of the neutral tick (0 on linear scale, 1 on log scale).
#' @param label Character or expression. The label for the axis. Default is NULL.
#' @param logscale Logical. Whether to use logarithmic scale. Default is FALSE.
#' @param b Numeric. The base for logarithmic scale. Default is 10.
#' @param show_axis Logical. Whether to show the axis. Default is TRUE.
#' @param secondary_ticks Logical. Whether to show secondary ticks. Default is TRUE.
#' @param options A page_options object. Default creates new page_options.
#'
#' @return A list containing:
#' \describe{
#'   \item{axis_function}{A function that scales a value to the position on the axis}
#'   \item{from}{The transformed starting value of the axis range}
#'   \item{to}{The transformed ending value of the axis range}
#'   \item{length}{The length of the axis line}
#' }
#'
#' @examples
#' \dontrun{
#' grid.newpage()
#' 
#' # Linear axis
#' pp_lin_axis <- plot_axis(xlength = 0.8, xpos = 0.5, ypos = 0.5, 
#'          from = 0, to = 10, n_ticks = 6, neutral_pos = 1)
#'
#' # Reversed axis
#' pp_rev_axis <- plot_axis(xlength = 0.8, xpos = 0.5, ypos = 0.4, 
#'          from = 10, to = 0, n_ticks = 6, neutral_pos = 1)
#' 
#' # Logarithmic axis
#' pp_log_axis <- plot_axis(xlength = 0.8, xpos = 0.5, ypos = 0.3, 
#'          from = 0.1, to = 1000, b = 10, n_ticks = 6, neutral_pos = 1, 
#'          logscale = TRUE, label = "Log Scale (0.1 to 1000)")
#' }
#'
#' @import grid
#' @export
plot_axis <- function(xlength, xpos, ypos, from, to, n_ticks, neutral_pos, 
                      label = NULL, logscale = FALSE, b = 10, show_axis = TRUE, 
                      secondary_ticks = TRUE, options = page_options$new()) {
  
  # Local helper to clamp values to bounds
  clamp_value <- function(x, min_val = 0, max_val = 1) {
    pmax(pmin(x, max_val), min_val)
  }
  
  # Input validation - xlength
  if (!is.numeric(xlength) || xlength <= 0 || xlength > 1) {
    stop("xlength must be a positive numeric value between 0 and 1", call. = FALSE)
  }
  
  # Validate and clamp xpos (warn, don't error)
  if (!is.numeric(xpos)) {
    stop("xpos must be a numeric value", call. = FALSE)
  }
  if (xpos < 0 || xpos > 1) {
    warning(sprintf("xpos (%.3f) is outside viewport bounds [0, 1], clamping.", xpos), call. = FALSE)
    xpos <- clamp_value(xpos, 0, 1)
  }
  
  # Validate and clamp ypos (warn, don't error)
  if (!is.numeric(ypos)) {
    stop("ypos must be a numeric value", call. = FALSE)
  }
  original_ypos <- ypos
  if (ypos < 0 || ypos > 1) {
    warning(sprintf(
      "Axis ypos (%.3f) is outside viewport bounds [0, 1]. Content will be clipped. Consider reducing content or using a taller output device.",
      ypos
    ), call. = FALSE)
    # Clamp to valid range
    ypos <- clamp_value(ypos, 0.01, 0.99)
  }
  
  if (!is.numeric(from) || !is.numeric(to)) {
    stop("from and to must be numeric values", call. = FALSE)
  }
  if (!is.numeric(n_ticks) || n_ticks < 2) {
    stop("n_ticks must be a numeric value >= 2", call. = FALSE)
  }
  if (!is.numeric(neutral_pos) || neutral_pos < 0 || neutral_pos > n_ticks) {
    stop("neutral_pos must be between 0 and n_ticks", call. = FALSE)
  }
  if (!is.logical(logscale)) {
    stop("logscale must be a logical value", call. = FALSE)
  }
  if (!is.numeric(b) || b <= 0 || b == 1) {
    stop("b (base) must be a positive numeric value not equal to 1", call. = FALSE)
  }
  
  # Check for log scale constraints
  if (logscale && (from <= 0 || to <= 0)) {
    stop("from and to must be > 0 when logscale is TRUE", call. = FALSE)
  }
  
  # Transform values for log scale
  if (logscale) {
    from <- log(from, base = b)
    to <- log(to, base = b)
  }
  
  # Adjust neutral position for reversed axis
  neutral_pos <- ifelse(from > to, n_ticks - neutral_pos, neutral_pos)
  
  # Calculate ratios for tick spacing
  ratio_rhs <- abs(ifelse(to > from, to / (n_ticks - neutral_pos), from / (n_ticks - neutral_pos)))
  ratio_lhs <- abs(ifelse(to > from, from / neutral_pos, to / neutral_pos))
  
  ratio <- max(ratio_rhs, ratio_lhs)
  ratio <- ifelse(logscale, ceiling(abs(ratio)) * sign(ratio), ratio)
  
  # Calculate linear tick size
  if (!logscale) {
    # Use safe_pretty to handle edge cases
    ratio_lhs_linear <- safe_pretty(abs(from) / max(neutral_pos, 1))
    ratio_rhs_linear <- safe_pretty(abs(to) / max(n_ticks - neutral_pos, 1))
    
    linear_tick_delta <- max(ratio_lhs_linear, ratio_rhs_linear)
    
    from_linear <- ifelse(from <= to, 
                          linear_tick_delta * neutral_pos, 
                          linear_tick_delta * (n_ticks - neutral_pos))
    to_linear <- ifelse(from <= to, 
                        linear_tick_delta * (n_ticks - neutral_pos), 
                        linear_tick_delta * neutral_pos)
    ratio <- linear_tick_delta
  }
  
  # Recalculate from and to based on ratio
  from <- ifelse(to > from, -ratio * neutral_pos, ratio * (n_ticks - neutral_pos))
  to <- ifelse(to > from, ratio * (n_ticks - neutral_pos), -ratio * neutral_pos)
  
  axis_range <- round(seq(from, to, length = n_ticks + 1), 2)
  
  # Define scale function
  scale_function <- function(x) {
    if (logscale) {
      return(xpos + xlength * (log(x, base = b) - from) / (to - from))
    } else {
      return(xpos + xlength * (x - from) / (to - from))
    }
  }
  
  # Set tick length
  tick_len <- options$axis.tick.len.ratio * xlength
  
  if (show_axis) {
    # Plot direction arrow
    dir_arrow <- arrow(type = "closed", angle = 10, length = unit(0.015, "npc"))
    
    if (from < to) {
      grid.lines(
        x = c(xlength + xpos - 0.01, xlength + xpos), 
        y = c(ypos, ypos),
        arrow = dir_arrow, 
        gp = gpar(fill = 'black')
      )
    } else {
      grid.lines(
        x = c(xpos + 0.01, xpos), 
        y = c(ypos, ypos),
        arrow = dir_arrow, 
        gp = gpar(fill = 'black')
      )
    }
    
    # Plot the main axis line
    grid.lines(x = c(xpos, xpos + xlength), y = c(ypos, ypos), gp = gpar(lwd = 1))
    
    # Plot the ticks
    for (i in seq_along(axis_range)) {
      tick_pos <- xpos + xlength * (axis_range[i] - from) / (to - from)
      grid.lines(
        x = c(tick_pos, tick_pos), 
        y = c(ypos - tick_len, ypos), 
        gp = gpar(lwd = 1)
      )
    }
    
    # Plot secondary ticks for log scale (base 10)
    if (secondary_ticks && logscale && b == 10) {
      seq_direction <- ifelse(from < to, 1, -1)
      
      ticks_small <- NULL
      if (seq_direction == 1) {
        ticks_small <- b^from + cumsum(rep(diff(b^axis_range) / 10, each = 10))
      } else {
        ticks_small <- rev(b^from + cumsum(rep(diff(b^axis_range) / 10, each = 10)))
      }
      
      for (i in seq_along(ticks_small)) {
        tick_pos <- scale_function(ticks_small[i])
        if (tick_pos >= xpos && tick_pos <= xpos + xlength) {
          grid.lines(
            x = c(tick_pos, tick_pos), 
            y = c(ypos - tick_len / 2, ypos), 
            gp = gpar(lwd = 1)
          )
        }
      }
    }
    
    # Plot secondary ticks on linear scale
    if (secondary_ticks && !logscale) {
      n_subticks <- calculate_subticks(ratio)
      
      subtick_delta <- if (log(ratio, 10) < 1) {
        ratio_conversion_factor <- 10^abs(floor(log(ratio, 10)))
        ((ratio * ratio_conversion_factor) / n_subticks) / ratio_conversion_factor
      } else {
        ratio / n_subticks
      }
      
      direction <- ifelse(axis_range[1] > axis_range[length(axis_range)], -1, 1)
      ticks_small <- seq(axis_range[1], axis_range[length(axis_range)], by = subtick_delta * direction)
      
      for (i in seq_along(ticks_small)) {
        tick_pos <- scale_function(ticks_small[i])
        if (tick_pos >= xpos && tick_pos <= xpos + xlength) {
          grid.lines(
            x = c(tick_pos, tick_pos), 
            y = c(ypos - tick_len / 2, ypos), 
            gp = gpar(lwd = 1)
          )
        }
      }
    }
    
    # Get font sizes
    axis_tick_font_size <- options$get_axis_ticks_font_size()
    axis_label_font_size <- options$get_axis_label_font_size() * 0.7
    
    axis_label_height <- convertHeight(
      unit(axis_label_font_size, 'points'),
      unitTo = 'npc', 
      valueOnly = TRUE
    )
    
    axis_ticks_label_height <- convertHeight(
      unit(axis_tick_font_size, 'points'),
      unitTo = 'npc', 
      valueOnly = TRUE
    )
    
    # Add tick labels
    for (i in seq_along(axis_range)) {
      tick_pos <- xpos + xlength * (axis_range[i] - from) / (to - from)
      
      tick_label <- if (logscale) {
        format_log_tick(axis_range[i], b, options$axis.ticks.label.nice)
      } else {
        as.character(axis_range[i])
      }
      
      grid.text(
        label = tick_label,
        x = tick_pos, 
        y = unit(ypos - 1.5 * tick_len - axis_ticks_label_height, 'npc'),
        just = "bottom", 
        gp = gpar(fontsize = axis_tick_font_size), 
        rot = options$axis.ticks.font.rotation
      )
    }
    
    # Add axis label
    if (!is.null(label)) {
      grid.text(
        label = label, 
        x = xpos + xlength / 2, 
        y = unit(ypos - 2 * tick_len - axis_ticks_label_height - axis_label_height, 'npc'), 
        just = "bottom", 
        gp = gpar(fontsize = axis_label_font_size, fontface = "bold")
      )
    }
  }
  
  # Return axis information
  return(list(
    axis_function = scale_function,
    from = from,
    to = to,
    length = xlength
  ))
}

#' Safe pretty function for tick calculation
#' @param x Numeric value
#' @return Second element of pretty sequence or 1 if not available
#' @keywords internal
safe_pretty <- function(x) {
  if (is.na(x) || is.infinite(x) || x == 0) return(1)
  p <- pretty(x)
  if (length(p) >= 2) p[2] else 1
}

#' Calculate number of subticks
#' @param ratio Tick ratio
#' @return Number of subticks
#' @keywords internal
calculate_subticks <- function(ratio) {
  ratio_conversion_factor <- 10^abs(floor(log(ratio, 10)))
  ratio_ <- if (log(ratio, 10) < 1) ratio * ratio_conversion_factor else ratio
  
  if (ratio_ %% 3 == 0) {
    3
  } else if (ratio_ %% 4 == 0) {
    4
  } else if (ratio_ %% 2 == 0) {
    2
  } else {
    1
  }
}

#' Format logarithmic tick labels
#' @param x Value to format
#' @param b Base of logarithm
#' @param nice Use nice formatting
#' @return Formatted label
#' @keywords internal
format_log_tick <- function(x, b, nice = TRUE) {
  value <- b^abs(x)
  
  if (value >= 1e3) {
    eval_txt <- sprintf('expression(%g^%g)', b, x)
    return(eval(parse(text = eval_txt)))
  } else {
    if (x < 0) {
      if (nice) {
        sprintf('1/%g', b^(-x))
      } else {
        sprintf('%.2f', b^x)
      }
    } else {
      sprintf('%g', b^x)
    }
  }
}
