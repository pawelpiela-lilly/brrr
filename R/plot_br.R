#' Plot Benefit-Risk Visualization
#'
#' This function creates a benefit-risk visualization plot with forest plot elements.
#' It supports multiple axes, customizable columns, and various styling options.
#'
#' @param data A data frame containing the trial data. Must include columns for:
#'   value (point estimate), lower (CI lower bound), upper (CI upper bound),
#'   and the columns specified in columns_specs.
#' @param columns_specs A named character vector. Names are the column headers to display,
#'   values are the corresponding column names in the data frame.
#' @param breaks_widths A numeric vector of column widths. Must match length of columns_specs.
#'   Positive values create visible separators, negative values create invisible gaps.
#' @param split_axis_by_col Character. Column name to split the plot into separate axes.
#' @param axis_labels_col Character. Column name containing axis labels.
#' @param split_box_by_col Character. Column name to split boxes within each axis.
#' @param vline_col Character or NULL. Column name for vertical reference line values.
#'   Default is NULL (no reference lines).
#' @param neutral_pos Integer. Position of the neutral point (e.g., 0 or 1). Default is 3.
#' @param num_ticks Integer. Number of ticks on the axis. Default is 6.
#' @param top_margin Numeric or NULL. Top margin override. Default is NULL.
#' @param userect Logical. Use rectangles instead of lines for CI. Default is FALSE.
#' @param arrow_labels Character vector of length 2. Labels for benefit arrows.
#'   Default is c('Favors\nTreatment', 'Favors\nPlacebo').
#' @param value_collapse Logical vector. Whether to collapse values in each column.
#'   Default is all FALSE, length must match columns_specs.
#' @param label_text_size Numeric vector or NULL. Size multipliers for label text.
#'   Default is NULL.
#' @param header_text_size Numeric vector or NULL. Size multipliers for header text.
#'   Default is NULL.
#' @param box_group A previous plot_br result to chain plots. Default is NULL.
#' @param colors_by Character or NULL. Column name to determine colors by. Default is NULL.
#' @param options_br A page_options object. Default creates new page_options.
#' @param is_draft Logical. Add draft watermark. Default is TRUE.
#'
#' @return A list containing:
#' \describe{
#'   \item{name}{Character "box_group" identifying the object type}
#'   \item{boxes}{List of box objects created}
#'   \item{options}{The page_options object used}
#'   \item{last_y}{The y-position of the bottom of the last box}
#' }
#'
#' @examples
#' \dontrun{
#' library(grid)
#' data(mock_data)
#' 
#' # Define column specifications
#' columns_specs <- c(
#'   'Benefits' = 'endpoint', 
#'   'Treatment\n(N=100)' = 'treatment', 
#'   'Placebo\n(N=100)' = 'placebo',
#'   'Comparison' = 'col3'
#' )
#' 
#' breaks_widths <- c(0.2, -0.1, 0.1, 0.2)
#' 
#' result <- plot_br(
#'   data = mock_data,
#'   columns_specs = columns_specs,
#'   breaks_widths = breaks_widths,
#'   split_axis_by_col = 'axis_number',
#'   axis_labels_col = 'estimator',
#'   split_box_by_col = 'endpoint'
#' )
#' }
#' 
#' @importFrom dplyr mutate select distinct pull filter arrange across all_of any_of if_all
#' @importFrom rlang !! :=
#' @import grid
#' @export
plot_br <- function(data, columns_specs, breaks_widths, 
                    split_axis_by_col, axis_labels_col, split_box_by_col,
                    vline_col = NULL,
                    neutral_pos = 3, num_ticks = 6,
                    top_margin = NULL, userect = FALSE,
                    arrow_labels = c('Favors\nTreatment', 'Favors\nPlacebo'),
                    value_collapse = NULL,
                    label_text_size = NULL,
                    header_text_size = NULL,
                    box_group = NULL, 
                    colors_by = NULL,
                    options_br = page_options$new(),
                    is_draft = TRUE) {
  
  # Input validation
  if (!is.data.frame(data)) {
    stop("data must be a data frame", call. = FALSE)
  }
  
  if (!is.character(columns_specs) || is.null(names(columns_specs))) {
    stop("columns_specs must be a named character vector", call. = FALSE)
  }
  
  if (!is.numeric(breaks_widths)) {
    stop("breaks_widths must be a numeric vector", call. = FALSE)
  }
  
  if (length(breaks_widths) != length(columns_specs)) {
    stop("breaks_widths must have the same length as columns_specs", call. = FALSE)
  }
  
  # Set default value_collapse if not provided
  if (is.null(value_collapse)) {
    value_collapse <- rep(FALSE, length(columns_specs))
  }
  
  if (length(value_collapse) != length(columns_specs)) {
    stop("value_collapse must have the same length as columns_specs", call. = FALSE)
  }
  
  # Validate required columns exist
  required_data_cols <- c("value", "lower", "upper", split_axis_by_col, 
                          axis_labels_col, split_box_by_col)
  missing_cols <- setdiff(required_data_cols, colnames(data))
  if (length(missing_cols) > 0) {
    stop(sprintf("data is missing required columns: %s", 
                 paste(missing_cols, collapse = ", ")), call. = FALSE)
  }
  
  # Check layout fit before starting to draw
  if (is.null(box_group)) {
    # Calculate total content that will be drawn
    unique_axes <- unique(data[[split_axis_by_col]])
    n_boxes <- length(unique_axes)
    
    # Count categories per axis
    categories_per_axis <- sapply(unique_axes, function(ax) {
      subset_data <- data[data[[split_axis_by_col]] == ax, ]
      length(unique(subset_data[[split_box_by_col]]))
    })
    
    # Check if content will fit
    fit_info <- check_layout_fit(data, split_axis_by_col, split_box_by_col, options_br)
    
    if (!fit_info$fits) {
      # Calculate suggested adjustments
      suggested <- auto_scale_to_fit(
        fit_info, 
        options_br$box.category.height,
        options_br$get_box_spacing(),
        n_boxes
      )
      
      if (suggested$needs_scaling) {
        message(sprintf(
          "Tip: To fit content, try setting:\n  opts$box.category.height <- unit(%.1f, 'mm')\n  opts$set_box_spacing(%.3f)",
          as.numeric(options_br$box.category.height) * suggested$scale_factor,
          options_br$get_box_spacing() * suggested$scale_factor
        ))
      }
    }
  }
  
  # Handle colors_by column
  if (is.null(colors_by)) {
    colors_by <- '.tmp_colors_by'
    data <- data |>
      dplyr::mutate(!!colors_by := 'x')
  }
  
  # Helper function to get color index
  get_color <- function(group, shift = 0) {
    color_values <- data |>
      dplyr::select(dplyr::all_of(colors_by)) |>
      dplyr::distinct() |>
      dplyr::pull(!!colors_by)
    which(color_values == as.character(group)) + shift
  }
  
  # Initialize new page if needed
  if (is.null(top_margin) && is.null(box_group)) {
    grid.newpage()
    pushViewport(viewport(width = 1, height = 1))
    if (is_draft) draw_draft_watermark()
  }
  
  # Handle top_margin and box_group
  if (!is.null(top_margin)) {
    options_br$set_page_parameter('PAGE_TOP_MARGIN', top_margin)
  } else if (!is.null(box_group)) {
    if (is.null(box_group$name) || box_group$name != 'box_group') {
      stop("box_group must be a result from a previous plot_br() call", call. = FALSE)
    }
    options_br <- box_group$options
    new_top_margin <- 1 - box_group$last_y + options_br$get_box_spacing()
    options_br$set_page_parameter('PAGE_TOP_MARGIN', new_top_margin)
  }
  
  # Create header
  header_br <- create_header(breaks_widths, names(columns_specs), header_text_size, options = options_br)
  
  # Get metadata for data
  data_meta <- get_metadata(data, split_axis_by_col, axis_labels_col, split_box_by_col, vline_col)
  
  # Track the last graph part for positioning
  last_graph_part <- header_br
  boxes <- list()
  
  # Sort data by axis column
  data_meta <- data_meta |>
    dplyr::arrange(dplyr::across(dplyr::any_of(split_axis_by_col)))
  
  # Get unique axis values
  unique_axes <- unique(data_meta[[split_axis_by_col]])
  
  # First pass: create all boxes
  for (est in unique_axes) {
    data_meta_subset <- data_meta |>
      dplyr::filter(dplyr::if_all(dplyr::all_of(split_axis_by_col), ~ .x == est))
    
    # Calculate axis range
    minval <- min(data_meta_subset$minval, na.rm = TRUE)
    maxval <- max(data_meta_subset$maxval, na.rm = TRUE)
    
    # Add small expansion factor
    expand_factor <- 0.01 * abs(maxval - minval)
    
    # Check for log scale
    has_logscale <- 'logscale' %in% colnames(data_meta_subset)
    logscale <- if (has_logscale) {
      any(data_meta_subset$logscale, na.rm = TRUE)
    } else {
      FALSE
    }
    
    # Adjust min/max for log scale
    if (logscale) {
      minval <- minval * 0.99
      maxval <- maxval * 1.01
    } else {
      minval <- minval - sign(minval) * expand_factor
      maxval <- maxval + sign(maxval) * expand_factor
    }
    
    # Get log base
    logbase <- if ('logbase' %in% colnames(data_meta_subset)) {
      unique_bases <- unique(data_meta_subset$logbase[!is.na(data_meta_subset$logbase)])
      if (length(unique_bases) > 0) unique_bases[1] else 2
    } else {
      2
    }
    
    # Get number of categories
    ncats <- unique(data_meta_subset$ncats)[1]
    
    # Determine spacing
    spacing <- if (last_graph_part$name == 'header') {
      0
    } else {
      options_br$get_box_spacing()
    }
    
    # Check if axis is reversed
    is_reversed <- if ('reversed' %in% colnames(data_meta_subset)) {
      any(data_meta_subset$reversed, na.rm = TRUE)
    } else {
      FALSE
    }
    
    # Get axis label
    axis_label <- data_meta_subset[[axis_labels_col]][1]
    
    # Get vertical line value
    vline_val <- NULL
    if (!is.null(vline_col) && 'vertical_line' %in% colnames(data_meta_subset)) {
      max_vline <- max(data_meta_subset$vertical_line, na.rm = TRUE)
      if (!is.infinite(max_vline) && !is.na(max_vline)) {
        vline_val <- max_vline
      }
    }
    
    # Create box
    last_graph_part <- add_box(
      obj = last_graph_part, 
      spacing = spacing, 
      n_categories = ncats, 
      single_category_height = options_br$box.category.height, 
      neutral_pos = neutral_pos, 
      n_ticks = num_ticks, 
      from = ifelse(is_reversed, maxval, minval), 
      to = ifelse(is_reversed, minval, maxval), 
      label = axis_label, 
      logscale = (!is.na(logscale) && logscale),
      b = ifelse(is.na(logbase), 2, logbase), 
      arrow_labels = arrow_labels,
      show_axis = TRUE, 
      vline = vline_val, 
      colbreaks = breaks_widths
    )
    
    boxes[[as.character(est)]] <- list(box = last_graph_part)
  }
  
  # Second pass: add labels and forest plot elements
  for (est in unique_axes) {
    column_names <- names(columns_specs)
    
    data_subset <- data |>
      dplyr::filter(dplyr::if_all(dplyr::all_of(split_axis_by_col), ~ .x == est))
    
    box <- boxes[[as.character(est)]]
    add_label <- box$box$label_fun
    
    unique_endpoints <- unique(data_subset[[split_box_by_col]])
    
    # Add labels and plot forest elements for each endpoint
    for (ben_idx in seq_along(unique_endpoints)) {
      ben <- unique_endpoints[ben_idx]
      
      data_sub_subset <- data_subset |>
        dplyr::filter(dplyr::if_all(dplyr::all_of(split_box_by_col), ~ .x == ben))
      
      # Add labels to each column
      for (cn_idx in seq_along(column_names)) {
        column_name <- columns_specs[cn_idx]
        
        # Check column exists
        if (!column_name %in% colnames(data_sub_subset)) {
          warning(sprintf("Column '%s' not found in data", column_name))
          next
        }
        
        unique_column_values <- data_sub_subset[[column_name]]
        
        if (value_collapse[cn_idx]) {
          unique_column_values <- unique(unique_column_values)
        }
        
        # Get text size
        text_size <- options_br$label.font.size
        if (!is.null(label_text_size) && length(label_text_size) >= cn_idx) {
          if (!is.null(label_text_size[cn_idx]) && !is.na(label_text_size[cn_idx])) {
            text_size <- text_size * label_text_size[cn_idx]
          }
        }
        
        # Add each label
        n_values <- max(length(unique_column_values), 1)
        for (j in seq_len(n_values)) {
          header_options <- box$box$header$options
          
          # Determine color
          col_value <- if (!is.null(colors_by) && colors_by != '.tmp_colors_by' && 
                          nrow(data_sub_subset) >= j) {
            color_idx <- get_color(data_sub_subset[j, colors_by])
            safe_get_palette_color(header_options$get_palette(), color_idx)
          } else {
            safe_get_palette_color(header_options$get_palette(), j)
          }
          
          label_color <- if (cn_idx == 1 || !header_options$label.font.usecolors) {
            'black'
          } else {
            col_value
          }
          
          add_label(
            label = unique_column_values[j], 
            collevel = cn_idx, 
            rowlevel = ben_idx, 
            n = j, 
            N = length(unique_column_values), 
            col = label_color, 
            fontsize = text_size
          )
        }
      }
      
      # Plot forest tree elements
      for (k in seq_len(nrow(data_sub_subset))) {
        header_options <- box$box$header$options
        
        # Determine color
        col_value <- if (!is.null(colors_by) && colors_by != '.tmp_colors_by') {
          color_idx <- get_color(data_sub_subset[k, colors_by])
          safe_get_palette_color(header_options$get_palette(), color_idx)
        } else {
          'black'
        }
        
        # Determine point character
        pch_idx <- if (!is.null(colors_by) && colors_by != '.tmp_colors_by') {
          get_color(data_sub_subset[k, colors_by]) + options_br$forest.pch.shift
        } else {
          NULL
        }
        
        plot_forest_tree(
          box = box$box$box, 
          x_lower = data_sub_subset[k, 'lower', drop = TRUE],
          x_upper = data_sub_subset[k, 'upper', drop = TRUE], 
          x_dot = data_sub_subset[k, 'value', drop = TRUE], 
          n1 = ben_idx, 
          n2 = k, 
          N2 = nrow(data_sub_subset), 
          userect = userect, 
          col = col_value, 
          pch = pch_idx, 
          options = header_options
        )
      }
    }
  }
  
  return(list(
    name = 'box_group', 
    boxes = boxes, 
    options = options_br, 
    last_y = min(last_graph_part$y_pos)
  ))
}


#' Safely get color from palette
#' @param palette Color palette vector
#' @param index Index to retrieve
#' @return Color string
#' @keywords internal
safe_get_palette_color <- function(palette, index) {
  if (is.null(index) || is.na(index) || index < 1 || index > length(palette)) {
    return("black")
  }
  palette[index]
}
