#' @keywords internal
"_PACKAGE"

#' brrr: Benefit-Risk Visualization for Clinical Trials
#'
#' The brrr package provides functions for creating benefit-risk visualization
#' plots commonly used in clinical trial analysis and regulatory submissions.
#'
#' @section Main Functions:
#' \describe{
#'   \item{\code{\link{plot_br}}}{Create complete benefit-risk visualizations}
#'   \item{\code{\link{create_header}}}{Create plot headers}
#'   \item{\code{\link{add_box}}}{Add forest plot boxes}
#'   \item{\code{\link{add_legend}}}{Add legends}
#' }
#'
#' @section Styling:
#' \describe{
#'   \item{\code{\link{page_options}}}{R6 class for customizing plot appearance}
#' }
#'
#' @section Data:
#' \describe{
#'   \item{\code{\link{mock_data}}}{Sample benefit (efficacy) data}
#'   \item{\code{\link{mock_data_risks}}}{Sample risk (safety) data}
#' }
#'
#' @docType _PACKAGE
#' @name brrr-package
#' @aliases brrr
#' 
#' @importFrom grid grid.newpage pushViewport viewport grid.rect grid.lines grid.text
#' @importFrom grid unit convertX convertY convertHeight convertWidth arrow gpar
#' @importFrom grid textGrob grobWidth is.unit grid.points
#' @importFrom dplyr mutate select filter arrange group_by ungroup distinct
#' @importFrom dplyr across all_of any_of if_all pull n
#' @importFrom rlang !! :=
#' @import R6
NULL
