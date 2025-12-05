#' Mock data for Benefit-Risk Visualization
#'
#' A sample dataset demonstrating the structure required for benefit-risk 
#' visualizations. Contains efficacy endpoint data with treatment comparisons.
#'
#' @format A data frame with clinical trial results:
#' \describe{
#'   \item{endpoint}{Character. The endpoint name (e.g., "BMI Change", "Weight Loss")}
#'   \item{treatment}{Character. The treatment arm result label}
#'   \item{placebo}{Character. The placebo arm result label}
#'   \item{estimator}{Character. The statistical estimator name (e.g., "Hazard Ratio", "Odds Ratio")}
#'   \item{axis_number}{Integer. The axis grouping number for separating different estimator types}
#'   \item{value}{Numeric. The point estimate}
#'   \item{lower}{Numeric. The lower bound of the confidence interval}
#'   \item{upper}{Numeric. The upper bound of the confidence interval}
#'   \item{col3}{Character. Text label for the comparison column (formatted estimate with CI)}
#' }
#' 
#' @details
#' This dataset is used in examples to demonstrate how to create benefit-risk
#' visualizations. The structure follows the standard format expected by \code{plot_br()}.
#' 
#' @examples
#' data(mock_data)
#' head(mock_data)
#' 
#' @seealso \code{\link{plot_br}}, \code{\link{mock_data_risks}}
"mock_data"


#' Mock risk data for Benefit-Risk Visualization
#'
#' A sample dataset demonstrating risk/safety endpoint data structure for 
#' benefit-risk visualizations. Contains adverse event data with treatment comparisons.
#'
#' @format A data frame with safety/risk results:
#' \describe{
#'   \item{endpoint}{Character. The safety endpoint name (e.g., "Nausea", "Headache")}
#'   \item{treatment}{Character. The treatment arm event rate or result}
#'   \item{placebo}{Character. The placebo arm event rate or result}
#'   \item{estimator}{Character. The statistical estimator name (e.g., "Risk Ratio")}
#'   \item{axis_number}{Integer. The axis grouping number}
#'   \item{value}{Numeric. The point estimate}
#'   \item{lower}{Numeric. The lower bound of the confidence interval}
#'   \item{upper}{Numeric. The upper bound of the confidence interval}
#'   \item{txt_val}{Character. Text label for the comparison column (formatted estimate with CI)}
#' }
#' 
#' @details
#' This dataset complements \code{mock_data} by providing safety/risk endpoints.
#' It can be used with \code{box_group} parameter in \code{plot_br()} to create
#' combined benefit-risk visualizations.
#' 
#' @examples
#' data(mock_data_risks)
#' head(mock_data_risks)
#' 
#' @seealso \code{\link{plot_br}}, \code{\link{mock_data}}
"mock_data_risks"
