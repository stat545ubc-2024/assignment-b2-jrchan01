#' @title Calculate the coefficient of variation
#'
#' @description This calculates the coefficient of variation (standard deviation divded by the mean) to assess variability in the dataset.

#' @param .x. The variable of interest we use from the dataset to calculate the coefficient of variation. I am using the label "x" to represent the variable of interest. I also chose to use "x" as this is common notation used in field of statistics.
#' @param .na.rm. This is a logical argument used to ensure that any missing values (NA) are removed from the dataset. I have used the label na.arm as this is the common label used in this field when defining what to do when there is missing data (i.e., NAs) in the dataset.

#' @return A numeric value is returned that tells us what the coefficient of variation is.

#' @examples
#' cv(gapminder$lifeExp)
#' cv(gapminder$pop)
#' cv(gapminder$gdpPercap)
#'
#' @importFrom stats sd
#'
#' @export
cv_function <- function(x, na.rm = TRUE) {
  if (!is.numeric(x)) {
    stop("Input must be numeric")
  }
  mean_value <- mean(x, na.rm = na.rm)
  sd_value <- stats::sd(x, na.rm = na.rm)
  return((sd_value / mean_value) * 100)
}
