#' @title Outliers
#'
#' @param data A quantitative vector
#'
#' @return A list containing possible and defined outliers. Possible outliers have a z-score between 2 and 3, while defined outliers have a z-score greater than 3.
#' @export
#'
#' @examples
#' d <- 1:40; outliers(data=d)

outliers=function(data){

  # Convert data to z-scores
  data.z = (data - mean(data))/sd(data)

  # Find possible outliers (3 ≥ |z| ≥ 2)
  possible.outliers <- data[abs(data.z) >= 2 & abs(data.z) <=3]

  # Find defined outliers (|z| > 3)
  defined.outliers <- data[abs(data.z) > 3]

  # Make a list containing possible and defined outliers
  outliers <- list("defined.outliers" = defined.outliers, "possible.outliers" = possible.outliers)
  outliers

}
