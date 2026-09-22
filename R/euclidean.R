#' Euclidean algorithm
#'
#' Finds the greatest common divisor of two numbers.
#'
#' The function divides the numbers and saves the remainder.
#' It continues until the remainder is 0.
#' The last value of a is the greatest common divisor.
#'
#' @param a A numeric value.
#'
#' @param b A numeric value.
#'
#' @return The greatest common divisor of a and b.
#'
#' @references
#' Wikipedia: Euclidean algorithm
#' \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}
#'
#' @export
euclidean <- function(a, b) {
  stopifnot(
    "a must be a numeric scalar" = is.numeric(a) && length(a) == 1,
    "b must be a numeric scalar" = is.numeric(b) && length(b) == 1
  )

  while (b != 0) {
    remainder <- a %% b
    a <- b
    b <- remainder
  }

  return(a)
}
?e
