#' My first function
#'
#' @param a A numerical vector.
#' @param b Also a numerical vector.
#'
#' @return A numerical vector of a + b * a.
#' @export
#'
#' @examples
#' myfunc(3, 5)
myfunc <- function(a, b) {
  result <- a * b + a
  return(result)
}

#' OLS estimator
#'
#' @param y A numerical vector, outcomes. 
#' @param X Also a numerical vector, observations.
#'
#' @return A numerical vector of (X^T X)^(-1) X^T y.
#' @export
estimate_beta <- function(y, X) {
  beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
  return(beta_hat)
}

#' A custom ggplot2 theme with no grid lines, a light gray background, and bold axis titles.
#'
#' @return A ggplot2 theme object.
#' @export
my_theme <- function() {
  ggplot2::theme_minimal() +  
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),  
      panel.grid.minor = ggplot2::element_blank(),  
      plot.background = ggplot2::element_rect(fill = "lightgray", color = NA),
      axis.title = ggplot2::element_text(size = 14, face = "bold")
    )
}
