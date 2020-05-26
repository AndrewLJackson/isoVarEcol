

#' Calculates stable isotope values of a population of consumers at equilibrium
#' with their diet
#'
#' more details here
#'
#' @param p a matrix of a population of dietary proportions. Each row is an
#'   individual. Each column the proportion of food source S_i in the diet.
#'   Defaults to a single consumer whose diet comprising 3 food sources is drawn
#'   from a Dirichlet distribution with alpha = [1, 1, 1].
#' @param S a matrix of stable isotope values of the consumers' food sources.
#'   Each row is a food source S_i and each column is a stable isotope tracer.
#'   Typically in trophic ecology, we would expect two columns, one for d13C and
#'   other for d15N. Other isotopes are also common. Defaults to 3 food sources
#'   for two isotopes drawn from a random uniform distribution between -10 and
#'   0.
#' @param TDF a scalar in the trivial case or a matrix of Trophic Discrimination
#'   Factors of the same size as S. The correction is applied to the food
#'   sources as per the stable isotope mixing models MixSIAR, SIAR and simmr.
#'   Defaults to a scalar 0.
#'
#' @return a matrix of stable isotope values for each consumer with number of
#'   rows equal to that of p and columns equal to that of S.
#' @export
#'
#' @examples


simPopEquilibrium <- function(p = MCMCpack::rdirichlet(1, rep(1, 3)),
                              S = matrix(runif(6, min = -10, max = 0),
                                         nrow = 3, ncol = 2), TDF = 0,
                              setSeed = NULL, ...) {

  if(!is.null(setSeed)) {set.seed(setSeed)}

  # Calculate the Stable isotope values of the consumers as a
  # p weighted sum of food source isotoep values (S) after applying the
  # Trophic Discrimination factor (TDF)
  consumerIso <- p %*% (S + TDF)

  return(consumerIso)
}
