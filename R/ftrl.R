#' FTRL Optimizer
#'
#' @param n_weights number of coefficients to fit
#' @param alpha just look at the paper
#' @param beta look at the paper
#' @param lambda1 paper
#' @param lambda2 paper paper paper
#'
#' Really not for production
#' @export
FTRLDenseOptimizer <- function(n_weights, alpha = 0.1,
                               beta = 1, lambda1 = 1, lambda2 = 1) {
  all_zeros <- rep.int(0, n_weights)
  z <- all_zeros
  n <- all_zeros
  last_weights <- all_zeros
  lambda1 <- rep_len(lambda1, n_weights)
  lambda2 <- rep_len(lambda2, n_weights)
  derive_weights <- function() {
    new_weights <- all_zeros
    update <- abs(z) > lambda1
    w <- z - sign(z) * lambda1
    w <- w / -((beta + sqrt(n)) / alpha + lambda2)
    new_weights[update] <- w[update]
    last_weights <<- new_weights
    last_weights
  }
  gradient <- function(p, y, x) {
    (p - y) * x
  }
  predict <- function(x) {
    stats::plogis(sum(last_weights * x))
  }
  fit <- function(x, y) {
    derive_weights()
    p <- predict(x)
    g <- gradient(p, y, x)
    nn <- n + g**2
    s <- (sqrt(nn) - sqrt(n)) / alpha
    z <<- z + g - s * last_weights
    n <<- nn
    invisible()
  }
  list(
    fit = fit,
    weights = derive_weights,
    predict = predict
  )
}
