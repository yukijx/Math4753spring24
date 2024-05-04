#' Title
#'
#' @param N number of seats
#' @param gamma probability of a plane being overbooked
#' @param p probability of a "show"
#' @importFrom stats pbinom pnorm
#' @importFrom graphics layout abline
#'
#' @return prints a named list containing nd, nc, N, p and gamma;
#'         creates a plot of Objective function Vs n
#' @export
#'
#' @examples ntickets(N=400,gamma = 0.02, p = 0.95)
#'
ntickets = function(N, gamma, p) {

  # discrete
  discrete_n = seq(N, floor(N + N/10), by = 1)
  d_objective = 1 - gamma - pbinom(N, size = discrete_n, prob = p)
  d_ind = which.min(abs(d_objective))
  nd = discrete_n[d_ind]

  # continuous
  continuous_n = seq(N, floor(N + N/10), by = 0.001)
  c_objective = 1 - gamma - pnorm(N + 0.5, mean = continuous_n*p, sd = sqrt(continuous_n*p*(1 - p)))
  c_ind = which.min(abs(c_objective))
  nc = continuous_n[c_ind]

  layout(matrix(1:2, nrow = 2, byrow = TRUE))

  # plotting discrete graph: Objective function Vs n
  plot(discrete_n, d_objective, type = "s", ylab = "Objective", pch = 16, cex = 0.75,
       col = ifelse(discrete_n == nd, "black", "purple"),
       main = paste("Objective Vs n to find optimal tickets sold (", nd, ")\n",
                    "gamma = ", gamma, " N = ", N, " discrete"))
  abline(h = d_objective[d_ind], v = nd, col = "black")

  # plotting continuous: Objective function Vs n
  plot(continuous_n, c_objective, type = "l", xlab = "n", ylab = "Objective",
       main = paste("Objective Vs n to find optimal tickets sold (", round(nc, 2), ")\n",
                    "gamma = ", gamma, " N = ", N, " continuous"))
  abline(h = c_objective[c_ind], v = nc, col = "black")

  # print named list containing nd, nc, N, p and gamma
  return(list(nd = nd, nc = round(nc, 2), N = N, p = p, gamma = gamma))

}
