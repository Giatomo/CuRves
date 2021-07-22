# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


#' Modified Richard Equation
#'
#' @param t     Float : Time
#' @param p_max Float : Population Maximum (Upper Asymptote)
#' @param p_min Float : Population Minimum (Lower Asymptote)
#' @param r_min Float : Maximum Growth/Death rate (positive for growth|negative fo death)
#' @param s     Float : Shift (Time at which r_max occurs)
#' @return The population at time \code{t} Given by \deqn{P(t) = p_{min} + \frac{p_{max}-p_{min}}{1 + e^{4r_{max}.(t-s)/p_{min}- p_{max}}}}
#' @return
#' @examples
#' add(1, 1)
#' add(10, 1)
richard <- function(t, p_max, p_min, r_max, s){
  p_t <- p_min + (p_max - p_min) / (1 + exp(4 * r_max * (t - s)/(p_min - p_max)))
  return(p_t)
  }


fit_richard <- function(y, t) {

  max_y <- max(y)
  min_y <- min(y)
  r_boundary <- plyr::round_any(max_y-min_y, 10^(floor(log10(max_y-min_y))), f = ceiling)
  s_boundary <- max(t)

  fit <- GA::ga(
    type = "real-valued",
    fitness = \(p) -sum((y - richard2(t = t, p[1], p[2], p[3], p[4]))^2),
    lower = c(
      p_max = max_y - 0.01*max_y,
      p_min = min_y - 0.01*max_y,
      r_max = -r_boundary,
      s     = 0),
    upper = c(
      p_max = max_y + 0.01*max_y,
      p_min = min_y + 0.01*max_y,
      r_max = r_boundary,
      s     = s_boundary),
    names = c(
      "p_max",
      "p_min",
      "r_max",
      "s"),
    popSize = 100,
    monitor = FALSE)@solution

  return(fit)
  }

