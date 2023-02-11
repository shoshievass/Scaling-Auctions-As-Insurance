#' Bin-scatter by group function
#'
#' This function does a binscatter for two-dimensional data
#' @param data Dataframe containing the x and y variables
#' @param y The independent variable y
#' @param x The dependent variable x
#' @param bins The number of bins
#' @param discrete Discrete?
#' @param scatter Not sure what this is yet...
#' @param theme Optional ggplot theme
#' @param fitline Option to add a linear fit through the bin scatter
#' @param controls Optional control variables to the binscatter
#' @param clustervars Optional cluster variables
#' @param group Grouping variable
#' @keywords binscatter
#' @export
#' @examples
#' @importFrom lfe felm
#' @import dplyr
#' @import ggplot2
#' @import broom

binscatter_manual_by_group_with_weights <- function(data, y, x, weightvar, weight_sizenorm = 20, bins=20, discrete=FALSE, scatter=FALSE, connectdots = FALSE,
                                grouping_var, theme=theme_binscatter, fitline=TRUE, controls=c(), absorb=c("0"),
                       clustervars=c("0"), pos="bottom right") {

  grouping_var = enquo(grouping_var)
  x_label = enquo(x)
  y_label = enquo(y)
  weightvar_label = enquo(weightvar)
  # weight_sizenorm_label = enquo(weight_sizenorm)

  # print(weight_sizenorm)

  # Produce binned residualized variables by group
  binned_data <- data %>%
    group_by(!!grouping_var) %>%
    do(
      # get_manual_binning_residuals(., !!y_label, !!x_label, controls, absorb, clustervars)
      get_manual_binning_residuals_with_weights(., !!y_label, !!x_label, !!weightvar_label, weight_sizenorm, controls, absorb, clustervars, bins)
    )

  # Produce raw residualized variables by group
  unbinned_data <- data %>%
    group_by(!!grouping_var) %>%
    do(
      # get_manual_binning_residuals(., !!y_label, !!x_label, controls, absorb, clustervars)
      get_unbinned_residuals(., !!y_label, !!x_label, controls, absorb, clustervars)
    )

  # print(data)

  # Make sure the grouping variable is treated as a discrete object
  binned_data <- binned_data %>%
    mutate(
      group_factor = as.character(!!grouping_var)
    )

  # Make sure the grouping variable is treated as a discrete object
  unbinned_data <- unbinned_data %>%
    mutate(
      group_factor = as.character(!!grouping_var)
    )


  g <- ggplot2::ggplot(binned_data, aes(x = x_binned , y= y_binned, color = factor(!!grouping_var) )) +
    theme() +
    labs(
      x = x_label,
      y = y_label,
      color = quo_name(grouping_var)
    ) +
    geom_point(size = log(binned_data$weight_binned_normed))


  if (fitline == TRUE) {
    g <- g + geom_smooth(data=unbinned_data , aes(x = x_binning , y= y_binning), method='lm',formula=y~x, se=FALSE, size=1)
    posx <- c(Inf, Inf, -Inf, -Inf)
    posy <- c(Inf, -Inf, Inf, -Inf)
    posname <- c("top right", "bottom right", "top left", "bottom left")
    adjh <- c(1,1,-1,-1)
    adjv <- c(1,-1,1,-1)
    posdf <- data.frame(posx, posy, adjh, adjv, row.names=posname)
  }

  return(g)
}
