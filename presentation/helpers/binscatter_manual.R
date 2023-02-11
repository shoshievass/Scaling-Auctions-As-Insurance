#' Basic Bin-scatter function
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
#' @keywords binscatter
#' @export
#' @examples
#' @importFrom lfe felm
#' @import dplyr
#' @import ggplot2
#' @import broom

binscatter_manual <- function(data, y, x, bins=20, discrete=FALSE, scatter=FALSE, connectdots = FALSE,
                       theme=theme_binscatter, fitline=TRUE, controls=c(), absorb=c("0"),
                       clustervars=c("0"), pos="bottom right") {
  x_label = enquo(x)
  y_label = enquo(y)

  if(length(controls) == 0) {
    formula = as.formula(paste(quo_name(y_label), "~", quo_name(x_label)  , "|" ,
                               paste(absorb,sep="",collapse=" + ") , "|" , "0" ,  "|" ,
                               paste(clustervars,sep="",collapse=" + "), sep=" "))
  }
  if(length(controls)!=0) {
    formula = as.formula(paste(quo_name(y_label), "~", quo_name(x_label), "+", paste(controls,sep="",collapse=" + ") , "|" ,
                               paste(absorb,sep="",collapse=" + ") , "|" , "0" ,  "|" ,
                               paste(clustervars,sep="",collapse=" + "), sep=" "))

    y_res_formula = as.formula(paste(quo_name(y_label), "~", paste(controls,sep="",collapse=" + ") , "|" ,
                                     paste(absorb,sep="",collapse=" + ") , "|" , "0" ,  "|" ,
                                     paste(c("0"),sep="",collapse=" + "), sep=" "))
    x_res_formula = as.formula(paste(quo_name(x_label), "~", paste(controls,sep="",collapse=" + ") , "|" ,
                                     paste(absorb,sep="",collapse=" + ") , "|" , "0" ,  "|" ,
                                     paste(c("0"),sep="",collapse=" + "), sep=" "))
    controls <- data[,controls]

    print(y_res_formula)
  }

  x <- data[[quo_name(x_label)]]
  y <- data[[quo_name(y_label)]]

  f <- lfe::felm(formula, data=data)
  print(broom::tidy(f)[2,2:3])
  beta <- paste("beta", formatC(broom::tidy(f)[2,2][[1]], digits=3,format="fg", flag="#"), sep="=")
  se <-   paste("s.e.", formatC(broom::tidy(f)[2,3][[1]], digits=3,format="fg", flag="#"), sep="=")

  if(length(controls) == 0) {
    data$x_binning <- x
    data$y_binning <- y
  } else {
    f_Xres <- lfe::felm(x_res_formula, data=data)
    f_Yres <- lfe::felm(y_res_formula, data=data)
    data$x_binning <- f_Xres$residuals + mean(x)
    data$y_binning <- f_Yres$residuals + mean(y)
  }

  data$x_manal_bin <-  mltools::bin_data(data$x_binning, bins=bins, binType = "explicit")

  binned_data <- data %>%
    group_by(x_manal_bin) %>%
    summarize(
      x_binned = mean(x_binning),
      y_binned = mean(y_binning)
    )

  g <- ggplot2::ggplot(data=binned_data, aes(x = x_binned , y= y_binned))  + theme() +
    xlab(x_label) + ylab(y_label) + geom_point(colour = "#0072B2", size = 2.5)

  if (fitline == TRUE) {
    g <- g + geom_smooth(data=binned_data , aes(x = x_binned , y= y_binned), method='lm',formula=y~x, se=FALSE, color="#D55E00", size=1)
    posx <- c(Inf, Inf, -Inf, -Inf)
    posy <- c(Inf, -Inf, Inf, -Inf)
    posname <- c("top right", "bottom right", "top left", "bottom left")
    adjh <- c(1,1,-1,-1)
    adjv <- c(1,-1,1,-1)
    posdf <- data.frame(posx, posy, adjh, adjv, row.names=posname)

    if(pos %in% posname){
      pos_choice = posdf[pos,]

      ## Calculate adjustments so that beta is on top of se in any position
      v_adj_beta = pos_choice$adjv * 2.5 + (pos_choice$adjv+1)*(-0.75)
      v_adj_se = pos_choice$adjv * 1 + (pos_choice$adjv+1)*(0.75)

      # print(posdf)
      g <- g +
        geom_text(data = data.frame(x=pos_choice$posx, y=pos_choice$posy), map = aes(x=x, y=y,hjust=pos_choice$adjh, vjust=(v_adj_beta), family = "Times New Roman"), label=beta) +
        geom_text(data = data.frame(x=pos_choice$posx, y=pos_choice$posy), map = aes(x=x, y=y,hjust=pos_choice$adjh, vjust=v_adj_se, family = "Times New Roman"), label=se)
    }

   }

  return(g)
}
