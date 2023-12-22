#' @title plot 1 to 1 line to examine linearity
#'
#' @description
#' \code{plot_121} plot 1 to 1 line to examine linearity
#'
#' @keywords violoin
#'
#' @param df a dataframe, include columns xvar and yvar
#' @param xvar unquoted variable for colmun name for x axis, should be categorical
#' @param yvar unquoted variable for column name for y axis, should be numeric
#' @param color `NULL`, unquoted variable for colmun name for group
#' @param labx string for x axis title, default is xvar.
#' @param laby string for y axis title, default is yvar

#' @return a ggplot with 1:1 line
#'
#' @author Tien-Cheng Wang
#'
#' @import ggplot2
#' @import rlang
#' @import ggpmisc
#' @import pacman
#' @export
#'
#' @examples
#'   pacman::p_load(ggplot2,toolPhD,ggpmisc)
#'   plot_121(iris,Sepal.Length,Petal.Length,color=Species)
#'   plot_121(iris,Sepal.Length,Petal.Length)

# function ----------------------------------------------------------------

plot_121 <- function(df,xvar,yvar,color=NULL,labx=NULL,laby=NULL,...){
  # plot 1 to 1 plot
  library(rlang)

  po <- ggplot2::ggplot(aes({{xvar}},{{yvar}},color={{color}}),data=df)
  x <- enquos(xvar)
  y <- enquos(yvar)
  lmt <-range(unlist(dplyr::select(df,!!!x,!!!y)))

  po+
    ggplot2::xlab(ifelse(is.null(labx),substitute(xvar),labx))+
    ggplot2::ylab(ifelse(is.null(laby),substitute(yvar),laby))+
    geom_point(shape=1)+
    scale_x_continuous(limits = lmt)+
    scale_y_continuous(limits=lmt)+
    theme_phd_talk()+
    geom_abline(slope = 1,intercept = 0,linewidth=1,
                linetype=3,color="darkgray")+
    ggpmisc::stat_poly_line(formula = y ~ x,se=F) +
    ggpmisc::stat_poly_eq(formula =  y ~x,
                          aes(label = paste(after_stat(eq.label),
                                            after_stat(rr.label),
                                            sep = "*\", \"*")),size = 4)+
    ggplot2::theme(legend.position = "bottom")+
    labs(subtitle = "gray dash line: 1 to 1 line")
}
