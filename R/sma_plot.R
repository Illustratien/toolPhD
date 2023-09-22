#' @title sma plot
#'
#' @description
#' \code{sma_plot} plot sma correlation analysis add regression line and add r2
#'
#' @keywords standardised major axis
#'
#' @param datf a dataframe, include columns x and y
#' @param xvar unquoted string for column name x
#' @param yvar unquoted string for column name y
# @param colorvar unquoted string for column name that used for coloring
#' @return a ggplot with regression line and r2 text
#'
#' @author Tien-Cheng Wang
#'
#' @importFrom smatr sma
#' @import ggplot2
#' @importFrom ggpp geom_text_npc
#' @import rlang
#' @importFrom usethis use_pipe
#' @importFrom Rdpack reprompt
#' @export
# @references
#\insertRef{warton2012}{toolPhD}
#'
#' @examples
#' sma_plot(mtcars,xvar=mpg,yvar = cyl)
# sma_plot(mtcars,xvar=mpg,yvar = cyl,colorvar=cyl)
# function ----------------------------------------------------------------
sma_plot <- function(datf,xvar,yvar){

  library(rlang)
  # sma plot
  # note that this is not yet applicable to groups.
  SMA <-  smatr::sma(paste0(substitute(yvar),"~",substitute(xvar)), method = "SMA", data = datf)
  SMA.slope <- coef(SMA)[[2]]
  SMA.intercept <- coef(SMA)[[1]]
  r2 <- paste("italic(y)==",format(SMA.slope, digits = 2),
              "%.%italic(x)","+", format(SMA.intercept, digits = 2),
              "~~~italic(r)^2==",format(SMA$r2, digits = 2),sep = "")

  p <- ggplot(data=datf) +
    geom_point(aes(x={{xvar}},y={{yvar}}), shape=1,size=3,show.legend=F)+
    geom_abline(aes(slope=SMA.slope,intercept=SMA.intercept),
                size=1.25,show.legend=F)+
    ggpp::geom_text_npc(aes(npcx="right",npcy="bottom",label =r2),
                        parse = T,size=4)+
    theme_phd_facet()
  # not successful
  # colorvar=NULL
  # if(!is.null(colorvar)){
  #   p <- p+
  #     geom_point(data=datf,
  #                mapping=aes(x={{xvar}},y={{yvar}},color=as.factor({{colorvar}})), shape=1,size=3)
  # }
  return(p)
}

