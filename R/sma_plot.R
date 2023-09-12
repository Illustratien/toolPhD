#' @title sma plot
#'
#' @description
#' \code{sma_plot} plot sma correlation analysis add regression line and add r2
#'
#' @keywords standardised major axis
#'
#' @param data a dataframe, include columns x and y
#' @param x_string string for colmun name x
#' @param y_string string for column name y

#' @return a ggplot with regression line and r2 text
#'
#' @author Tien-Cheng Wang
#'
#' @importFrom smatr sma
#' @import ggplot2
#' @importFrom ggpp geom_text_npc
#' @importFrom purrr map_dfr
#' @importFrom usethis use_pipe
#' @importFrom Rdpack reprompt
#'
#' @references
#' \insertRef{warton2012}{toolPhD}
#'
#' @export
#'
#' @examples
#' sma_plot(mtcars,x_string="mpg",y_string = "cyl")
# function ----------------------------------------------------------------
sma_plot <- function(data,x_string,y_string){
  # sma plot
  # note that this is not yet applicable to groups.
  SMA <-  smatr::sma(paste0(y_string,"~",x_string), method = "SMA", data = data)
  SMA.slope <- SMA %>% coef %>% .[[2]]
  SMA.intercept <- SMA %>% coef %>% .[[1]]
  r2 <- paste("italic(y)==",format(SMA.slope, digits = 2),
              "%.%italic(x)","+", format(SMA.intercept, digits = 2),
              "~~~italic(r)^2==",format(SMA$r2, digits = 2),sep = "")

  data %>%
    ggplot() +
    geom_point(aes(x=!!sym(x_string),y=!!sym(y_string)), size=3,show.legend=F)+
    geom_abline(aes(slope=SMA.slope,intercept=SMA.intercept),
                size=1.25,show.legend=F)+
    ggpp::geom_text_npc(aes(npcx="right",npcy="bottom",label =r2),
                        parse = T,size=4)+
    theme_test()+
    theme(axis.text = element_text(size=12,face = 2),
          axis.title=element_text(size=15,face=2))
}
