#' @title violin plot
#'
#' @description
#' \code{violin_plot} plot distribution of numeric y over categorical x
#'
#' @keywords violoin
#'
#' @param df a dataframe, include columns xvar and yvar
#' @param xvar unquoted variable for colmun name for x axis, should be categorical
#' @param yvar unquoted variable for column name for y axis, should be numeric
#' @param oderx logical, default is FALSE, if TRUE, order the x axis based on y.
#' @param labx string for x axis title, default is xvar.
#' @param laby string for y axis title, default is yvar

#' @return a ggplot with violin plot
#'
#' @author Tien-Cheng Wang
#'
#' @import ggplot2
#' @importFrom ggbeeswarm geom_quasirandom
#' @import rlang
#' @importFrom usethis use_pipe
#' @importFrom Rdpack reprompt
#' @export
#'
#' @examples
#' violin_plot(mtcars,cyl,mpg,orderx=T)
#' violin_plot(mtcars,cyl,mpg,labx="Number of cylinders",laby="Miles/gallon")
# function ----------------------------------------------------------------

violin_plot <- function(df,xvar,yvar,orderx=F,labx=NULL,laby=NULL,...){
  library(rlang)
  if(orderx==T){
    po <- ggplot2::ggplot(aes(reorder({{xvar}},desc({{yvar}})),{{yvar}},group={{xvar}},fill=as.factor({{xvar}})),data=df)
  }else{
    po <- ggplot2::ggplot(aes({{xvar}},{{yvar}},group={{xvar}},fill=as.factor({{xvar}})),data=df)
  }

  po+
    ggplot2::scale_fill_viridis_d( option = "D")+
    ggplot2::xlab(ifelse(is.null(labx),substitute(xvar),labx))+
    ggplot2::ylab(ifelse(is.null(laby),substitute(yvar),laby))+
    ggplot2::geom_violin(alpha=0.5,position = position_dodge(width = .75),size=1,color=NA) +
    ggplot2::geom_boxplot(outlier.size = -1, color="black",
                 position = position_dodge(width = .75),
                 lwd=.5,
                 width=.1,
                 alpha = 0.1,show.legend = F)+
    ggbeeswarm::geom_quasirandom(size=2, dodge.width = .75,
                                 color="black")+
    theme_phd_facet()+
    ggplot2::theme(
      panel.grid.major.x = element_blank(),

      legend.position = "none"
    )
}
