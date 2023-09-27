#' @title theme phd main
#'
#' @description
#' \code{theme_phd_main} plot ggplot like a pro for facet
#'
#' @keywords standardised major axis
#'
#' @param ax.txt.siz axis text size
#' @param ax.tit.siz axis title size
#' @param lgd.txt.siz legend text size
#' @param lgd.tit.siz legend title size
#' @param strp.txt.siz strip(facet) text size
#' @param t distance to top border
#' @param r distance to right border
#' @param b distance to bottom border
#' @param l distance to left border
#'
#' @return aesthetic facet frame
#'
#' @author Tien-Cheng Wang
#'
#' @import ggplot2
#' @importFrom usethis use_pipe
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars,aes(x=disp,y=hp)) + geom_point(size=3,shape=1)
#' cowplot::plot_grid(p,p+theme_phd_main())

theme_phd_main<- function(ax.txt.siz=NULL,ax.tit.siz=NULL,lgd.txt.siz=NULL,lgd.tit.siz=NULL,strp.txt.siz=NULL,
                     t=NULL,r=NULL,b=NULL,l=NULL,frame=NULL,frame.tick=NULL,...){

  if (is.null(ax.txt.siz)){ax.txt.siz <-10}
  if (is.null(ax.tit.siz)){ax.tit.siz <- 12}
  if (is.null(lgd.txt.siz)){lgd.txt.siz <- 8}
  if (is.null(lgd.tit.siz)){lgd.tit.siz <- 8}
  if (is.null(strp.txt.siz)){strp.txt.siz <- 12}
  if(is.null(t)){t<-10}
  if(is.null(r)){r<-10}
  if(is.null(b)){b<-10}
  if(is.null(l)){l<-10}
  if(is.null(frame)){frame<-2}
  if(is.null(frame.tick)){frame.tick<-1.5}
  ggplot2::theme_test()+
    ggplot2::theme(

      # axis text
      axis.text.x = element_text(size=ax.txt.siz,face="bold",vjust=-2), # tick label size
      axis.text.y = element_text(size=ax.txt.siz,face="bold",margin = margin(r=8)), # tick label size
      # axis title
      axis.title   =element_text(size=ax.tit.siz,face="bold"),# axis title size
      axis.title.y = element_text(margin = margin(r=10),vjust=-2),# spacing of y and tick
      axis.title.x = element_text(margin = margin(b=),vjust=-2), # spacing of x and axis
      # axis tick
      axis.ticks   =element_line(size = frame.tick),# tick thickness
      axis.ticks.length=unit(.3, "cm"),# tick length
      # facet
      strip.text.x   = element_text(size=strp.txt.siz,face="bold"),
      strip.text.y   = element_text(size=strp.txt.siz,face="bold"),
      strip.background = element_rect(colour = "White", fill=NA),
      # frame
      panel.border = element_rect(colour = "black", fill=NA, size=frame),
      # axis.line    = element_line(size = 2, linetype = "solid"),# axis line
      axis.line.x.top = element_line(size = frame, linetype = "solid"),# axis line
      # axis.line.x.bottom = element_line(size = frame, linetype = "solid"),# axis line
      axis.line.y.right = element_line(size = frame, linetype = "solid"),# axis line
      # axis.line.y.left = element_line(size = frame, linetype = "solid"),# axis line
      # legend
      legend.title = element_text(size = lgd.tit.siz,face='bold'),# legend size
      legend.text  = element_text(size =lgd.txt.siz,face='bold'),# legend text
      legend.background = element_rect(fill=NA),
      # legend.justification=c(1,0), legend.position=c(.99,0.1),# legend position lower right
      plot.margin = margin(t = t, r = r, b = b, l = l, unit = "pt"),
      plot.background = element_rect(
        fill = "white"
      ),
      ...
    )
}
