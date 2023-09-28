#' @title theme phd talk
#'
#' @description
#' \code{theme_phd_talk} plot ggplot like a pro for facet
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
#' @return aesthetic theme frame
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
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars,aes(x=disp,y=hp)) + geom_point(size=3,shape=1)
#' cowplot::plot_grid(p,p+theme_phd_talk())

theme_phd_talk<- function(ax.txt.siz=NULL,ax.tit.siz=NULL,lgd.txt.siz=NULL,lgd.tit.siz=NULL,strp.txt.siz=NULL,
                          t=NULL,r=NULL,b=NULL,l=NULL,frame=NULL,frame.tick=NULL,...){

  if (is.null(ax.txt.siz)){ax.txt.siz <-18}
  if (is.null(ax.tit.siz)){ax.tit.siz <- 20}
  if (is.null(lgd.txt.siz)){lgd.txt.siz <-18}
  if (is.null(lgd.tit.siz)){lgd.tit.siz <- 19}
  if (is.null(strp.txt.siz)){strp.txt.siz <- 25}
  if(is.null(t)){t<-3}
  if(is.null(r)){r<-3}
  if(is.null(b)){b<-5}
  if(is.null(l)){l<-3}
  if(is.null(frame)){frame<-2}
  if(is.null(frame.tick)){frame.tick<-1.5}
  ggplot2::theme_test()+
    ggplot2::theme(
      # axis text
      axis.text.x = element_text(size=ax.txt.siz,face="bold",vjust=-1), # tick label size
      axis.text.y = element_text(size=ax.txt.siz,face="bold",margin = margin(r=8)), # tick label size
      # axis title
      axis.title   =element_text(size=ax.tit.siz,face="bold"),# axis title size
      axis.title.y = element_text(margin = margin(r=20),vjust=-1),# spacing of y and tick,
      axis.title.x = element_text(margin = margin(b=20),vjust=-5), # spacing of x and axis
      # axis tick
      axis.ticks   =element_line(size = frame.tick),# tick thickness
      axis.ticks.length=unit(.3, "cm"),# tick length
      # facet
      strip.text.x   = element_text(size=strp.txt.siz,face="bold"),
      strip.text.y   = element_text(size=strp.txt.siz,face="bold"),
      # strip.background = element_rect(colour = "White", fill=NA),
      # frame
      panel.border = element_rect(colour = "black", fill=NA, size=frame),
      panel.grid = element_blank(),
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
