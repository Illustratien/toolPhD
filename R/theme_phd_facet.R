#' @title theme phd facet
#'
#' @description
#' \code{theme_phd_facet} plot ggplot like a pro for facet
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
#' @param frame logical, default is FALSE, if TRUE, frame will be added.
#'
#' @return aesthetic facet frame
#'
#' @author Tien-Cheng Wang
#'
#' @import ggplot2
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars,aes(x=disp,y=hp)) + geom_point(size=3,shape=1)+facet_grid(~cyl)
#' cowplot::plot_grid(p,p+theme_phd_facet())
#' cowplot::plot_grid(p,p+theme_phd_facet(frame=T))
theme_phd_facet <- function(ax.txt.siz=NULL,ax.tit.siz=NULL,lgd.txt.siz=NULL,lgd.tit.siz=NULL,strp.txt.siz=NULL,
                     t=NULL,r=NULL,b=NULL,l=NULL, frame=FALSE,...){
  if (is.null(ax.txt.siz)){ax.txt.siz <-9}
  if (is.null(ax.tit.siz)){ax.tit.siz <- 9.2}
  if (is.null(lgd.txt.siz)){lgd.txt.siz <- 8}
  if (is.null(lgd.tit.siz)){lgd.tit.siz <- 8}
  if (is.null(strp.txt.siz)){strp.txt.siz <- 12}
  if(is.null(t)){t<-15}
  if(is.null(r)){r<-1}
  if(is.null(b)){b<-2}
  if(is.null(l)){l<-1}

  ggplot2::theme_test()+
    ggplot2::theme(

      # axis text
      axis.text.x = element_text(size=ax.txt.siz,vjust=-1), # tick label size
      axis.text.y = element_text(size=ax.txt.siz,margin = margin(r=4)), # tick label size
      # axis title
      axis.title   =element_text(size=ax.tit.siz,face="bold"),# axis title size
      axis.title.y = element_text(margin = margin(r=15),vjust=-1),# spacing of y and tick
      axis.title.x = element_text(margin = margin(b=3),vjust=-4), # spacing of x and axis
      # axis tick
      axis.ticks   =element_line(size = .6,color="black"),# tick thickness
      axis.ticks.length=unit(.2, "cm"),# tick length
      # facet
      strip.text.x   = element_text(size=strp.txt.siz,face='bold'),
      strip.text.y   = element_text(size=strp.txt.siz,face='bold'),
      strip.background = element_blank(),
      # frame
      panel.border = element_rect(colour = ifelse(is.null(frame),NA,"black"),
                                  fill=NA, size=.6),
      axis.line = element_line(colour = "black",size=.6),
      # axis.line    = element_line(size = 1, linetype = "solid"),# axis line
      # axis.line.x.top = element_line(size = 1, linetype = "solid"),# axis line
      # axis.line.y.right = element_line(size = 1, linetype = "solid"),# axis line
      # legend
      legend.title = element_text(size = lgd.tit.siz,face='bold'),# legend size
      legend.text  = element_text(size =lgd.txt.siz,face='bold'),# legend text
      # legend.justification=c(1,0), legend.position=c(.99,0.1),# legend position lower right
      plot.margin = margin(t = t, r = r, b = b, l = l, unit = "pt"),
      legend.background = element_rect(fill=NA),
      plot.background = element_rect(
        fill = "transparent"
      ), ...

    )
}
