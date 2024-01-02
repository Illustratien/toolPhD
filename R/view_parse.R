#' @title view parse text
#'
#' @description
#' \code{view_parse} view the parse text immediately
#'
#' @keywords parse
#'
#' @param parse.vec character vector of parse text

#' @return view the parse text immediately
#'
#' @author Tien-Cheng Wang
#'
#' @import ggplot2
#' @import pacman
#' @export
#'
#' @examples
#'   pacman::p_load(ggplot2,toolPhD)
#'   view_parse( c('P[i,DM["straw, 87"]]'))
#'   view_parse( c('P[i,DM["straw, 87"]]','bolditalic(P)["i, DM"["straw, 87"]]'))


# function ----------------------------------------------------------------
view_parse <- function(parse.vec){
  ggplot()+
    geom_text(aes(1:length(parse.vec),rep(1,length(parse.vec))),
              label=parse.vec,parse=T,size=6)+
    scale_x_continuous(limits = c(0,length(parse.vec)+1))+
    theme_void()
}
