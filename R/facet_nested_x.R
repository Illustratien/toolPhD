#' @title wrap your x axis in a beautiful way
#'
#' @description
#' \code{facet_nested_x} extension of facet_grid using package `ggh4x` and make grouping x
#'
#' @keywords format for scientific display
#'
#' @param ggobj a ggplot object, which contains columns to split for x axis
#' @param cols string, containing columns names to be separated, separated by "+" when more than one.
#' @param ... other argument to pass to facet_grid(...)

#' @return a character vector, which turn pvalue into significance for each element
#'
#' @author Tien-Cheng Wang
#'
#' @import ggplot2
#' @import ggh4x
#' @importFrom glue glue
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(cowplot)
#' library(toolPhD)
#'
#' mg <- ggplot(mtcars, aes(x = mpg, y = wt)) +
#'  geom_point()+theme_classic()+
#'  theme(axis.text.x = element_text(angle=90))
#' p1 <- ggplot(mtcars, aes(x = interaction(mpg,vs,am), y = wt)) +
#'   geom_point()+ theme_classic()+
#'   theme(axis.text.x = element_text(angle=90))+ggtitle("interaction of 3 columns")
#' p2 <- mg + facet_grid(.~vs + am , margins = TRUE)+ggtitle("facet 2 columns")
#' p3 <- facet_nested_x(mg,"vs + am")+ggtitle("facet and nested 2 columns")
#' cowplot::plot_grid(p1,p2,p3,ncol=3)
#'
#' facet_nested_x(mg,"vs + am",labeller = labeller( .cols = label_both))


# form<- "~ Treatment + Location"
facet_nested_x<- function(ggobj,cols,...){
  # g is ggplot object
  rows <- "." #"." means not split for rows
  glue_formula <- function(.formula, .envir = parent.frame(), ...){
    # author :moodymudskipper
    # https://stackoverflow.com/questions/21588096/pass-string-to-facet-grid-ggplot2

    formula_chr <- gsub("\\n\\s*","",as.character(.formula)[c(2,1,3)])
    args <- c(as.list(formula_chr), .sep=" ", .envir = .envir)
    as.formula(do.call(glue::glue, args),env = .envir)
  }

  ggobj+
    ggh4x::facet_nested(glue_formula({rows} ~ {cols}),switch = "x",...)+
    theme_classic()+
    theme(axis.text.x = element_text(angle=90),
          strip.placement = "outside",                      # Place facet labels outside x axis labels.
          strip.background = element_blank(),  # Make facet label background white.
          axis.title.x  = element_blank(),
          ggh4x.facet.nestline = element_line(colour = "darkgrey")
    )
}
