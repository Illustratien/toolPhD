#' @title overview of data frame
#'
#' @description
#' \code{view_df} display the range/elements of each column
#'
#' @keywords dataframe overview
#'
#' @param x a dataframe, preferably the wide format

#' @return a data table with range/elements of each column
#'
#' @author Tien-Cheng Wang
#'
#' @importFrom purrr map_dfr
#' @importFrom usethis use_pipe
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @examples
#' view_df(mtcars)
view_df <- function(x){
  # have the range/elements of each column
  x <- as.data.frame(x)
  purrr::map_dfr(1:ncol(x),~{
    ue <- unique(x[,.x])
    if(length(ue)<5){
      if(is.numeric(ue)){
        ue <- round_scale(sort(ue))
      }
      content <- paste(sort(ue),collapse=",")

    }else{
      if(all(is.na(ue))){
        content <- "NA"
      }else if(is.numeric(na.omit(ue))|all(grepl("^[0-9]+$", na.omit(ue)))){
        ue <- round_scale(ue)
        if (any(grepl("NA",ue))){
          content <- paste0(paste(range(na.omit(suppressWarnings(as.numeric(ue)))),collapse="~")," include NA")
        }else{
          content <- paste(range(as.numeric(ue)),collapse="~")
        }

      }else{
        content <- paste0("Levels number:",length(ue))      }
    }
    data.frame(colnam=names(x)[.x],
               info=content)
  })
}

