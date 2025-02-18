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
  x <- as.data.frame(x)
  purrr::map_dfr(1:ncol(x), ~{
    ue <- unique(x[, .x])

    if (all(is.na(ue))) {# all NA
      content <- "NA"
    } else if (length(na.omit(ue))<length(ue)){# some NA
      suffix <- " include NA"
      if(is.numeric(na.omit(ue)) | all(grepl("^[0-9]+$",na.omit(ue)))) {
        # for those number only numeric or character vector
        ue <- as.numeric(round_scale(as.numeric(na.omit(ue))))
      }else{# for character vector
        ue <- as.character(na.omit(ue))
      }
    }else{ # completely no NA
      if(is.numeric(ue) | all(grepl("^[0-9]+$",ue))) {
        ue <- as.numeric(round_scale(as.numeric(ue)))
      }
      suffix <- ""
    }

    if (all(is.na(ue))){
    }else if (length(ue) < 5) { # list all element if length smaller than 5
      content <- paste(c(sort(ue),suffix), collapse = ",")
    }else  { # otherwise list the range or number of levels for factors
      if (is.numeric(ue)) {
        content <- paste0(paste(range(ue),
                                collapse = "~"),suffix)
      }else{
        content <- paste0("Levels number:", length(ue),suffix)
      }
    }
    data.frame(colnam = names(x)[.x], info = content)
  })
}
