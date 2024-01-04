#' @title overview of data frame with group
#'
#' @description
#' \code{view_group} display the range/elements of each column for selected group
#'
#' @keywords dataframe overview in groups
#'
#' @param dat a dataframe, preferably the wide format
#' @param g_vec grouping vector, contain column names for grouping
#' @param t_vec target vector, contain column names for examining the range and elements.
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
#' view_group(iris,c("Species"),c("Sepal.Length","Petal.Length"))
#' view_group(iris,c("Species"))
view_group<- function(dat,g_vec,t_vec=NULL){
  if(is.null(t_vec)){
    # if no specific target vector is given, then assume all columns are important
    t_vec <- setdiff(names(dat),g_vec)
  }

  res <- Reduce(function(x,y) dplyr::left_join(x,y,by="colnam"),
                purrr::map(
                  dplyr::group_split(
                    dplyr::group_by(dplyr::select(dat,all_of(c(g_vec,t_vec))),
                                    across(all_of(g_vec)))),
                  ~{
                    toolPhD::view_df(.x)
                  })

  )
  res <- data.frame(t(res))
  names(res) <- res[1,]
  res <- res[-1,]
  row.names(res) <- NULL
  return(res)
}


