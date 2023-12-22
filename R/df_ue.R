#' @title Unique Elements
#'
#' @description
#' \code{df_ue} find unique elements plus matching services
#'
#' @keywords unique pattern match
#'
#' @param df a dataframe, include columns xvar and yvar
#' @param coln unquote column name, if `coln`="cnames", then return colnames
#' @param pattern `NULL`, else match pattern in unique coln values
#'
#' @return unique values of a column
#'
#' @author Tien-Cheng Wang
#'
#' @import rlang
#' @import dplyr
#' @export
#'
#' @examples
#'   library(dplyr)
#'   df_ue(iris,Species)
#'   df_ue(iris,Species,"virg")
# function ----------------------------------------------------------------

df_ue <- function(df,coln,pattern=NULL){
  coln <- enquos(coln)
  if(!coln=="cnames"){
    res <- df %>% dplyr::select(!!!coln) %>%
      unlist() %>% as.vector() %>% unique()
  }else{
    res <- names(df)
  }
  res <- df %>% dplyr::select(!!!coln) %>%
    unlist() %>% as.vector() %>% unique()

  if(!is.null(pattern)){
    res <- res%>% .[grepl(pattern,.)]
  }

  return(res)
}
