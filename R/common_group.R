#' @title overview of data frame with group
#'
#' @description
#' \code{common_group} find the common elements of selected groups
#'
#' @keywords dataframe overview in groups
#'
#' @param dat a dataframe, preferably the wide format
#' @param group_vec grouping vector, contain column names to be examine
#' @param ref_vec reference vector, contain column names of target to be check
#' @return a data table with range/elements of each column
#'
#' @author Tien-Cheng Wang
#'
#' @importFrom purrr map
#' @importFrom dplyr inner_join group_by across select all_of
#' @importFrom usethis use_pipe
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @examples
#' common_group(mtcars,"cyl","gear")
#' common_group(iris,"Species","Sepal.Length")
common_group<- function(dat,group_vec,ref_vec){
message(sprintf("Unique %s of %i groups",ref_vec,nrow(dplyr::distinct(dat[group_vec]))))
  # function to join at list level
  if(length(ref_vec)>1){
    Reduce(function(x,y) dplyr::inner_join(x,y,by=ref_vec),
           # generate list based on group
           purrr::map(dplyr::group_split(
             dplyr::group_by(dplyr::select(dat,all_of(c(group_vec,ref_vec))),
                             across(all_of(group_vec)))),
             # extract the target columns
             ~{.[,ref_vec]})
    )
  }else{
    Reduce("intersect",
           purrr::map(dplyr::group_split(
             dplyr::group_by(dplyr::select(dat,all_of(c(group_vec,ref_vec))),
                             across(all_of(group_vec))))
             ,~{.[[ref_vec]]})
    )
  }


}

# slower version
# common_group<- function(dat,g_vec,ref_vec,tar){
#
#   dplyr::select(dat,all_of(c(g_vec,ref_vec,tar))) %>%
#     tidyr::pivot_wider(names_from = g_vec,
#                        values_from = tar) %>%
#     na.omit() %>%
#     dplyr::select(all_of(ref_vec))
# }
