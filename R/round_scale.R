#' @title tidy digit of numbers
#'
#' @description
#' \code{round_scale} display the tidy unit of your observations
#'
#' @keywords format for scientific display
#'
#' @param vec a vector, which may include different scale of numeric number.

#' @return a vector, which adjust format unit for each element
#'
#' @author Tien-Cheng Wang
#'
#' @importFrom purrr map_dbl
#' @importFrom dplyr %>%
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @examples
round_scale <-function(vec){
  # choose approriate scale for each element in the vector
  # useful in table dsiplay
  # return the appropriate formatted digit vector
  purrr::map_dbl(vec,~{
    if(is.na(.x)){
      .x
    } else if (abs(.x)>5){
      round(.x,1)
    } else if (abs(.x)<.01) {
      round(.x,3)
    } else{
      round(.x,2)
    }
  })
}

