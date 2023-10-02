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
#' @importFrom usethis use_pipe
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @examples
#' vec <- c(100.1,10.1,1.1,0.1,0.01,0.001,0.0001,0.00001)
#' round_scale(vec)
round_scale <-function(vec){
  # choose approriate scale for each element in the vector
  # useful in table dsiplay
  # return the appropriate formatted digit vector
  as.character(
    purrr::map_dbl(vec,~{

      if(is.na(.x)|abs(.x)<0.0001){
        .x
      }else if(abs(.x)<0.001){round(.x,4)
      }else if(abs(.x)>=0.001&abs(.x)<0.01){round(.x,3)
      }else if(abs(.x)>=0.01&abs(.x)<1){round(.x,2)
      }else if(abs(.x)>=1){
        round(.x,1)
      }
    }))
}

