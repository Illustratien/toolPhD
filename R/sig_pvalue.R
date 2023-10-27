#' @title tidy digit of numbers
#'
#' @description
#' \code{sig_pvalue} display the tidy unit of your observations
#'
#' @keywords format for scientific display
#'
#' @param vec a vector, which may include different scale of numeric number.

#' @return a character vector, which turn pvalue into significance for each element
#'
#' @author Tien-Cheng Wang
#'
#' @importFrom purrr map_chr
#' @importFrom usethis use_pipe
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @examples
#' vec <- c(100.1,10.1,1.1,0.1,0.01,0.001,0.0001,0.00001)
#' signif(vec)

sig_pvalue<-function(vec){

  if(!is.numeric(vec)){
    stop(sprintf('input must be numeric, instead of %s',class(vec)))
  }
  # significance transformer for display
  purrr::map_chr(vec,function(x){
    if(is.na(x)) {y=NA
    }else if(x<0.001){y="***"
    }else if(x>=0.001&x<0.01){y="**"
    }else if(x>=0.01&x<0.05){y="*"
    }else if(x>=1){y="error"
    }else{y=""}
    # if(x>=0.05&x<0.1){y="."}
    # if(x>=0.1&x<1){y="ns"}
    return(y)
  })
}

