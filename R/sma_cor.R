#' @title sma correlation matrix
#'
#' @description
#' \code{sma_cor} calculate sma correlation matrix for each complete pairs.
#'
#' @keywords standardised major axis
#'
#' @param mat a dataframe, include numeric columns for correlation

#' @return a list of three elements: 1. `r.mat`r matrix, 2.`p.mat` p-value matrix and 3 `s.mat` slope matrix.
#'
#' @author Tien-Cheng Wang
#'
#' @importFrom smatr sma
#' @importFrom usethis use_pipe
#' @importFrom Rdpack reprompt
#'
#' @export
# @references
#\insertRef{warton2012}{toolPhD}
#'
#'
#'
#' @examples
#' #Define example dataset
#' sma_cor(mtcars[,1:7])

sma_cor<- function(mat){
  # sma will throw error if there is an column containing identical values
  nonidentical_col <-    which(apply(na.omit(mat),MARGIN = 2,FUN = function(x){length(unique(x))>1}))
  # first screen it.
  mat <- mat[,nonidentical_col]
  nam_mat<- names(mat)
  n <- ncol(mat)
  names(mat) <- paste0("V",1:n)

  p.mat<- matrix(NA, n, n)
  r.mat<- matrix(NA, n, n)
  s.mat <- matrix(NA, n, n)

  diag(p.mat) <- 0
  diag(r.mat) <- 1
  diag(s.mat) <- 1
  amat <- as.matrix(mat)

  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {

      tmp <- smatr::sma(paste0(names(mat)[j],"~",names(mat)[i]), method = "SMA",
                        data = mat[complete.cases(amat[,i],amat[,j]),])$groupsummary

      p.mat[i, j] <- p.mat[j, i] <- tmp$pval
      r.mat[i, j] <- r.mat[j, i] <- sqrt(tmp$r2)*ifelse(tmp$Slope>0,1,-1)
      s.mat[i, j] <- s.mat[j, i] <- tmp$Slope

    }
  }


  colnames(p.mat) <- rownames(p.mat) <- nam_mat
  colnames(r.mat) <- rownames(r.mat) <- nam_mat
  colnames(s.mat) <- rownames(s.mat) <- nam_mat
  return(list(r.mat,p.mat,s.mat))

}
