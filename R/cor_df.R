#' @title wrapper for correlation calculation
#'
#' @description
#' \code{cor_df} calculate correlation matrix/stripe for each complete pairs.
#'
#' @keywords correlation pearson spearman sma
#'
#' @param dat a dataframe, include *only* numeric columns for correlation
#' @param tar.column a dataframe, include *only* numeric columns for correlation
#' @param metohd default is NULL
#' @return a list of three elements: 1. `r.mat`r matrix, 2.`p.mat` p-value matrix
#'
#' @author Tien-Cheng Wang
#'
#' @importFrom smatr sma
#' @importFrom usethis use_pipe
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @examples
#' # numeric dataframe
#' cor_df(mtcars[,1:3])
#' cor_df(mtcars,"cyl")
#'
#' # dataframe contains character
#' cor_df(iris)
#' cor_df(iris[,-5])
#' cor_df(iris[,-5], method='sma')
#' cor_df(iris[,-5],'Sepal.Length' ,method='sma')
#'
cor_df <- function(dat,tar.column=NULL,method="pearson"){
  # dat: data without non-numeric column
  # from, tar , r, r2,method, sig, NoP

  # embeded function -------------------------------------------------------------------------
  cor.mtest <- function(mat, ...) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], ...)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }

  # -------------------------------------------------------------------------

  if(!is.null(tar.column)){
    dat <-dplyr::relocate( dat ,tar.column)
  }
  condi <- setdiff(unique(sapply(dat,"class")),"numeric")

  if(!identical(condi,character(0))){
    stop(sprintf("data contains %s columns",paste0(condi,collapse=" & ")))
  } else if (!is.null(tar.column)&length(tar.column)>1){
    stop(sprintf("only one target at a time, instead of %i",length(tar.column)))
  }

  if (!method=="sma"){
    # method <- ifelse(is.null(method),"pearson",method)
    cor_ <- cor(dat, use="complete.obs",method = method)
    p.mat <- cor.mtest(dat)
  } else {
    tmp <- toolPhD::sma_cor(dat)
    cor_ <- tmp[[1]]
    p.mat <- tmp[[2]]
  }
  res <- list(r.value=cor_,p.value=p.mat)

  if(!is.null(tar.column)){
    # if target been assigned, then get only the first row out
    res <-  data.frame(
      from=colnames(cor_)[2:ncol(p.mat)],
      to=rep(tar.column,ncol(p.mat)-1),
      r=cor_[1,2:ncol(p.mat)],
      r2=cor_[1,2:ncol(p.mat)]^2,
      sign=ifelse(cor_[1,2:ncol(p.mat)]>0,'+','-'),
      p=p.mat[1,2:ncol(p.mat)],
      p.sig=toolPhD::sig_pvalue(p.mat[1,2:ncol(p.mat)])
    )
    rownames(res) <- NULL
  }
  message(sprintf("using %s correlation of coefficient",method))
  return(res)
}
