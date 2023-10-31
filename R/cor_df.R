#' @title wrapper for correlation calculation
#'
#' @description
#' \code{cor_df} calculate correlation matrix/stripe for each complete pairs.
#'
#' @keywords correlation matrix pearson spearman sma
#'
#' @param dat a dataframe, include *only* numeric columns for correlation
#' @param tar.column default is `NULL`, when set to *one string of a column name*, then extract the output only related to this column.
#' @param metohd default is 'pearson', 'spearman' and 'sma' is also available
#' @param nice.format default is `FALSE`, when `TRUE`, then arrange with `r2` and subset for the significance one.
#'
#' @return a list of three elements: 1. `r.mat`r matrix, 2.`p.mat` p-value matrix
#' or a dataframe containing `r`,`r2`,`sign`,`p`,`p.value`,`p.sig`.
#'
#' @author Tien-Cheng Wang
#'
#' @importFrom smatr sma
#' @importFrom usethis use_pipe
#' @importFrom Rdpack reprompt
#' @importFrom dplyr filter mutate across arrange
#'
#' @export
#'
#' @examples
#' # numeric dataframe
#' cor_df(mtcars[,1:3])
#' cor_df(mtcars,"cyl")
#' cor_df(mtcars,"cyl",nice.format=T)
#' cor_df(mtcars,"cyl",nice.format=T,method='sma')
#'
#' # dataframe contains character
#' cor_df(iris)
#' cor_df(iris[,-5])
#'
cor_df <- function(dat,tar.column=NULL,method="pearson",nice.format=FALSE){
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

  if (method%in%c("pearson","spearman")){
    # method <- ifelse(is.null(method),"pearson",method)
    cor_ <- cor(dat,
                use = ifelse(method=="pearson","pairwise.complete.obs","complete.obs"),
                method = method)
    p.mat <- cor.mtest(dat)
  } else if (method=="sma") {
    #sma
    tmp <- toolPhD::sma_cor(dat)
    cor_ <- tmp[[1]]
    p.mat <- tmp[[2]]
  } else{
    stop(sprintf("unknown method %s",method))
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

    if(nice.format){
      res <- dplyr::filter(dplyr::mutate(dplyr::arrange(res,dplyr::desc(r2)),
                                         across(c(r:r2,p),round_scale)),
                           !p.sig=='')
    }
  }


  message(sprintf("using %s correlation of coefficient",method))
  return(res)
}
