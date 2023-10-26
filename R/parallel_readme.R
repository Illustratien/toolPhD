#' @title readme script for parallel computing
#'
#' @description
#' \code{parallel_readme} provide framework for parallel
#'
#' @keywords parallel
#'
#' @return scripts for initialization
#'
#' @author Tien-Cheng Wang
#'
#'
#' @export
#'
#' @examples
#' parallel_readme()
#'
parallel_readme<- function(){
  message('
n.cores <- parallel::detectCores() - 1
#create the cluster
my.cluster <- parallel::makeCluster(
  n.cores,
  type = "PSOCK"
)
doParallel::registerDoParallel(cl = my.cluster)

system.time(
  res <- foreach(
    i  = 1:length(data_list),
    j = data_list,
    .packages = "dplyr"
  ) %dopar% {

   load(your_custom_script.R)
   # or define it here
    function(i,j)

  }
)

doParallel::stopImplicitCluster()
          ')


}
