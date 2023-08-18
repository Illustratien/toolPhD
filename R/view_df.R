view_df <- function(x){
  # have the range/elements of each column
  x <- as.data.frame(x)
  purrr::map_dfr(1:ncol(x),~{
    ue <- unique(x[,.x])
    if(length(ue)<10){
      content <- paste(sort(ue),collapse=",")
    }else{
      if(all(is.na(ue))){
        content <- "NA"
      }else if(is.numeric(na.omit(ue))|all(grepl("^[0-9]+$", na.omit(ue)))){
        ue <- round_scale(ue)
        if (any(is.na(ue))){
          content <- paste(range(as.numeric(na.omit(ue))),collapse="~") %>%
            paste0(.," include NA")
        }else{
          content <- paste(range(as.numeric(ue)),collapse="~")
        }

      }else{
        content <- paste0("Levels number:",length(ue))      }
    }
    data.frame(colnam=names(x) %>% .[.x],
               info=content)
  })
}

