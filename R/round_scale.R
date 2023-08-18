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

