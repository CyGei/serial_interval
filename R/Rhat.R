
split_chain <- function(vector, num_parts) {
  #replace NAs
  vector[is.na(vector)] <- -1
  max_length <- length(vector) %/% num_parts
  split_vector <- split(vector,
                        cut(seq_along(vector), breaks = num_parts, labels = FALSE))
  split_vector <-
    lapply(split_vector, function(x)
      head(x, max_length))
  return(split_vector)
}

getRhat <- function(vec) {
  split_chain(vec, 4) %>% as.data.frame() %>% as.matrix() %>% 
    rstan::Rhat()
} 

#run misc.R
#o2 -> this dataset cannot be shared on github please contact the authors

all_Rhat <- lapply(o2, 
                   function(e)
                     tapply(
                       e[["post"]], 
                       e[["LHS"]], 
                       getRhat
                     )
)


all_Rhat <- lapply(all_Rhat, function(vec) {
  vec[is.na(vec)] <- 100
  vec
})

treshold <- sapply(all_Rhat, function(e) mean(e < 1.05))
round(mean(treshold), digits = 2)


