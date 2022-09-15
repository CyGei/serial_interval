#produce LHS params
get_lhs <- function(n, MUs, SDs){
  
intervals <- seq(0,1, 1/n) #threshold values


#generate random points sampled from equi-probable intervals:
intervals_df <- 
  data.frame(intervals) %>% 
  mutate(lwr = intervals, upr = lead(intervals)) %>% 
  select(-intervals) %>% 
  slice(-n()) %>% 
  rowwise() %>% 
  mutate( point = runif(1, lwr, upr)) %>%  
  ungroup() 

#use qtruncnorm to get the sample mu, 
# we assume a minimun GT and incub of 1 day:
q_mu <- truncnorm::qtruncnorm(intervals_df$point, 
                              a = 1,
                              mean = mean(MUs),
                              sd = sd(MUs))  
  
#use qnorm to get the sample cv, 
CVs <- SDs / MUs

q_cv <- qnorm(intervals_df$point, 
                mean = mean(CVs),
                sd = sd(CVs))  

#reshuffle the samples:
mu_random <- sample(q_mu, size = length(q_mu)) 
cv_random <- sample(q_cv, size = length(q_cv))

#convert to gamma params:
params <- epitrix::gamma_mucv2shapescale(mu_random,cv_random)
return(params)
  
}


# replace tailing 0s with minimal exponential values
tails_pmf <- function(dens){
  
  is_positive <- is.finite(log(dens)) # finds positive values
  
  first_positive <- min(which(is_positive==TRUE)) # fist positive index
  
  last_positive <- max(which(is_positive==TRUE)) # last positive index
  
  min_val <- min(dens[is_positive]) #finds minimum value
  
  
  
  ## starting 0s 
  if( is_positive[1] == FALSE){ #if the 1st value is 0
    
    starting_indices <- seq_len(first_positive - 1) # finds starting 0s indices 
    
    val_to_replace <- rev(stats::dexp(starting_indices, 1)) # reverse exponential densities
    
    val_to_replace <- (min_val*1e-4)*(val_to_replace/sum(val_to_replace)) # sum to a value smaller than any other value in dens
    
    dens <- replace(dens, starting_indices, val_to_replace ) # replace satrting 0s with exp densities
    
    warning("starting 0s were replaced with small exponential values")
    
  }
  ## ending 0s
  if( is_positive[length(is_positive)] == FALSE){ #if the last index returns 0
    
    ending_indices <- (last_positive+1) : length(dens) # finds ending 0s indices 
    
    val_to_replace <- stats::dexp(1:length(ending_indices), 1) # reverse exponential densities
    
    val_to_replace <- (min_val*1e-4)*(val_to_replace/sum(val_to_replace)) # sum to a value smaller than any other value in dens
    
    dens <- replace(dens, ending_indices, val_to_replace ) # replace satrting 0s with exp densities
    
    warning("ending 0s were replaced with small exponential values")
    
  }
  
  dens <- dens/sum(dens) #rescale dens to sum to 1
  
  return(dens)
}


sanitize_pmf <- function(dens){
  
  is_positive <- is.finite(log(dens)) #finds positive values
  
  to_replace <- !is_positive #finds non positive values
  
  min_val <- min(dens[is_positive]) #finds the minimum probability
  
  val_replace <- 1e-4*min_val #generate a smaller probability than the min_val
  
  dens[to_replace] <- val_replace  #replaces non positive values with a tiny probability 
  
  dens <- dens / sum(dens) # rescale density to one
  
  return(dens)
}


