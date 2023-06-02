
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




# hangartner procedure ----------------------------------------------------
# for categorical data such as the alpha chains

#https://click.endnote.com/viewer?doi=arxiv%3A1706.04919&token=WzM1ODU3OTcsImFyeGl2OjE3MDYuMDQ5MTkiXQ.UdyyZUW7J7tsfLXqkpauZaoFrMA

mcmc_chain <- sample(LETTERS[1:3], size = 1e4, replace = TRUE, prob = c(0.1, 0.5, 0.25)) 

# separate chain into independent segments
segments <- list(mcmc_chain[1:200], mcmc_chain[2000:2200], mcmc_chain[9000:9200]) 

# count frequencies 
segment_frequencies <- lapply(segments, function(seg) table(seg))
segment_frequencies

# apply chisquare test to compare the frequency distributions between segments
hangartner_test <- chisq.test(do.call(rbind, segment_frequencies))
hangartner_test

# The test statistic indicates the discrepancy of the frequency distribution between segments, 
# and the p-value assesses the significance of this discrepancy. 
# A lower p-value suggests a lack of convergence in the MCMC output.


hangartner <- function(mcmc_chain, num_segments, gap) {
  
  # Function to separate the chain into independent segments
  separate_segments <- function(chain, num_segments, gap) {
    chain_length <- length(chain)
    total_space <- (num_segments - 1) * gap
    segment_length <- (chain_length - total_space) %/% num_segments  
    segments <- vector("list", num_segments)
    
    for (i in 1:num_segments) {
      start_index <- (i - 1) * (segment_length + gap) + 1
      end_index <- start_index + segment_length - 1
      segments[[i]] <- chain[start_index:end_index]
      
      # NA to "I"
      segments[[i]][is.na(segments[[i]])] <- "I"
    }
    
    segments
  }
  
  # Separate chain into independent segments
  segments <- separate_segments(mcmc_chain, num_segments, gap)
  
  # Count frequencies
  segment_frequencies <- lapply(segments, function(seg) table(seg))
  
  # Apply chi-square test to compare the frequency distributions between segments
  hangartner_test <- chisq.test(do.call(rbind, segment_frequencies))
  
  # Return the test result
  hangartner_test
}

hangartner(mcmc_chain = mcmc_chain, num_segments = 3, gap = 100)



library(dplyr)
#o2 <- readRDS("Data/o2.RData")

result_list <- lapply(o2, function(df) {
  alpha_columns <- df %>% select(grep("alpha", names(.)))
  
  result <- lapply(alpha_columns, hangartner, num_segments = 2, gap = 0)
  
  return(result)
})

p_values <- lapply(result_list, function(results) {
  lapply(results, function(result) {
    result$p.value
  })
})

mean(unlist(p_values) > 0.05)
