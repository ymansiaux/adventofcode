# q1 sample

library(readr)
library(dplyr)
sample <- read_table("day3/sample_q1.txt", col_names = FALSE)

sample_transf <- do.call(rbind,(strsplit(sample$X1, split = "")))

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getantimode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.min(tabulate(match(v, uniqv)))]
}


glue_collapse(apply(sample_transf, 2, getmode) ) %>% strtoi( base = 2) * glue_collapse(apply(sample_transf, 2, getantimode) ) %>% strtoi( base = 2)

## q1

library(readr)
library(dplyr)
library(glue)
fileq1<- read_table("day3/q1.txt", col_names = FALSE)

sample_transf <- do.call(rbind,(strsplit(fileq1$X1, split = "")))

getmode <- function(v) {
  uniqv <- sort(unique(v), decreasing = TRUE)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getantimode <- function(v) {
  uniqv <- sort(unique(v))
  uniqv[which.min(tabulate(match(v, uniqv)))]
}


glue_collapse(apply(sample_transf, 2, getmode) ) %>% strtoi( base = 2) * glue_collapse(apply(sample_transf, 2, getantimode) ) %>% strtoi( base = 2)


## q2 sample

Start with all 12 numbers and consider only the first bit of each number. There are more 1 bits (7) than 0 bits (5), so keep only the 7 numbers with a 1 in the first position: 11110, 10110, 10111, 10101, 11100, 10000, and 11001.
Then, consider the second bit of the 7 remaining numbers: there are more 0 bits (4) than 1 bits (3), so keep only the 4 numbers with a 0 in the second position: 10110, 10111, 10101, and 10000.
In the third position, three of the four numbers have a 1, so keep those three: 10110, 10111, and 10101.
In the fourth position, two of the three numbers have a 1, so keep those two: 10110 and 10111.
In the fifth position, there are an equal number of 0 bits and 1 bits (one each). So, to find the oxygen generator rating, keep the number with a 1 in that position: 10111.
As there is only one number left, stop; the oxygen generator rating is 10111, or 23 in decimal.


library(readr)
library(dplyr)
sample <- read_table("day3/q2.txt", col_names = FALSE)

sample_transf <- do.call(rbind,(strsplit(sample$X1, split = "")))


data_analysee <- sample_transf

# value 1
for (i in 1:ncol(sample_transf)) {
  most_freq <- getmode(data_analysee[, i])
  
  data_analysee <- data_analysee[which(data_analysee[,i] == most_freq), ]

    if(is.null(nrow(data_analysee)))  {
    value_to_keep1 <- data_analysee %>% glue_collapse() %>% strtoi( base = 2)
    break
    
  }  else {
    value_to_keep1 <- data_analysee[1,] %>% glue_collapse() %>% strtoi( base = 2)
  }

}

# value 2
data_analysee <- sample_transf

for (i in 1:ncol(sample_transf)) {
  most_freq <- getantimode(data_analysee[, i])
  
  data_analysee <- data_analysee[which(data_analysee[,i] == most_freq), ]
  
  if(is.null(nrow(data_analysee)))  {
    value_to_keep2 <- data_analysee %>% glue_collapse() %>% strtoi( base = 2)
    break
    
  }  else {
    value_to_keep2 <- data_analysee[1,] %>% glue_collapse() %>% strtoi( base = 2)
  }
  
}

value_to_keep2 * value_to_keep1
