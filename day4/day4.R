library(stringr)
library(purrr)
library(readr)

day4_sample <- readLines("day4/sample_q1.txt")

split <- str_split(day4_sample, pattern = "\n")

split <- compact(map(split, .f = function(.x) if (.x != "") .x))

bingo_grids <- map(1:(length(split) / 5), .f = function(.x) {
  
  index_min <- 5 * (.x-1) + 1
  index_max <- 5 * .x
  
  a <- str_c(split[index_min:index_max]) %>% str_split(., pattern = " ") %>% unlist()
  a <- a[a!= ""]
  matrix(as.numeric(a), nrow = 5, byrow = TRUE)
  
})

results_grid <- map(1:length(bingo_grids), function(.x) {
  matrix(0, nrow = 5, ncol = 5)
})

tirage <- c(7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1)
winner <- rep(0, length(bingo_grids))
# for (i in 1:(floor(length(tirage) / 5)+1)) {
for (i in 1:(length(tirage))) {
  # browser()
  numero <- tirage[i]
  if(sum(winner)>0) {
    final_number <- tirage[i-1]
    break
  }
  
  print(numero)
  for (j in 1:length(bingo_grids)) {
    
    if(numero %in% bingo_grids[[j]]) {
      results_grid[[j]][which(bingo_grids[[j]] == numero)] <- 1
    }
    
    haswinner <- any(apply(results_grid[[j]], 1, mean) == 1)| any(apply(results_grid[[j]], 2, mean) == 1) 
    
    if(haswinner) {
      winner[j] <- 1
      break
    }
  }
}

winning_grid1 <- results_grid[[which(winner == 1) ]]
winning_grid2 <- bingo_grids[[which(winner == 1) ]]
  
sum(winning_grid2[which(winning_grid1 == 0)]) * final_number

  
  # index_min <- 5 * (i-1) + 1
  # index_max <- pmin(5 * i, length(tirage))
  # 
  # sub_tirage <- tirage[index_min:index_max]
  # 
  # for (j in 1:length(sub_tirage)) {
  #   
  #   numero <- sub_tirage[j]
  #   
  # }
  
  
####
library(stringr)
library(purrr)
library(readr)

day4_sample <- readLines("day4/q1.txt")

split <- str_split(day4_sample, pattern = "\n")

split <- compact(map(split, .f = function(.x) if (.x != "") .x))

bingo_grids <- map(1:(length(split) / 5), .f = function(.x) {
  
  index_min <- 5 * (.x-1) + 1
  index_max <- 5 * .x
  
  a <- str_c(split[index_min:index_max]) %>% str_split(., pattern = " ") %>% unlist()
  a <- a[a!= ""]
  matrix(as.numeric(a), nrow = 5, byrow = TRUE)
  
})

results_grid <- map(1:length(bingo_grids), function(.x) {
  matrix(0, nrow = 5, ncol = 5)
})

tirage <- c(27,14,70,7,85,66,65,57,68,23,33,78,4,84,25,18,43,71,76,61,34,82,93,74,26,15,83,64,2,35,19,97,32,47,6,51,99,20,77,75,56,73,80,86,55,36,13,95,52,63,79,72,9,10,16,8,69,11,50,54,81,22,45,1,12,88,44,17,62,0,96,94,31,90,39,92,37,40,5,98,24,38,46,21,30,49,41,87,91,60,48,29,59,89,3,42,58,53,67,28)
winner <- rep(0, length(bingo_grids))
# for (i in 1:(floor(length(tirage) / 5)+1)) {
for (i in 1:(length(tirage))) {
  # browser()
  numero <- tirage[i]
  if(sum(winner)>0) {
    final_number <- tirage[i-1]
    break
  }
  
  print(numero)
  for (j in 1:length(bingo_grids)) {
    
    if(numero %in% bingo_grids[[j]]) {
      results_grid[[j]][which(bingo_grids[[j]] == numero)] <- 1
    }
    
    haswinner <- any(apply(results_grid[[j]], 1, mean) == 1)| any(apply(results_grid[[j]], 2, mean) == 1) 
    
    if(haswinner) {
      winner[j] <- 1
      break
    }
  }
}

winning_grid1 <- results_grid[[which(winner == 1) ]]
winning_grid2 <- bingo_grids[[which(winner == 1) ]]

sum(winning_grid2[which(winning_grid1 == 0)]) * final_number





