library(data.table)
library(purrr)
d <- data.frame("position" = c("forward", "down", "forward", "up", "down", "forward"),
                "value" = c(5,5,8,3,8,2))


position <- 0
depth <- 0

for (i in 1:nrow(d)) {
  if(d$position[i] == "forward") {
    position <- d$value[i]  + position
  } else if (d$position[i] == "up") {
    depth <- depth - d$value[i]
  } else {
    depth <- depth + d$value[i]
    
  }
}

position * depth


d <- read.table("/home/y.mansiaux/misc/adventofcode/day2/day2_part1.txt")
colnames(d) <-  c("position", "value")

position <- 0
depth <- 0

for (i in 1:nrow(d)) {
  if(d$position[i] == "forward") {
    position <- d$value[i]  + position
  } else if (d$position[i] == "up") {
    depth <- depth - d$value[i]
  } else {
    depth <- depth + d$value[i]
    
  }
}

position * depth

## exo 2

library(purrr)
library(glue)
d <- read.table("/home/y.mansiaux/misc/adventofcode/day2/day2_part2.txt")
colnames(d) <-  c("position", "value")


position <- 0
depth <- 0
aim <- 0

for (i in 1:nrow(d)) {
  if(d$position[i] == "forward") {
    position <- d$value[i]  + position
    depth <- depth + aim * d$value[i]
  } else if (d$position[i] == "up") {
    # depth <- depth - d$value[i]
    aim <- aim -  d$value[i]
    
  } else {
    # depth <- depth + d$value[i]
    aim <- aim +  d$value[i]
    
  }
  print(glue("position {position}
       aim {aim}
       depth {depth}"))

}
position * depth

