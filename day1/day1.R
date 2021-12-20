library(data.table)

d <- readLines("/home/y.mansiaux/misc/adventofcode/day1/day1.txt") %>% as.numeric()

depth <- data.frame(depth = d) %>% setDT()


depth %>% 
  .[,depth_lag := dplyr::lag(depth)] %>% 
  .[, increased := depth_lag < depth] #%>% 
  # print()

sum(depth$increased, na.rm = TRUE)


###

library(slider)
library(purrr)
library(data.table)
d <- readLines("/home/y.mansiaux/misc/adventofcode/day1/day1_part2.txt") %>% as.numeric()

d <- data.frame("depth" = d)

ss <- slide(d$depth, ~.x, .before = 0, .after = 2, .complete = TRUE)
ss <- map(ss, sum) %>% unlist()

depth2 = data.frame("depth" = ss) %>% as.data.table()
depth2 %>% 
  .[,depth_lag := dplyr::lag(depth)] %>% 
  .[, increased := depth_lag < depth] %>% 
 print()

sum(depth2$increased, na.rm = TRUE)
