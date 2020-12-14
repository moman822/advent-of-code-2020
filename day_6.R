library(data.table)

setwd("~/GitHub/advent-of-code-2020/")

###                                  *
###                                 / \
###                                /   \
###                                /   \
###                               /     \
###                              /       \
### ADVENT OF CODE, DAY 6        /       \
### https://adventofcode.com/2020/day/6


# read and wrangle data

dat <- readLines("Data/day_6_1")
dat <- c(dat, "") # add a break at the end as well




##########################################################
## Puzzle 1: For each group, count the number of questions to which anyone answered "yes". What is the sum of those counts?

group_count <- c()
all_groups <- c()

for(i in 1:length(dat)){
  
  group_count <- c(group_count, dat[i])
  
  if(dat[i]==""){
    
    cnt <- uniqueN(strsplit(paste0(group_count, collapse = ""), "")[[1]])
    all_groups <- c(all_groups, cnt)
    group_count <- c()
    print(i)
    
  }
}; rm(cnt, i)

print(paste0("A: ", sum(all_groups)))
### A: 7,128 



##########################################################
## Puzzle 2: For each group, count the number of questions to which everyone answered "yes". What is the sum of those counts?

group_count <- c()
all_groups <- c()

for(i in 1:length(dat)){
  
  group_count <- c(group_count, dat[i])
  
  if(dat[i]==""){
    
    x1 <- data.table(
      a = strsplit(paste0(group_count, collapse = ""), "")[[1]]
    )[, .N, by=.(a)]
    # x1 <- strsplit(group_count, "")
    
    cnt <- x1[N==(length(group_count)-1), .N]
    all_groups <- c(all_groups, cnt)
    group_count <- c()
    print(i)
    
  }
}

print(paste0("A: ", sum(all_groups)))
### A: 3,640 





# What did I look up?

# - Nothing































































