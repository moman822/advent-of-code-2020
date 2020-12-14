library(data.table)

setwd("~/GitHub/advent-of-code-2020/")

###                                  *
###                                 / \
###                                /   \
###                                /   \
###                               /     \
###                              /       \
###                              /       \
###                             /         \
###                            /           \
### ADVENT OF CODE, DAY 9     /             \
### https://adventofcode.com/2020/day/9


# read and wrangle data

dat <- readLines("data/day_9_1")
dat <- as.numeric(dat)

##########################################################
## Puzzle 1: What is the first number that does not have this property [it is not the sum of 2 of the previous 25 numbers]?

pos <- 26
ans <- TRUE

while(ans==TRUE){
  pos <- pos + 1
  sub_d <- dat[(pos-25):(pos-1)]
  
  ans <- dat[pos] %in% unlist(lapply(sub_d, function(x){
    
    x + sub_d[sub_d!=x]
    
  }))
  if(!ans){
    print(paste0("A: ", dat[pos]))
  }
  
}

## A: 1,930,745,883

rm(sub_d, pos)

##########################################################
## Puzzle 2: What is the encryption weakness in your XMAS-encrypted list of numbers [the smallest and largest numbers in an unspecificed range that all sum to part one's answer]?

ans <- 1930745883


dat
x <- 0
p1 <- 1

while(x!=ans){
  
  x <- sum(dat[p1:(p1+i)])
  
  if(x<ans | p1==1000){
    
    i <- i + 1
    
  } else if(x>ans){
    
    p1 <- p1 + 1
    i <- 1
    
  } else {
    
    print(p1)
    print(p1+i)
    print(paste0("A: ", min(dat[p1:(p1+i)]) + max(dat[p1:(p1+i)])))
    
  }
  
}

## A: 268,878,261



# What did I look up?

# Nothing






































































