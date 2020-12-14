library(data.table)

setwd("~/GitHub/advent-of-code-2020/")

###                                 *
###                                / \
### ADVENT OF CODE, DAY 2         /   \
### https://adventofcode.com/2020/day/2


## Data input: downloaded from browser as text file
x <- read.table("data/day_2_1")

# Overview:
# Each line gives the password policy and then the password. The password policy indicates the lowest
# and highest number of times a given letter must appear for the password to be valid. For example,
# "1-3 a" means that the password must contain a at least 1 time and at most 3 times.


## Clean the data up
names(x) <- c("num","key","pw")
setDT(x)
x[, key:=gsub(":","", key)]   # remove the colon


##########################################################
## Puzzle 1: How many passwords are valid according to their policies?

# split the policy range into high and low ends
x[, low:=as.numeric(tstrsplit(num, "-")[[1]])][, high:=as.numeric(tstrsplit(num, "-")[[2]])]
x

count <- 0

for(i in 1:nrow(x)){
  
  sp <- strsplit(x[i]$pw, "")[[1]]
  
  matches <- length(which(sp==x[i]$key))
  
  if(matches>=x[i]$low & matches<=x[i]$high){
    
    count <- count+1
    
  }
}

print(paste0("Puzzle 1, valid passwords: ", count))

##########################################################
## Puzzle 2: How many passwords are valid according to the new interpretation of the policies?

# New rules:
# Each policy actually describes two positions in the password, where 1 means the first character, 
# 2 means the second character, and so on. (Be careful; Toboggan Corporate Policies have no concept
# of "index zero"!) Exactly one of these positions must contain the given letter. Other occurrences
# of the letter are irrelevant for the purposes of policy enforcement.

count2 <- 0

for(i in 1:nrow(x)){
  
  sp <- strsplit(x[i]$pw, "")[[1]]
  
  
  sm <- sum(sp[x[i]$low] == x[i]$key, sp[x[i]$high] == x[i]$key)
  
  if(sm==1){
    
    count2 <- count2 + 1
    
  }
}

print(paste0("Puzzle 2, valid passwords: ", count2))
































