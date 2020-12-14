library(data.table)

setwd("~/GitHub/advent-of-code-2020/")

###                                  *
###                                 / \
###                                /   \
###                                /   \
###                               /     \
###                              /       \
###                              /       \
### ADVENT OF CODE, DAY 7       /         \
### https://adventofcode.com/2020/day/7


# read and wrangle data

dat <- readLines("data/day_7_1")

# swap "bags" with "bag"
dat <- gsub(" bags"," bag", dat)

##########################################################
## Puzzle 1: How many bag colors can eventually contain at least one shiny gold bag?

# get data into a tabular format
dat2 <- rbindlist(lapply(strsplit(dat, "contain"), function(x){
  data.table(
    parent = trimws(x[1]),
    contain = trimws(strsplit(x[2], ",")[[1]])
  )
}))

dat2[, amount:=as.numeric(tstrsplit(contain, " ")[[1]])]
dat2[, type:=trimws(gsub("[0-9]", "", contain))][, type:=trimws(gsub("\\.", "", type))]
dat2 <- dat2[, -"contain"]

p1 <- dat2[type=="shiny gold bag", unique(parent)]
cmlt <- p1
i <- 1

while(i>=1){
  
  p1 <- dat2[type %in% p1, unique(parent)]
  print(dat2[type %in% p1])
  cmlt <- c(cmlt, p1)
  # print(i)
  i <- length(p1)
  
}; rm(i)

print(paste0("Number of bags: ", uniqueN(cmlt)))
## A: 229

rm(cmlt, p1)

##########################################################
## Puzzle 1: How many individual bags are required inside your single shiny gold bag?

size <- function(bag, n){
  
  s1 <- dat2[parent==bag]
  
  ans <- 1
  
  if(nrow(s1)>0  & s1$type[1]!="no other bags"){
    
    
    ans <- mapply(size, s1$type, s1$amount)
    # print(ans)
    # print(paste0(bag, ": ", sum(s1$amount)))
    # sum(s1$amount, na.rm=T) * n
    
  } else {
    
  }
  
  n * (1+sum(ans, na.rm=T))
  
}

# ans <- 0
size("shiny gold bag", n=1) - 1





# What did I look up?

# - Basically everything for puzzle 2, cribbed heavily from here: https://selbydavid.com/2020/12/06/advent-2020/
#     struggled with this puzzle for a long time with loops and lists, etc. I have an idea of what is going on now, at least.










