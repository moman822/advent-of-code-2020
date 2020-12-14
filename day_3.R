library(data.table)

setwd("~/GitHub/advent-of-code-2020/")

###                                 *
###                                / \
###                               /   \
### ADVENT OF CODE, DAY 3         /   \    
### https://adventofcode.com/2020/day/3

# a roundabout way of getting this data into a matrix, surely...
text <- readLines(con = "data/day_3_1")
cols <- nchar(text)
chars <- unlist(strsplit(text, ""))
x <- matrix(chars, ncol=cols, byrow = T)

# paste a bunch together
x2 <- cbind(x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x)



##########################################################
## Puzzle 1: start by counting all the trees (i.e. pound signs) you would encounter for the slope right 3, down 1:

right <- 3
down <- 1

start_row <- 1
start_col <- 1

i <- 1
n_trees <- 0

while(i<=nrow(x2)){
  
  print(i)
  
  val <- x2[start_row, start_col]
  
  if(val=="#"){
    n_trees <- n_trees + 1
  }
  
  start_row <- start_row + down
  start_col <- start_col + right
  i <- i + down
  print(i)
  
  
}

print(paste0("Number of trees hit: ", n_trees))
# A: 145

rm(right, down, start_row, start_col, i, n_trees)

##########################################################
## Puzzle 2: multiply the number of trees hit on each of the following slopes:
## Right 1, down 1.
## Right 3, down 1. (This is the slope you already checked.)
## Right 5, down 1.
## Right 7, down 1.
## Right 1, down 2.


moves <- data.table(
  right = c(1,3,5,7,1),
  down =  c(1,1,1,1,2)
)

# How many columns do I need?
# 

max(moves$right) * nrow(x)

ncol(x2)
x3 <- cbind(x2, x2)

n_trees_overall <- list()

for(j in 1:nrow(moves)){
  
  down <- moves[j]$down
  right <- moves[j]$right
  
  start_row <- 1
  start_col <- 1
  
  i <- 1
  n_trees <- 0
  
  while(i<=nrow(x3)){
    
    val <- x3[start_row, start_col]
    
    if(val=="#"){
      n_trees <- n_trees + 1
    }
    
    start_row <- start_row + down
    start_col <- start_col + right
    i <- i + down
    print(start_col)
    print(i)
    
  }
  n_trees_overall[[j]] <- n_trees
  print(j)
}

unlist(n_trees_overall)

prod(unlist(n_trees_overall))

# A: 3,424,528,800


# What did I look up?

# - `prod()` function to multiply a vector together
# - reading in text data (`readLines()`)
# - format for while loops














