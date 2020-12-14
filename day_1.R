library(data.table)

setwd("~/GitHub/advent-of-code-2020/")

###                                 *
### ADVENT OF CODE, DAY 1          / \
### https://adventofcode.com/2020/day/1


## Data input: downloaded from browser as text file
x <- read.table("data/day_1_1")
x <- x$V1


##########################################################
## Puzzle 1: find the two entries that sum to 2020, what do you get if you multiply them together?

# Solution: nested loop

for(i in 1:length(x)){
  
  for(k in 1:length(x)){
    if(k==i){ next }
    s <- x[i] + x[k]
    
    if(s==2020){
      print(i)
      print(k)
      print(paste0("Solution: ", x[i]*x[k]))
      stop("Found!")
    }
  }
}

##########################################################
## Puzzle 2: find the three entries that sum to 2020, what do you get if you multiply them together?


# Solution: double nested loop

for(i in 1:length(x)){
  
  for(k in 1:length(x)){
    
    l1 <- x[i] + x[k]
    
    if(k==i) { next }
    
    for(j in 1:length(x)){
      
      if(j==i | j==k){ next }
      
      l2 <- l1 + x[j]
      
      if(l2==2020){
        
        print(paste0("Solution: ", x[i]*x[k]*x[j]))
        
        stop("Found!")
      }
      
      
    }
    
  }
  
  
}































  