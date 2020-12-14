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
### ADVENT OF CODE, DAY 8      /           \
### https://adventofcode.com/2020/day/8


# read and wrangle data

dat <- readLines("data/day_8_1")
dat <- data.table('init' = dat)
dat[, cmd:=substr(init, 0, 3)]
dat[, val:=as.numeric(trimws(gsub("+", "", substr(init, 4, nchar(init)))))]

##########################################################
## Puzzle 1: Immediately before any instruction is executed a second time, what value is in the accumulator?

accumulator <- 0
position <- 1
been <- c()

while(position<nrow(dat)){
  
  if(position %in% been){
    print(accumulator)
    stop()
  }
  
  been <- c(been, position)
  
  if(dat[position]$cmd=="acc"){
    
    accumulator <- accumulator + dat[position]$val
    position <- position + 1
    
  } else if (dat[position]$cmd=="jmp"){
    
    position <- position + dat[position]$val
    
  } else {
    position <- position + 1
  }
  
  # print(position)
  
}

##########################################################
## Puzzle 2: Fix the program so that it terminates normally by changing exactly one jmp (to nop) or nop (to jmp).
##    What is the value of the accumulator after the program terminates?

run <- function(data, switch = 'nop', pos = 1){
  
  data <- copy(data)
  
  if(pos>nrow(data[cmd==switch])){
    
    stop(paste0("pos too high: ", pos))
    
  }
  
  swap_pos <- which(data$cmd==switch)[pos]
  
  if(switch=='nop'){
    data[swap_pos, cmd:='jmp']
  } else if(switch=='jmp') {
    data[swap_pos, cmd:='nop']
  }
  
  # print(data)
  
  accumulator <- 0
  position <- 1
  been <- c()
  
  while(position<nrow(data)){
    
    
    if(position %in% been){
      # print(accumulator)
      been <- c(been, position)
      stop("Nope")
    }
    
    been <- c(been, position)
    
    if(data[position]$cmd=="acc"){
      
      accumulator <- accumulator + data[position]$val
      position <- position + 1
      
    } else if (data[position]$cmd=="jmp"){
      
      position <- position + data[position]$val
      
    } else {
      position <- position + 1
    }
    
  }
  
  accumulator
  
}


nrow(dat[cmd=='jmp']) # need to run loop this many times for each command type
nrow(dat[cmd=='nop'])

for(i in 1:227){
  
  ans <- tryCatch({
    run(data = dat, switch='jmp', pos=i)
  }, error = function(e){
    # print()
    # "error"
  })
  
  if(is.numeric(ans)){
    print(ans)
  }
  
}

## A: 2,477



# What did I look up?

# Nothing
















