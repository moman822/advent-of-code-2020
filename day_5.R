library(data.table)

setwd("~/GitHub/advent-of-code-2020/")

###                                  *
###                                 / \
###                                /   \
###                                /   \
###                               /     \
### ADVENT OF CODE, DAY 5        /       \
### https://adventofcode.com/2020/day/5



# read data

dat <- read.table("data/day_5_1")
names(dat) <- "pass"
setDT(dat)
dat[, row_key:=substr(pass, 1, 7)][, seat_key:=substr(pass, 8, 10)]


##########################################################
## Puzzle 1: What is the highest seat ID on a boarding pass?


# make functions
get_row <-Vectorize(function(key){
  v <- strsplit(key, "")[[1]]
  init <- 1:128
  for(i in 1:length(v)){
    if(v[i]=="F"){
      init <- init[1:(length(init)/2)]
    } else if(v[i]=="B"){
      init <- init[((length(init)/2)+1):length(init)]
    }
  }
  init - 1
}, USE.NAMES = F)

get_seat <- Vectorize(function(key){
  v <- strsplit(key, "")[[1]]
  init <- 1:8
  for(i in 1:length(v)){
    if(v[i]=="L"){
      init <- init[1:(length(init)/2)]
    } else if(v[i]=="R"){
      init <- init[((length(init)/2)+1):length(init)]
    }
  }
  init - 1
}, USE.NAMES = F)


#calculate new columns

dat$row <- get_row(dat$row_key)
dat$seat <- get_seat(dat$seat_key)
dat[, seat_id:=(row*8)+seat]

print(paste0("A: ", max(dat$seat_id)))

##########################################################
## Puzzle 1: What is the ID of your seat?

# find the missing seat
dat[order(seat_id), missing:=seat_id-shift(seat_id, type='lag')]

dat[missing!=1, seat_id-1]


# What did I look up?

# - How to use `Vectorize()`



























