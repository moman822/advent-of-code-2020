library(data.table)

setwd("~/GitHub/advent-of-code-2020/")

###                                 *
###                                / \
###                               /   \
###                               /   \
### ADVENT OF CODE, DAY 4        /     \
### https://adventofcode.com/2020/day/4


# load and clean data: collapse all groups of key pairs into single text strings in a list by line breaks

dat <- readLines("data/day_4_1")

dat <- c(dat, "") # need to add this on so the loop below does the last line

l <- list()
counter <- 1
brk <- 0
for(i in 1:length(dat)){
  
  if(dat[i]==""){
    print(i)
    l[[counter]] <- paste0(dat[(brk+1):(i-1)], collapse = " ")
    
    counter <- counter + 1
    brk <- i
  }
}

rm(counter, brk, i)

##########################################################
## Puzzle 1: Count the number of valid passports - those that have all required fields. Treat cid as optional. In your batch file, how many passports are valid?


fields <- c("byr","iyr","eyr","hgt","hcl","ecl","pid","cid")

fields1 <- fields[fields!='cid']

good_passports <- 0

for(i in 1:length(l)){
  
  f <- strsplit(strsplit(l[[i]], " ")[[1]], ":")
  f2 <- unlist(lapply(f, function(x){
    x[1]
  }))
  
  if(all(fields1 %in% f2)){
    
    good_passports <- good_passports + 1
    
  }
  
  
}; rm(f, f2, i)


## A: 264



##########################################################
## Puzzle 2: fields need validation now, as well:  count the passports where all required fields are both present and valid according to the rules

## Rules:
## byr (Birth Year) - four digits; at least 1920 and at most 2002.
## iyr (Issue Year) - four digits; at least 2010 and at most 2020.
## eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
## hgt (Height) - a number followed by either cm or in:
##   If cm, the number must be at least 150 and at most 193.
##   If in, the number must be at least 59 and at most 76.
## hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
## ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
## pid (Passport ID) - a nine-digit number, including leading zeroes.
## cid (Country ID) - ignored, missing or not.

# get data into a data.table

l2 <- lapply(l, function(x){
  
  ot <- lapply(strsplit(strsplit(x, " ")[[1]], ":"), function(y){
    out <- data.frame(
      col = y[2]
    )
    names(out) <- c(y[1])
    out
  })
  
  do.call('cbind', ot)
  
})

final <- rbindlist(l2, fill = TRUE)
final[, byr:=as.numeric(byr)][, iyr:=as.numeric(iyr)][, eyr:=as.numeric(eyr)]
str(final)

# conditions for: byr, iyr, eyr, ecl
check1 <- final[byr>=1920 & byr<=2002][iyr>=2010 & iyr<=2020][eyr>=2020 & eyr<=2030][ecl %in% c("amb","blu","brn","gry","grn","hzl","oth")]

# conditions for: pid
check2 <- check1[nchar(pid)==9][grepl("[0-9]{9}", pid)]

# conditions for: hcl
check3 <- check2[grepl("#[0-9a-zA-Z]{6}", hcl)]

# conditions for: hgt

check3[grepl("cm", hgt), hgt_cm:=as.numeric(gsub("cm", "", hgt))]
check3[grepl("in", hgt), hgt_in:=as.numeric(gsub("in", "", hgt))]

final2 <- rbind(
  check3[(hgt_cm>=150 & hgt_cm<=193)],
  check3[(hgt_in>=59 & hgt_in<=76)]
)

nrow(final2)

## A: 224


# What did I look up?

# - some regex stuff





