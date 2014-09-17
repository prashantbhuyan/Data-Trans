# Prashant B. Bhuyan
# is607 Assignment 3
# Due 9.16.2014

##### Problem 1 #####
# Solution
naCount <- function(naVec){
   sum <- sum(is.na(naVec))
   print(sum)
}

# Results
# 
# Test 1:
# naVec <- c(NA, NA, NA, NA, NA)
# > naVec
# [1] NA NA NA NA NA
naCount(naVec)
# [1] 5
#
# Test 2: 
# > naVec <- c(NA,1,NA,2,NA,-4,NA,0)
# > naVec
# [1] NA  1 NA  2 NA -4 NA  0
naCount(naVec)
# [1] 4

##### Problem 2 #####

# Solution
na.dfCount <- function(df){
  naCount <- function(naVec){
    sum <- sum(is.na(naVec))
    print(sum)
    
  }
  
  print(colnames(df))
  for(col in df){
    naCount(col)
  
    
    
  
  }
  
  
}

# Result
# 
# Test 1: 
# > df
# Col1 Col2
# 1    6   NA
# 2    4   NA
# 3    3   NA
# 4   NA    6
# 5    4    4
# 6    6   NA
#
# 
# > na.dfCount(df)
[1] "Col1" "Col2"
[1] 1
[1] 4
#
# Test 2:
# > df2
# col1 col2
# 1   NA   NA
# 2   NA   NA
# 3   NA   NA
# 4   NA    1
#
# > na.dfCount(df2)
# [1] "col1" "col2"
# [1] 4
# [1] 3

##### Problem 3 #####

# Solution

grades <- c(99,88,52,73,77,100)

findStats <- function(vec){
  findMinMax(vec)
  print("Mean")
  findMean(vec)
  print("Med")
  findMed(vec)
  print("NA Count")
  naCount(vec)
  print("Quartiles")
  findQuartiles(vec)
  print("Std Dev")
  findStdDev(vec)
}
  

findMinMax <- function(vec){
    min <- vec[1]
    max <- vec[1]
  
    i <- 0
    
    for(i in 1:length(vec)){
      if(vec[i] > max){
        max <- vec[i]
      }
      else{ 
        if(vec[i] < min){
        min <- vec[i]
        }
      }
    }
    print("max")
    print(max)
    print("min")
    print(min)
  
}
# Results
# 
#  findMinMax(grades)
# [1] "max"
# [1] 100
# [1] "min"
# [1] 52

# Find Mean
findMean <- function(vec){
  mean <- sum(vec)/length(vec)
  print(mean)
}
#
# Results
# > findMean(grades)
# [1] 81.5

# Find Median 
findMed <- function(vec){
  sortedVec <- sort(vec)
  len <- length(sortedVec)
  place <- 0
  med <- 0
  if(len%%2 == 0){
    place <- len/2
    med <- sortedVec[place]
  }
  else if(len%%2 != 0){
    place <- (len+1)/2
    med <- sortedVec[place]
  }
  print(med)
}

# Results
# > grades
# [1]  99  88  52  73  77 100
# > findMed(grades)
# [1] 77
#
# > scores
# [1]     55     44     22    131   4123    523    422   1313 123113 
# > findMed(scores)
# [1] 422

# Count NA Values
naCount <- function(naVec){
  sum <- sum(is.na(naVec))
  print(sum)
}

# Results
# > grades <- c(99,NA,44,88,86,NA)
# > naCount(grades)
# [1] 2

# First Quartile
findQuartiles <- function(vec){
  firstQ <- (length(vec)+1)/4.0
  print(vec[firstQ])
  secondQ <- (length(vec)+1)/2.0
  print(vec[secondQ])
  thirdQ <- 3*(length(vec)+1)/4.0
  print(vec[thirdQ])
  
}

# Results
# dollars <- c(3, 5, 6, 7, 9, 22, 33)
# > findQuartiles(dollars)
# [1] 5
# [1] 7
# [1] 22

# Standard Deviation
findStdDev <- function(vec){
  findMean2 <- function(vec){
    mean <- sum(vec)/length(vec)
    # print(mean)
  }
  mean <- findMean2(vec)
  squaredDiff <- 0
  for(i in 1:length(vec)){
    squaredDiff <- c(squaredDiff,((vec[i]-mean)^2))
  }
  sqDiffMean<- findMean2(squaredDiff)
  print(sqrt(sqDiffMean))
  
}
 
# Results
# dollars <- c(3, 5, 6, 7, 9, 22, 33)
# > findStdDev(dollars)
# [1] 9.623261

##### Problem 4 #####


find.element.stats <- function(vec){
  # distinct elements
  findDistinctElements(vec)
  
  # commonly occuring
  findCommonlyOccuringElements(vec)
  
  # find missing
  findMissing(vec)
  
}

# Results
# > find.element.stats(letters)
# [1] "Unique Elements"
# [1] "A" "B" "D"
# [1] "Common Elements"
# A 
# 0 
# [1] "True Count"
# [1] 0
# [1] "False Count"
# [1] 6
# [1] "Proportion of True Values"
# [1] 0
# [1] "missing elements:"
# [1]  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE
  

# distinct elements
findDistinctElements <- function(vec){
  print("Unique Elements")
  print(unique(vec))
}
# Results
# [1] "Unique Elements"
# [1] "A" "B" "D"

# most commonly occuring elements and the # of times they occur
findCommonlyOccuringElements <- function(vec){
  tab <- table(vec)
  print("Common Elements")
  print(which.max(tab)-1)
}

# Results
# > letters <- c('A', 'B', 'D', 'D', 'E','E','E')
# > findCommonlyOccuringElements(letters)
# E 
# 3 

# find missing elements
findMissing <- function(vec){
  print("missing elements:")
  print(is.na(vec))
}
#
# Results
# letters <- c(NA,NA,NA,2,3,4,'123')
# > findMissing(letters)
# [1] "missing elements:"
# [1]  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE



##### Problem 5 #####

# Solution

# Number of True Values and False Values, Missing and Proportion of Trues
# vec <- c(TRUE,NA,FALSE,FALSE,TRUE)
findTRUE.FALSE <- function(vec){
  # missing values
  print("NA Values")
  print(table(is.na(vec)))
  
  trueCount <- 0
  falseCount <- 0
  
  for(i in 1:length(vec)){
    if(isTRUE(vec[i])){
      trueCount <- trueCount + 1
    }
    else{
      falseCount <- falseCount + 1
    }
  }
  
  
  print("True Count")
  print(trueCount)
  print("False Count")
  print(falseCount)
  print("Proportion of True Values")
  print(trueCount/(trueCount+falseCount))
  
}
# Results
# [1] "NA Values"

# FALSE  TRUE 
# 4     1 
# [1] "True Count"
# [1] 2
# [1] "False Count"
# [1] 3
# [1] "Proportion of True Values"
# [1] 0.4
  
##### Problem 6 #####
# Solution

df <- createDf(10,3)
createDf <- function(iters, vars){
  iterations = 10
  variables = 3
  
  df <- matrix(ncol=vars, nrow=iters)
  
  for(i in 1:iters){
    df[i,] <- runif(3)
    
  }
  return(df)
}
  
data.summary <- function(df){
  print("Element Summary")
  find.element.stats(df)
  print("Stats")
  findStats(df)
  print("Logical Summary")
  findTRUE.FALSE(df)
  
  
}

# Results: 
# > data.summary(df)
# [1] "Element Summary"
# [1] "Unique Elements"
# [,1]      [,2]       [,3]
# [1,] 0.17888421 0.9303867 0.46226735
# [2,] 0.19462493 0.3553124 0.74759055
# [3,] 0.69752811 0.8843233 0.02616216
# [4,] 0.06573142 0.9900918 0.28985548
# [5,] 0.22154871 0.2030924 0.92353765
# [6,] 0.53985119 0.8227908 0.76002356
# [7,] 0.84057131 0.1863256 0.64139393
# [8,] 0.19563999 0.6855336 0.97907527
# [9,] 0.99717800 0.6146654 0.23299351
# [10,] 0.96846111 0.3305740 0.97723930
# [1] "Common Elements"
# 0.0261621579993516 
# 0 
# [1] "missing elements:"
# [,1]  [,2]  [,3]
# [1,] FALSE FALSE FALSE
# [2,] FALSE FALSE FALSE
# [3,] FALSE FALSE FALSE
# [4,] FALSE FALSE FALSE
# [5,] FALSE FALSE FALSE
# [6,] FALSE FALSE FALSE
# [7,] FALSE FALSE FALSE
# [8,] FALSE FALSE FALSE
# [9,] FALSE FALSE FALSE
# [10,] FALSE FALSE FALSE
# [1] "Stats"
# [1] "max"
# [1] 0.997178
# [1] "min"
# [1] 0.02616216
# [1] "Mean"
# [1] 0.5647751
# [1] "Med"
# [1] 0.6146654
# [1] "NA Count"
# [1] 0
# [1] "Quartiles"
# [1] 0.8405713
# [1] 0.2030924
# [1] 0.02616216
# [1] "Std Dev"
# [1] 0.3182881
# [1] "Logical Summary"
# [1] "NA Values"

# FALSE 
# 30 
# [1] "True Count"
# [1] 0
# [1] "False Count"
# [1] 30
# [1] "Proportion of True Values"
# [1] 0  

