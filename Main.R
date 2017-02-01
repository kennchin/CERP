#check working directory
getwd()

set.seed(90)

#set working directory
setwd("~/Desktop/Midterm/part2")

#libraries
library(rpart)
require(party)
require(partykit)
install.packages("Formula")

#source functions
source("Split.R")
source("optimal.R")
source("Partition.R")
source("Prediction.R")
source("Confusion.R")
source("Confusion_part.R")


#import data
lymphoma = read.csv("lymphoma_imputed.csv")
lymphoma_noid= lymphoma[,3:length(lymphoma)]
lymphoma_noid[,"y"]=factor(lymphoma_noid[,"y"])  #convert y to factor

part = partition(lymphoma_noid,4027,36,109)
Confusion_part(lymphoma_noid,part$part_train,part$part_test) 