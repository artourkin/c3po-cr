setwd("/Users/artur/rnd/git/c3po-cr")

mydata=read.csv("data/data.csv",header=TRUE)
library("foreign")
write.arff(x =mydata ,file= "data/data.arff")