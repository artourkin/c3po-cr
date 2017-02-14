setwd("/Users/artur/rnd/git/c3po-cr")

mydata=read.csv("data/Data\ processed2.csv",header=TRUE)
library("foreign")
write.arff(x =mydata ,file= "data/data_processed.arff")