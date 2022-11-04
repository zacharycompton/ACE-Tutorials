library(phytools)
Data <- read.csv("sampleData.csv", row.names = 1)
tree <- read.tree("sampleTree.nwk")
View(Data)
x<-as.matrix(read.csv("sampleData.csv",row.names=1))[,1]
dotTree(tree,x,length=10,ftype="i")
