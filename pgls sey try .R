library(devtools)
install_github("liamrevell/phytools")
library(phytools)
library(geiger)
library(caper)
library(tidyverse)
Data <- read.csv("min50_ZACH.csv")
View(Data)
Data <- Data[Data$Clade=="Mammalia", 
             c('Species', "Clade", "NeoplasiaPrevalence", "MalignancyPrevalence", "adult_weight",
               "litter_size", "Gestation", "max_longevity", "SE_simple", "metabolic_rate")]
View(Data)

##after using the above filtering step you can count the number of columns
##choose the column numbers you need for your regression. Should only need species, neoplasia prev
## standard error (sqrt(1/TotalRecords)) and your LH variable
cutData <- Data[,c(1,3, 6, 9),drop=FALSE] 

cutData[cutData < 0] <-NA

cutData <- na.omit(cutData)
tree <- read.tree("min50Species.nwk")


#prune the tree to match the data
cutData$Species <- gsub(" ", "_", cutData$Species)
includedSpecies <- cutData$Species
newtips<-str_remove_all(tree$tip.label,"_ott")
newtips<-str_remove_all(newtips,".ott")
newtips<-str_remove_all(newtips,"-ott")
newtips<-str_remove_all(newtips,"[1234567890]")
newtips<-sub('^([^_]+_[^_]+).*', '\\1', newtips)
#pruning the tree
tree$tip.label <- newtips
pruned.tree<-drop.tip(
  tree, setdiff(
    tree$tip.label, includedSpecies))
pruned.tree <- keep.tip(pruned.tree,pruned.tree$tip.label)

#Removing discrepancies
cutData$Keep <- cutData$Species %in% pruned.tree$tip.label
cutData <- cutData[!(cutData$Keep==FALSE),]


## species labels as row names
rownames(cutData)<-cutData$Species
## pull out the SEs
SE<-setNames(cutData$SE_simple,cutData$Species)[rownames(cutData)]
##body mass
adult.weight.neo<-pgls.SEy(NeoplasiaPrevalence~log10(adult_weight),data=cutData,
               tree=pruned.tree,method="ML",se=SE)
summary(adult.weight.neo)

adult.weight.mal<-pgls.SEy(MalignancyPrevalence~log10(adult_weight),data=cutData,
                           tree=pruned.tree,method="ML") #,se=SE)
summary(adult.weight.mal)

##Gestation models 
gestation.neo<-pgls.SEy(NeoplasiaPrevalence~log10(Gestation),data=cutData,
                       tree=pruned.tree,method="ML") #,se=SE)
summary(gestation.neo)
gestation.mal<-pgls.SEy(MalignancyPrevalence~log10(Gestation),data=cutData,
                        tree=pruned.tree,method="ML") #,se=SE)
summary(gestation.mal)

#litter size models 
litter.neo<-pgls.SEy(NeoplasiaPrevalence~log10(litter_size),data=cutData,
                        tree=pruned.tree,method="ML",se=SE)
summary(litter.neo)

litter.mal <- pgls.SEy(MalignancyPrevalence~log10(litter_size),data=cutData,
                       tree=pruned.tree,method="ML") #,se=SE)
summary(litter.mal)
### Longevity model
longevity.neo<-pgls.SEy(NeoplasiaPrevalence~log10(max_longevity),data=cutData,
                     tree=pruned.tree,method="ML") #,se=SE)
summary(longevity.neo)

longevity.mal<-pgls.SEy(MalignancyPrevalence~log10(max_longevity),data=cutData,
                     tree=pruned.tree,method="ML") #,se=SE)
summary(longevity.mal)

##BMR models
BMR.neo<-pgls.SEy(NeoplasiaPrevalence~log10(metabolic_rate),data=cutData,
                        tree=pruned.tree,method="ML") #,se=SE)
summary(BMR.neo)

BMR.mal<-pgls.SEy(MalignancyPrevalence~log10(metabolic_rate),data=cutData,
                        tree=pruned.tree,method="ML") #,se=SE)
summary(BMR.mal)
###use this command after your analysis for one LH trait and then re-run script
### for your next LH trait
rm(cutData)
