## download some files
download.file(
  url="https://liamrevell.github.io/hula2025/data/bonyfish.csv",
  destfile="bonyfish.csv")

download.file(
  url="https://liamrevell.github.io/hula2025/data/bonyfish.tre",
  destfile="bonyfish.tre")

## load phytools
library(phytools)

## read in my tree
bonyfish_tree<-read.tree(file="bonyfish.tre")
bonyfish_tree

## read in my data
bonyfish_data<-read.csv(file="bonyfish.csv",
  row.names=1)
head(bonyfish_data,20)

## to test the hypothesis that these characters'
## evolution is linked, we'll use the Pagel '94 model

?fitPagel

## first extract the individual variables
spawning_mode<-setNames(bonyfish_data$spawning_mode,
  rownames(bonyfish_data))
paternal_care<-setNames(bonyfish_data$paternal_care,
  rownames(bonyfish_data))
head(spawning_mode)
head(paternal_care)

## fit full interdependent Pagel '94 model
bonyfish_full.model<-fitPagel(bonyfish_tree,
  x=spawning_mode,y=paternal_care)
plot(bonyfish_full.model,lwd.by.rate=TRUE)

## fit model where parental care depends on spawning mode
bonyfish_dep.y<-fitPagel(bonyfish_tree,
  x=spawning_mode,y=paternal_care,dep.var="y")
bonyfish_dep.y
plot(bonyfish_dep.y,lwd.by.rate=TRUE)

## compare models
anova(bonyfish_dep.y,bonyfish_full.model)
