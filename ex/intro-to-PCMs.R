## continuing on after coffee break!
list.files()

## reading in the anole.data.csv
anole_data<-read.csv(file="anole.data.csv",
  row.names=1)
head(anole_data)

## in geiger there's a convenient function
## to check if a data frame & tree "match"
name.check(anolis_tree,anole_data)

## load our other data file
ecomorph_data<-read.csv(file="ecomorph.csv",
  row.names=1)
head(ecomorph_data,10)

## run name.check again with this new data frame
name.check(anolis_tree,ecomorph_data)->anolis_chk
summary(anolis_chk)

## prune our tree
pruned.anolis_tree<-drop.tip(anolis_tree,
  tip=anolis_chk$tree_not_data)
plotTree(anolis_tree,lwd=1,type="fan",
  fsize=0.6)
plotTree(pruned.anolis_tree,lwd=1,type="fan",
  fsize=0.6)

## run name.check using our pruned tree
name.check(pruned.anolis_tree,ecomorph_data)
name.check(pruned.anolis_tree,anole_data)

## subsample anole_data to include only our 
## pruned tree species
pruned.anole_data<-anole_data[pruned.anolis_tree$tip.label,]

## this should now pass name.check
name.check(pruned.anolis_tree,pruned.anole_data)

## digression on names!
head(pruned.anole_data)
pruned.anole_data[,"SVL"]
pruned.anole_data[,1]
pruned.anole_data$SVL->svl
svl<-setNames(pruned.anole_data$SVL,
  rownames(pruned.anole_data))
head(svl)

## let's do a phylogenetic PCA!
anolis_pca<-phyl.pca(pruned.anolis_tree,
  pruned.anole_data)
anolis_pca

## pull out our principal component scores
anolis_pca.scores<-scores(anolis_pca)
head(anolis_pca.scores)

## close plotting device
dev.off()

## let's create a phylomorphospace plot.
phylomorphospace(pruned.anolis_tree,
  anolis_pca.scores[,1:2])

## project phylomorphospace (don't do this!)
project.phylomorphospace(pruned.anolis_tree,
  anolis_pca.scores[,1:2])

## let's take a step back
plot(anolis_pca.scores[,1:2])

## clean this up
phylomorphospace(pruned.anolis_tree,
  anolis_pca.scores[,1:2],ftype="off",
  node.size=c(0,1))

## add back in our ecomorph
head(ecomorph_data,20)
ecomorph<-setNames(ecomorph_data$ecomorph,
  rownames(ecomorph_data))
head(ecomorph,20)

## convert to factor
ecomorph<-as.factor(ecomorph)
ecomorph

tiplabels()
nodelabels()

phylomorphospace(pruned.anolis_tree,
  anolis_pca.scores[,1:2],ftype="off",
  node.size=c(0,1))
ECOMORPH<-to.matrix(ecomorph,levels(ecomorph))
head(ECOMORPH,10)

tiplabels(pie=ECOMORPH,cex=0.5)

## as a digression
points(anolis_pca.scores[,1:2],pch=15)


