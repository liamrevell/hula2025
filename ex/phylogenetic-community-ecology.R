## download our data files
download.file(
  url="https://liamrevell.github.io/hula2025/data/SJ_ComMatrix.csv",
  destfile="SJ_ComMatrix.csv")
download.file(
  url="https://liamrevell.github.io/hula2025/data/SJtree.phy",
  destfile="SJtree.phy")

## load libraries
library(phytools)

## read in my tree first
SJ_tree<-read.tree(file="SJtree.phy")
SJ_tree
plotTree(SJ_tree,ftype="off",lwd=1)

## read in my data
SJ_data<-read.csv(file="SJ_ComMatrix.csv",row.names=1)
head(SJ_data,2)

## plot our presence/absence data 
plotTree.datamatrix(SJ_tree,SJ_data)

## plot fan tree
plotFanTree.wTraits(SJ_tree,SJ_data,type="fan",
  ftype="off")

## plot our presence/absence data 
?par
par()$fg
par(fg="black")
plotTree.datamatrix(SJ_tree,SJ_data)

palette()

palette()[c(4,2)]

cols<-replicate(ncol(SJ_data),
  setNames(c("lightblue","forestgreen"),0:1),
  simplify=FALSE)
cols
par(fg="transparent")
plotTree.datamatrix(SJ_tree,SJ_data,colors=cols,
  fsize=0,xexp=1.6,yexp=1)
par(fg="black")
legend("bottomleft",legend=c("absent","present"),
  pch=22,pt.bg=c("lightblue","forestgreen"),bty="n",
  pt.cex=2)

pdf(file="SJ_community_tree_plot.pdf",
  width=10,height=10)
par(fg="transparent")
plotTree.datamatrix(SJ_tree,SJ_data,colors=cols,
  fsize=0,xexp=1.6,yexp=1)
par(fg="black")
legend("bottomleft",legend=c("absent","present"),
  pch=22,pt.bg=c("lightblue","forestgreen"),bty="n",
  pt.cex=2)
dev.off()

## install picante
install.packages("picante")

## load picante
library(picante)

## calculate PD
?pd
SJ_all_pd<-pd(t(SJ_data),SJ_tree,include.root=TRUE)
head(SJ_all_pd)

## how correlated are species richness & PD?
dev.off()
plot(SJ_all_pd[,2:1],bty="n",pch=21,bg="grey",
  las=1,cex.axis=0.8,xlab="species richness",
  ylab="phylogenetic diversity")
grid()

## number of tips in our tree
Ntip(SJ_tree)

## top part of our object
head(SJ_all_pd)

## data for one site: Aleck Rock
SJ_data[,"Aleck_Rock",drop=FALSE]

## species richness
sum(sample(SJ_data[,"Aleck_Rock"]))

## generate null distibution
null_Aleck_Rock<-matrix(0,nrow=nrow(SJ_data),
  ncol=1000,
  dimnames=list(rownames(SJ_data),
    paste("perm",1:1000,sep="")))
dim(null_Aleck_Rock)
head(rownames(null_Aleck_Rock))
head(colnames(null_Aleck_Rock))
null_Aleck_Rock[,1]<-SJ_data[,"Aleck_Rock"]
for(i in 2:ncol(null_Aleck_Rock)){
  null_Aleck_Rock[,i]<-sample(SJ_data[,"Aleck_Rock"])
}
null_pd.Aleck_Rock<-pd(t(null_Aleck_Rock),SJ_tree)
head(null_pd.Aleck_Rock)

## visualize null distribution
hist(null_pd.Aleck_Rock$PD,breaks=20)
abline(v=SJ_all_pd["Aleck_Rock","PD"],lwd=3,col="red")

## consider a second island
SJ_data[,"Posey_Island"]

## repeat same analysis
null_Posey_Island<-matrix(0,nrow=nrow(SJ_data),
  ncol=1000,
  dimnames=list(rownames(SJ_data),
    paste("perm",1:1000,sep="")))
dim(null_Posey_Island)
null_Posey_Island[,1]<-SJ_data[,"Posey_Island"]
for(i in 2:ncol(null_Posey_Island)){
  null_Posey_Island[,i]<-sample(SJ_data[,"Posey_Island"])
}
null_pd.Posey_Island<-pd(t(null_Posey_Island),SJ_tree)
head(null_pd.Posey_Island)
hist(null_pd.Posey_Island$PD,breaks=20)
abline(v=SJ_all_pd["Posey_Island","PD"],lwd=3,
  col="red")

## automate this across all our communities
SJ_pd.test<-ses.pd(t(SJ_data),SJ_tree,
  null.model="richness")
head(SJ_pd.test)

## graph z-scores
hist(SJ_pd.test$pd.obs.z,breaks=20)


## lastly, let's look at community dissimilarity
## this is conceptually equivalent to measuring phylogenetic
## "beta diversity"
?comdist
?cophenetic.phylo

## compute phylogenetic patristic distance matrix
SJ_cophenetic<-cophenetic(SJ_tree)
dim(SJ_cophenetic)

## compute community distance matrix
SJ_comdist<-comdist(t(SJ_data),SJ_cophenetic)
SJ_comdist

## convert to a clustergram & visualize
SJ_community_dendro<-hclust(SJ_comdist)
plot(SJ_community_dendro,cex=0.4)
