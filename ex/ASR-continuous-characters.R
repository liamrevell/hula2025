## load packages
library(phytools)
library(geiger)

## load tree
data("cordylid.tree")

## plot this tree
plotTree(cordylid.tree,ftype="i",fsize=0.9)
nodelabels(cex=0.6,frame="circle",bg="white")

## load in my data
data("cordylid.data")
head(cordylid.data)

## pull out our character of interest
cordylid_armoring<-setNames(cordylid.data$pPC1,
  rownames(cordylid.data))
cordylid_armoring

## estimate ML ancestral states using fastAnc
cordylid_ml<-fastAnc(cordylid.tree,cordylid_armoring,
  CI=TRUE)
cordylid_ml

## compare 95% CI on root to range of trait
range(cordylid_armoring)

## visualize our reconstruction using contMap
cordylid_cmap<-contMap(cordylid.tree,cordylid_armoring,
  plot=FALSE)
cordylid_cmap
plot(cordylid_cmap,fsize=0.8)

## update our color gradient
cordylid_cmap<-setMap(cordylid_cmap,hcl.colors(n=100))
plot(cordylid_cmap,fsize=0.8)
nodelabels(cex=0.6,frame="circle",bg="white")

## let's Bayesian MCMC ancestral state reconstruction
cordylid_mcmc<-anc.Bayes(cordylid.tree,
  cordylid_armoring,ngen=500000)

cordylid_mcmc

str(cordylid_mcmc)

obj<-print(cordylid_mcmc)
str(obj)

## let's compare our Bayesian and ML estimates
dev.off()
plot(cordylid_ml$ace,obj,bty="n",pch=21,bg="grey",
  xlab="ML estimates",ylab="Bayesian estimates",
  las=1)
grid()

## let's specify an informative prior
Prior_mean<-c(
  1000,
  min(cordylid_armoring),
  rep(mean(cordylid_armoring),cordylid.tree$Nnode-1))
Prior_var<-c(
  1e6,
  0.01,
  rep(1000,cordylid.tree$Nnode-1))
cordylid_mcmc.informative<-anc.Bayes(cordylid.tree,
  cordylid_armoring,ngen=500000,
  control=list(pr.mean=Prior_mean,
    pr.var=Prior_var))

cordylid_mcmc.informative

obj<-print(cordylid_mcmc.informative)
plot(cordylid_ml$ace,obj,bty="n",pch=21,bg="grey",
  xlab="ML estimates",ylab="Bayesian estimates (informative prior)",
  las=1)
grid()

cordylid_cmap.bayes<-contMap(cordylid.tree,
  cordylid_armoring,method="user",
  anc.states=obj,plot=FALSE)
cordylid_cmap.bayes<-setMap(cordylid_cmap.bayes,
  hcl.colors(n=100))
plot(cordylid_cmap.bayes,fsize=0.8)

par(mfrow=c(1,2))
plot(cordylid_cmap,ftype="off",fsize=0.6)
plot(cordylid_cmap.bayes,ftype="off",
  direction="leftwards",fsize=0.6)

dev.off()
plot(cordylid_cmap.bayes,ftype="off",
  direction="rightwards",fsize=0.6)
nodelabels(frame="circle",cex=0.6)

cordylid_node.42<-density(cordylid_mcmc.informative,
  what=42)
dev.off()
plot(cordylid_node.42)

cordylid_node.42

