## download salamanders.nex file
download.file(
  url="https://liamrevell.github.io/hula2025/data/salamanders.nex",
  destfile="salamanders.nex")

## load phytools package
library(phytools)

## read in my phylogeny
salamander_tree<-read.nexus(file="salamanders.nex")
salamander_tree

## let's plot our salamander tree
plotTree(salamander_tree,ftype="i",fsize=0.9)

## the first thing that we can do is create
## a lineage-through-time plot
salamander_ltt<-ltt(salamander_tree,plot=FALSE)
salamander_ltt

## plot it
dev.off()
plot(salamander_ltt,log.lineages=FALSE,log="y",
  show.tree=TRUE,lwd=2)

## check our gamma 
salamander_ltt

## fit birth-death model
salamander_bd<-fit.bd(salamander_tree)
salamander_bd

## how many Plethodon species are there
n_plethodon<-58
plethodon_rho<-Ntip(salamander_tree)/n_plethodon
plethodon_rho

## re-fit our model accounting for this rho
salamander_bd.sampling<-fit.bd(salamander_tree,
  rho=plethodon_rho)
salamander_bd.sampling
salamander_bd

## install diversitree
install.packages("diversitree")

## load diversitree package
library(diversitree)

## create birth-death likelihood function for our
## salamander_tree object
salamander_bd_fn<-make.bd(salamander_tree,
  sampling.f=plethodon_rho)
args(salamander_bd_fn)

## take our fitted BD model from phytools
salamander_bd.sampling
salamander_bd_fn(pars=c(0.02082,0.016))

## now let's actually optimize this model
salamander_bd.fitted<-find.mle(salamander_bd_fn,
  x.init=c(1,0.1),method="optim",lower=c(0,0))
salamander_bd.fitted

## fit a BiSSE model!
grunts_data<-read.csv(
  file="https://liamrevell.github.io/hula2025/data/grunts.csv",
  row.names=1)
grunts_data
grunts_tree<-read.tree(
  file="https://liamrevell.github.io/hula2025/data/grunts.phy")
grunts_tree

## let's make a likelihood function!
grunts_habitat<-setNames(grunts_data$habitat,
  rownames(grunts_data))
head(grunts_habitat)
grunts_bisse_fn<-make.bisse(grunts_tree,
  states=grunts_habitat)
grunts_bisse_fn

## find a reasonable starting point for optimization
?starting.point.bisse
grunts_init<-starting.point.bisse(grunts_tree)
grunts_init

Ntip(grunts_tree)

grunts_bisse_fn.fitted<-find.mle(grunts_bisse_fn,
  x.init=grunts_init)
grunts_bisse_fn.fitted
