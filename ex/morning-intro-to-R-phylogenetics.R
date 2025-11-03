## the first step will be to install
## our phylogenetic packages
install.packages("phytools")
install.packages("geiger")

## check package version
packageVersion("phytools")
packageVersion("geiger")

## load packages
library(phytools)
library(geiger)

## five vertebrate species
## blue_tang
## arabian_babbler
## tailed_frog
## largemouth_bass
## domestic_cat

## Newick string
newick_vertebrate_string<-
  "((blue_tang,largemouth_bass),(tailed_frog,(domestic_cat,arabian_babbler)));"

## read this Newick string using ape::read.tree
vertebrate_tree<-read.tree(text=newick_vertebrate_string)

## calling list all the objects in our current session
ls()

## the class of our veterbrate tree is "phylo"
class(vertebrate_tree)

## type the name of the object
vertebrate_tree

## run print on this object
print(vertebrate_tree)

## use the plot method
plot(vertebrate_tree)

## let's learn about the structure of this "phylo" object
str(vertebrate_tree)

## print the edge matrix
vertebrate_tree$edge

## call nodelabels and tiplabels
nodelabels()
tiplabels()

## plot in slanted stype
plot(vertebrate_tree,type="cladogram")

## args
## help
help(plot)

## where do I find help for generic method
help(plot.phylo)
?plot.phylo

## plot in an unrooted style
plot(vertebrate_tree,type="unrooted")

## argument names:
args(plot.phylo)

## get working directory
getwd()

## set working directory
setwd("courses/Israel-2025/work")
getwd()

## check to make sure you have the file!
list.files()

## our next step will be to read this tree!
anolis_tree<-read.tree(file="Anolis.tre")
anolis_tree

## we could've also read this directly from the web!
anolis_tree<-read.tree(
  file="https://liamrevell.github.io/hula2025/data/Anolis.tre")

## we can download a file using R
download.file(
  url="https://liamrevell.github.io/hula2025/data/Anolis.tre",
  destfile="Anolis2.tre")
anolis_tree<-read.tree(file="Anolis2.tre")

## read from book webpage
anolis_tree<-read.tree(file="http://www.phytools.org/Rbook/1/Anolis.tre")
anolis_tree

## last step before break: plot our tree!
plotTree(anolis_tree)

## plot using phytools
plotTree(anolis_tree,type="fan",lwd=1,fsize=0.6)

## plot to PDF
pdf(file="my-anolis-tree.pdf",width=7,height=7)
plotTree(anolis_tree,type="fan",lwd=1,fsize=0.6)
dev.off()

## plot to a higher resolution PNG
png(filename="my-anolis-tree.png",width=7,height=7,
  units="in",res=600)
plotTree(anolis_tree,type="fan",lwd=1,fsize=0.6)
dev.off()

