## download vertebrate tree file
download.file(
  url="https://liamrevell.github.io/hula2025/data/vertebrate.tre",
  destfile="vertebrate.tre")

## loading phytools
library(phytools)

## read in tree from file
vertebrate_tree<-read.tree(file="vertebrate.tre")

## let's see how a tree is plotted in R!
plotTree(vertebrate_tree,
  mar=c(3.1,3.1,1.1,1.1))
args(axis)
axis(side=1)
axis(2)
grid()

## now that we've learned this... 
abline(v=100,lwd=2,col="blue")

## calculate the total height of the tree
H<-nodeHeights(vertebrate_tree)
H
total_height<-max(H)
total_height

## graph an event 100 mya
abline(v=total_height-100,col="red",lwd=2)

## let's replot our tree
plotTree(vertebrate_tree,
  mar=c(3.1,3.1,1.1,1.1))
axis(1)
axis(2)

dev.off()
plot(NA,xlim=c(0,473),ylim=c(0,12))

nodelabels()

## let's pull out last plot info out of
## that parallel environment
plotTree(vertebrate_tree,
  mar=c(3.1,3.1,1.1,1.1))

pp<-get("last_plot.phylo",envir=.PlotPhyloEnv)
pp

points(pp$xx,pp$yy,pch=16)

## some discussion about environments
copy_of_plotTree<-phytools::plotTree
ls()
copy_of_plotTree(vertebrate_tree)

## re-graph & get "last_plot.phylo" object
plotTree(vertebrate_tree,
  mar=c(3.1,3.1,1.1,1.1))
pp<-get("last_plot.phylo",envir=.PlotPhyloEnv)
axis(1)
axis(2)

## get the common ancestor of mammals
mammal_node<-getMRCA(vertebrate_tree,
  c("Homo_sapiens","Bos_taurus"))
mammal_node

## overlay a point at the common ancestor of mammals
points(pp$xx[mammal_node],pp$yy[mammal_node],
  pch=16,col="blue",cex=1.2)

## some discussion about bind.tip
?bind.tip
bind.tip(vertebrate_tree,"Pan_troglobytes",
  interactive=TRUE)->modified.vert_tree

bind.tip(modified.vert_tree,"Platypus",
  interactive=TRUE)->modified.vert_tree2

## re-graph & get "last_plot.phylo" object
plotTree(vertebrate_tree,
  mar=c(3.1,3.1,1.1,1.1))
pp<-get("last_plot.phylo",envir=.PlotPhyloEnv)
axis(1)
axis(2)

## get mammal node
mammal_node<-getMRCA(vertebrate_tree,
  c("Homo_sapiens","Bos_taurus"))
mammal_node

## add point
points(pp$xx[mammal_node],pp$yy[mammal_node],
  pch=16,col="blue",cex=1.2)

## add arrow pointing to node
args(arrows)
arrows(x0=pp$xx[mammal_node]-50,
  y0=pp$yy[mammal_node]+2,
  x1=pp$xx[mammal_node]-5,
  y1=pp$yy[mammal_node]+0.1,length=0.1,
  lwd=3,col="blue")

## now let's figure out how to draw a clade box
?getDescendants

dd<-getDescendants(vertebrate_tree,
  node=mammal_node)
dd

points(pp$xx[dd],pp$yy[dd])

## figure out the coordinate of the plot
min_x<-min(pp$xx[c(mammal_node,dd)])
abline(v=min_x,lty="dotted")
max_x<-max(pp$xx[c(mammal_node,dd)])
abline(v=max_x,lty="dotted")
min_y<-min(pp$yy[c(mammal_node,dd)])
max_y<-max(pp$yy[c(mammal_node,dd)])
abline(h=min_y,lty="dotted")
abline(h=max_y,lty="dotted")

pp$x.lim

## draw box
args(polygon)

polygon(
  x=c(min_x-10,pp$x.lim[2],pp$x.lim[2],min_x-10),
  y=c(min_y-0.25,min_y-0.25,max_y+0.25,max_y+0.25),
  col="lightblue",border=FALSE)

## how to have the tree on top instead of behind the box?
## set plot=FALSE

## plot tree without plotting it
plotTree(vertebrate_tree,
  mar=c(3.1,3.1,1.1,1.1),plot=FALSE)
pp<-get("last_plot.phylo",envir=.PlotPhyloEnv)
axis(1)
axis(2)
grid()

## add clade box
polygon(
  x=c(min_x-10,pp$x.lim[2],pp$x.lim[2],min_x-10),
  y=c(min_y-0.25,min_y-0.25,max_y+0.25,max_y+0.25),
  col="lightblue",border=FALSE)

## add our tree
plotTree(vertebrate_tree,
  mar=c(3.1,3.1,1.1,1.1),add=TRUE)

## how about using "last_plot.phylo" to plot a tree?
plotTree(vertebrate_tree,plot=FALSE)
pp<-get("last_plot.phylo",envir=.PlotPhyloEnv)
points(pp$xx,pp$yy)
for(i in 1:nrow(pp$edge)){
  lines(pp$xx[pp$edge[i,]],pp$yy[pp$edge[i,]])
}
for(i in 1:pp$Ntip){
  text(x=pp$xx[i],y=pp$yy[i],
    gsub("_"," ",
      vertebrate_tree$tip.label[i]),pos=4)
}

## let's make a custom function!
hulaPlotter<-function(phy){
  plotTree(phy,plot=FALSE)
  pp<-get("last_plot.phylo",envir=.PlotPhyloEnv)
  for(i in 1:nrow(pp$edge)){
    lines(pp$xx[pp$edge[i,]],pp$yy[pp$edge[i,]])
  }
  for(i in 1:pp$Ntip){
    text(x=pp$xx[i],y=pp$yy[i],
      gsub("_"," ",
        phy$tip.label[i]),pos=4)
  }
}

## it works!
hulaPlotter(vertebrate_tree)
data("salamanders")
hulaPlotter(salamanders)
