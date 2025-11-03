## show that Brownian motion evolution can arise
## from coin-flipping
n<-100 ## number of individuals
t<-200 ## "time" (number of flips)
y<-matrix(0,t,n) ## this is where I'm going to put data
cols<-hcl.colors(n=n) ## colors
for(i in 2:t){
  X<-sample(c(-1,0,1),n,replace=TRUE)
  y[i,]<-y[i-1,]+X
  plot(NA,xlim=c(0,t),ylim=range(y),xlab="time",
    ylab="phenotype")
  for(j in 1:n) lines(1:i,y[1:i,j],col=cols[j])
  Sys.sleep(0.1)
}

for(i in 1:t){
  hist(y[i,],
    breaks=seq(min(y)-0.5,max(y)+0.5,length.out=20),
    xlab="phenotype",main=paste("time =",i))
  Sys.sleep(0.1)
}

## back to phylogenies
library(phytools)
library(geiger)

## load mammal dataset and tree
?mammal.data
data(mammal.tree)
data(mammal.data)

## review our tree
mammal.tree

## plot our tree
plotTree(mammal.tree,ftype="i",fsize=0.7)

## review our data
head(mammal.data)

## we could run
name.check(mammal.tree,mammal.data)

## pull out bodyMass
lnBodyMass<-setNames(log(mammal.data$bodyMass),
  rownames(mammal.data))
head(lnBodyMass)

## put out home range size
lnHomeRange<-setNames(log(mammal.data$homeRange),
  rownames(mammal.data))
head(lnHomeRange)

?pic

## compute PICs for each variable
pic.lnBodyMass<-pic(lnBodyMass,mammal.tree)
pic.lnHomeRange<-pic(phy=mammal.tree,x=lnHomeRange)

pic.lnBodyMass

## add nodelabels
nodelabels(cex=0.6)

## fit our contrasts regression
mammal_lm<-lm(pic.lnHomeRange~pic.lnBodyMass+0)
mammal_lm

## plot our contrasts
dev.off()
plot(pic.lnBodyMass,pic.lnHomeRange,
  pch=16,xlab="contrasts in log(body mass)",
  ylab="contrasts in log(home range)",
  bty="n",las=1)
grid()
abline(mammal_lm,lwd=2)

## test hypothesis
summary(mammal_lm)

## load another package:nlme
library(nlme)

## preliminaries
spp<-rownames(mammal.data)
spp
?corBrownian
corBM<-corBrownian(phy=mammal.tree,form=~spp)
corBM

## fit our mammal PGLS
mammal_pgls<-gls(log(homeRange)~log(bodyMass),
  data=mammal.data,correlation=corBM)
coef(mammal_pgls)
coef(mammal_lm)

## load primate data
data("primate.data")
data("primate.tree")
head(primate.data)

## preliminaries
spp<-rownames(primate.data)
corBM<-corBrownian(phy=primate.tree,
  form=~spp)

plotTree(primate.tree,ftype="i",fsize=0.4,lwd=1)

## fit ANCOVA model
primate_ancova<-gls(log(Orbit_area)~log(Skull_length)+
    Activity_pattern,data=primate.data,
  correlation=corBM)

## summary of results
summary(primate_ancova)

## Challenge problem

## take the primate tree
## simulate data for x & y on that tree independent
## of each other
## calculate contrasts for x & y (or fit a PGLS model)
## compute a P-value
## do the same thing ignoring the phylogeny
## if you're abitious, do it 100 times

## clues
x<-fastBM(primate.tree)
y<-fastBM(primate.tree)
lm_xy<-lm(y~x)
lm_pic<-lm(pic(y,primate.tree)~pic(x,primate.tree)+0)
lm_xy
lm_pic

anova(lm_xy)
anova(lm_pic)

Pval.pic<-Pval.lm<-vector()

for(i in 1:100){
  x<-fastBM(primate.tree)
  y<-fastBM(primate.tree)
  lm_xy<-lm(y~x)
  lm_pic<-lm(pic(y,primate.tree)~pic(x,primate.tree)+0)
  Pval.lm[i]<-anova(lm_xy)$'Pr(>F)'[1]
  Pval.pic[i]<-anova(lm_pic)$'Pr(>F)'[1]
}
