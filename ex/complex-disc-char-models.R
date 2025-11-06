## load dataset and tree
download.file(
  url="https://liamrevell.github.io/hula2025/data/Liolaemidae.data.csv",
  destfile="Liolaemidae.data.csv")
download.file(
  url="https://liamrevell.github.io/hula2025/data/Liolaemidae.MCC.nex",
  destfile="Liolaemidae.MCC.nex")

## load packages
library(phytools)

## read in the data from file.
liol_data<-read.csv(file="Liolaemidae.data.csv",
  row.names=1,stringsAsFactors=TRUE)
head(liol_data)

liol_data$parity_mode

## read the tree from file
liol_tree<-read.nexus(file="Liolaemidae.MCC.nex")
liol_tree
plotTree(liol_tree,type="arc",fsize=0.3,lwd=1,
  arc_height=0.2,part=0.95,ftype="i")

## let's pull out parity mode
parity<-setNames(liol_data$parity_mode,
  rownames(liol_data))
head(parity)

geiger::name.check(liol_tree,liol_data)

## start by fitting our Mk models
liol_parity_er<-fitMk(liol_tree,parity,
  model="ER",pi="fitzjohn")
liol_parity_er
liol_parity_ard<-fitMk(liol_tree,parity,
  model="ARD",pi="fitzjohn")
liol_parity_ard

## now let's fit the threshold model
?fitThresh
liol_parity_thresh<-fitThresh(liol_tree,
  parity)
liol_parity_thresh

## let's compare our three models so far
anova(liol_parity_er,liol_parity_ard,
  liol_parity_thresh)

## let's fit a hidden-rate model
?fitHRM
liol_parity_hrm<-fitHRM(liol_tree,parity,ncat=2,
  pi="fitzjohn",parallel=TRUE)
plot(liol_parity_hrm)

## let's compare our four models!
anova(liol_parity_er,liol_parity_ard,
  liol_parity_thresh,liol_parity_hrm)

## our last model is a model in which oviparity<->viviparity
## varies as a continuous function of environmental temp.
?fitcontMk
liol_temp<-setNames(liol_data$temperature,
  rownames(liol_data))
liol_cont_disc<-fitcontMk(liol_tree,
  parity,liol_temp,parallel=TRUE,ncores=10,
  levs=50,maxit=5000)
