## introduction to Mk models

## load packages
library(phytools)
library(geiger)

## load dataset
data(primate.tree)
data(primate.data)

## review our character data
head(primate.data)

## function for fitting the Mk model
?fitMk

## this is how our data typically look:
activity<-setNames(primate.data$Activity_pattern,
  rownames(primate.data))
head(activity)

## this is what a matrix representation of the same data
Activity<-to.matrix(activity,levels(activity))
head(Activity,20)

## don't follow along here, but this demonstrates how we
## might code uncertainty
Activity[2,1:2]<-0.5
head(Activity)

## let's fit the model

## this is our equal rates model
er_primate<-fitMk(primate.tree,activity,model="ER")
er_primate

## plot our fitted model
plot(er_primate,signif=10)

## let's jump to the most parameter complex model
## this is called the "ARD"
ard_primate<-fitMk(primate.tree,activity,model="ARD")
ard_primate

## let's plot our ARD model
plot(ard_primate,width=TRUE,color=TRUE)

## let's assume that transitions are symmetric
sym_primate<-fitMk(primate.tree,activity,model="SYM")
sym_primate

## plot our model
plot(sym_primate)

## let's compare among these fitted models
anova(er_primate,sym_primate,ard_primate)

## download files for Brandley et al. analysis using R
download.file(
  url="https://liamrevell.github.io/hula2025/data/brandley_table.csv",
  destfile="brandley_table.csv")
download.file(
  url="https://liamrevell.github.io/hula2025/data/squamate.tre",
  destfile="squamate.tre")

## read in our files
squamate_data<-read.csv(file="brandley_table.csv",
  row.names=1)
squamate_tree<-read.nexus(file="squamate.tre")

## we can look at our tree
squamate_tree

## let's look at our data
## we see that our data have spaces instead of underscores
head(squamate_data)

## let's fix this problem
sp_names<-rownames(squamate_data)
sp_names<-gsub(" ","_",sp_names)
head(sp_names)
rownames(squamate_data)<-sp_names
head(squamate_data)

## let's run a name check
## we find that some species are in the data but not the tree
name.check(squamate_tree,squamate_data)

## let's remove the species in my data but not tree
subsampled.squamate_data<-
  squamate_data[squamate_tree$tip.label,]

## look at our data frame
head(squamate_data)

## re-run name.check
## this time it passes
name.check(squamate_tree,subsampled.squamate_data)

## let's pull out the front digits ("Fingers")
fore_digits<-setNames(
  subsampled.squamate_data$Fingers,
  rownames(subsampled.squamate_data))

## round
fore_digits<-round(fore_digits)

## let's start off with an ER model
er_squamates<-fitMk(squamate_tree,fore_digits,
  model="ER",pi="fitzjohn")
er_squamates

## plot our fitted model
plot(er_squamates,signif=5)

## now let's jump to a more complex, custom model
## this will be a one-rate, directional, loss-only model

## making the design matrix of our model
design_matrix<-matrix(0,6,6)
design_matrix
rownames(design_matrix)<-colnames(design_matrix)<-
  0:5
design_matrix
design_matrix[2,1]<-1
design_matrix
design_matrix[3,2]<-2
design_matrix
design_matrix[4,3]<-3
design_matrix[5,4]<-4
design_matrix[6,5]<-5
design_matrix

## fit the model
lossonly_multirate<-fitMk(squamate_tree,
  fore_digits,model=design_matrix,pi="fitzjohn")
lossonly_multirate

## plot the model
plot(lossonly_multirate,show.zeros=FALSE)

## let's imagine a gain-only 
design_matrix<-matrix(c(
  0,1,0,0,0,0,
  0,0,2,0,0,0,
  0,0,0,3,0,0,
  0,0,0,0,4,0,
  0,0,0,0,0,5,
  0,0,0,0,0,0),6,6,byrow=TRUE)
rownames(design_matrix)<-
  colnames(design_matrix)<-0:5
design_matrix

## don't do this
## let's make an ER matrix (just to see the equivalence)
er_matrix<-matrix(1,6,6)
diag(er_matrix)<-0
er_matrix
fitMk(squamate_tree,fore_digits,model=er_matrix,
  pi="fitzjohn")

## gain-only model
gainonly_multirate<-fitMk(squamate_tree,
  fore_digits,model=design_matrix,pi="fitzjohn",
  rand_start=TRUE)

## loss-only + developmental jump
design_matrix<-matrix(c(
  0,0,0,0,0,0,
  1,0,0,0,0,0,
  2,3,0,0,0,0,
  2,0,4,0,0,0,
  2,0,0,5,0,0,
  2,0,0,0,6,0),6,6,byrow=TRUE,
  dimnames=list(0:5,0:5))
design_matrix
 
## fit our lossdevjump model
lossdevjump<-fitMk(squamate_tree,
  fore_digits,model=design_matrix,
  pi="fitzjohn",rand_start=TRUE)
lossdevjump

## often this model won't converge to the MLE

## running multiple optimization iterations in parallel
library(doParallel)
library(foreach)

## detect number of computer cores
detectCores()

## set number of optimization iterations
niter<-10

## set number of cores (should be < detectCores value)
ncores<-10

## create cluster
mc<-makeCluster(ncores,type="PSOCK")
registerDoParallel(cl=mc)

## run our optimization iterations in parallel
devjump_fits<-foreach(i=1:niter)%dopar%{
  phytools::fitMk(squamate_tree,
    fore_digits,model=design_matrix,
    pi="fitzjohn",rand_start=TRUE)
}

## this is our set of all fitted models
devjump_fits

## this is just one log-likelihood
logLik(er_squamates)

## apply across our set of optimizations
lnL<-sapply(devjump_fits,logLik)
lnL

## how does it compare to our multi-rate loss-only model?
lossonly_multirate

## this is our best model from our set
## (probably our true MLE)
best_lossdevjump<-devjump_fits[[which.max(lnL)]]
best_lossdevjump

## let's plot our fitted model
plot(best_lossdevjump,width=TRUE,color=TRUE,
  show.zeros=FALSE)

## compare our set of four fitted models
anova(er_squamates,lossonly_multirate,
  gainonly_multirate,best_lossdevjump)

## never forget to stop your cluster when you're
## done with it!
stopCluster(mc)
