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
