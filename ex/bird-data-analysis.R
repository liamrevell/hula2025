## this is a continuation of what we started this morning...
bird_data

## use a function called aggregate
?aggregate

colnames(bird_data)

wing_length<-aggregate(bird_data$wing,
  by=list(bird_data$species),
  FUN=mean)
wing_length

unique_species

cbind(unique_species,wing_length)

wings<-setNames(wing_length$x,unique_species)
wings

tail_length<-aggregate(bird_data$tail,
  by=list(bird_data$species),FUN=mean)
tail_length
tails<-setNames(tail_length$x,unique_species)
tails

wings[1]<-93
tails[1]<-73

?phylosig

## compute phylogenetic signal for each of the two
## characters
wing_K<-phylosig(subsampled.bird_tree,log(wings),
  test=TRUE)
wing_K
plot(wing_K)

null_wings<-fastBM(subsampled.bird_tree,nsim=1000)
dim(null_wings)

null_K<-apply(null_wings,2,phylosig,
  tree=subsampled.bird_tree)
head(null_K)
hist(null_K)

 tail_K<-phylosig(subsampled.bird_tree,log(tails),
  test=TRUE)

 wing_K$K
 
Pval<-2*mean(wing_K$K>=null_K)
Pval

wing_lambda<-phylosig(subsampled.bird_tree,
  log(wings),method="lambda",test=TRUE)
wing_lambda

plot(wing_lambda)
