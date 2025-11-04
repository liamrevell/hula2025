## https://liamrevell.github.io/hula2025/data/big-bird-tree.nex
download.file(
  url="https://liamrevell.github.io/hula2025/data/big-bird-tree.nex",
  destfile="big-bird-tree.nex")

## load my phylogeny packages
library(phytools)
library(geiger)

## use help.search
help.search("nexus")

## load our bird tree
bird_tree<-read.nexus(file="big-bird-tree.nex")
bird_tree

## plot it
plotTree(bird_tree,type="fan",lwd=1,ftype="off")

## plot it to PDF
pdf(file="bird-tree.pdf",width=30,height=30)
plotTree(bird_tree,type="fan",lwd=1,ftype="i",
  fsize=0.3)
dev.off()

## get our bird ringing data
download.file(
  url="https://liamrevell.github.io/hula2025/data/bird-ringing-data.csv",
  destfile="bird-ringing-data.csv")

## read this into R
bird_data<-read.csv(file="bird-ringing-data.csv")
head(bird_data)
bird_data

## let's write a function in R
hello<-function(x){
  cat("Hello world!\n")
}
hello(1)

## new function
hello2<-function(x){
  for(i in 1:x){
    cat("Hello world!\n")
  }
}

## does not work
hello2(x)

## assign value to x
x<-4

## works
hello2(x)

## function 3
hello3<-function(x){
  for(i in 1:x){
    hello(1)
  }
}
hello3(4)

## how do we substitute the first letter
?substr

## fix spelling error in my name
string<-"Liem Revell"
substr(string,1,4)
substr(string,1,4)<-"Liam"

## how do we change to upper case
?toupper
 
## fix case error in my name
string<-"liam Revell"
toupper(string)
substr(string,1,1)<-toupper(substr(string,1,1))
string

## combine into a function
firstup<-function(species_name){
  substr(species_name,1,1)<-
    toupper(substr(species_name,1,1))
  return(species_name)
}

## here are the species in our data
species<-bird_data$species
species

## fix the first species name
firstup(species[1])

## substr & toupper are vectorized so this is not necessary
sapply(species,firstup)

## fix all species labels
species<-firstup(species)
species

## add underscore for spaces
?gsub
gsub(" ","_",species)->species
species

## fix spelling error of Locustella and Phylloscopus
species<-gsub("Locustela","Locustella",species)
species<-gsub("Phylloscupus","Phylloscopus",species)

## update Saxicola_torquata
species<-gsub("Saxicola_torquata","Saxicola_rubicola",
  species)

## get unique species
unique_species<-sort(unique(species))
unique_species

## function to keep tips
?keep.tip

## subsampled bird tree
subsampled.bird_tree<-keep.tip(bird_tree,
  tip=unique_species)

## find Locustella species
Locustella_species<-bird_tree$tip.label[
  grep("Locustella",bird_tree$tip.label)]
Locustella_species

## change name in tree
bird_tree$tip.label[grep("Locustella_luscinioides",
  bird_tree$tip.label)]<-"Locustella_luscinialis"

## find Phylloscopus species
Phylloscopus_species<-bird_tree$tip.label[
  grep("Phylloscopus",bird_tree$tip.label)]
Phylloscopus_species

unique_species

## since we only have one Phylloscopus in our data
## we can just swap the labels
bird_tree$tip.label[grep("Phylloscopus_orientalis",
  bird_tree$tip.label)]<-"Phylloscopus_collybita"

## see the Saxicola species
Saxicola_species<-bird_tree$tip.label[
  grep("Saxicola",bird_tree$tip.label)]
Saxicola_species

## create our subsampled tree
subsampled.bird_tree<-keep.tip(bird_tree,
  tip=unique_species)

## plot our subsampled tree
plotTree(subsampled.bird_tree,ftype="i")
