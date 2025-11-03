---
title: Challenge Problem 1
---
  
<link rel="stylesheet" href="../../assets/style.css">
    
# Challenge problem 1
  
1.1 Download the following two files for Phelsuma geckos from the course website: `phel.csv` and `phel.phy` (Harmon et al. 2010). `phel.csv` is a CSV file containing trait values for ten different morphological traits. `phel.phy` is a phylogeny of thirty-three species. Read both data and tree in from file and use `name.check` to identify any differences between the two data sets. If you find differences, prune the phylogeny and subsample the trait data to include only the species present in both the data file and the tree. Plot the tree.

1.2 Use `phyl.pca` to run a phylogenetic principal component analysis (PCA) of the morphological data set and tree from practice problem 1.1. When data for different variables in a PCA have different orders of magnitude, it often makes sense to transform by the natural logarithm and conduct our analysis on the log-transformed values instead of on the original traits. Inspect your data to see if this applies and then decide whether or not to log-transform before undertaking your phylogenetic PCA. After you’ve obtained a result for the PCA, create a screeplot to visualize the distribution of variation between the different principal component axes.

1.3 Use phylomorphospace to create a single projection of the phylogeny into morphospace for the first two PC axes from practice problem 1.2. Can you think of a way to project the tree into a space defined by more than two principal component dimensions? Hint: look up the help pages for `phylomorphospace3d` and `phyloScattergram` for ideas, or consider simply subdividing your plotting device using `par(mfrow)`.