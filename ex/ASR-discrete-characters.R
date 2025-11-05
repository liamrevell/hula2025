## discrete character ASR

## load primate data
data(primate.tree)
data(primate.data)

## pull out our character
activity<-setNames(primate.data$Activity_pattern,
  rownames(primate.data))
activity

## fit three models: ER, SYM, ARD models
er_primates<-fitMk(primate.tree,activity,
  model="ER")
sym_primates<-fitMk(primate.tree,activity,
  model="SYM")
ard_primates<-fitMk(primate.tree,activity,
  model="ARD")

## traditionally, we would compare these three
## models and choose the "best"
anova(er_primates,
  sym_primates,
  ard_primates)

## estimate marginal ancestral states under
## "best" (i.e., ER) model
primate_er.marginal<-ancr(er_primates)
primate_er.marginal

## we can plot this
plot(primate_er.marginal,
  args.plotTree=list(direction="upwards"),
  args.nodelabels=list(piecol=c("pink","gold","navy")))

## re-calculate our model comparison
anova(er_primates,
  sym_primates,
  ard_primates)->aov_primates
aov_primates

primate_modelaveraged.marginal<-ancr(aov_primates)
primate_modelaveraged.marginal

plot(primate_modelaveraged.marginal,
  args.plotTree=list(direction="upwards"),
  args.nodelabels=list(piecol=c("pink","gold","navy")))


