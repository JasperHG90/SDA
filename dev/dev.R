## Dev code

sic_clusters <- kmeans(gpg_core$DiffMedianHourlyPercent, centers = 7, nstart = 20)
sic_clusters



gpg_core$sic_clustered <- as.factor(sic_clusters$cluster)

## Stratify
table(gpg_core$sic_clustered)

# proportional-to-size stratified sample for 'division'
sample_stratum <- stratify(gpg_core, "sic_clustered", n=1000, seed=400)


## We can test this using an ANOVA difference of means
a <- aov(DiffMedianHourlyPercent ~ abbreviate(division, 20), gpg_core)
TukeyHSD(a, conf.level=0.95) 

## Number of divisions in cluster
cl <- gpg_core %>%
  group_by(sic_clustered, division) %>%
  tally()

# Surveydesign for stratified sample
stratdesign_prop <- svydesign(ids=sample_stratum$uuid, 
                              fpc=~fpc, 
                              strata = ~Stratum,
                              data = sample_stratum)

# Calculate the population value & design effect
svymean(~DiffMedianHourlyPercent, design=stratdesign_prop, deff=TRUE)

ggplot(gpg_core, aes(x=sic_clustered, y=DiffMedianHourlyPercent)) +
  geom_jitter(aes(color=gpg_core$division, alpha=0.4)) +
  theme(legend.position="None")

library(tree)
myTree <- tree(division ~ DiffMedianHourlyPercent + EmployerSize + female + DiffMedianBonusPercent, data = gpg_core)
summary(myTree)
plot(myTree)
text(myTree, pretty = 0)
gpg_core$treeGroup <- myTree$where

table(gpg_core$treeGroup)

# proportional-to-size stratified sample for 'division'
sample_stratum <- stratify(gpg_core, "treeGroup", n=1000, seed=20)
# Surveydesign for stratified sample
stratdesign_prop <- svydesign(ids=sample_stratum$uuid, 
                              fpc=~fpc, 
                              strata = ~Stratum,
                              data = sample_stratum)
svymean(~DiffMedianHourlyPercent, design=stratdesign_prop, deff=TRUE)

ggplot(gpg_core, aes(x=as.factor(treeGroup), y=DiffMedianHourlyPercent)) +
  #geom_jitter(aes(color=gpg_core$division, alpha=0.4)) +
  geom_violin() + 
  theme(legend.position="None")

mt_pred = predict(myTree, gpg_core, type="class")
table(mt_pred, gpg_core$treeGroup)

# Use neyman allocation to stratify optimally
sample_optim <- stratify(gpg_core, 
                         "sic_clustered", 
                         n=1000, 
                         seed=400,
                         optim=TRUE, 
                         optimVar = "DiffMedianHourlyPercent")

length(which(sample_optim$Stratum == 1))

# Surveydesign for stratified sample
sample_optim$Prob <- 1 / sample_optim$Prob
stratdesign_optim <- svydesign(ids=sample_optim$uuid, 
                               fpc=~fpc, 
                               strata = ~Stratum,
                               #weights = ~Prob,
                               data = sample_optim)


# Calculate the population value & design effect
svymean(~DiffMedianHourlyPercent, design=stratdesign_optim, deff=TRUE)

## We can test this using an ANOVA difference of means
a <- aov(DiffMeanHourlyPercent ~ sic_clustered, gpg_core)
# Post-hoc tests
TukeyHSD(a, conf.level=0.95) # --> no difference between the groups
