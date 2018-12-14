library(dplyr)
order <- gpg_core %>%
  group_by(county) %>%
  tally() %>%
  arrange(desc(n)) %>%
  mutate(order = 1:n())

# Data
gpg_core2 <- gpg_core %>% 
  left_join(order, by="county") %>%
  # Remove NA values
  na.omit() %>%
  # Filter for these values
  filter(abs(DiffMedianHourlyPercent) <= 1,
         n > cutoff)

# Filter companies at this cutoff
cutoff <- 20
library(ggplot2)
ggplot(gpg_core2, 
        aes(x=reorder(county, order), y = DiffMedianHourlyPercent, color=county)) +
          labs(caption=paste0("\nNB. Only counties with more than ", cutoff, 
                              " observations are considered.\n", nrow(gpg_core) - nrow(gpg_core2), 
                              " companies are not included due to missing data")) +
          geom_jitter(alpha=0.5) +
          geom_boxplot(width=0.4) +
          theme_bw() +
          theme(legend.position = "None") +
          scale_x_discrete(name = "County, ordered by size") +
          theme(axis.text.x = element_blank(),
                axis.ticks.x = element_blank())

# proportional-to-size stratified sample for 'division'
sample_stratum <- stratify(gpg_core, "london", n=1000, seed=400)

# Surveydesign for stratified sample
stratdesign_prop <- svydesign(ids=sample_stratum$uuid, 
                              fpc=~fpc, 
                              strata = ~Stratum,
                              data = sample_stratum)


# Calculate the population value & design effect
svymean(~DiffMedianHourlyPercent, design=stratdesign_prop, deff=TRUE)
