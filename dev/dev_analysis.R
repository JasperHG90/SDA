## Dev code for real analysis

## For sic division
order <- gpg_core %>%
  group_by(division) %>%
  summarize(iqr = IQR(DiffMedianHourlyPercent)) %>%
  arrange(desc(iqr)) %>%
  mutate(order = 1:n())
order

library(ggplot2)
ggplot(gpg_core %>% left_join(order, by="division"), aes(x=reorder(abbreviate(as.factor(division), 20), order), 
                                                         y=DiffMedianHourlyPercent)) +
  geom_jitter(aes(color=gpg_core$division)) +
  geom_boxplot(width=0.2,
               outlier.shape = NA) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "None")

## For company size
order <- gpg_core %>%
  group_by(EmployerSize) %>%
  summarize(iqr = IQR(DiffMedianHourlyPercent)) %>%
  arrange(desc(iqr)) %>%
  mutate(order = 1:n())
order

library(ggplot2)
ggplot(gpg_core %>% left_join(order, by="EmployerSize"), aes(x=reorder(EmployerSize, order), 
                                                             y=DiffMedianHourlyPercent)) +
  geom_jitter(aes(color=gpg_core$EmployerSize)) +
  geom_boxplot(width=0.2,
               outlier.shape = NA) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "None") +
  scale_x_discrete("Employer Size")

calc_weight <- function(x) {
  
  switch(
    x,
    "Not Provided" = 0,
    "Less than 250" = 250 / 2,
    "250 to 499" = 375,
    "500 to 999" = 750,
    "1000 to 4999" = 3000,
    "5000 to 19,999" = 12500,
    "20,000 or more" = 20000
  )
  
}

gpg_core <- gpg_core %>%
  mutate(weight = sapply(as.character(gpg_core$EmployerSize), calc_weight))