library(tidyverse)
library(moderndive)
library(infer)

# Chapter 8: "Virtual" sampling from a bowl of red/white balls
bowl
bowl %>% 
  rep_sample_n(size = 50, replace = FALSE, reps = 1)












# Chapter 9: "Virtual" resampling from a sample of 50 pennies
pennies_sample_2
bowl %>% 
  rep_sample_n(size = 50, replace = TRUE, reps = 1)










# Chapter 10: Constructing a null distribution from scratch
null_distribution <- 
  # Diffs in prop'n promoted: male - female = 87.5% - 58.3% = 29.2%
  promotions %>% 
  # Variables of interest from data frame
  specify(formula = decision ~ gender, success = "promoted") %>% 
  # H0 of no difference
  hypothesize(null = "independence") %>% 
  # Simulate replicate data under H0
  generate(reps = 1000, type = "permute") %>% 
  # Summary statistic
  calculate(stat = "diff in props", order = c("male", "female"))

ggplot(null_distribution, aes(x=stat)) +
  geom_histogram(bins = 10) +
  labs(x = "Diff in proportion", title = "Null distribution") +
  geom_vline(xintercept = 0.292, col="red")
