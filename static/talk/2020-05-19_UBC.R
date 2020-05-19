library(moderndive)
library(tidyverse)
library(infer)






# Bowl
library(moderndive)
bowl

# Use shovel with n = 2 five times
bowl %>% rep_sample_n(size = 2, reps = 5)

bowl %>% 
  rep_sample_n(size = 50, reps = 1000) %>% 
  group_by(replicate) %>% 
  summarize(red = sum(color == "red")) %>% 
  mutate(prop_red = red / 50)

bowl %>% 
  rep_sample_n(size = 50, reps = 1000) %>% 
  group_by(replicate) %>% 
  summarize(red = sum(color == "red")) %>% 
  mutate(prop_red = red / 50) %>%
  ggplot(aes(x = prop_red)) +
  geom_histogram(binwidth = 0.05, col = "white") +
  labs(x = "Proportion of 50 balls that were red", 
       title = "Sampling distribution of p-hat with n = 50") 






# Resumes
# Barplot
ggplot(promotions, aes(x = gender, fill = decision)) +
  geom_bar() +
  labs(x = "Gender of name on résumé (recorded as binary)", title = "Using 48 identical résumés: Diff of 29.2%", fill = "Promotion\ndecision", y = "Number of résumés")

set.seed(4)
data("promotions")
promotions <- promotions %>% sample_frac(1)

library(moderndive)
promotions


promotions <- promotions %>%
  mutate(gender_shuffle = sample(gender))

# Under H0: p_m - p_f = 0
promotions

# Two extra arguments given both vars are binary:






# Superimpose t over histogram
null_distribution_movies_t <- movies_sample %>% 
  specify(formula = rating ~ genre) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 1000, type = "permute") %>% 
  # Notice we switched stat from "diff in means" to "t"
  calculate(stat = "t", order = c("Action", "Romance"))
visualize(null_distribution_movies_t, bins = 10)


obs_two_sample_t <- movies_sample %>% 
  specify(formula = rating ~ genre) %>% 
  calculate(stat = "t", order = c("Action", "Romance"))
obs_two_sample_t


visualize(null_distribution_movies_t, method = "both") +
  shade_p_value(obs_stat = obs_two_sample_t, direction = "both")
