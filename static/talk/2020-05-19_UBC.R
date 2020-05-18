library(moderndive)
library(tidyverse)

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

