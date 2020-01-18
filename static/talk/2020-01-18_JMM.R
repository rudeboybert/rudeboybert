library(tidyverse)
library(moderndive)
library(patchwork)
library(infer)


# First evals plots
ggplot(evals, aes(x = age, y = score, color = gender)) +
  geom_point() +
  labs(
    x = "Age", 
    y = "Teaching Score", 
    color = "Gender\n(recorded\nas binary)",
    title = "Teaching evals for 463 UT Austin courses (taught by 94 profs)"
    ) +
  geom_smooth(method = "lm", se = FALSE)
ggsave("~/Desktop/evals.png", width = 8, height = 4.5)  


# Interaction vs parallel slopes for evals
interaction_plot <- ggplot(evals, aes(x = age, y = score, color = gender)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  labs(x = "Age", y = "Teaching Score", title = "Interaction model",
       color = "gender\n(recorded\nas binary)") +
  theme(legend.position = "none")

parallel_slopes_plot <- ggplot(evals, aes(x = age, y = score, color = gender)) +
  geom_point() +
  geom_parallel_slopes(se = FALSE) +
  labs(x = "Age", y = "Teaching Score", title = "Parallel slopes model",
       color = "gender\n(recorded\nas binary)") +
  theme(legend.position = "none")

interaction_plot %>% 
  ggsave("~/Desktop/interaction1.png", ., width = 4, height = 4.5)  

parallel_slopes_plot %>% 
  ggsave("~/Desktop/parallel1.png", .,  width = 4, height = 4.5)  



bootstrap_distribution <- pennies_sample %>% 
  specify(response = year) %>% 
  generate(reps = 1000) %>% 
  calculate(stat = "mean")
(
  visualize(bootstrap_distribution) +
    labs(x = "mean year", y = "# of replicates", 
         title = "Bootstrap distribution")
) %>% 
  ggsave("~/Desktop/bootstrap.png", .,  width = 6/1.5, height = 4.5/1.5)  

