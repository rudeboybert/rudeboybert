library(tidyverse)
library(janitor)
SDS <- read_csv("~/Downloads/SDS majors and minors over time.csv", skip = 1)  %>%
  rename(type = ...1) %>%
  mutate(
    type = case_when(
      type == "SDS major (double)" ~ "SDS double-major",
      type == "SDS major (single)" ~ "SDS major",
      type == "STS minor" ~ "Applied stats minor",
      type == "SDS minor" ~ "SDS minor"
    )
  ) %>%
  pivot_longer(cols = -type) %>%
  mutate(
    name = str_sub(name, -4, -1) %>% as.numeric(),
    degree = type,
    degree = factor(degree, levels = c("SDS double-major", "SDS major", "SDS minor", "Applied stats minor")),
    type = ifelse(degree %in% c("SDS double-major", "SDS major"), "Majors", "Minors")
    )

ggplot(SDS, aes(x=name, y = value, col = degree)) +
  geom_line() +
  facet_wrap(~type) +
  geom_point() +
  labs(
    title = "Graduating Smith College SDS Students",
    col = "Degree", x = "Graduation year", y = "Number of students"
    ) +
  theme_light() +
  coord_cartesian(ylim = c(0, 30)) +
  # geom_vline(xintercept = 2022.5, linetype = "dashed") +
  theme(panel.grid.minor = element_blank())
