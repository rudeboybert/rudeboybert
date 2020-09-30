library(tidyverse)
library(here)
library(lubridate)
library(tsibble)


# Plot all trees ------------------------------------------------------------
all_trees <- "static/talk/data/scbi.stem3.csv" %>% 
  here() %>% 
  read_csv()
all_trees
ggplot(all_trees, aes(x = gx, y = gy, col = sp)) +
  geom_point(size = 0.5) +
  coord_fixed() +
  labs(x = "x-coordinate (meters)", y = "y-coordinate (meters)", col = "species",
       title = "Census 2018: 72,555 cataloged trees") +
  annotate("point", x = 147, y = 620, size = 4)
ggsave(filename = "static/talk/figure/2020-09-30_ES&P_Fig0.png", width = 4*2, height = 5*2)

all_trees %>% 
  filter(tag == 082422)



# 5-parameter model ------------------------------------------------------------
generalized_logistic_function <- function(params, doy) {
  L <- params[1]
  K <- params[2]
  doy.ip <- params[3]
  r <- params[4]
  theta <- params[5]
  dbh <- L + ((K - L) / (1 + 1/theta * exp(-(r * (doy - doy.ip) / theta)) ^ theta))
  return(dbh)
}

K <- 13
L <- 15
doy.ip <- 200
r <- 0.075
theta <- 1
sigma <- 0.05
params <- c(K, L, doy.ip, r, theta)

set.seed(79)
observed_values <- tibble(
  doy = seq(from = 1, to = 365, by = 5),
  dbh = generalized_logistic_function(params, doy)
) %>%
  mutate(dbh = dbh + rnorm(n(), sd = sigma))

# Not a great fit!
base_plot <- ggplot() +
  geom_point(data = observed_values, mapping = aes(x = doy, y = dbh)) +
  labs(x = "Day of year", y = "Diameter at breast height", title = "Idealized example of dendroband measurements")

base_plot
ggsave(filename = "static/talk/figure/2020-09-30_ES&P_Fig1.png",width = 8, height = 4.5)

base_plot +
  geom_hline(yintercept = K, linetype = "dashed", size = 0.75)
ggsave(filename = "static/talk/figure/2020-09-30_ES&P_Fig2.png", width = 8, height = 4.5)

base_plot +
  geom_hline(yintercept = K, linetype = "dashed", size = 0.75) + 
  geom_hline(yintercept = L, linetype = "dashed", size = 0.75)
ggsave(filename = "static/talk/figure/2020-09-30_ES&P_Fig3.png",width = 8, height = 4.5)

base_plot +
  geom_hline(yintercept = K, linetype = "dashed", size = 0.75) + 
  geom_hline(yintercept = L, linetype = "dashed", size = 0.75) +
  geom_vline(xintercept = doy.ip, linetype = "dashed", size = 0.75) 
ggsave(filename = "static/talk/figure/2020-09-30_ES&P_Fig4.png",width = 8, height = 4.5)

base_plot +
  geom_hline(yintercept = K, linetype = "dashed", size = 0.75) + 
  geom_hline(yintercept = L, linetype = "dashed", size = 0.75) +
  geom_vline(xintercept = doy.ip, linetype = "dashed", size = 0.75) +
  geom_abline(slope = r, intercept = 14 - r*doy.ip, size = 0.75)
ggsave(filename = "static/talk/figure/2020-09-30_ES&P_Fig5.png",width = 8, height = 4.5)

base_plot +
  geom_hline(yintercept = K, linetype = "dashed", size = 0.75, col = "grey") + 
  geom_hline(yintercept = L, linetype = "dashed", size = 0.75, col = "grey") +
  geom_vline(xintercept = doy.ip, linetype = "dashed", size = 0.75, col = "grey") +
  geom_abline(slope = r, intercept = 14 - r*doy.ip, size = 0.75, col = "grey") +
  stat_function(fun = generalized_logistic_function, args = list(params = params), col = "red", n = 500, size = 1) 
ggsave(filename = "static/talk/figure/2020-09-30_ES&P_Fig6.png",width = 8, height = 4.5)



# Time series ------------------------------------------------------------
# Copied from 2020/9/25 Friday SCBI talk

litu_stem <- "static/talk/data/all_stems.csv" %>% 
  here() %>% 
  read_csv() %>%
  # Create date variable
  unite("date", c(year, month, day), sep = "-") %>%
  mutate(date = ymd(date)) %>%
  filter(tag %in% c(082422)) %>%
  dplyr::select(tag, date, measure) %>%
  # Convert to tsibble = time series tibble data type
  # Not sure what to make of regular vs irregular
  as_tsibble(index = date, regular = TRUE) %>%
  # Create growth variable by taking differences in measurements
  mutate(
    measure_before = lag(measure),
    growth = measure - measure_before
  )

ggplot(litu_stem, aes(x = date, y = measure)) +
  geom_line() + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = "date", y = "measurement", title = "Tulip Poplar: Tag 082422")
ggsave(filename = "static/talk/figure/2020-09-30_ES&P_Fig7.png",width = 8, height = 4.5)

litu_stem_growth_plot <- 
  ggplot(litu_stem, aes(x = date, y = growth)) +
  geom_hline(yintercept = 0, linetype = "dashed", col = "grey") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = "date", y = "growth", title = "Tulip Poplar: Tag 082422") +
  geom_line()
litu_stem_growth_plot
ggsave(filename = "static/talk/figure/2020-09-30_ES&P_Fig8.png",width = 8, height = 4.5)

litu_stem_growth_plot +
  geom_smooth(se = FALSE, span = 0.1) + 
  geom_smooth(se = FALSE, method = "lm", col = "red")
ggsave(filename = "static/talk/figure/2020-09-30_ES&P_Fig9.png",width = 8, height = 4.5)

litu_stem_growth_plot +
  geom_vline(xintercept = as.Date("2017-07-01"), linetype = "dashed", col = "red", size = 1)
ggsave(filename = "static/talk/figure/2020-09-30_ES&P_Fig10.png",width = 8, height = 4.5)

litu_stem_growth_plot +
  geom_vline(xintercept = as.Date("2020-07-01"), linetype = "dashed", col = "red", size = 1) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c(NA, "2020-08-01")))
ggsave(filename = "static/talk/figure/2020-09-30_ES&P_Fig11.png",width = 8, height = 4.5)





