library(tidyverse)
library(forestecology)
library(snakecase)

# Big Woods
# Read in census data from 2008 & 2014
bw_2008 <- 
  "https://deepblue.lib.umich.edu/data/downloads/z603qx485" %>% 
  read_delim(delim = "\t") %>% 
  mutate(spcode = to_any_case(spcode)) %>%
  select(
    treeID = treeid, stemID = stemtag, sp = spcode, quadrat, gx, gy, dbh, 
    date, codes
  )
bw_2014 <- 
  "https://deepblue.lib.umich.edu/data/downloads/1831ck00f" %>% 
  read_delim(delim = "\t") %>% 
  mutate(spcode = to_any_case(spcode)) %>%
  select(
    treeID = treeid, stemID = stemtag, sp = spcode, quadrat, gx, gy, dbh, 
    date, codes
  )

# Read in grouping classification data
bw_species <- 
  "https://deepblue.lib.umich.edu/data/downloads/000000086" %>% 
  read_delim(delim = "\t") %>% 
  # convert all to snake case:
  mutate_at(c("species", "genus", "family", "idlevel", "spcode"), to_any_case) %>% 
  # join trait group
  left_join(families, by = c("spcode", "family")) %>% 
  mutate(
    sp = str_sub(genus, 1, 2), 
    sp = str_c(sp, str_sub(species, 1, 2)),
    sp = tolower(sp),
    latin = str_c(genus, species, sep = " "),
    latin = to_any_case(latin)
  ) %>% 
  select(sp = spcode, genus, species, latin, family, trait_group)

bw_2008 <- bw_2008 %>%
  left_join(bw_species,by='sp')



census_df1 <- bw_2008
# we need to filter out the resprouts
census_df2 <- bw_2014 %>% 
  filter(!str_detect(codes, 'R'))
id <- "treeID"


set.seed(76)
bw <- census_df1 %>% 
  select(treeID, dbh08 = dbh, trait_group, family, latin) %>% 
  left_join(census_df2, by = "treeID") %>% 
  rename(dbh14 = dbh) %>% 
  mutate(avg_growth = (dbh14 - dbh08)/6) %>% 
  unite("coord", gx:gy, remove = FALSE, sep = ", ") %>% 
  mutate(coord = str_c("(", coord, ")")) %>% 
  select(treeID, sp, latin, family, trait_group, dbh08, dbh14, avg_growth, coord) %>% 
  arrange(treeID) %>% 
  na.omit() %>% 
  sample_n(6) %>% 
  mutate(treeID = 1:n())

bw

bw %>% 
  select(-c(latin, family, trait_group))

bw %>% 
  select(-c(latin, family, trait_group)) %>% 
  sample_frac(1)



bw_growth_df <- 
  # Merge both censuses and compute growth:
  compute_growth(census_df1, census_df2, id) %>% 
  mutate(sp = to_any_case(sp))