# packages 
library(readr)
library(dplyr)
library(ggplot2)
library(here)

# Task 1: Load data into R and combine the two datasets
finch_beaks_1975 <- read_csv("data/finch_beaks_1975.csv")
finch_beaks_2012 <- read_csv("data/finch_beaks_2012.csv")
glimpse(finch_beaks_1975)
glimpse(finch_beaks_2012)

# use the same names in both
names(finch_beaks_1975) <- c("band", "species", 
                             "beak_length", "beak_depth")
names(finch_beaks_2012) <- c("band", "species", 
                             "beak_length", "beak_depth")

# combine
finches<-bind_rows(
  finch_beaks_1975 %>% 
    mutate(year="1975"),
  finch_beaks_2012 %>% 
    mutate(year="2012"))

# Task 2: Do you see any data quality issues? 
# Can you address them before continuing on with further analysis?

# we have some missing values   
finches %>% 
  summarise(missing_beak_length = sum(is.na(beak_length)),
            missing_beak_depth = sum(is.na(beak_depth)))
# will drop any records with a missing value
finches <- finches %>% 
  filter(!is.na(beak_length) & 
         !is.na(beak_depth))

# Task 3: How did beak length change from 1975 to 2012?
finches %>% 
  group_by(year) %>% 
  summarise(mean_beak_length = mean(beak_length))

finches %>% 
  ggplot(aes(x=year, y=beak_length)) + 
  facet_grid(. ~ species) +
  geom_boxplot() +
  theme_bw()
   
# Task 4: How did beak depth change from 1975 to 2012?
finches %>% 
  group_by(year) %>% 
  summarise(mean_beak_depth = mean(beak_depth))
   
finches %>% 
  ggplot(aes(x=year, y=beak_depth)) + 
  facet_grid(. ~ species) +
  geom_boxplot() +
  theme_bw()

# Task 5: Is there a correlation between beak length and depth? Has it changed over time?
finches %>% 
  ggplot(aes(x=beak_length, y=beak_depth)) +
  facet_grid(species ~ .) +
  geom_point()  + 
  geom_smooth(method='lm') +
  theme_bw()
   
# Task 6: Make a function to import and combine the two datasets into a single tibble
get_finches <- function(dir){
  finch_beaks_1975 <- read_csv(here::here(dir,"finch_beaks_1975.csv"),
                               show_col_types = FALSE)
  finch_beaks_2012 <- read_csv(here::here(dir,"finch_beaks_2012.csv"),
                               show_col_types = FALSE)
  names(finch_beaks_1975) <- c("band", "species", 
                               "beak_length", "beak_depth")
  names(finch_beaks_2012) <- c("band", "species", 
                               "beak_length", "beak_depth")
  finches<-bind_rows(
    finch_beaks_1975 %>% 
      mutate(year="1975"),
    finch_beaks_2012 %>% 
      mutate(year="2012"))
  return(finches)
}
get_finches(dir=here("data"))


# Task 7: Make a function to summarise the change in either beak length change or beak depth change
summarise_finches <- function(var){
  finches %>% 
    group_by(year, species) %>% 
    summarise(min = min({{ var }}, na.rm=TRUE),
              mean = mean({{ var }}, na.rm=TRUE),
              median = quantile({{ var }}, 0.5, na.rm=TRUE),
              max = max({{ var }}, na.rm=TRUE))
}
summarise_finches(beak_length)
summarise_finches(beak_depth)



