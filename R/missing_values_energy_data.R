library(tidyverse)
library(ggplot2)
library(patchwork)
library(ggpubr)

# Source data
setwd("C:\\Users\\Jannik\\OneDrive\\Columbia\\02_Courses\\GR5702-Data_Visualization\\03_Project\\R")

source("plot_missing_patterns.R")

energy_data <- read.csv("..\\data\\owid_energy_data.csv", header=TRUE, stringsAsFactors=FALSE)
data_country_info <- read.csv("..\\data\\worldbank_country_info.csv", header=TRUE, stringsAsFactors=FALSE)

df_energy <- as.data.frame(energy_data)
df_country_info <- data_country_info

df_country_info <- as.data.frame(data_country_info) %>% 
  rename(iso_code = "ï..Country.Code")

# Calculate share of missing data
df_energy_na <- as.data.frame(round(colSums(is.na(df_energy)) / nrow(df_energy) * 100, 1)) %>% 
  rename(perc_na = 1) %>%
  mutate(variable = rownames(.))

## Highlight variables with missing data over 80 or 70% 
df_energy_na[df_energy_na$perc_na > 80,]$variable
df_energy_na[df_energy_na$perc_na > 70,]$variable

summary(df_energy)

missing(df_energy)

#Draw plots on missing parameter
plt_miss_params <- plot_missing_parameters(df_energy, TRUE)
# plt_miss_params

df_energy_per_capita <- df_energy %>% 
  select(contains(c("per_capita"))) %>% 
  rename_with(~gsub("_per_capita", "", .x))
plt_miss_params_energy_per_capita <- plot_missing_parameters(df_energy_per_capita, TRUE)

df_energy_change_pct <- df_energy %>% 
  select(contains(c("change_pct"))) %>% 
  rename_with(~gsub("_change_pct", "", .x))
plt_miss_params_energy_change_pct <- plot_missing_parameters(df_energy_change_pct, TRUE)

df_energy_consumption <- df_energy %>% 
  select(contains(c("consumption"))) %>% 
  rename_with(~gsub("_consumption", "", .x))
plt_miss_params_consumption <- plot_missing_parameters(df_energy_consumption, TRUE)

df_energy_electricity <- df_energy %>% 
  select(contains(c("electricity"))) %>% 
  rename_with(~gsub("_electricity", "", .x))
plt_miss_params_electricity <- plot_missing_parameters(df_energy_electricity, TRUE)

figure1 <- ggarrange(plt_miss_params_electricity, plt_miss_params_consumption, 
                    plt_miss_params_energy_per_capita, plt_miss_params_energy_change_pct,
                    labels = c("Electricity", "Consumption", "Per Capita", "Percentage Change"),
                    vjust = 1,
                    hjust = -0.6,
                    ncol = 2, 
                    nrow = 2)
figure1

# View data by country
df_by_country <- df_energy %>% mutate(available_perc = 1-rowSums(apply(is.na(df_energy), 2, as.numeric))/ncol(df_energy))
df_by_country <- df_by_country %>% filter(iso_code != "") %>% select(c(iso_code, country, year, available_perc))


df_by_country <- left_join(df_by_country, df_country_info, by = "iso_code") %>%
  select(c(iso_code, country, year, available_perc, Region, IncomeGroup)) %>%
  arrange(Region)

order_country <- unique(df_by_country[, c("country","Region")]) %>% select(country)
levels(df_by_country$country) <- order_country$country

plt_available_years_by_country <- ggplot(df_by_country, aes(x=year, y=available_perc, color=Region)) + 
  geom_col() + 
  ylim(0, 1) +
  facet_wrap(country~.)

plt_available_years_by_country

# View data by region
df_by_region <- left_join(df_by_country, df_country_info, by = "iso_code") %>%
  select(c(Region.x, year, available_perc)) %>%
  group_by(Region.x, year) %>%
  summarise(available_perc = mean(available_perc))

plt_available_years_by_region <- ggplot(df_by_region, aes(x=year, y=available_perc)) + 
  geom_col() + 
  ylim(0, 1) +
  facet_wrap(~Region.x)

plt_available_years_by_region


# Summarise data by year
df_by_year <- df_by_country %>% group_by(year) %>% summarise(available_perc = mean(available_perc))

plt_available_years_avg <- ggplot(df_by_year, aes(x=year, y=available_perc)) + 
  geom_col() + 
  ylim(0, 1)

plt_available_years_avg


## Select years since 2010
df_recent_energy <- df_energy[df_energy$year >= 1980,]

