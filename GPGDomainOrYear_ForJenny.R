library(shiny)
library(dplyr)
library(tidyverse)
library(ggplot2)

#Load datasets

data <- read_csv("pay_gap_Europe.csv", show_col_types = FALSE)
colnames(data) <- c("region","year","GDP","Urban population","Industry","Business","Mining", 
                    "Manufacturing","Electricity supply","Water supply","Construction",
                    "Retail","Transportation","Accommodation","Information",
                    "Financial","Real estate","Science",
                    "Administrative","Public administration","Education",
                    "Health","Arts","Other")

tidyData <- data %>%
  pivot_longer(cols = -c("region","year","GDP","Urban population"), names_to = 'Domain', values_to = 'GPG')

tidyData2 <- tidyData %>%
  mutate(JobSectors = case_when(Domain %in% c('Retail') ~ 'Trade and commerce',
                                Domain %in% c('Manufacturing') ~ 'Manufacturing and production',
                                Domain %in% c('Electricty supply', 'Water supply','Mining', 'Construction') ~ 'Primary Industry and Infrastructure',
                                Domain %in% c("Business","Transportation","Accommodation","Information",
                                              "Financial","Real estate","Science",
                                              "Administrative") ~ 'Service and information',
                                is.na(Domain) ~ 'Public sector and social services',
                                TRUE ~ 'Others'
  )) %>%
  filter(region == 'France') %>%
  group_by(JobSectors, year) %>%
  summarize(GPG=mean(GPG,na.rm = TRUE))
tidyData2 %>%
  ggplot() +
  geom_point(aes(x = year, y = GPG, color = (JobSectors), group= JobSectors)) +
  geom_line(aes(x = year, y = GPG, color = (JobSectors), group= JobSectors)) +
  facet_wrap(~region)
  theme_bw()

