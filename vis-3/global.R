library(dplyr)
library(ggplot2)
library(maps)
library(plotly)
library(readxl)
library(ggrepel)
library(lubridate)
library(shiny)
library(stringr)
library(tidyverse)
library(scales)

df <- readxl::read_excel("gun_violence_curated.xlsx") %>% 
  filter(incident_date > '2014-12-31') %>%
  select(incident_date, state, n_injured) %>% 
  group_by(year = floor_date(incident_date, unit = 'year'), state) %>%
  summarise(total_incidents = n(),
            injured = sum(n_injured),
            .groups = 'drop') %>%
  mutate(year = year(year)) %>% 
  arrange(desc(total_incidents))