library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggrepel)

excel_data <- readxl::read_excel("gun_violence_data.xlsx")

category <- c('Domestic Violence', 'Mass Murder/Shooting', 'School Shooting', 'Suicide', 'Home Invasion', 'Drug/Gang Involvement', 'Accident', 'Armed Robbery')

categorised_data <- excel_data %>%
  select(incident_id, state, incident_characteristics) %>% 
  mutate(incident_characteristics = ifelse(grepl('Domestic Violence', incident_characteristics), category[1], incident_characteristics)) %>% 
  mutate(incident_characteristics = ifelse(grepl('Mass Murder|Mass Shooting', incident_characteristics), category[2], incident_characteristics)) %>% 
  mutate(incident_characteristics = ifelse(grepl('School Shooting', incident_characteristics), category[3], incident_characteristics)) %>% 
  mutate(incident_characteristics = ifelse(grepl('Suicide', incident_characteristics), category[4], incident_characteristics)) %>% 
  mutate(incident_characteristics = ifelse(grepl('Home Invasion', incident_characteristics), category[5], incident_characteristics)) %>% 
  mutate(incident_characteristics = ifelse(grepl('Drug|Gang', incident_characteristics), category[6], incident_characteristics)) %>%
  mutate(incident_characteristics = ifelse(grepl('Accidental', incident_characteristics), category[7], incident_characteristics)) %>% 
  mutate(incident_characteristics = ifelse(grepl('Armed robbery', incident_characteristics), category[8], incident_characteristics))

other_incidents <- categorised_data %>% 
  filter(!incident_characteristics %in% category) %>% 
  mutate(incident_characteristics = 'Others')

categorised_data <- categorised_data %>% 
  filter(incident_characteristics %in% category)

final_df <- categorised_data %>% 
  bind_rows(other_incidents) %>% 
  group_by(incident_characteristics) %>% 
  summarise(count = n())

percent_df <- final_df %>% 
  mutate(perc = count / sum(count)) %>%
  arrange(perc) %>%
  mutate(perc = scales::percent(perc))

ggplot(data = percent_df,
       mapping = aes(x = '', y = perc, fill = incident_characteristics))+
  geom_col(width = 2, colour = 'black')+
  coord_polar(theta = 'y', start = 0)+
  scale_fill_brewer(palette = 'Pastel1')+
  geom_text(mapping = aes(label = perc), position = position_stack(vjust = 0.5))+
  guides(fill = guide_legend(title = "Types of Incidents"))+
  labs(title = 'Types of gun-related incident in the US (2013-2018)')+
  theme_void()
  
  
  

