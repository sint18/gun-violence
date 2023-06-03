# Top 10 states with most gun-related incidents (2013-2018)

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)

excel_data <- readxl::read_excel("gun_violence_data.xlsx")

all_incidents <- excel_data %>% 
  group_by(state) %>% 
  summarise(total_incidents = n()) %>% 
  arrange(desc(total_incidents))

# cleaned_incidents includes cleaned data in participant_gender without any gibberish
# Contains only (male, female, unknown)
cleaned_incidents <- excel_data %>%
  select(incident_id, state, participant_gender) %>% 
  separate_longer_delim(participant_gender, delim = stringr::regex('\\|\\||[:digit:]::|\\|')) %>%
  mutate(participant_gender = replace_na(participant_gender, 'Unknown')) %>%
  filter(complete.cases(.), participant_gender %in% c('Female', 'Male', 'Unknown'))

male_incidents <- cleaned_incidents %>% 
  filter(participant_gender == 'Male') %>% 
  group_by(state) %>% 
  summarise(male_incidents = n_distinct(incident_id)) %>% 
  arrange(desc(male_incidents))

female_incidents <- cleaned_incidents %>% 
  filter(participant_gender == 'Female') %>% 
  group_by(state) %>% 
  summarise(female_incidents = n_distinct(incident_id)) %>% 
  arrange(desc(female_incidents))

all_incidents <- list(all_incidents, male_incidents, female_incidents) %>% 
  reduce(left_join, by = 'state')

final_dataset <- slice_head(all_incidents, n = 10)

ggplot(data = final_dataset,
       mapping = aes(x = state,
                     y = total_incidents,
                     fill = factor(1)))+
  geom_col(alpha = 0.5)+
  geom_col(mapping = aes(y = male_incidents,
                         fill = factor(2)),
           alpha = 0.5)+
  geom_col(mapping = aes(y = female_incidents,
                         fill = factor(3)),
           alpha = 0.7)+
  scale_fill_discrete(labels = c('Unknown', 'Male', 'Female'))+
  guides(fill = guide_legend('Gender'))+
  labs(title = 'Total incidents broken down by gender of participants in top 10 states in the US (2013-2018)',
       x = 'States',
       y = 'Total Incidents')+
  theme_bw()


