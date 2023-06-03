# Top 10 states with most gun-related incidents (2013-2018)

library(readxl)
library(dplyr)
library(ggplot2)

excel_data <- readxl::read_excel("gun_violence_data.xlsx")

df <- excel_data %>%
  group_by(year = floor_date(incident_date, unit = 'year'), state) %>%
  summarise(total_incidents = n()) %>% 
  arrange(desc(total_incidents))

final_df <- slice_head(df, n = 10)
# safest_10 <- slice_tail(df, n = 10)
# final_df <- rbind(top_10, safest_10)

ggplot(data = final_df,
       mapping = aes(x = year,
                     y = total_incidents))+
  geom_point()+
  geom_line()+
  labs(title = 'Total incidents of top 10 states involving guns over the years 2013-2018',
       x = 'Year',
       y = 'Total Incidents')+
  scale_fill_discrete(name = 'States')+
  facet_wrap(vars(state))+
  theme_bw()

ggplot(data = df)+
  geom_line(mapping = aes(x = year,
                          y = count,
                          by = state,
                          colour = state))
