
library(readxl)
library(dplyr)
library(ggplot2)
library(maps)

excel_data <- readxl::read_excel("gun_violence_data.xlsx")

df <- excel_data %>%
  select(incident_date, state) %>% 
  group_by(year = floor_date(incident_date, unit = "year"), state) %>% 
  summarise(total_incidents = n()) %>% 
  mutate(state = tolower(state))

map_base <- map_data('state')

final_map <- inner_join(map_base, df, by = c('region' = 'state'))

# ggplot()+
#   geom_polygon(data = final_map,
#                mapping = aes(x = long,
#                              y = lat,
#                              group = group,
#                              fill = total_incidents),
#                colour = 'white')+
#   scale_fill_continuous(low='thistle2', high='darkred', 
#                         guide='colorbar')

ggplot()+
  geom_map(data = map_base,
           map = map_base,
           aes(x = long,
               y = lat,
               map_id = region),
           colour = 'white',
           fill = 'thistle2')+
  geom_map(data = df,
           map = map_base,
           aes(fill = total_incidents, map_id = state),
           colour = 'white')+
  scale_fill_continuous(low='thistle2', high='darkred')+
  facet_wrap(vars(year))




