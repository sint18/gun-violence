source("global.R", local = TRUE)
# Prepare the data
df <- excel_data %>%
  select(state, n_killed) %>% 
  group_by(state) %>% 
  summarise(deaths = sum(n_killed)) %>% 
  mutate(state = tolower(state))

# Get map data
map_base <- map_data('state')

# combine map data and actual data
final_map <- inner_join(map_base, df, by = c('region' = 'state'))

# Plotting
plot <- ggplot()+
  geom_map(data = map_base,
           map = map_base,
           aes(x = long,
               y = lat,
               map_id = region),
           colour = 'white',
           fill = 'thistle1')+
  geom_map(data = df,
           map = map_base,
           aes(fill = deaths, map_id = state),
           colour = 'black')+
  scale_fill_continuous(low='white', high='darkred')+
  labs(title = 'Heatmap of gun-related incidents in US (2015-2018)',
       x = NULL,
       y = NULL,
       fill = 'Deaths')+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

# Change to plotly, so users can hover on the states and view the exact values
ggplotly(plot)




