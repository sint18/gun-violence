# Total Number of Incidents in the US by year (2013-2018)
source("global.R", local = TRUE)
# Prepare the data
df <- excel_data %>%
  group_by(year = floor_date(incident_date, unit = "year")) %>%
  summarise(total_incidents = n(),
            deaths = sum(n_killed))

# Plotting
ggplot(data = df,
       mapping = aes(x = year(year),
                     y = total_incidents))+
  geom_point(mapping = aes(size = deaths,
                           colour = 'red'))+
  geom_smooth(method = loess)+
  geom_text(mapping = aes(label = paste(deaths, 'deaths')),
            hjust = 0.5,
            vjust = -2)+
  scale_size(range = c(2, 18))+
  labs(title = 'Total number of incidents and deaths in the US',
       x = 'Year',
       y = 'Number of Incidents')+
  guides(colour = FALSE, alpha = FALSE, size = FALSE)+
  scale_y_continuous(labels=scales::comma)+
  theme_bw()

