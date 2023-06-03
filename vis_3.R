library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)

#show label
# year, all
# cities or states
excel_data <- readxl::read_excel("gun_violence_data.xlsx")

# dataframe for grouping with year
df <- excel_data %>% 
  group_by(year = floor_date(incident_date, unit = "year")) %>% 
  mutate(year = year(year)) %>% 
  summarise(total_incidents = n(),
            killed = sum(n_killed),
            injured = sum(n_injured)) %>% 
  arrange(desc(total_incidents),
          desc(killed),
          desc(injured))

# dataframe without year
df <- excel_data %>% 
  group_by(state) %>% 
  summarise(total_incidents = n(),
            killed = sum(n_killed),
            injured = sum(n_injured)) %>% 
  arrange(desc(total_incidents),
          desc(killed),
          desc(injured))

# dataframe with year and state
df <- excel_data %>% 
  group_by(state, year = floor_date(incident_date, unit = "year")) %>% 
  summarise(total_incidents = n(),
            killed = sum(n_killed),
            injured = sum(n_injured)) %>% 
  arrange(desc(total_incidents),
          desc(killed),
          desc(injured))

top_10 <- df %>%
  slice_head(n = 10)

bot_10 <- df %>% 
  slice_tail(n = 10)

df <- top_10

df <- bind_rows(top_10, bot_10)

#dataframe with cities
df <- excel_data %>% 
  group_by(city_or_county) %>% 
  summarise(total_incidents = n(),
            killed = sum(n_killed),
            injured = sum(n_injured)) %>% 
  arrange(desc(total_incidents),
          desc(killed),
          desc(injured))

ggplot(data = df,
       mapping = aes(x = total_incidents,
                     y = injured))+
  geom_point(mapping = aes(size = killed, colour = city_or_county),
             alpha = 0.5)+
  scale_size(range = c(2,18))+
  # geom_text(mapping = aes(label = city_or_county),
  #           hjust = 0.5,
  #           vjust = -1)+
  labs(x = 'Total Incidents',
       y = 'Injured')+
  guides(colour = FALSE, alpha = FALSE, size = FALSE)+
  theme_bw() -> plot
ggplotly(plot)

ggplot(data = df,
       mapping = aes(x = total_incidents,
                     y = injured))+
  geom_point(mapping = aes(size = killed, colour = state),
             alpha = 0.5)+
  scale_size(range = c(2,18))+
  geom_text(mapping = aes(label = state),
            hjust = 0.5,
            vjust = -1)+
  labs(x = 'Total Incidents',
       y = 'Injured')+
  guides(colour = FALSE, alpha = FALSE, size = FALSE)+
  theme_bw() -> plot
ggplotly(plot)

ggplot(data = df,
       mapping = aes(x = total_incidents,
                     y = injured))+
  geom_point(mapping = aes(size = killed),
             colour = 'red',
             alpha = 0.5)+
  geom_smooth(method = loess, se = FALSE)+
  scale_size(range = c(2,18))+
  geom_text(mapping = aes(label = year),
            hjust = 0.5,
            vjust = -1)+
  labs(x = 'Total Incidents',
       y = 'Injured')+
  guides(colour = FALSE, alpha = FALSE, size = FALSE)+
  theme_bw()

