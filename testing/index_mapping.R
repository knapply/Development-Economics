library(ggmap)
library(scales)

country_data <- combo_norm %>%
  
# country_data <- WBD_weighted %>%
  # rename(`Russia` = `Russian Federation`,
  # `Syria` = `Syrian Arab Republic`) %>%
  group_by(`Country Name`) %>%
  # summarise(Score = sum(
  #   `Improved water source`,
  #   `Life expectancy at birth`,
  #   `Urban population growth`)) %>%
  rename(Country = `Country Name`)
  
  
Countries <- recode(country_data$Country, 
                    "Russian Federation" = "Russia", "Syrian Arab Republic" = "Syria")
country_data$Country <- Countries

map_world <- map_data(map = "world")


total_map <- map_world %>%
  inner_join(country_data, by = c("region" = "Country"))

mp <- NULL

mp <- ggplot() +
  geom_polygon(data = total_map,
               aes(x = long, y = lat, 
                   group = group, 
                   fill = Score), 
               color = "black", size=0.2) +
  geom_polygon(data = map_world,
               aes(x = long, y = lat, group = group),
               alpha = 0.1, color = "black") +
  coord_map() +
  scale_fill_distiller(type="seq", 
                       palette = "OrRd",
                       breaks=pretty_breaks(n=10)) +
  coord_fixed()

mp

