library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(readxl)
library(maps)
library(ggrepel)
library(ggmap)
library(viridis)
library(writexl)
library(hrbrthemes)
library(stringdist)
library(fuzzyjoin)
library(kableExtra)
library(gganimate)
library(gifski)
 
edtg_data <- read_excel("EDTG_Data.xls") 
religious_terrorism <- subset(edtg_data, rel == 1)                    # Subsetting the data
# write_xlsx(religious_terrorism, "religious_terrorism.xlsx")
# Tidy the base column by hand in religious terrorism_data. Only take the first country in base column.

tidied_religious_terrorism_edtg <- read_excel("religious_terrorism.xlsx") 
groups_names  <- distinct(religious_terrorism, gname, .keep_all = FALSE) # 203 groups exist in
country_names <- distinct(tidied_religious_terrorism_edtg, base, .keep_all = FALSE) # 42 country


turkey <- tidied_religious_terrorism_edtg %>%                        
  filter (base == "Turkey")

unique(turkey$gname)                                            # 2 groups exist in Turkey


unique_groups <- distinct(tidied_religious_terrorism_edtg, gname, .keep_all = TRUE)


########################################################################
################## EDTG - DATA VISUALISATIONS ##########################
########################################################################

countries_data <- unique_groups %>% 
  group_by(base)

number_of_countries <- countries_data %>% 
  dplyr::summarise(freq = n())

dt_number_of_countries <- as.data.frame(number_of_countries)
dt_number_of_countries$base[6] <- "Democratic Republic of the Congo"
dt_number_of_countries$base[38] <- "USA"
dt_number_of_countries$base[41] <- "Palestine"

world <- map_data("world")
countries <- map_data("world", region = dt_number_of_countries$base)



names(dt_number_of_countries)[1] <- "region"
geocoded_countries <- left_join(countries, dt_number_of_countries, by = "region")

### Polygon Map


ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill="#69b3a2", alpha = 0.7)+
  geom_polygon(data = geocoded_countries, aes(x = long, y = lat, group = group, fill = freq))+
  scale_fill_viridis(name = "Numbers") +
  scale_color_viridis(discrete = TRUE, option = "D") +
  theme_void()+
  theme(
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    legend.position = c(0.95,0.60), plot.title = element_text(hjust = 0.5),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.title = element_text(family="Arial Narrow",color="gray35"),
    legend.text = element_text(family="Arial Narrow",color="gray35"),
    legend.key = element_rect()) +
  ggtitle ("Number of Religious Terror Groups in Each Country") 

# ggsave("number_of_terror_groups.png", width = 10, height = 6, dpi = "screen")


### Facetted Map


rt_events_gtd2$period[rt_events_gtd2$iyear > 1969  & rt_events_gtd2$iyear < 1980] = "1970-1979"
rt_events_gtd2$period[rt_events_gtd2$iyear > 1979  & rt_events_gtd2$iyear < 1990] = "1980-1989"
rt_events_gtd2$period[rt_events_gtd2$iyear > 1989  & rt_events_gtd2$iyear < 2000] = "1990-1999"
rt_events_gtd2$period[rt_events_gtd2$iyear > 1999  & rt_events_gtd2$iyear < 2010] = "2000-2009"
rt_events_gtd2$period[rt_events_gtd2$iyear > 2009  & rt_events_gtd2$iyear < 2020] = "2010-2019"

mybreaks <- c(150, 300, 450, 600, 1500)
summary(rt_events_gtd2$nkill)
boxplot(rt_events_gtd2$nkill)


rt_events_gtd2 %>%
  arrange(nkill) %>% 
  mutate(eventid=factor(eventid, unique(eventid))) %>% 
  ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="#69b3a2", alpha=0.8) +
  geom_point(data = rt_events_gtd2, aes(x=longitude, y=latitude, size=nkill, color=nkill), 
             shape = 20, stroke = FALSE) + 
  scale_size_continuous(range=c(1,12), breaks=mybreaks) +
  scale_alpha_continuous(range=c(0.1, .9), breaks=mybreaks) +
  scale_color_viridis(option="viridis", breaks=mybreaks, name = "Number of Killings") +
  theme_void() +
  guides( colour = guide_legend()) +
  ggtitle("Religious Terror Attacks Worldwide (1970-2019)") +
  theme(
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    legend.position = "none",
    text = element_text(color = "#22211d"),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size= 14, hjust=0.5, vjust = 0.7, 
    )) +
  facet_wrap(~`period`)

# ggsave("facetted.png", width = 14, height = 6, dpi = "screen")



