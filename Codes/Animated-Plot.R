##########################################################################################
################## ANIMATED PLOT ##########################################################
##########################################################################################
p <- ggplot() +
  borders("world", colour = "gray90", fill = "gray85") +
  theme_map() + 
  geom_point(data = rt_events_gtd3, aes(x=longitude, y=latitude, size=nkill, color=nkill, 
                                        alpha = 0.5)) + 
  labs(title = "Date: {frame_time}") +
  transition_time(iyear) +
  ease_aes("linear")

animate(p)

rt_events_gtd3 %>%
  arrange(nkill) %>% 
  mutate(eventid=factor(eventid, unique(eventid))) %>% 
  ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="#69b3a2", alpha=0.8) +
  geom_point(data = rt_events_gtd3, aes(x=longitude, y=latitude, size=nkill, color=nkill), 
             shape = 20, stroke = FALSE,
             show.legend = FALSE) + 
  scale_size_continuous(range=c(1,12), breaks=mybreaks) +
  scale_alpha_continuous(range=c(0.1, .9), breaks=mybreaks) +
  scale_color_viridis(option="viridis", breaks=mybreaks, name = "Number of Killings") +
  theme_void() +
  transition_time(iyear)

p + facet_wrap(~`period`) +
  transition_time(iyear) +
  labs(title = "Year: {frame_time}")