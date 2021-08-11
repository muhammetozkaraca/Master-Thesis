final_combined <- read_xlsx("final_combined_regression.xlsx")

final_combined <- final_combined %>%
  mutate(secular_constitution = as.factor(secular_constitution))

final_combined$total_killed[is.na(final_combined$total_killed)] <- 0
final_combined$number_of_attacks[is.na(final_combined$number_of_attacks)] <- 0

summary(final_combined)
str(final_combined)
final_combined$secular_constitution


levels(final_combined$secular_constitution) <- 
  list("No Official Rel."  = "0", 
       "Multiple Rel." = "1",
       "One Official Rel." = "2")


number_of_attacks <- final_combined %>%
  drop_na(secular_constitution) %>%
  ggplot(aes(x=secular_constitution, y=number_of_attacks, color=secular_constitution)) +
  geom_boxplot() +
  scale_fill_viridis(alpha = 0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)) +
  ggtitle("Number of attacks based on Constitution Types") +
  xlab("Types of Constitution") +
  ylab("Number of Attacks") +
  labs(color = "secular_constitution")



total_killed <- final_combined %>%
  drop_na(secular_constitution) %>%
  drop_na(total_killed) %>%
  ggplot(aes(x=secular_constitution, y=total_killed, color=secular_constitution)) +
  geom_boxplot() +
  scale_fill_viridis(alpha = 0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Number of killed people based on Constitution Types") +
  xlab("Types of Constitution") +
  ylab("Number of killed people") +
  labs(color = "secular_constitution")




final_box_plot <- number_of_attacks + total_killed
# ggsave("final_box_plot.png", width = 10, height = 6, dpi = "screen")
