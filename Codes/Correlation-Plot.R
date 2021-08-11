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


# final stage

names(final_combined)[3] <- "Secular Constitutions"
names(final_combined)[4] <- "Official State Support"
names(final_combined)[5] <- "Restriction on Religious Parties"
names(final_combined)[6] <- "Number of Attacks"
names(final_combined)[7] <- "Number of Killed People"
names(final_combined)[8] <- "Military Spending"
names(final_combined)[9] <- "Literacy Rate"
names(final_combined)[10] <- "GDP per capita"
names(final_combined)[11] <- "Polity Index"
names(final_combined)[12] <- "State Fragility Index"


corr3 <- final_combined %>%
  dplyr::select("Restriction on Religious Parties",
                "Number of Attacks", "Number of Killed People", "Military Spending", "Literacy Rate",
                "GDP per capita", "Polity Index", "State Fragility Index") %>%
  drop_na()


#correlation plot
r4 <- cor(corr3, use="complete.obs")
ggcorrplot(r4, 
           hc.order = TRUE, 
           type = "lower",
           lab = TRUE) +
  ggtitle ("Correlation Analysis")





