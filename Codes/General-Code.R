library(dplyr)
library(plyr)
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
library(reshape2)
library(tibble)
library(arsenal) # for comparedf function
library(psych)   # for estimating biserial correlation
library(patchwork)
library(polycor)
library(ggpubr)
library(car)
library(stargazer) # for the tables
library(rstatix)   # for Kruskal effect size
library(scales)
library(lubridate)
library(GGally)
library(corrplot)
library(MASS)
library(magrittr)
library(pscl)
library(ggcorrplot)
library(gclus)
library(kableExtra)
library(gganimate)
library(gifski)


gtd_data <- read_excel('GTD(2019).xlsx')
edtg_data <- read_excel("EDTG_Data.xls") 
data_military <- read_excel("military-index.xls")
literacy_rate <- read_excel("literacy-rate.xls")
gdp_per_capita <- read_excel("gdp-per-capita-current-us.xls") 
polity <- read_excel("polity.xls") 
state_fragility <- read_excel("state-fragility.xls")
ras3_state <- read_excel("Religion-and-State.XLSX")   



religious_terrorism <- subset(edtg_data, rel == 1)                    # Subsetting the data
# write_xlsx(religious_terrorism, "religious_terrorism.xlsx")
# Tidy the base column by hand in religious terrorism_data. Only take the first country in base column.

tidied_religious_terrorism_edtg <- read_excel("religious_terrorism.xlsx") 
groups_names  <- distinct(religious_terrorism, gname, .keep_all = FALSE) # 203 groups exist in
country_names <- distinct(tidied_religious_terrorism_edtg, base, .keep_all = FALSE) # 42 country


turkey <- tidied_religious_terrorism_edtg %>%                        
  filter (base == "Turkey")

unique(turkey$gname)                                            # 2 groups exist in Turkey


##########################################################################################
################## EDTG - GTD COMPARISON ################################################# 
##########################################################################################

# edtg_base <- read_excel('EDTG-subsetted-final.xlsx')
names_whole_gtd <- distinct(gtd_data, gname, .keep_all = FALSE)
names_whole_gtd_dt <- as.data.frame(names_whole_gtd)  

names_edtg_rt <- distinct(tidied_religious_terrorism_edtg, gname, .keep_all = FALSE) # names of the religious terror groups in EDTG                                              

names_edtg_rt_dt <- as.data.frame(names_edtg_rt)              # convert the names into dataframe 

idx1 <- which(gtd_data$gname %in% names_edtg_rt_dt$gname)     # Subset GTD data based on edtg's groups
rt_events_gtd <- gtd_data[idx1,]                              # Finding the specific events

names_gtd_rt <- unique(rt_events_gtd$gname)                   # List the names of RT groups in GTD
names_gtd_rt_dt <- as.data.frame(names_gtd_rt)                # Make it dataframe
names(names_gtd_rt_dt) [1] <- "gname"

diff_before <- setdiff(names_edtg_rt_dt,names_gtd_rt_dt)
diff_names_df_before <- as.data.frame(diff_before)



# Based on the EDTG Data where 203 groups indicated, the GTD has only 153 different coded data. To figure out whether the differences are due to the punctuation problems or something related, I want to find out how similar the groups names in the GTD and in the EDTG datasets are.
final_result <- stringdist_join(names_whole_gtd_dt, names_edtg_rt_dt, by = "gname",
                                mode = "left", ignore_case = FALSE,
                                method = "jw",
                                max_dist = 99, distance_col = "dist") %>% 
  group_by(gname.x) %>%
  slice_min(order_by = dist, n = 1)

# Note that I made the inquiry into the whole GTD dataset, comparing 3617 terror organizations.


final_result$dist <- 1 - final_result$dist
most_similar <- final_result %>% 
  filter(dist > 0.7) %>% 
  filter (dist < 1)

# write_xlsx(most_similar, 'most_similar.xlsx')

# after exploring the data on my own, I tidied the existing EDTG data in line with the punctuation problems I found out in between the GTD and the EDTG. Now, I reload the tidied data into R ennvironment.

tidied_edtg_base <- read_excel('EDTG-tidied.xlsx')
tidied_edtg_names <- tidied_edtg_base$gname
tidied_edtg_names_dt <- as.data.frame(tidied_edtg_names)


idx2 <- which(gtd_data$gname %in% tidied_edtg_base$gname)     # Subset GTD data based on edtg's groups
rt_events_gtd2 <- gtd_data[idx2,]                             # Finding the specific events


names_gtd2 <-unique(rt_events_gtd2$gname)                       # List the names of RT groups
rt_events_gtd_names_dt2 <- as.data.frame(names_gtd2) 
#write_xlsx(rt_events_gtd_names_dt2, 'edteg-gtd.xlsx')
names(tidied_edtg_names_dt) [1] <- "gname"
names(rt_events_gtd_names_dt2) [1] <- "gname"

diff_after <- setdiff(tidied_edtg_names_dt$gname,rt_events_gtd_names_dt2$gname)
diff_names_df_after <- as.data.frame(diff_after)

# after cross-checking, as can be seen, the difference decreased to 15 group from 50.
rt_events_gtd2_1990_2014 <- rt_events_gtd2 %>%
  filter(iyear>1989) %>%
  filter(iyear<2015)

rt_events_gtd2 <- as.data.frame(rt_events_gtd2)
rt_events <- rt_events_gtd2 %>%
  dplyr::select(gname, nkill)

rt_events_gtd2_summarised <- rt_events_gtd2 %>%
  group_by(gname) %>%
  dplyr::summarise(number_of_attacks = n(),
                   number_of_murdered = sum(nkill, na.rm=TRUE)) %>%
  dplyr::select (gname,number_of_attacks,number_of_murdered) 

names(rt_events_gtd2_summarised)[1] <- "Names"
names(rt_events_gtd2_summarised)[2] <- "Number of Attacks"
names(rt_events_gtd2_summarised)[3] <- "Number of Killed People"

# write_xlsx(rt_events_gtd2_summarised, "terrorgroupsnames.xlsx")

##########################################################################################
################## REGRESSİON RAS AND EDTG DATA ANALYSIS ############################################
##########################################################################################

# military
data_military <- data_military %>%
  dplyr::select("Country Name", "1990","1991","1992","1993","1994","1995","1996","1997",
                "1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010",
                "2011","2012","2013","2014")

data_military_long <- data_military %>%
  pivot_longer(!"Country Name", names_to = "years", values_to = "military-spending")

# literacy
literacy_rate <- literacy_rate %>%
  dplyr::select("Country Name", "1990","1991","1992","1993","1994","1995","1996","1997",
         "1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010",
         "2011","2012","2013","2014")

literacy_rate_long <- literacy_rate %>%
  pivot_longer(!"Country Name", names_to = "years", values_to = "literacy-rate")

# gdp
gdp_per_capita <- gdp_per_capita %>%
  dplyr::select("Country Name", "1990","1991","1992","1993","1994","1995","1996","1997",
         "1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010",
         "2011","2012","2013","2014")

gdp_per_capita_long <- gdp_per_capita %>%
  pivot_longer(!"Country Name", names_to = "years", values_to = "gdp_per_capita")

merged <- cbind(data_military_long,
                literacy_rate_long$`literacy-rate`,
                gdp_per_capita_long$gdp_per_capita)


names(merged)[1] <- "country"
names(merged)[2] <- "year"
names(merged)[3] <- "military_spending"
names(merged)[4] <- "literacy_rate"
names(merged)[5] <- "gdp_per_capita"

merged$year <- as.numeric(merged$year)
head(merged)
str(merged)

# polity
polity <- polity %>% 
  filter(year>1989) %>%
  filter(year<2015) %>%
  dplyr::select(country, year, polity) %>%
  filter(polity<11) %>%
  filter(polity>-11)

names(polity)[1] <- "country"
head(polity)

# state-fragility
state_fragility <- state_fragility %>%
  filter(year<2015) %>%
  dplyr::select(country, year, sfi)

head(state_fragility)

# RAS DATA

ras3_state$MX01X1990 # Restrictions on public observance of rel. services,
# festivals and/or holidays, including the Sabbath.

ras3_state$NX01X1990 # Restrictions on religious political parties

ras3_state$SBX1990   # Official Support


# secular constitutions
ras3_data_secular <- ras3_state %>%
  dplyr::rename("country" = "COUNTRY",
                "1990" = "SAX1990", "1991" = "SAX1991", "1992" = "SAX1992", "1993" = "SAX1993",
                "1994" = "SAX1994", "1995" = "SAX1995", "1996" = "SAX1996", "1997" = "SAX1997", 
                "1998" = "SAX1998", "1999" = "SAX1999", "2000" = "SAX2000", "2001" = "SAX2001",
                "2002" = "SAX2002", "2003" = "SAX2003", "2004" = "SAX2004", "2005" = "SAX2005",
                "2006" = "SAX2006", "2007" = "SAX2007", "2008" = "SAX2008", "2009" = "SAX2009",
                "2010" = "SAX2010", "2011" = "SAX2011", "2012" = "SAX2012", "2013" = "SAX2013",
                "2014" = "SAX2014") %>%
  dplyr::select("country", "1990","1991","1992","1993","1994","1995","1996","1997","1998",
                "1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010",
                "2011","2012","2013","2014") %>%
  pivot_longer(!"country", names_to = "year", values_to = "secular_constitution") 

ras3_data_secular$year <- as.numeric(ras3_data_secular$year)


# official support of state

ras3_data_state_support <- ras3_state %>%
  dplyr::rename("country" = "COUNTRY",
                "1990" = "SBX1990", "1991" = "SBX1991", "1992" = "SBX1992", "1993" = "SBX1993",
                "1994" = "SBX1994", "1995" = "SBX1995", "1996" = "SBX1996", "1997" = "SBX1997", 
                "1998" = "SBX1998", "1999" = "SBX1999", "2000" = "SBX2000", "2001" = "SBX2001",
                "2002" = "SBX2002", "2003" = "SBX2003", "2004" = "SBX2004", "2005" = "SBX2005",
                "2006" = "SBX2006", "2007" = "SBX2007", "2008" = "SBX2008", "2009" = "SBX2009",
                "2010" = "SBX2010", "2011" = "SBX2011", "2012" = "SBX2012", "2013" = "SBX2013",
                "2014" = "SBX2014") %>%
  dplyr::select("country", "1990","1991","1992","1993","1994","1995","1996","1997","1998",
                "1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010",
                "2011","2012","2013","2014") %>%
  pivot_longer(!"country", names_to = "year", values_to = "state_support") 

ras3_data_state_support$year <- as.numeric(ras3_data_state_support$year)

# restrictions on religious political parties

ras3_data_religious_parties <- ras3_state %>%
  dplyr::rename("country" = "COUNTRY",
                "1990" = "NX01X1990", "1991" = "NX01X1991", "1992" = "NX01X1992", 
                "1993" = "NX01X1993", "1994" = "NX01X1994", "1995" = "NX01X1995", 
                "1996" = "NX01X1996", "1997" = "NX01X1997", "1998" = "NX01X1998", 
                "1999" = "NX01X1999", "2000" = "NX01X2000", "2001" = "NX01X2001",
                "2002" = "NX01X2002", "2003" = "NX01X2003", "2004" = "NX01X2004", 
                "2005" = "NX01X2005", "2006" = "NX01X2006", "2007" = "NX01X2007", 
                "2008" = "NX01X2008", "2009" = "NX01X2009", "2010" = "NX01X2010", 
                "2011" = "NX01X2011", "2012" = "NX01X2012", "2013" = "NX01X2013",
                "2014" = "NX01X2014") %>%
  dplyr::select("country", "1990","1991","1992","1993","1994","1995","1996","1997","1998",
                "1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010",
                "2011","2012","2013","2014") %>%
  pivot_longer(!"country", names_to = "year", values_to = "restriction_on_rp") 

ras3_data_religious_parties$year <- as.numeric(ras3_data_religious_parties$year)
ras_data_final <- cbind(ras3_data_secular,
                                  ras3_data_state_support$state_support,
                                  ras3_data_religious_parties$restriction_on_rp)
names(ras_data_final)[4] <- "state_support"
names(ras_data_final)[5] <- "restriction_on_rp"

# write_xlsx(ras_subsetted_final , "ras_subsetted.xlsx")

subsetted_gtd <- rt_events_gtd2 %>%
  dplyr::select(iyear, country_txt, nkill) %>%
  filter(iyear>1989) %>%
  filter(iyear<2015) %>%
  group_by(country_txt, iyear) %>%
  dplyr::summarize(number_of_attacks = n(), total_killed = sum(nkill, na.rm=TRUE)) 
subsetted_gtd <- rename(subsetted_gtd,
                        c("iyear" = "year", "country_txt" = "country"))

subsetted_gtd <- as.data.frame(subsetted_gtd)

str(subsetted_gtd)
str(merged)
str(ras_data_final)
str(polity)
str(state_fragility)


merged$country[526:550] <- "Bahamas"
merged$country[726:750] <- "Brunei"
merged$country[1026:1050] <- "Democratic Republic of the Congo"
merged$country[1051:1075] <- "Congo-Brazzaville"
merged$country[1626:1650] <- "Egypt"
merged$country[2101:2125] <- "Gambia"
merged$country[2751:2775] <- "Iran"
merged$country[3001:3025] <- "Kyrgyzstan"
merged$country[3101:3125] <- "South Korea"
merged$country[3176:3200] <- "Laos"
merged$country[3876:3900] <- "Macedonia"
merged$country[4776:4800] <- "North Korea"
merged$country[4851:4875] <- "West Bank and Gaza Strip"
merged$country[5626:5650] <- "Syria"
merged$country[5001:5025] <- "Russia"
merged$country[6301:6325] <- "Venezuela"
merged$country[6501:6525] <- "Yemen"

state_fragility$country[3233:3252] <- "Democratic Republic of the Congo"

polity$country[745:754] <- "Congo-Brazzaville"
polity$country[853:855] <- "Czech Republic"
polity$country[1659:1674] <- "Cote d'Ivoire"
polity$country[2301:2325] <- "Myanmar"
polity$country[3715:3716] <- "Russia"
polity$country[2772:2796] <- "North Korea"
polity$country[2822:2846] <- "South Korea"
polity$country[2993:2996] <- "North Sudan"
polity$country[3566:3590] <- "United Arab Emirates"
polity$country[3832:3842] <- "Democratic Republic of the Congo"

unique(ras_data_final$country)
unique(polity$country)
unique(state_fragility$country)

state_fragility$country[2390:2409] <- "South Korea"
state_fragility$country[2350:2369] <- "North Korea"

# There are differences in country names, I will handle them by hand.

# RAS vs. GTD 
# "South Sudan" in (merged & ras)
# "West Bank and Gaza Strip" (GTD & Merged) ---> Palestinian Authority (West Bank) in (RAS)
# Democratic Republic of the Congo (GTD & State-Fragility) --> Zaire in (RAS)



# Myanmar in (GTD & State Fragility & Merged) ---> Myanmar (Burma) in (RAS & Polity) 
# Cote d'Ivoire (Merged & State_fragility & Polity) ---> Ivory Coast (RAS)
# United Arab Emirates (GTD & Merged & Fragility Index) ---> UAE (RAS & Polity)
# United States (GTD & Merged & Polity & state_fragility) vs. USA (RAS)
# United Kingdom (GTD & Polity & Fragility Index & Merged) --> UK (RAS) 

# write_xlsx(ras_data_final, "ras_data_final.xlsx")
u_ras_data <- read_xlsx("updated_ras_data_final.xlsx")


combined_gtd_ras <- full_join(u_ras_data,subsetted_gtd)
combined_gtd_ras_merged <- full_join(combined_gtd_ras,merged)
combined_gtd_ras_merged_2 <- combined_gtd_ras_merged[1:4575, ] # as the rest is not combined with RAS                                                               # and GTD

a <- combined_gtd_ras_merged %>%
  filter(country == "Democratic Republic of the Congo")


combined_gtd_ras_merged_polity <- full_join(combined_gtd_ras_merged_2, polity, 
                                            by = c("country","year"))

combined_gtd_ras_merged_polity_2 <- combined_gtd_ras_merged_polity[1:4575, ]

combined_gtd_ras_merged_polity_state_fragility <- full_join(combined_gtd_ras_merged_polity_2,
                                                            state_fragility,
                                                            by = c("country","year"))

final_combined <- combined_gtd_ras_merged_polity_state_fragility[1:4575, ]

# write_xlsx(final_combined, "final_combined_regression.xlsx")

final_combined <- final_combined %>%
  mutate(secular_constitution = as.factor(secular_constitution))

final_combined$total_killed[is.na(final_combined$total_killed)] <- 0
final_combined$number_of_attacks[is.na(final_combined$number_of_attacks)] <- 0

summary(final_combined)
str(final_combined)
final_combined$secular_constitution


levels(final_combined$secular_constitution)[levels(final_combined$secular_constitution)=="No Official Rel."] <- "0"
levels(final_combined$secular_constitution)[levels(final_combined$secular_constitution)=="One Official Rel."] <- "2"
levels(final_combined$secular_constitution)[levels(final_combined$secular_constitution)=="Multiple Rel."] <- "1"


# Muslim Majority Countries

muslim_majority <- read.csv("oic_data.csv")
countries_oic <- muslim_majority$Country
countries_oic_dt <- as.data.frame(countries_oic)
names(countries_oic_dt)[1] <- "countries"


unique_final_combined <- unique(final_combined$country)
unique_final_combined_dt <- as.data.frame(unique_final_combined)
names(unique_final_combined_dt)[1] <- "countries"

a <- setdiff(countries_oic_dt, unique_final_combined_dt)
countries_oic_dt$countries[40] <- "West Bank and Gaza Strip"

idx15 <- which(final_combined$country %in% countries_oic_dt$countries)
final_muslim_majority <-  final_combined[idx15,] 


# write_xlsx(final_muslim_majority, "final_muslim_majority.xlsx")


final_combined <- final_combined %>% 
  mutate(specific_hostility = case_when (state_support == 0 ~ 1,
                                         state_support == 1 ~ 1,
                                         state_support == 2 ~ 0,
                                         state_support == 3 ~ 0,
                                         state_support == 4 ~ 0,
                                         state_support == 5 ~ 0,
                                         state_support == 6 ~ 0,
                                         state_support == 7 ~ 0,
                                         state_support == 8 ~ 0,
                                         state_support == 9 ~ 0,
                                         state_support == 10 ~ 0,
                                         state_support == 11 ~ 0,
                                         state_support == 12 ~ 0, 
                                         state_support == 13 ~ 0))



final_combined$specific_hostility <- as.factor(final_combined$specific_hostility)
summary(final_combined$specific_hostility)



# secular constitutions + polity
output1 <-glm.nb(formula = number_of_attacks ~ secular_constitution, 
                 data = final_combined, na.action = na.omit)

print(summary(output1))


# secular constitutions + polity
output2 <-glm.nb(formula = number_of_attacks ~ secular_constitution +  polity,
                 data = final_combined, na.action = na.omit)

print(summary(output2))

# secular constitutions + polity + log(gdp_per_capita)
output3 <-glm.nb(formula = number_of_attacks ~ secular_constitution +  polity +  log(gdp_per_capita),
                 data = final_combined, na.action = na.omit)

print(summary(output3))

# secular constitutions + polity + log(gdp_per_capita) + military spending
output4 <-glm.nb(formula = number_of_attacks ~  secular_constitution + polity + 
                   log(gdp_per_capita) + military_spending, data = final_combined, 
                 na.action = na.omit)

print(summary(output4))


# secular constitutions + polity + log(gdp_per_capita) + military spending + literacy rate


output5 <-glm.nb(formula = number_of_attacks ~  secular_constitution + polity + 
                   log(gdp_per_capita) + military_spending + literacy_rate, 
                 data = final_combined,
                 na.action = na.omit)

print(summary(output5))
plot(output5)



# secular constitutions + polity + log(gdp_per_capita) + sfi + log(gdp_per_capita) +military spending 
output6 <-glm.nb(formula = number_of_attacks ~  secular_constitution + polity + sfi + 
                   log(gdp_per_capita) + military_spending,
                 data = final_combined,
                 na.action = na.omit)

print(summary(output6))



# restriction_on_rp + polity + log(gdp_per_capita) + sfi + log(gdp_per_capita) +military spending
output7 <-glm.nb(formula = number_of_attacks ~  restriction_on_rp + polity + sfi + 
                   log(gdp_per_capita) + military_spending + secular_constitution +
                   specific_hostility,
                   data = final_combined,
                 na.action = na.omit)

print(summary(output7))


# Muslim Majority Countries


muslimmajority <- read_excel("muslimmajority.xlsx")



names(muslimmajority)[3] <- "secular"
names(muslimmajority)[4] <- "statesupport"
names(muslimmajority)[5] <- "restrictiononrp"
names(muslimmajority)[6] <- "numberofattacks"
names(muslimmajority)[7] <- "totalkilled"
names(muslimmajority)[8] <- "military"
names(muslimmajority)[9] <- "literacy"
names(muslimmajority)[10] <- "gdppercapita"

muslimmajority <- as.data.frame(muslimmajority)
muslimmajority$gdppercapita <- log(muslimmajority$gdppercapita)

muslimmajority$secular <- as.numeric(muslimmajority$secular)
muslimmajority$statesupport <- as.factor(muslimmajority$statesupport)




muslimmajority <- muslimmajority %>% 
  mutate(hostility = case_when (statesupport == 0 ~ 1,
                                statesupport == 1 ~ 1,
                                statesupport == 2 ~ 0,
                                statesupport == 3 ~ 0,
                                statesupport == 4 ~ 0,
                                statesupport == 5 ~ 0,
                                statesupport == 6 ~ 0,
                                statesupport == 7 ~ 0,
                                statesupport == 8 ~ 0,
                                statesupport == 9 ~ 0,
                                statesupport == 10 ~ 0,
                                statesupport == 11 ~ 0,
                                statesupport == 12 ~ 0,
                                statesupport == 13 ~ 0))


muslimmajority <- as.data.frame(muslimmajority)
muslimmajority$secular <- as.factor(muslimmajority$secular)
muslimmajority$restrictiononrp <- as.factor(muslimmajority$restrictiononrp)
muslimmajority$hostility <- as.factor(muslimmajority$hostility)


colnames(final_combined)

finalmodel1attacks <- glm.nb (formula = number_of_attacks ~ secular_constitution + polity +
                              sfi + gdp_per_capita + military_spending, 
                              data = final_combined,
                              na.action = na.omit)

finalmodel2attacks <- glm.nb (formula = number_of_attacks ~  restriction_on_rp + polity + sfi +
                                gdp_per_capita + military_spending, 
                              data = final_combined,
                              na.action = na.omit)

finalmodel3attacks <-glm.nb(formula = number_of_attacks ~ specific_hostility + polity + sfi +
                              gdp_per_capita + military_spending, 
                            data = final_combined,
                            na.action = na.omit)


finalmodel4killed <- glm.nb (formula = total_killed ~ secular_constitution + polity + sfi +
                               gdp_per_capita + military_spending, 
                             data = final_combined,
                             na.action = na.omit)

finalmodel5killed <- glm.nb (formula = total_killed ~  restriction_on_rp + polity + sfi +
                               gdp_per_capita + military_spending, 
                             data = final_combined,
                             na.action = na.omit)

finalmodel6killed <-glm.nb(formula = total_killed ~ specific_hostility + polity + sfi +
                             gdp_per_capita + military_spending, 
                           data = final_combined,
                           na.action = na.omit)




stargazer(finalmodel1attacks, finalmodel2attacks, finalmodel3attacks,
          finalmodel4killed, finalmodel5killed, finalmodel6killed,
          align=TRUE,
          float.env = "sidewaystable",
          dep.var.labels=c("Number of Attacks","Number of Killed People"),
          type = "text",
          output = "textfile",
          title = "Regression Results", 
          omit.stat=c("LL","ser","f"),
          header = FALSE,
          single.row = TRUE,
          no.space = TRUE,
          digits=3,
          font.size = "small")

stargazer(output1,output2,output3,output4,output5,output6,output7,
          title = "Determinants of Religious Terror Attacks",
          covariate.labels = c("Multiple Official Religion", 
                               "One Official Religion",
                               "Restrictions on Religious Parties",
                               "Polity Index",
                               "State Fragility Index (1995-2014)",
                               "Log(GDP per capita)",
                               "Military Spending",
                               "Literacy Rate"),
          dep.var.caption = "Number of Attacks per year",
          notes.label = "Significance levels",
          type="latex",
          out="number_of_attacks")




#dispersion parameter


output <-glm(formula = number_of_attacks ~ military_spending + literacy_rate + 
               secular_constitution + polity + sfi + log(gdp_per_capita),
             na.action = na.omit,
             data = final_combined)

print(summary(output))




#################################
#### Total Killed Regression ####
#################################

# secular constitutions
output1_killed <-glm.nb(formula = total_killed ~ secular_constitution, 
                        data = final_combined, na.action = na.omit)

print(summary(output1_killed))


# secular constitutions + polity
output2_killed <-glm.nb(formula = total_killed ~ secular_constitution +  polity,
                        data = final_combined, na.action = na.omit)

print(summary(output2_killed))

# secular constitutions + polity + log(gdp_per_capita)
output3_killed <-glm.nb(formula = total_killed ~ secular_constitution +  polity +  
                          log(gdp_per_capita),
                        data = final_combined, na.action = na.omit)

print(summary(output3_killed))

# secular constitutions + polity + log(gdp_per_capita) + military spending
output4_killed <-glm.nb(formula = total_killed ~  secular_constitution + polity + 
                          log(gdp_per_capita) + military_spending, data = final_combined, 
                        na.action = na.omit)

print(summary(output4_killed))



# secular constitutions + polity + log(gdp_per_capita) + military spending + literacy rate


output5_killed <-glm.nb(formula = total_killed ~  secular_constitution + polity + 
                          log(gdp_per_capita) + military_spending + literacy_rate, 
                        data = final_combined,
                        na.action = na.omit)

print(summary(output5_killed))


# AIC is good
output6_killed <-glm.nb(formula = total_killed ~  secular_constitution + polity + sfi + 
                          log(gdp_per_capita) + military_spending,
                        data = final_combined,
                        na.action = na.omit)

plot(output6)
print(summary(output6_killed))


output7_killed <-glm.nb(formula = total_killed ~  restriction_on_rp + polity + sfi + 
                          log(gdp_per_capita) + military_spending,
                        data = final_combined,
                        na.action = na.omit)


# Regression Tables

stargazer(output1_killed,output2_killed,output3_killed,output4_killed,output5_killed,output6_killed,
          output7_killed,
          title = "Determinants of Religious Terror Attacks",
          covariate.labels = c("Multiple Official Religion", 
                               "One Official Religion",
                               "Restrictions on Religious Parties",
                               "Polity Index",
                               "State Fragility Index (1995-2014)",
                               "Log(GDP per capita)",
                               "Military Spending",
                               "Literacy Rate"),
          dep.var.caption = "Number of Killed People per Year",
          notes.label = "Significance levels",
          type="html",
          out="models_killed1.htm")



output5 <-glm.nb(formula = number_of_attacks ~  secular_constitution + polity + 
                   log(gdp_per_capita) + military_spending + literacy_rate, 
                 data = final_combined,
                 na.action = na.omit)


output5_restrictions <-glm.nb(formula = number_of_attacks ~  restriction_on_rp + polity + 
                                log(gdp_per_capita) + military_spending + literacy_rate, 
                              data = final_combined,
                              na.action = na.omit)


stargazer(output5_restrictions, output5,
          title = "Determinants of Religious Terror Attacks",
          covariate.labels = c("Multiple Official Religion", 
                               "One Official Religion",
                               "Restrictions on Religious Parties",
                               "Polity Index",
                               "State Fragility Index (1995-2014)",
                               "Log(GDP per capita)",
                               "Military Spending",
                               "Literacy Rate"),
          dep.var.caption = "Number of Attacks",
          notes.label = "Significance levels",
          type="html",
          out="comparison(secular-restrictions).htm")



print(summary(output5_killed))






###EXTRAS####
###EXTRAS####
###EXTRAS####



# Coefficient Plots

m2_df <- tidy(output2) %>% filter(term != "secular_constitution1") %>% mutate(model = "Model 1")
m3_df <- tidy(output3) %>% filter(term != "secular_constitution1") %>% mutate(model = "Model 2")
m4_df <- tidy(output4) %>% filter(term != "secular_constitution1") %>% mutate(model = "Model 3")
m5_df <- tidy(output5) %>% filter(term != "secular_constitution1") %>% mutate(model = "Model 4")
m6_df <- tidy(output6) %>% filter(term != "secular_constitution1") %>% mutate(model = "Model 5")
m7_df <- tidy(output7) %>% filter(term != "secular_constitution1") %>% mutate(model = "Model 6")

six_models <- rbind(m2_df, m3_df,m4_df, m5_df,m6_df, m7_df)


m2_df_killed <- tidy(output2_killed) %>% filter(term != "secular_constitution1") %>% 
  mutate(model = "Model 1")
m3_df_killed <- tidy(output3_killed) %>% filter(term != "secular_constitution1") %>% 
  mutate(model = "Model 2")
m4_df_killed <- tidy(output4_killed) %>% filter(term != "secular_constitution1") %>% 
  mutate(model = "Model 3")
m5_df_killed <- tidy(output5_killed) %>% filter(term != "secular_constitution1") %>% 
  mutate(model = "Model 4")
m6_df_killed <- tidy(output6_killed) %>% filter(term != "secular_constitution1") %>% 
  mutate(model = "Model 5")
m7_df_killed <- tidy(output7_killed) %>% filter(term != "secular_constitution1") %>%
  mutate(model = "Model 6")

six_models_killed <- rbind(m2_df_killed, m3_df_killed,m4_df_killed, m5_df_killed,
                           m6_df_killed, m7_df_killed)



dw_1 <- dwplot(m7_df_killed, ci = .95,show.values = TRUE,value.offset = .3, 
               vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) %>%
  relabel_predictors(c(secular_constitution2 = "Multiple Official Religions",
                       polity = "Polity Score",
                       "log(gdp_per_capita)" = "Log(GDP per capita)", 
                       military_spending = "Military Spending",
                       literacy_rate = "Literacy Rate",
                       state_fragility_index = "State Fragility Index")) +
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle ("Number of Attacks") +
  theme(plot.title = element_text(face="bold"),
        legend.position = c(0.75, 0.5),
        legend.background = element_rect(colour="grey80"))

dw_2 <- dwplot(six_models_killed, ci = .99,
               vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) %>%
  relabel_predictors(c(secular_constitution2 = "Multiple Official Religions",
                       polity = "Polity Score",
                       "log(gdp_per_capita)" = "Log(GDP per capita)", 
                       military_spending = "Military Spending",
                       literacy_rate = "Literacy Rate",
                       state_fragility_index = "State Fragility Index")) +
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle ("Number of Killed People") +
  theme(plot.title = element_text(face="bold"),
        legend.position = c(0.75, 0.5),
        legend.background = element_rect(colour="grey80"))

dw_1 + dw_2









