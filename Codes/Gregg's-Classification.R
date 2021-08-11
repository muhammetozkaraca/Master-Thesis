library(kableExtra)
library(tidyverse)


firstline <- c("Left-Wing Terrorism","Anarchist/Marxist/Socialist", 
               "Red Brigades-Colombian ELM")
secondline <- c("Right-Wing Terrorism","Racist/Fascist/Nationalist", 
                "Ku Klux Klan-Neo-Nazis")
thirdline <- c("Ethnic-Seperatist Terrorism",
               "Dispel foreign occupying force-Create ethnically independent state", "Irgun-IRA")
forthline <- c("Religious Terrorism","Apocalyptic and Create Religious State/Government", 
               "Aum Shinrikyo")


data <- data.frame(c("Left-Wing Terrorism","Right-Wing Terrorism",
                     "Ethnic-Seperatist Terrorism","Religious Terrorism"),
                   c("Anarchist/Marxist/Socialist", "Racist/Fascist/Nationalist",
                     "Dispel foreign occupying force-Create ethnically independent state",
                     "Apocalyptic and Create Religious State/Government"),
                   c("Red Brigades-Colombian ELM", "Ku Klux Klan-Neo Nazis","Irgun-IRA",
                     "Aum Shinrikyo"))

names(data)[1] <- "Types of Terrorism"
names(data)[2] <- "Defining Goals"
names(data)[3] <- "Examples"

kbl(data, booktabs = T, 
    caption = "Gregg's Classification of Different Types of Terrorism") %>% 
  kable_styling(full_width = F) %>%
  footnote(symbol = c("Source: Gregg's Classification."))
