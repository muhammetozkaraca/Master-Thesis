library(dplyr)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(readxl)
library(viridis)
library(writexl)
library(hrbrthemes)
library(scales)
library(lubridate)
library(ggrepel)



df <- read_excel("sekuler-reforms.xlsx")
df <- df[with(df, order(date)), ]
head(df)

status_levels <- c("Before the Proclamation of the Republic", "Early Republican Era", 
                   "Final Phase")
status_colors <- c("#0070C0", "#00B050", "#FFC000")

df$status <- factor(df$status, levels=status_levels, ordered=TRUE)

positions <- c(0.5, -0.5, 1.0, -1.0, 1.75, -1.5)
directions <- c(1, -1)

line_pos <- data.frame(
  "date"=unique(df$date),
  "position"=rep(positions, length.out=length(unique(df$date))),
  "direction"=rep(directions, length.out=length(unique(df$date)))
)

df <- merge(x=df, y=line_pos, by="date", all = TRUE)
df <- df[with(df, order(date, status)), ]

head(df)

text_offset <- 0.15

df$month_count <- ave(df$date==df$date, df$date, FUN=cumsum)
df$text_position <- (df$month_count * text_offset * df$direction) + df$position
head(df)

## Month Formatting

month_buffer <- 2

month_date_range <- seq(min(df$date) - months(month_buffer), max(df$date) + months(month_buffer), by='month')
month_format <- format(month_date_range, '%b')
month_df <- data.frame(month_date_range, month_format)



## Year Formatting
year_buffer <- 10

year_date_range <- seq(min(df$date) - months(year_buffer), max(df$date) + months(year_buffer), by='year')

year_format <- format(year_date_range, '%Y')
year_df <- data.frame(year_date_range, year_format)


timeline_plot<-ggplot(df,aes(x=date,y=0, label=reforms))
timeline_plot<-timeline_plot+labs(col="reforms")
timeline_plot<-timeline_plot+theme_classic()

# Plot horizontal black line for timeline
timeline_plot <- timeline_plot+geom_hline(yintercept=0, 
                                        color = "black", size=9)

# Plot vertical segment lines for milestones
timeline_plot<-timeline_plot+geom_segment(data=df[df$month_count == 1,], aes(y=position,yend=0,xend=date), color='grey', size=11)

# Plot scatter points at zero and date
timeline_plot<-timeline_plot + geom_point(aes(y=0), size=30, color = "#00B050") 

# Don't show axes, appropriately position legend
timeline_plot<-timeline_plot+theme(axis.line.y=element_blank(),
                                   axis.text.y=element_blank(),
                                   axis.title.x=element_blank(),
                                   axis.title.y=element_blank(),
                                   axis.ticks.y=element_blank(),
                                   axis.text.x =element_blank(),
                                   axis.ticks.x =element_blank(),
                                   axis.line.x =element_blank(),
                                   legend.position = "bottom")
  


# Show year text
timeline_plot<-timeline_plot+geom_text(data=year_df, aes(x=year_date_range,y=-0.2,label=year_format, fontface="bold"),size=35, color="#0070C0")

# Show text for each milestone
timeline_plot <- timeline_plot + 
  geom_text(aes(y=text_position,label=reforms, fontface="bold"),size=40, color = "#C00000") +
  labs(title = "")
print(timeline_plot)


ggsave("timeline_plot.png", width = 120, height = 45, dpi = "screen", limitsize = FALSE)