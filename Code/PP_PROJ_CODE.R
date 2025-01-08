### Packages ###

library(dplyr)
library(readr)
library(tidyverse)
library(tidytext)
library(stringr)
library(wordcloud2)
library(ggplot2)
library(lubridate)
library(gridExtra)
library(readxl)
library(scales)
library(knitr)
library(kableExtra)

### DATA ###

# Load corpus CSV #

PP_HANSARD <-  read_csv("Data/PP_38_44.1E_2024_10_20AND28.csv")

# Sort corpus by date #

PP_HANSARD <- PP_HANSARD %>% arrange(Date)

# Add a space after each point #

PP_HANSARD$Text <- gsub("\\.", ". ", PP_HANSARD$Text)

# Extract Date, Time, and Text unto Tibble # 

text_df <- tibble(Date = PP_HANSARD$Date, Text = PP_HANSARD$Text)

### Sentiment Analysis ###

# Tokenization #

text_tokenized <- text_df %>%
  unnest_tokens(word, Text)

# Remove stop words #

stopwords_custom <- tribble( ~word, ~lexicon, "mr", "custom", "pierre", "custom",
                             "poilievre", "custom", "nepean",
                             "custom", "carleton", "custom",
                             "cpc", "custom", "english", "custom", "translation", "custom", 
                             "speaker", "custom", "bill", "custom", 
                             "c", "custom", "act", "custom",
                             "prime", "custom", "minister", "custom",
                             "hon", "custom", "member", "custom",
                             "members", "custom", "honourable", "custom")

text_tokenized <- text_tokenized %>%
  anti_join(stopwords_custom)

text_tokenized <- text_tokenized %>%
  anti_join(get_stopwords())

text_tokenized <- text_tokenized %>% filter(!grepl("^\\d+$", word))

# Load sentiment dictionary #

Positive <- get_sentiments("bing") %>%
  filter(sentiment == "positive")

Negative <- get_sentiments("bing") %>%
  filter(sentiment == "negative")

# Identify positive and negative words #

Positive_Words <- text_tokenized %>%
  semi_join(Positive)

Positive_Words$Sentiment <- "Positive"


Negative_Words <- text_tokenized %>%
  semi_join(Negative)

Negative_Words$Sentiment <- "Negative"

# Count Sentiment by date #

Positive_Count <- Positive_Words %>%
  count(Sentiment, Date = Date, sort = TRUE, name = "Positive_Count")

Negative_Count <- Negative_Words %>%
  count(Sentiment, Date = Date, sort = TRUE, name = "Negative_Count")

# Combine positive and negative sentiment data frames #

Sentiments_PP <- merge(Negative_Count, Positive_Count, by = "Date", all = TRUE)

### Sentiment Proportions ###

# Calculate Positive and Negative proportions #

Sentiments_PP_Proportions <- data.frame(Date = Sentiments_PP$Date,
                                        Positive_Proportion = 100*(Sentiments_PP$Positive_Count / (Sentiments_PP$Positive_Count + Sentiments_PP$Negative_Count)),
                                        Negative_Proportion = 100*(Sentiments_PP$Negative_Count / (Sentiments_PP$Positive_Count + Sentiments_PP$Negative_Count)))

# Plot Positive and Negative proportions # 

Positive_Prop_Plot <- ggplot(Sentiments_PP_Proportions, aes(x = Date, y = Positive_Proportion)) +
  geom_line(color = "blue", linewidth = 0.4) + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(y = "Positive Proportion") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
        axis.title.x = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "gray90")) +
  geom_vline(xintercept = as.Date("2004-06-28"), color = "black", linetype = "dashed", size = 0.5) +
  geom_vline(xintercept = as.Date("2006-01-23"), color = "black", linetype = "dashed", size = 0.5) +
  geom_vline(xintercept = as.Date("2008-10-14"), color = "black", linetype = "dashed", size = 0.5) +
  geom_vline(xintercept = as.Date("2011-05-02"), color = "black", linetype = "dashed", size = 0.5) +
  geom_vline(xintercept = as.Date("2015-10-19"), color = "black", linetype = "dashed", size = 0.5) +
  geom_vline(xintercept = as.Date("2019-09-21"), color = "black", linetype = "dashed", size = 0.5) +
  geom_vline(xintercept = as.Date("2021-09-20"), color = "black", linetype = "dashed", size = 0.5) +
  geom_vline(xintercept = as.Date("2022-09-10"), color = "black", linetype = "dashed", size = 0.5)
  
Negative_Prop_Plot <- ggplot(Sentiments_PP_Proportions, aes(x = Date, y = Negative_Proportion)) +
  geom_line(color = "red", linewidth = 0.4) + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(y = "Negative Proportion") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
        axis.title.x = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "gray90")) +
  geom_vline(xintercept = as.Date("2004-06-28"), color = "black", linetype = "dashed", size = 0.5) +
  geom_vline(xintercept = as.Date("2006-01-23"), color = "black", linetype = "dashed", size = 0.5) +
  geom_vline(xintercept = as.Date("2008-10-14"), color = "black", linetype = "dashed", size = 0.5) +
  geom_vline(xintercept = as.Date("2011-05-02"), color = "black", linetype = "dashed", size = 0.5) +
  geom_vline(xintercept = as.Date("2015-10-19"), color = "black", linetype = "dashed", size = 0.5) +
  geom_vline(xintercept = as.Date("2019-09-21"), color = "black", linetype = "dashed", size = 0.5) +
  geom_vline(xintercept = as.Date("2021-09-20"), color = "black", linetype = "dashed", size = 0.5) +
  geom_vline(xintercept = as.Date("2022-09-10"), color = "black", linetype = "dashed", size = 0.5)

grid.arrange(Positive_Prop_Plot, Negative_Prop_Plot, ncol = 1)

# Plots by Intervals #

# 38th General Election, June 28, 2004  - LIB MIN #
# 39th General Election, January 23, 2006  - CON MIN #        
# 40th General Election, October 14, 2008 - CON MIN #
# 41st General Election, May 2, 2011 - CON MAJ #
# 42nd General Election, October 19, 2015 - LIB MAJ #
# 43rd General Election, October 21, 2019 - LIB MIN #
# 44th General Election, September 20, 2021 - LIB MIN #

# Define date intervals
intervals <- list(
  c("2004-06-28", "2006-01-22"),
  c("2006-01-23", "2008-10-13"),
  c("2008-10-14", "2011-05-01"),
  c("2011-05-02", "2015-10-18"),
  c("2015-10-19", "2019-10-20"),
  c("2019-10-21", "2021-09-19"),
  c("2021-09-20", "2024-10-23")
  )

# Create a list of dataframes for each interval
interval_dfs <- lapply(intervals, function(interval) {
  Sentiments_PP_Proportions %>%
    filter(Date >= as.Date(interval[1]) & Date <= as.Date(interval[2]))
})

# Name the dataframes in the list for easy reference
names(interval_dfs) <- c(
  "Interval_1", "Interval_2", "Interval_3", "Interval_4",
  "Interval_5", "Interval_6", "Interval_7"
)

# Plot the Positive_Proportion for interval 1 to 8 #

Pos_Prop_I1 <- ggplot(interval_dfs$Interval_1, aes(x = Date, y = Positive_Proportion)) +
  geom_line(color = "blue", linewidth = 0.4) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", col = "orange") +  
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 100)) +
  labs(y = element_blank()) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
        axis.title.x = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "gray90"))

Pos_Prop_I1_Mod <- lm(Positive_Proportion ~ Date, data = interval_dfs$Interval_1)

Pos_Prop_I2 <- ggplot(interval_dfs$Interval_2, aes(x = Date, y = Positive_Proportion)) +
  geom_line(color = "blue", linewidth = 0.4) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", col = "orange") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 100)) +
  labs(y = element_blank()) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
        axis.title.x = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "gray90"))

Pos_Prop_I2_Mod <- lm(Positive_Proportion ~ Date, data = interval_dfs$Interval_2)

Pos_Prop_I3 <- ggplot(interval_dfs$Interval_3, aes(x = Date, y = Positive_Proportion)) +
  geom_line(color = "blue", linewidth = 0.4) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", col = "green") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 100)) +
  labs(y = element_blank()) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
        axis.title.x = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "gray90"))

Pos_Prop_I3_Mod <- lm(Positive_Proportion ~ Date, data = interval_dfs$Interval_3)

Pos_Prop_I4 <- ggplot(interval_dfs$Interval_4, aes(x = Date, y = Positive_Proportion)) +
  geom_line(color = "blue", linewidth = 0.4) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", col = "green") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 100)) +
  labs(y = element_blank()) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
        axis.title.x = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "gray90"))

Pos_Prop_I4_Mod <- lm(Positive_Proportion ~ Date, data = interval_dfs$Interval_4)

Pos_Prop_I5 <- ggplot(interval_dfs$Interval_5, aes(x = Date, y = Positive_Proportion)) +
  geom_line(color = "blue", linewidth = 0.4) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", col = "orange") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 100)) +
  labs(y = element_blank()) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
        axis.title.x = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "gray90"))

Pos_Prop_I5_Mod <- lm(Positive_Proportion ~ Date, data = interval_dfs$Interval_5)

Pos_Prop_I6 <- ggplot(interval_dfs$Interval_6, aes(x = Date, y = Positive_Proportion)) +
  geom_line(color = "blue", linewidth = 0.4) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", col = "green") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 100)) +
  labs(y = element_blank()) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
        axis.title.x = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "gray90"))

Pos_Prop_I6_Mod <- lm(Positive_Proportion ~ Date, data = interval_dfs$Interval_6)

Pos_Prop_I7 <- ggplot(interval_dfs$Interval_7, aes(x = Date, y = Positive_Proportion)) +
  geom_line(color = "blue", linewidth = 0.4) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", col = "orange") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 100)) +
  labs(y = element_blank()) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
        axis.title.x = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "gray90"))

Pos_Prop_I7_Mod <- lm(Positive_Proportion ~ Date, data = interval_dfs$Interval_7)

# Plot the Negative_Proportion for interval 1 to 8 #

Neg_Prop_I1 <- ggplot(interval_dfs$Interval_1, aes(x = Date, y = Negative_Proportion)) +
  geom_line(color = "red", linewidth = 0.4) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", col = "green") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(limits = c(0, 100)) +
  labs(y = element_blank()) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
        axis.title.x = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "gray90"))

Neg_Prop_I1_Mod <- lm(Negative_Proportion ~ Date, data = interval_dfs$Interval_1)

Neg_Prop_I2 <- ggplot(interval_dfs$Interval_2, aes(x = Date, y = Negative_Proportion)) +
  geom_line(color = "red", linewidth = 0.4) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", col = "green") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 100)) +
  labs(y = element_blank()) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
        axis.title.x = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "gray90"))

Neg_Prop_I2_Mod <- lm(Negative_Proportion ~ Date, data = interval_dfs$Interval_2)

Neg_Prop_I3 <- ggplot(interval_dfs$Interval_3, aes(x = Date, y = Negative_Proportion)) +
  geom_line(color = "red", linewidth = 0.4) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", col = "orange") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 100)) +
  labs(y = element_blank()) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
        axis.title.x = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "gray90"))

Neg_Prop_I3_Mod <- lm(Negative_Proportion ~ Date, data = interval_dfs$Interval_3)

Neg_Prop_I4 <- ggplot(interval_dfs$Interval_4, aes(x = Date, y = Negative_Proportion)) +
  geom_line(color = "red", linewidth = 0.4) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", col = "orange") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 100)) +
  labs(y = element_blank()) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
        axis.title.x = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "gray90"))

Neg_Prop_I4_Mod <- lm(Negative_Proportion ~ Date, data = interval_dfs$Interval_4)

Neg_Prop_I5 <- ggplot(interval_dfs$Interval_5, aes(x = Date, y = Negative_Proportion)) +
  geom_line(color = "red", linewidth = 0.4) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", col = "green") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 100)) +
  labs(y = element_blank()) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
        axis.title.x = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "gray90"))

Neg_Prop_I5_Mod <- lm(Negative_Proportion ~ Date, data = interval_dfs$Interval_5)

Neg_Prop_I6 <- ggplot(interval_dfs$Interval_6, aes(x = Date, y = Negative_Proportion)) +
  geom_line(color = "red", linewidth = 0.4) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", col = "orange") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 100)) +
  labs(y = element_blank()) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
        axis.title.x = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "gray90"))

Neg_Prop_I6_Mod <- lm(Negative_Proportion ~ Date, data = interval_dfs$Interval_6)

Neg_Prop_I7 <- ggplot(interval_dfs$Interval_7, aes(x = Date, y = Negative_Proportion)) +
  geom_line(color = "red", linewidth = 0.4) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", col = "green") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 100)) +
  labs(y = element_blank()) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
        axis.title.x = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "gray90"))

Neg_Prop_I7_Mod <- lm(Negative_Proportion ~ Date, data = interval_dfs$Interval_7)

# Plot comparative grid # 

grid.arrange(Pos_Prop_I1,
             Pos_Prop_I2,
             Pos_Prop_I3,
             Pos_Prop_I4,
             Pos_Prop_I5,
             Pos_Prop_I6,
             Pos_Prop_I7,
             Neg_Prop_I1,
             Neg_Prop_I2,
             Neg_Prop_I3,
             Neg_Prop_I4,
             Neg_Prop_I5,
             Neg_Prop_I6,
             Neg_Prop_I7,
             ncol = 7)

# Create slopes vectors 

Pos_Prop_I1_Mod
Pos_Prop_I2_Mod
Pos_Prop_I3_Mod
Pos_Prop_I4_Mod
Pos_Prop_I5_Mod
Pos_Prop_I6_Mod
Pos_Prop_I7_Mod

Neg_Prop_I1_Mod
Neg_Prop_I2_Mod
Neg_Prop_I3_Mod
Neg_Prop_I4_Mod
Neg_Prop_I5_Mod
Neg_Prop_I6_Mod
Neg_Prop_I7_Mod

### Intervals descriptive statistics ###

# Function to calculate descriptive statistics #
calculate_stats <- function(df) {
  df %>%
    summarise(
      Positive_Mean = round(mean(Positive_Proportion, na.rm = TRUE),1),
      Negative_Mean = round(mean(Negative_Proportion, na.rm = TRUE),1),
      Difference = round(mean(Positive_Proportion, na.rm = TRUE) - mean(Negative_Proportion, na.rm = TRUE),1)
    )
}

# Apply the function to each dataframe in the list #
stats_list <- lapply(interval_dfs, calculate_stats)

# Convert the list of tibbles into a single dataframe #
stats_df <- bind_rows(stats_list, .id = "Parliament")

# Define the new interval names
new_intervals <- c(
  "38th - LIB MIN",
  "39th - CON MIN",
  "40th - CON MIN",
  "41st - CON MAJ",
  "42nd - LIB MAJ",
  "43rd - LIB MIN",
  "44th - LIB MIN"
)

# Assign the new interval names to the Parliament column in stats_df
stats_df$Parliament <- new_intervals

# Create table #
stats_df %>%
  kable(align = 'c') %>%
  kable_styling(full_width = FALSE, position = "center")

### Poll Analysis ###

# Load Poll Data Frame #

Polls_CAN <- read_excel("Data/Polls_CAN_20SEP21_18NOV24.xlsx")

# Calculate Conservative - Liberal Gap #

Polls_CAN$ConLibGap = Polls_CAN$CON - Polls_CAN$LIB

# Set Date Format #

Polls_CAN$Date <- as.Date(Polls_CAN$Date)

# Plot Conservative - Liberal Gap #

custom_breaks <- seq.Date(from = min(Polls_CAN$Date), to = max(Polls_CAN$Date), by = "month")
custom_labels <- ifelse(format(custom_breaks, "%m") == "01", format(custom_breaks, "%b %Y"), format(custom_breaks, "%b"))

Poll_Gap_Plot <- ggplot(Polls_CAN, aes(x = Date, y = ConLibGap)) +
  geom_line(color = "royalblue", size = 0.75) + 
  scale_x_date(breaks = custom_breaks, labels = custom_labels) +
  scale_y_continuous(labels = percent_format()) +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  geom_point(size = 1.25, color = "navy") +
  # 44th General Election, September 20, 2021 #
  geom_vline(xintercept = as.Date("2021-09-20"), color = "red", linetype = "dashed", size = 0.5) +
    # Poilievre becomes leader if the Opposition #
  geom_vline(xintercept = as.Date("2022-09-10"), color = "navy", linetype = "dashed", size = 0.5)

Poll_Gap_Plot
