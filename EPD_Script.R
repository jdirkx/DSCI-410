"Script for EPD/CAHOOTS Analysis
for DSCI 410L: ADSSJ
"

library("tidyverse")
library("ggplot2")

setwd("C:/Users/jacob/OneDrive/Desktop/HW/DSCI410")
EPD_2024 = read.csv("EPD_2024_Clean.csv")
EPD_2025 = read.csv("EPD_2025_Clean.csv")

#frequency of call types
EPD_2024_Nature_Freqs = EPD_2024 %>% as.tibble() %>% count(nature)
arrange(EPD_2024_Nature_Freqs, desc(n))
EPD_2025_Nature_Freqs = EPD_2025 %>% as.tibble() %>% count(nature)
arrange(EPD_2025_Nature_Freqs, desc(n))

#concatenate EPD 2024 and 2025 data, calculate percent changes
EPD_Concat = merge(EPD_2024_Nature_Freqs, EPD_2025_Nature_Freqs, by = "nature")
colnames(EPD_Concat) <- c("Nature", "N_2024", "N_2025")
EPD_Concat$Difference = EPD_Concat$N_2025 - EPD_Concat$N_2024
EPD_Concat$Percent_Difference = (EPD_Concat$Difference / EPD_Concat$N_2024)*100
arrange(EPD_Concat, desc(abs(Percent_Difference)))

#filter for EPD and CAHOOTS overlap
BOTH_PD_TYPES = list("ASSIST FD", "ASSIST PD", "CHECK WELFARE", 
"DISORDERLY SUBJECT", "DISORIENTED SUBJECT", "INTOXICATED SUBJECT", 
"PUBLIC ASSIST", "SUICIDAL SUBJECT", "TRAFFIC HAZARD", "TRANSPORT") #removed INFO/ATL
CAHOOTS_AND_EPD = subset(EPD_Concat, Nature %in% BOTH_PD_TYPES)

#visualization for colisted EPD/CAHOOTS Calls
graphing_df = pivot_longer(CAHOOTS_AND_EPD, cols = c(N_2024, N_2025),
                           names_to = "Year", values_to = "Count")

Colisted_trends = ggplot(graphing_df, aes(x = Nature, y = Count, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "EPD call types colisted with CAHOOTS",
       x = "Nature",
       y = "Count") +
  scale_fill_manual(labels = c("2024", "2025"), values = c("cyan", "orange")) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 6, angle = 45, hjust = 1))

Colisted_trends

#calculating trend for CHECK WELFARE
All_2024 = read.csv("class_data_2024.csv")
All_2025 = read.csv("class_data_2025.csv")
All_Data = rbind(All_2024, All_2025)
All_Data$call_date = as.Date(All_Data$calltime)

CW_Counts = All_Data %>% select(nature, call_date) %>%
  filter(nature == 'CHECK WELFARE') %>% group_by(call_date) %>%
  summarise(num_calls = n(), .groups = "drop") %>%
  mutate(yr = year(call_date))

CW_Counts_2024 = CW_Counts %>% filter(call_date <= '2024-04-22') %>% 
  filter(call_date >= '2024-03-24') %>%
  subset(call_date != '2024-02-29')
CW_Counts_2025 = CW_Counts %>% filter(call_date >= '2025-03-24')
CW_Counts_Overlap = bind_rows(CW_Counts_2024, CW_Counts_2025) %>%
  mutate(call_date = format(call_date, "%m-%d")) %>%
  mutate(yr = as.character(yr))

CW_Trend <- ggplot(CW_Counts_Overlap, aes(x = call_date, y = num_calls, group = yr)) +
  geom_line(aes(linetype=yr)) + 
  labs(title = "Check Welfare Calls Over Time",
       x = "Date",
       y = "Number of Calls") +
  scale_linetype_manual(
    values = c("dashed", "solid"),
    labels = c("2024", "2025")
  ) +
  geom_vline(xintercept='04-07', linetype = "dotted", colour = "green") + 
  theme_minimal()

CW_Trend
