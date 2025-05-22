"Script for EMS Analysis
for DSCI 410L: ADSSJ
"

library("tidyverse")
library("ggplot2")
library("ggbreak")
library("lubridate")

setwd("C:/Users/jacob/OneDrive/Desktop/HW/DSCI410")
EMS_2024 = read.csv("EMS_2024_Clean.csv")
EMS_2025 = read.csv("EMS_2025_Clean.csv")

#frequency of call types
EMS_2024_Nature_Freqs = EMS_2024 %>% as.tibble() %>% count(Description)
arrange(EMS_2024_Nature_Freqs, desc(n))
EMS_2025_Nature_Freqs = EMS_2025 %>% as.tibble() %>% count(Description)
arrange(EMS_2025_Nature_Freqs, desc(n))

#concatenate EMS 2024 and 2025 data, calculate percent changes
EMS_Concat = merge(EMS_2024_Nature_Freqs, EMS_2025_Nature_Freqs, by = "Description")
colnames(EMS_Concat) <- c("Description", "N_2024", "N_2025")
EMS_Concat$Difference = EMS_Concat$N_2025 - EMS_Concat$N_2024
EMS_Concat$Percent_Difference = (EMS_Concat$Difference / EMS_Concat$N_2024)*100

Largest_Diffs = arrange(EMS_Concat, desc(abs(Percent_Difference)))
Largest_Cats =arrange(EMS_Concat, desc(N_2024))

#Call volume analysis

LCats_List = Largest_Cats$Description[1:10]
By_Date = rbind(EMS_2024, EMS_2025) %>% 
  select(Description, Date) %>% 
  subset(Description %in% LCats_List) %>%
  group_by(Date, .drop=FALSE) 

Daily_Calls = summarise(By_Date, N = n()) %>% 
  mutate(Date = as.Date(Date)) %>%
  mutate(Year = as.factor(year(Date))) %>%
  mutate(Date = format(Date, "%m-%d"))

Daily_Calls_Trend <- ggplot(Daily_Calls, aes(x = Date, y = N, group=Year)) +
  geom_line(aes(linetype=Year)) + 
  labs(title = "Fire/EMS Call Volume Over Time",
      x = "Date",
      y = "Number of Calls") +
  scale_linetype_manual(
    values = c("dashed", "solid"),
    labels = c("2024", "2025")
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1)) +
  scale_x_discrete(breaks = c("03-24", "04-06", "04-21", "05-05", "05-14")) +
  geom_vline(xintercept='04-06', colour = "blue")

Daily_Calls_Trend

anchor_date = as.Date("2024-04-06")

Daily_Calls_Agg = Daily_Calls %>% 
  mutate(Date = as.Date(paste0(Year, "-", Date), format = "%Y-%m-%d"),
    adjusted_date = Date - as.numeric(difftime(Date, anchor_date, units = "days")) %% 7,
    Week = floor_date(adjusted_date, unit = "week")) %>%
  group_by(Year, Week) %>%
  summarise(avg_N = mean(N)) %>%
  mutate(Year = as.factor(year(Week)))

Weekly_Calls_Trend <- ggplot(Daily_Calls_Agg, aes(x = Week, y = avg_N, group=Year)) +
  geom_line(aes(linetype=Year)) + 
  labs(title = "Fire/EMS Call Volume Over Time",
       x = "Date",
       y = "Number of Calls") +
  scale_linetype_manual(
    values = c("dashed", "solid"),
    labels = c("2024", "2025")
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1))
  #geom_vline(xintercept='2024-04-06', colour = "blue")

Weekly_Calls_Trend

#Analysis of most frequent categories

LCats_Diff_Df = Largest_Cats %>%
  subset(Description %in% LCats_List) %>%
  pivot_longer(
    cols = c(N_2024, N_2025),
    names_to = "Year",
    values_to = "N"
  )

LCats_Diff = ggplot(LCats_Diff_Df, aes(x = Description, y = N, fill=Year)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Number Of Most Frequent Fire/EMS Call Descriptions",
       x = "Description",
       y = "Count") +
  scale_fill_manual(labels = c("2024", "2025"), values = c("cyan", "orange")) +
  scale_y_break(c(500, 4000)) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 6, angle = 45, hjust = 1)) + 
  theme(plot.title = element_text(hjust = 0.5, vjust = 1))


LCats_Diff

#MEDICAL RESPONSE Analaysis

MR_Trend_Df = By_Date %>% 
  subset(Description == "MEDICAL RESPONSE") %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(Year = as.factor(year(Date))) %>%
  mutate(Date = format(Date, "%m-%d")) %>%
  group_by(Date, Year) %>%
  summarise(N = n())
  
MR_Trend = ggplot(MR_Trend_Df, aes(x = Date, y = N, group=Year)) +
  geom_line(aes(linetype=Year)) + 
  labs(title = "Medical Response Calls Over Time",
       x = "Date",
       y = "Number of Calls") +
  scale_linetype_manual(
    values = c("dashed", "solid"),
    labels = c("2024", "2025")
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(breaks = c("03-24", "04-06", "04-21")) +
  geom_vline(xintercept='04-06', colour = "blue") 

MR_Trend

#ASSIST POLICE Call Analysis

AP_Trend_Df = By_Date %>% 
  subset(Description == "ASSIST POLICE") %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(Year = as.factor(year(Date))) %>%
  mutate(Date = format(Date, "%m-%d")) %>%
  group_by(Date, Year) %>%
  summarise(N = n())

AP_Trend = ggplot(AP_Trend_Df, aes(x = Date, y = N, group=Year)) +
  geom_line(aes(linetype=Year)) + 
  labs(title = "Assist Police Calls Over Time",
       x = "Date",
       y = "Number of Calls") +
  scale_linetype_manual(
    values = c("dashed", "solid"),
    labels = c("2024", "2025")
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(breaks = c("03-24", "04-06", "04-21")) +
  geom_vline(xintercept='04-06', colour = "blue")

AP_Trend

#Split 

EMS_2024_Before = EMS_2024 %>% filter(Date <= '2024-04-06')
EMS_2024_After = EMS_2024 %>% filter(Date > '2024-04-06')
EMS_2025_Before = EMS_2025 %>% filter(Date <= '2025-04-06')
EMS_2025_After = EMS_2025 %>% filter(Date > '2025-04-06')

EMS_2024_Before_Nature_Freqs = EMS_2024_Before %>% as.tibble() %>% count(Description)
arrange(EMS_2024_Before_Nature_Freqs, desc(n))
EMS_2024_After_Nature_Freqs = EMS_2024_After %>% as.tibble() %>% count(Description)
arrange(EMS_2024_After_Nature_Freqs, desc(n))

EMS_2025_Before_Nature_Freqs = EMS_2025_Before %>% as.tibble() %>% count(Description)
arrange(EMS_2025_Before_Nature_Freqs, desc(n))
EMS_2025_After_Nature_Freqs = EMS_2025_After %>% as.tibble() %>% count(Description)
arrange(EMS_2025_After_Nature_Freqs, desc(n))

Daily_Calls_2025 <- Daily_Calls %>%
  filter(Year == '2025') %>%
  mutate(Period = ifelse(Date < "04-07", "Before 04/07", "After 04/07"))

ggplot(Daily_Calls_2025, aes(x = N, fill = Period)) +
  geom_histogram(binwidth = 5, alpha = 0.6, position = "identity") +
  labs(title = "2025 Call Volume Distributions", x = "Number of Calls", y = "Count") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

t.test(N ~ Period, data = Daily_Calls_2025)

AP_Before = EMS_2025_Before %>%
  filter(Description == "ASSIST POLICE") %>%
  mutate(Period = "Before 04/07") %>%
  select(Description, Date, Period)

AP_After = EMS_2025_After %>%
  filter(Description == "ASSIST POLICE") %>%
  mutate(Period = "After 04/07") %>%
  select(Description, Date, Period)

AP_2025 = rbind(AP_Before, AP_After) %>%  
  group_by(Date, Period) %>%
  summarise(Daily_Count = n())

ggplot(AP_2025, aes(x = Daily_Count, fill = Period)) +
  geom_histogram(binwidth = 1, alpha = 0.6, position = "identity") +
  labs(title = "2025 ASSIST POLICE Distributions", x = "Number of Calls", y = "Count") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

shapiro.test(AP_2025$Daily_Count[AP_2025$Period == "Before 04/07"])
shapiro.test(AP_2025$Daily_Count[AP_2025$Period == "After 04/07"])

wilcox.test(Daily_Count ~ Period, data = AP_2025)
t.test(Daily_Count ~ Period, data = AP_2025)

Desc_df = EMS_2025 %>%
  mutate(Period = ifelse(Date < "2025-04-07", "Before 04/07", "After 04/07")) %>%
  group_by(Description, Period) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = "Period", values_from = "Count") %>% 
  replace(is.na(.), 0)

Desc_vals = Desc_df$Description
Desc_matrix = as.matrix(Desc_df[, c("Before 04/07", "After 04/07")])
rownames(Desc_matrix) = Desc_vals
chisq.test(Desc_matrix)
chisq.test(Desc_matrix)$stdres

LCats_Diff = ggplot(LCats_Diff_Df, aes(x = Description, y = N, fill=Year)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Number Of Most Frequent Fire/EMS Call Descriptions",
       x = "Description",
       y = "Count") +
  scale_fill_manual(labels = c("2024", "2025"), values = c("cyan", "orange")) +
  scale_y_break(c(500, 4000)) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 6, angle = 45, hjust = 1)) + 
  theme(plot.title = element_text(hjust = 0.5, vjust = 1))
