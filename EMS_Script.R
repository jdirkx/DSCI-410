"Script for EMS Analysis
for DSCI 410L: ADSSJ
Created by Jacob Dirkx; last updated May 2025
"

#INSTALL PACKAGES IF YOU DON'T HAVE THEM
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("ggbreak")
#install.packages("lubridate")

library("tidyverse")
library("ggplot2")
library("ggbreak")
library("lubridate")

setwd("C:/Users/jacob/OneDrive/Desktop/HW/DSCI410")
EMS_2024 = read.csv("EMS_2024_Clean.csv")
EMS_2025 = read.csv("EMS_2025_Clean.csv")

#frequency of call types
EMS_2024_Desc_Freqs = EMS_2024 %>% as.tibble() %>% count(Description)
arrange(EMS_2024_Desc_Freqs, desc(n))
EMS_2025_Desc_Freqs = EMS_2025 %>% as.tibble() %>% count(Description)
arrange(EMS_2025_Desc_Freqs, desc(n))

#concatenate EMS 2024 and 2025 data, calculate percent changes
EMS_Concat = merge(EMS_2024_Desc_Freqs, EMS_2025_Desc_Freqs, by = "Description")
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
  scale_x_discrete(breaks = c("02-24", "03-09", "03-23", "04-06", "04-20", "05-04", "05-18")) +
  geom_vline(xintercept='04-06', colour = "blue")

Daily_Calls_Trend

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
  scale_x_discrete(breaks = c("02-24", "03-09", "03-23", "04-06", "04-20", "05-04", "05-18")) +
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
  scale_x_discrete(breaks = c("02-24", "03-09", "03-23", "04-06", "04-20", "05-04", "05-18")) +
  geom_vline(xintercept='04-06', colour = "blue")

AP_Trend

#Split 

EMS_2024_Before = EMS_2024 %>% filter(Date <= '2024-04-06')
EMS_2024_After = EMS_2024 %>% filter(Date > '2024-04-06')
EMS_2025_Before = EMS_2025 %>% filter(Date <= '2025-04-06')
EMS_2025_After = EMS_2025 %>% filter(Date > '2025-04-06')

EMS_2024_Before_Desc_Freqs = EMS_2024_Before %>% as.tibble() %>% count(Description)
arrange(EMS_2024_Before_Desc_Freqs, desc(n))
EMS_2024_After_Desc_Freqs = EMS_2024_After %>% as.tibble() %>% count(Description)
arrange(EMS_2024_After_Desc_Freqs, desc(n))

EMS_2025_Before_Desc_Freqs = EMS_2025_Before %>% as.tibble() %>% count(Description)
arrange(EMS_2025_Before_Desc_Freqs, desc(n))
EMS_2025_After_Desc_Freqs = EMS_2025_After %>% as.tibble() %>% count(Description)
arrange(EMS_2025_After_Desc_Freqs, desc(n))

Daily_Calls_2024 <- Daily_Calls %>%
  filter(Year == '2024') %>%
  mutate(Period = ifelse(Date < "04-07", "Before 04/07/24", "After 04/07/24"))

Daily_Calls_2025 <- Daily_Calls %>%
  filter(Year == '2025') %>%
  mutate(Period = ifelse(Date < "04-07", "Before 04/07/25", "After 04/07/25"))

Daily_calls_vols = rbind(Daily_Calls_2024, Daily_Calls_2025) %>%
  mutate(Period = factor(Period, levels = c("Before 04/07/24", "After 04/07/24", "Before 04/07/25", "After 04/07/25")))

Daily_Call_Volumes = ggplot(Daily_calls_vols, aes(x = Period, y = N, fill = Period)) +
  geom_boxplot() +
  labs(title = "Call Volume Distributions", x = "Period", y = "Calls per Day") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(labels = c("Before 04/07/24", "After 04/07/24", "Before 04/07/25", "After 04/07/25"), 
                    values = c("coral2", "red", "cadetblue2", "cyan"))

Daily_Call_Volumes

ggplot(Daily_Calls_2025, aes(x = N, fill = Period)) +
  geom_histogram(position = "dodge", bins=10) +
  labs(title = "2025 Call Volume Distribution", x = "Number of Calls", y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(labels = c("Before 04/07/25", "After 04/07/25"), 
                    values = c("cadetblue3", "cyan"))

EMS_2024_Before_Desc_Freqs = mutate(EMS_2024_Before_Desc_Freqs, Period = "Before 04/07/24")
EMS_2024_After_Desc_Freqs = mutate(EMS_2024_After_Desc_Freqs, Period = "After 04/07/24")
EMS_2025_Before_Desc_Freqs = mutate(EMS_2025_Before_Desc_Freqs, Period = "Before 04/07/25")
EMS_2025_After_Desc_Freqs = mutate(EMS_2025_After_Desc_Freqs, Period = "After 04/07/25")

EMS_Concat_ByPeriod = bind_rows(EMS_2024_Before_Desc_Freqs, EMS_2024_After_Desc_Freqs, EMS_2025_Before_Desc_Freqs, EMS_2025_After_Desc_Freqs) %>%
  pivot_wider(names_from = Period, values_from = n, values_fill = 0) %>%
  subset(Description %in% LCats_List) 


t.test(N ~ Period, data = Daily_Calls_2025)
#high p value rip

t.test(N)

#Description Analysis

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
std_res = as.table(chisq.test(Desc_matrix)$stdres) %>%
  as.data.frame() %>%
  arrange(desc(Freq)) %>%
  select(-c("Var2")) %>%
  rename(c("Call Description" = "Var1", "ChiSqVal" = "Freq"))

head(std_res, 10)

Largest_Cats_2024 = EMS_2024 %>%
  mutate(Period = ifelse(Date < "2024-04-07", "Before 04/07/24", "After 04/07/24")) %>%
  group_by(Description, Period) %>%
  summarise(Count = n()) %>%
  replace(is.na(.), 0)

Largest_Cats_2025 = EMS_2025 %>%
  mutate(Period = ifelse(Date < "2025-04-07", "Before 04/07/25", "After 04/07/25")) %>%
  group_by(Description, Period) %>%
  summarise(Count = n()) %>%
  replace(is.na(.), 0)

Largest_Cats_ByPeriod = bind_rows(Largest_Cats_2024, Largest_Cats_2025) %>%
  subset(Description %in% LCats_List)

Largest_Cats_ByPeriod$Period = factor(Largest_Cats_ByPeriod$Period, levels = c("Before 04/07/24", "After 04/07/24", 
    "Before 04/07/25", "After 04/07/25"))

LCats_ByPeriod_Chart = ggplot(Largest_Cats_ByPeriod, aes(x = Description, y = Count, fill=Period)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Number Of Most Frequent Fire/EMS Call Descriptions",
       x = "Description",
       y = "Count") +
  scale_y_break(c(200, 3500)) +
  scale_fill_manual(labels = c("Before 04/07/24", "After 04/07/24", "Before 04/07/25", "After 04/07/25"), 
                    values = c("coral2", "red", "cadetblue2", "cyan")) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 6, angle = 45, hjust = 1)) + 
  theme(plot.title = element_text(hjust = 0.5, vjust = 1))

LCats_ByPeriod_Chart

#Medical Response Analysis
MR_Before24 = EMS_2024_Before %>%
  filter(Description == "MEDICAL RESPONSE") %>%
  mutate(Period = "Before 04/07/24") %>%
  select(Description, Date, Period)

MR_Before25 = EMS_2025_Before %>%
  filter(Description == "MEDICAL RESPONSE") %>%
  mutate(Period = "Before 04/07/25") %>%
  select(Description, Date, Period)

MR_After24 = EMS_2024_After %>%
  filter(Description == "MEDICAL RESPONSE") %>%
  mutate(Period = "After 04/07/24") %>%
  select(Description, Date, Period)

MR_After25 = EMS_2025_After %>%
  filter(Description == "MEDICAL RESPONSE") %>%
  mutate(Period = "After 04/07/25") %>%
  select(Description, Date, Period)

MR_Df = rbind(MR_Before24, MR_Before25, MR_After24, MR_After25) %>%  
  group_by(Date, Period) %>%
  summarise(Daily_Count = n()) %>%
  mutate(Period = factor(Period, levels = c("Before 04/07/24", "After 04/07/24", "Before 04/07/25", "After 04/07/25")))

MR_Dist = ggplot(MR_Df, aes(x = Period, y = Daily_Count, fill=Period)) +
  geom_boxplot() +
  labs(title = "MEDICAL RESPONSE Call Distributions",
       x = "Period",
       y = "MEDICAL RESPONSE Calls Per Day") +
  scale_fill_manual(labels = c("Before 04/07/24", "After 04/07/24", "Before 04/07/25", "After 04/07/25"), 
                    values = c("coral2", "tomato", "cadetblue2", "cyan")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1))

MR_Dist

shapiro.test(MR_Df$Daily_Count[MR_Df$Period == "Before 04/07/24"])
shapiro.test(MR_Df$Daily_Count[MR_Df$Period == "After 04/07/24"])
shapiro.test(MR_Df$Daily_Count[MR_Df$Period == "Before 04/07/25"])
shapiro.test(MR_Df$Daily_Count[MR_Df$Period == "After 04/07/25"])
#All MR data is normally distributed

MR_2025 = MR_Df %>% filter(Period %in% c("Before 04/07/25", "After 04/07/25"))
MR_ByYear = MR_Df %>%
  mutate(Year = ifelse(Period %in% c("Before 04/07/24", "After 04/07/24"), "2024", "2025"))

t.test(Daily_Count ~ Period, data = MR_2025)
#2025 data time frames do not differ significantly

t.test(Daily_Count ~ Year, data = MR_ByYear)

#AP Analysis

AP_Before24 = EMS_2024_Before %>%
  filter(Description == "ASSIST POLICE") %>%
  mutate(Period = "Before 04/07/24") %>%
  select(Description, Date, Period)

AP_Before25 = EMS_2025_Before %>%
  filter(Description == "ASSIST POLICE") %>%
  mutate(Period = "Before 04/07/25") %>%
  select(Description, Date, Period)

AP_After24 = EMS_2024_After %>%
  filter(Description == "ASSIST POLICE") %>%
  mutate(Period = "After 04/07/24") %>%
  select(Description, Date, Period)

AP_After25 = EMS_2025_After %>%
  filter(Description == "ASSIST POLICE") %>%
  mutate(Period = "After 04/07/25") %>%
  select(Description, Date, Period)

AP_Df = rbind(AP_Before24, AP_Before25, AP_After24, AP_After25) %>%  
  group_by(Date, Period) %>%
  summarise(Daily_Count = n()) %>%
  mutate(Period = factor(Period, levels = c("Before 04/07/24", "After 04/07/24", "Before 04/07/25", "After 04/07/25")))

AP_Dist = ggplot(AP_Df, aes(x = Period, y = Daily_Count, fill=Period)) +
  geom_boxplot() +
  labs(title = "ASSIST POLICE Call Distributions",
       x = "Period",
       y = "ASSIST POLICE Calls Per Day") +
  scale_fill_manual(labels = c("Before 04/07/24", "After 04/07/24", "Before 04/07/25", "After 04/07/25"), 
                    values = c("coral2", "tomato", "cadetblue2", "cyan")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1))

AP_Dist

shapiro.test(AP_Df$Daily_Count[AP_Df$Period == "Before 04/07/24"])
shapiro.test(AP_Df$Daily_Count[AP_Df$Period == "After 04/07/24"])
shapiro.test(AP_Df$Daily_Count[AP_Df$Period == "Before 04/07/25"])
shapiro.test(AP_Df$Daily_Count[AP_Df$Period == "After 04/07/25"])
#not normally distributed

wilcox.test(Daily_Count ~ Period, data = AP_2025)