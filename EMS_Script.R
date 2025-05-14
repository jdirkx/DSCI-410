"Script for EMS Analysis
for DSCI 410L: ADSSJ
"

library("tidyverse")
library("ggplot2")

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
arrange(EMS_Concat, desc(abs(Percent_Difference)))