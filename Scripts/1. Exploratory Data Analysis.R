##############################
## MP Expenses Analysis ##
## 1. Data Exploratory Analysis ##
## Author: Martin Holding ##
## V0.1
##############################
library(dplyr)
library(readr)
library(ggplot2)

### Run some exploratory and descriptive analysis, average spends across categories, correlations etc.

master_data <- read.csv("Data/master_data.csv", header = TRUE)

#Some blank lines inserted - remove these

master_data <- master_data[!is.na(master_data$Year), ]

#How many MPs are represented?

length(unique(master_data$X.MP.s.name))

#How many constituencies?

length(unique(master_data$Constituency))

#How many mps occur more than once?

length(unique(master_data$X.MP.s.name[duplicated(master_data$X.MP.s.name)]))

#Store this list for later
mp_multiple_entries <- unique(master_data$X.MP.s.name[duplicated(master_data$X.MP.s.name)])

#First visualise the average expenditures for office expenditure

#Average office spend for each year across all MPs
mean_office <- master_data %>% 
  group_by(Year) %>%
  summarise(avg = mean(Office.spend, na.rm = TRUE))

mean_office_bar <- ggplot(mean_office, aes(x = Year, y = avg)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#Distribution of office spends across all MPS 

office_scatter <- ggplot(master_data, aes(Year, Office.spend, fill = Year)) +
  geom_violin() +
  geom_point(position = position_jitter(seed = 1, width = 0.2)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

show(office_scatter)

office_scatter2 <- ggplot(master_data, aes(Year, Office.spend, fill = Year)) +
  geom_violin() +
  geom_boxplot (width=0.1, color="grey", alpha=0.2) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

show(office_scatter2)






mean_staffing <- master_data %>% 
  group_by(Year) %>%
  summarise(avg = mean(Staffing.spend, na.rm = TRUE))

mean_travel <- master_data %>% 
  group_by(Year) %>%
  summarise(avg = mean(Travel.and.subsistence.spend, na.rm = TRUE))


mean_staffing_bar <- ggplot(mean_staffing, aes(x = Year, y = avg)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

mean_travel_bar <- ggplot(mean_travel, aes(x = Year, y = avg)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

show(mean_office_bar)
show(mean_staffing_bar)
show(mean_travel_bar)




office_scatter2 <- ggplot(master_data, aes(Year, Office.spend, fill = Year)) +
  geom_violin() +
  geom_boxplot (width=0.1, color="grey", alpha=0.2) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
show(office_scatter2)
