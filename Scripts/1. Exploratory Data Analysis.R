##############################
## MP Expenses Analysis ##
## 1. Data Exploratory Analysis ##
## Author: Martin Holding ##
## V0.1
##############################
library(dplyr)
library(readr)
library(ggplot2)

options(scipen = 999)

#FYI Election years: 2010, 2015, 2017, 2019, 2024

### Run some exploratory and descriptive analysis, average spends across categories, correlations etc.

master_data <- read.csv("Data/totalSpend_AllYears2.csv", header = TRUE)

master_data$Office.spend.int <- as.integer(master_data$Office...spend)
#How many MPs are represented?

length(unique(master_data$MP.name))

#How many constituencies?

length(unique(master_data$Constituency))

#How many mps occur more than once?

length(unique(master_data$MP.name[duplicated(master_data$MP.name)]))

#Store this list for later
mp_multiple_entries <- unique(master_data$MP.name[duplicated(master_data$MP.name)])

#First look at overall spends across the years. Use calculated as reported isn't available for all years. Calculated is the sum of 
#all available spend categories

mean_overall <- master_data %>%
  group_by(Financial.year) %>%
  summarise(avg = mean(Overall.total.spend..calculated.))

mean_overall_bar <- ggplot(mean_overall, aes(x = Financial.year, y = avg)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

show(mean_office_bar)

#Distributions of overall spends across MPs

office_scatter <- ggplot(master_data, aes(Financial.year, Overall.total.spend..calculated., fill = Financial.year)) +
  geom_violin() +
  geom_point(position = position_jitter(seed = 1, width = 0.2)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

show(office_scatter)

office_scatter2 <- ggplot(master_data, aes(Financial.year, Overall.total.spend..calculated., fill = Financial.year)) +
  geom_violin() +
  geom_boxplot (width=0.1, color="grey", alpha=0.2) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

show(office_scatter2)


#Based on this data, what is the predicted spend each year? Does anybody significantly exceed this?

#Simple model of just overall spend by year

master_data$Financial.year.factor <- factor(master_data$Financial.year)

spend_by_year_model <-  lm(Overall.total.spend..calculated. ~ Financial.year,  data = master_data)

#Check assumptions

# set up a 2x2 plot area
par(mfrow = c(2,2))

# generate diagnostic plots
plot(spend_by_year_model)


#Summarise results and plot the regression model
summary(spend_by_year_model)


# Convert category to numeric index
master_data$fy_index <- as.numeric(master_data$Financial.year.factor)

lm_year_by_overall_spend <- ggplot(master_data, aes(x = fy_index, y = Overall.total.spend..calculated.)) +
  geom_point() + 
  geom_smooth(method = "lm", se = TRUE) +
  scale_x_continuous(
    breaks = master_data$fy_index ,
    labels = master_data$Financial.year.factor
  ) +
  labs(title = "Year on year total overall spend", x = "Financial Year", y = "Overall Total Spend") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

show(lm_year_by_overall_spend)

#Get a list of MPs who have significantly overspent or underspent - based on absolute Z >= 2

master_data <- master_data %>%
  group_by(Financial.year) %>%
  mutate(z_score_overall = (Overall.total.spend..calculated. - mean(Overall.total.spend..calculated.)) / sd(Overall.total.spend..calculated.)) %>%
  ungroup()

over_under_spends <- master_data %>%
  filter(abs(z_score_overall) >= 2)

ggplot(over_under_spends, aes(x = Financial.year.factor, y = z_score_overall)) +
  geom_point()

#find those who over or under spend in multiple years 

over_under_spends %>% 

#Stacked bar chart showing breakdown of spends by category across the years



stacked_bar_overall <- ggplot(master_data, aes(Financial.year, ))

#Now visualise the average expenditures for office expenditure

#Average office spend for each year across all MPs
mean_office <- master_data %>% 
  group_by(Financial.year) %>%
  summarise(avg = mean(Office_spend, na.rm = TRUE))

mean_office_bar <- ggplot(mean_office, aes(x = Financial.year, y = avg)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

show(mean_office_bar)

#Distribution of office spends across all MPS 

office_scatter <- ggplot(master_data, aes(Financial.year, Office_spend, fill = Financial.year)) +
  geom_violin() +
  geom_point(position = position_jitter(seed = 1, width = 0.2)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

show(office_scatter)

office_scatter2 <- ggplot(master_data, aes(Financial.year, Office_spend, fill = Financial.year)) +
  geom_violin() +
  geom_boxplot (width=0.1, color="grey", alpha=0.2) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

show(office_scatter2)






mean_staffing <- master_data %>% 
  group_by(Financial.year) %>%
  summarise(avg = mean(Staffing.spend, na.rm = TRUE))

mean_travel <- master_data %>% 
  group_by(Financial.year) %>%
  summarise(avg = mean(Travel.and.subsistence.spend, na.rm = TRUE))


mean_staffing_bar <- ggplot(mean_staffing, aes(x = Financial.year, y = avg)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

mean_travel_bar <- ggplot(mean_travel, aes(x = Financial.year, y = avg)) +
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
