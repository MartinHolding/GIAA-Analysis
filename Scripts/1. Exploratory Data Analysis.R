##############################
## MP Expenses Analysis ##
## 1. Data Exploratory Analysis ##
## Author: Martin Holding ##
## V0.1
##############################
library(dplyr)
library(readr)
library(ggplot2)
library(corrplot)

options(scipen = 999)

#FYI Election years: 2010, 2015, 2017, 2019, 2024

### Run some exploratory and descriptive analysis, average spends across categories, correlations etc.

master_data <- read.csv("Data/totalSpend_AllYears2.csv", header = TRUE)

#How many MPs are represented?

length(unique(master_data$MP.name))

#How many constituencies?

length(unique(master_data$Constituency))

#How many mps occur more than once?

length(unique(master_data$MP.name[duplicated(master_data$MP.name)]))

#Store this list for later
mp_multiple_entries <- unique(master_data$MP.name[duplicated(master_data$MP.name)])

#for controlling how much spend MPs are doing, note if their office in London Area or not (office and staffing), they own, rent or use hotels for accomondation in London
constiuencies_in_london <- master_data$Constituency[grep("^Standard London Area budget.*$", master_data$Office_reason.for.budget.set)]
constiuencies_not_in_london <-  master_data$Constituency[grep("^Standard non-London Area budget.*$", master_data$Office_reason.for.budget.set)]

master_data$london_office <- ifelse(master_data$Constituency %in% constiuencies_in_london, 1, 0)

#Note this will only work where I have access to accomodation reason for budget
master_data$accomodation_own_rent_hotel <- 0 #Other/unknown accomoodation status

master_data$accomodation_own_rent_hotel[grep(".*owns.*$", master_data$Accommodation_reason.for.budget.set)] <- 1 #owns their own home in London

master_data$accomodation_own_rent_hotel[grep(".*renting.*$", master_data$Accommodation_reason.for.budget.set)] <- 2 #rents home

master_data$accomodation_own_rent_hotel[grep(".*Hotels.*$", master_data$Accommodation_reason.for.budget.set)] <- 3 #Uses hotels


#First look at overall spends across the years. Use calculated as reported isn't available for all years. Calculated is the sum of 
#all available spend categories

mean_overall <- master_data %>%
  group_by(Financial.year) %>%
  summarise(avg = mean(Overall.total.spend..calculated.))

mean_overall_bar <- ggplot(mean_overall, aes(x = Financial.year, y = avg)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

show(mean_overall_bar)

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
#Controlling for office location and accommodation spend status

master_data <- master_data %>%
  group_by(Financial.year, london_office, accomodation_own_rent_hotel) %>%
  mutate(z_score_overall = (Overall.total.spend..calculated. - mean(Overall.total.spend..calculated.)) / sd(Overall.total.spend..calculated.)) %>%
  ungroup()

over_under_spends <- master_data %>%
  filter(abs(z_score_overall) >= 2)

ggplot(over_under_spends, aes(x = Financial.year.factor, y = z_score_overall)) +
  geom_point()

#find those who over or under spend in multiple years 

unique_mps <- unique(over_under_spends$MP.name)

mps_over_under_spending_multiple <- over_under_spends %>%  
  # Keep only relevant categories
  group_by(MP.name) %>%
  summarise(
    num_appearances = n(),                   # Total occurrences
    overspend_count = sum(z_score_overall > 0),     # Count where score > 0
    underspend_count   = sum(z_score_overall < 0),      # Count where score < 0
    .groups = "drop"
  )

# Repeat this analysis for the office, staffing and accomodation spends

#Office - control for office location london vs non london


master_data <- master_data %>%
  group_by(Financial.year, london_office) %>%
  mutate(z_score_office = (Office_spend - mean(Office_spend)) / sd(Office_spend)) %>%
  ungroup()

over_under_spends_office <- master_data %>%
  filter(abs(z_score_office) >= 2)

ggplot(over_under_spends_office, aes(x = Financial.year.factor, y = z_score_office)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#Staffing - control for office location london vs non london
master_data <- master_data %>%
  group_by(Financial.year, london_office) %>%
  mutate(z_score_staffing = (Staffing_spend - mean(Staffing_spend)) / sd(Staffing_spend)) %>%
  ungroup()

over_under_spends_staffing <- master_data %>%
  filter(abs(z_score_staffing) >= 2)

ggplot(over_under_spends_staffing, aes(x = Financial.year.factor, y = z_score_staffing)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#Accommodation
master_data <- master_data %>%
  group_by(Financial.year, accomodation_own_rent_hotel) %>%
  mutate(z_score_accomodation = (Accommodation_spend - mean(Accommodation_spend)) / sd(Accommodation_spend)) %>%
  ungroup()

over_under_spends_accomodation <- master_data %>%
  filter(abs(z_score_accomodation) >= 2)

ggplot(over_under_spends_accomodation, aes(x = Financial.year.factor, y = z_score_accomodation)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))



#Can spend predict office location? Use a logistic regression to predict office location (london vs non london)
#For this analysis I'm interested in whether the spend amounts can be used to predict office location, and if so, where any 
#spends misclassified. This might be indication of over/under spending and warrant further investigation

log_reg_data <- master_data %>% 
  select(MP.name, Office_spend, Staffing_spend, Accommodation_spend, Overall.total.spend..calculated., london_office)


#check correlations of predictor variables

# Remove the target column
predictors_data <- log_reg_data[, !(names(log_reg_data) %in% c("london_office", "MP.name"))]

#Some NAs are causing problems with the correlation function - remove these for now. Only affects a small number of rows
predictors_data <- na.omit(predictors_data)

# Calculate the correlation matrix
correlation_matrix <- cor(predictors_data)

# Plot the correlation heatmap

library(reshape2)
melted_corr_matrix <- melt(correlation_matrix)

# Plot the heatmap with ggplot2
ggplot(data = melted_corr_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name="Correlation") +
  theme_minimal() + # Minimal theme for a clean look
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(x = "", y = "") + # Remove axis labels
  geom_text(aes(Var1, Var2, label = round(value, 2)), color = "black", size = 4) +
  theme(axis.text=element_text(size=15))

#Overall total spend and staffing spend are highly correlated, and overall and office spend moderately so. Omit overall spend from regression.
#other variables look ok.

set.seed(1)

#Use 70% of dataset as training set and remaining 30% as testing set


sample <- sample(c(TRUE, FALSE), nrow(log_reg_data), replace=TRUE, prob=c(0.7,0.3))
train <- log_reg_data[sample, ]
test <- log_reg_data[!sample, ]  

#fit model
log_reg_model <- glm(london_office~Office_spend+Staffing_spend+Accommodation_spend, family="binomial", data=train) #omit overall spend as it will be correlated to the other 3


#view model summary
summary(log_reg_model)

#Assess fit using Mcfaddens R2 - values closer to 1 indicate better predictive performance

pscl::pR2(log_reg_model)["McFadden"] #.47 is quite good for McFadden


#predict the test set and see where it differs from 'reality'

test$office_loc_predict <- predict(log_reg_model, test, type="response")

test$london_office_prediction <- ifelse(test$office_loc_predict >= 0.5, 1, 0)

xtabs(~ london_office + london_office_prediction, data = test)


#Sensitivity and specificity analysis

sensitivity_london_office <- 268/(268+193) 

specificity_london_office <- 2592/(2592 + 113)

#Good at avoiding missclassifing non-london offices, not so good at finding the london ones

#Of the those whose office locations were incorrectly identified as being in London, how many are also over/underspending?

#bring together a data frame of the MPs who both have a non london location (but log reg suggested they did) and are in the overunderspend dataset
#Look at their z scores to see if htey are over or under spenders overall

mps_false_positive_london <- unique(test$MP.name[test$london_office_prediction == 1 & test$london_office == 0])

mps_FP_and_over_underspend <- mps_false_positive_london[(mps_false_positive_london %in% over_under_spends$MP.name)]


MPs_to_investigate <- master_data %>%
  filter(MP.name %in% mps_FP_and_over_underspend, abs(z_score_overall)>=2 ) %>%
  select (MP.name, Financial.year, Constituency, z_score_office, z_score_staffing, z_score_accomodation, z_score_overall)



##############################
  
#k means clustering? Just to see...if I can get it to work
library(factoextra)
library(cluster)


gap_stat <- clusGap(df,
                    FUN = kmeans,
                    nstart = 25,
                    K.max = 10,
                    B = 50)
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
