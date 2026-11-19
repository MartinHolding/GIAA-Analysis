##############################
## MP Expenses Analysis ##
## 0. Data PreProcessing ##
## Author: Martin Holding ##
## V0.1
##############################

setwd("C:/Users/marti/OneDrive/GIAA Analysis")

library(dplyr)
library(readr)


#Section 1

# Bring in all the files and do some exploratory analysis


#Load in each file separately and also create a master set containing all years data

all_files <- list.files(path = 'Data/', full.names = TRUE, recursive = TRUE)
filenames <- list.files(path = 'Data/', full.names = FALSE, recursive = TRUE)

 #Loop through all files and load each in, one at a time
for (f in 1:length(all_files)) {
  print(f)
  temp_data <- read.csv(all_files[f], colClasses = "character")
  assign(filenames[f], temp_data, envir = .GlobalEnv)  
}

# Bind them together to create a master set containing all years and all data
data_names <- ls(pattern = "^totalSpend_")
data_frame_list <- lapply(data_names, function(x) get(x, envir = .GlobalEnv))
names(data_frame_list) <- data_names
master_data <- bind_rows(data_frame_list, .id = "Year")


###Section 2

# Do some data cleaning

#In order to get all data into one frame, I had to load in all columns as characters. Go through and set the correct columns
# back to integer.

# Some years include a £ sign in the budgets, strip this and create a clean version
master_data <- master_data %>%
  mutate(across(
    where(~ any(grepl("£", .x))),  # Select columns with £ present
    ~ as.integer(parse_number(.x)) # Strip £ and convert to integer
  ))

#Check to make sure all integer columns are integers
all_cols <- colnames(master_data)
integer_cols <- c(all_cols[4], all_cols[6], all_cols[7], all_cols[8], all_cols[10], all_cols[11], 
                  all_cols[12], all_cols[14], all_cols[15], all_cols[16], all_cols[17],
                  all_cols[19], all_cols[20], all_cols[22], all_cols[24],
                  all_cols[25], all_cols[26], all_cols[28], all_cols[29], all_cols[30],
                  all_cols[32], all_cols[33], all_cols[34], all_cols[35], all_cols[36], all_cols[37],
                  all_cols[38], all_cols[39])

col_types <- sapply((master_data[integer_cols]), class)

sum(col_types != "integer")

col_types[which(col_types != "integer")]


#Save Master Data

write.csv(master_data, "Data/master_data.csv", quote = FALSE)

