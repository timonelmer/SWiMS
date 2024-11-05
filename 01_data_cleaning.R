#### Data cleaning of the SWiMS 2024 data #####

# Load necessary libraries
library(ggplot2)
library(foreign)
library(dplyr)
library(sjPlot)

# Read the SPSS data (adjust the file path as necessary)
dat <- foreign::read.spss("../data/data_project_22944_2024_10_18.sav", to.data.frame = TRUE, use.value.labels = TRUE)

### compute some variables ###
dat$datetime <- as.POSIXct(dat$datetime, format = "%Y-%m-%d %H:%M:%S")
dat$date <- as.Date(dat$datetime)

##data cleaning ## 

# these are the institutions we missed
table(dat$institutionother)

# Filter the data: remove rows without institution or with fewer than 10 observations per institution
table(dat$institution %in% c("0","Prefer not to say",NA))
dat <- dat %>%
  filter(!is.na(institution) & 
           institution != "0" & 
           institution != "Prefer not to say") %>%
  group_by(institution) %>%
  filter(n() >= 10) %>%
  ungroup()

### TODO: How many observations were removed due to less than 10 observations per institution?
##institution which were removed, because of less than 10 observations
# Count the number of observations per institution in the original dataset
institution_counts <- dat %>%
  filter(!is.na(institution) & 
           institution != "0" & 
           institution != "Prefer not to say") %>%
  group_by(institution) %>%
  summarise(count = n()) %>%
  ungroup()

# Identify institutions with fewer than 10 observations
removed_institutions <- institution_counts %>%
  filter(count < 10)

# Display the institutions removed and their counts --> none were removed
removed_institutions

##number of institutions removed because of less than 10 observations
# Original number of rows in the dataset
original_count <- nrow(dat)

# Filter the data to remove institutions with fewer than 10 observations
dat_filtered <- dat %>%
  filter(!is.na(institution) & 
           institution != "0" & 
           institution != "Prefer not to say") %>%
  group_by(institution) %>%
  filter(n() >= 10) %>%
  ungroup()

# Count the number of rows in the filtered dataset
filtered_count <- nrow(dat_filtered)

# Calculate the number of observations removed
observations_removed <- original_count - filtered_count
cat("Number of observations removed due to institutions with fewer than 10 observations:", observations_removed, "\n")


# remove responses before 2024-05-13 (the official start of the survey)
table(dat$date >= as.Date("2024-05-13"))

dat <- dat %>%
  filter(as.Date(date, format = "%d.%m.%Y") >= as.Date("2024-05-13"))


### TODO: why are there data values 0, for example jobsatisfaciton, should be between 1 and 7
##table with job satisfaction frequencies
job_satisfaction_table <- dat %>%
  group_by(jobSatisfaction) %>%
  summarise(Frequency = n()) %>%
  arrange(desc(Frequency))  # Optional: Sort by frequency, if desired

print(job_satisfaction_table)

# Filter rows where jobSatisfaction is 0
zero_job_satisfaction <- dat %>%
  filter(jobSatisfaction == 0)

# Display the rows with jobSatisfaction = 0
print(zero_job_satisfaction)


### TODO: determine which participants just clicked through and are thus invalid
# Set the threshold for minimum duration (e.g. 60 seconds)
min_duration_threshold <- 60

# Filter participants who have a duration below the threshold
clicked_through_participants <- dat %>%
  filter(duration < min_duration_threshold)

# Display these participants
print(clicked_through_participants)

# Count the number of excluded participants --> 151
num_excluded <- nrow(clicked_through_participants)
cat("Number of participants excluded due to clicking through:", num_excluded, "\n")


### TODO: generate report of how many observations were excluded due to the various reasons.
# Initial total number of observations
total_observations <- nrow(dat)

# Exclude based on institution with fewer than 10 observations
excluded_institution <- dat %>%
  filter(!is.na(institution) & institution != "0" & institution != "Prefer not to say") %>%
  group_by(institution) %>%
  filter(n() < 10) %>%
  ungroup()
num_excluded_institution <- nrow(excluded_institution)
percent_excluded_institution <- (num_excluded_institution / total_observations) * 100

# Filter data to remove institutions with fewer than 10 observations
dat_filtered_institution <- dat %>%
  filter(!is.na(institution) & institution != "0" & institution != "Prefer not to say") %>%
  group_by(institution) %>%
  filter(n() >= 10) %>%
  ungroup()

# Exclude based on duration below threshold
min_duration_threshold <- 60
excluded_duration <- dat_filtered_institution %>%
  filter(duration < min_duration_threshold)
num_excluded_duration <- nrow(excluded_duration)
percent_excluded_duration <- (num_excluded_duration / total_observations) * 100

# Calculate remaining observations
remaining_observations <- total_observations - num_excluded_institution - num_excluded_duration

# Create summary table
exclusion_summary <- tibble(
  Criterion = c("Total Observations", 
                "Excluded: Fewer than 10 Observations per Institution", 
                "Excluded: Duration Below Threshold",
                "Remaining Observations"),
  Count = c(total_observations, 
            num_excluded_institution, 
            num_excluded_duration, 
            remaining_observations),
  Percentage = c(100, 
                 percent_excluded_institution, 
                 percent_excluded_duration, 
                 (remaining_observations / total_observations) * 100)
)

# Display the table using sjPlot
sjPlot::tab_df(exclusion_summary, title = "Exclusion Summary", show.rownames = FALSE)


## save data as rdata object
codebook <- openxlsx::read.xlsx("SWiMS_Codebook_v2.xlsx")
save(dat, codebook, file = paste0("../data/SWiMS2024_Data_",Sys.Date(),".RData"))
