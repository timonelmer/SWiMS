#### Data cleaning of the SWiMS 2024 data #####

# Load necessary libraries
library(ggplot2)
library(foreign)
library(dplyr)
library(sjPlot)

# Read the SPSS data (adjust the file path as necessary)
dat <- foreign::read.spss("../data/data_project_22944_2024_11_14.sav", 
                          to.data.frame = TRUE, use.value.labels = TRUE)

### TODO: What is NA and what is O --> did a testrun with "Hallo da ist Bianca test" in the variable improvementMenthalhealth
# Filter the dataset to show only rows where improvementMentalhealth is "Hallo da ist Bianca test"
filtered_dat <- subset(dat, grepl("Hallo da ist Bianca test", improvementMentalhealth))

##burnOut = skipped (0)
filtered_dat$burnOut # --> 0

##stress = skipped (0)
filtered_dat$stress # --> 0

#nonacademicjob = skipped (0) --> workloadAdditionaljob, careKids, careFamily, careNo, careWorkhours should be NA (not shown)
filtered_dat$nonacademicJob # --> 0
filtered_dat$workloadAdditionaljob # --> NA
filtered_dat$careKids # --> NA
filtered_dat$careFamily # --> NA
filtered_dat$careNo # --> NA
filtered_dat$careWorkhours # --> NA

#conclusion --> 0 = skipped, NA = not shown

# Remove row where improvementMentalhealth contains "Hallo da ist Bianca"
dat <- subset(dat, !grepl("Hallo da ist Bianca", improvementMentalhealth, fixed = TRUE))

### compute some variables ###
dat$datetime <- as.POSIXct(dat$datetime, format = "%Y-%m-%d %H:%M:%S")
dat$date <- as.Date(dat$datetime)

## Data cleaning ## 
# these are the institutions we missed
table(dat$institutionother)

# Count the number of observations per institution in the original dataset, excluding "0" and "Prefer not to say"
institution_counts <- dat %>%
  filter(!is.na(institution) & institution != "0" & institution != "Prefer not to say") %>%  # Exclude "0" and "Prefer not to say"
  group_by(institution) %>%
  summarise(count = n()) %>%
  ungroup()

# Identify institutions with fewer than 10 observations
removed_institutions <- institution_counts %>%
  filter(count < 10)

# Display the observations removed and their counts
removed_institutions
sum(removed_institutions$count)
# --> total 41 observations removed because of fewer than 10 observations
# --> total of 15 institutions affected
dat <- dat[!(dat$institution %in% removed_institutions$institution),]


# Remove responses before 2024-05-13 (the official start of the survey)
official_start_date <- as.Date("2024-05-13")
excluded_before_start <- sum(as.Date(dat$date, format = "%d.%m.%Y") < official_start_date)
sum(excluded_before_start)

# Filter dataset to retain only responses on or after the official start date
dat <- dat %>%
  filter(as.Date(date, format = "%d.%m.%Y") >= official_start_date)

### Determine which participants just clicked through and are thus invalid
# Set the threshold for minimum duration (e.g., 4 minutes in seconds)
min_duration_threshold <- 4 * 60

# Filter participants who have a duration below the threshold
clicked_through_participants <- dat %>%
  filter(duration < min_duration_threshold & duration > -1)

# Count the number of excluded participants due to clicking through
num_excluded_clickthrough <- nrow(clicked_through_participants)
cat("Number of participants excluded due to clicking through:", num_excluded_clickthrough, "\n")

# Summary calculations for the table
# Total observations in the original data
total_observations <- nrow(dat) + sum(removed_institutions$count) +
  excluded_before_start + num_excluded_clickthrough

# Calculating each exclusion count for the table
excluded_few_observations <- sum(removed_institutions$count)  # Observations removed due to fewer than 10
excluded_before_start <- excluded_before_start  # Before official start
num_excluded_clickthrough <- num_excluded_clickthrough  # Participants excluded due to clicking through

# Calculate remaining observations after exclusions
remaining_observations <- total_observations - (excluded_few_observations +
                                                  excluded_before_start + num_excluded_clickthrough)

# Create a summary table with the counts and percentages
summary_table <- data.frame(
  Category = c(
    "Total Observations",
    "Excluded: Fewer than 10 Observations per Institution",
    "Excluded: Before official Start of the Survey",
    "Excluded: Clicked Through (Below Duration Threshold)",
    "Remaining Observations"
  ),
  Count = c(
    total_observations,
    excluded_few_observations,
    excluded_before_start,
    num_excluded_clickthrough,
    remaining_observations
  ),
  Percentage = c(
    100,
    (excluded_few_observations / total_observations) * 100,
    (excluded_before_start / total_observations) * 100,
    (num_excluded_clickthrough / total_observations) * 100,
    (remaining_observations / total_observations) * 100
  )
)

# Display the table with sjPlot
tab_df(summary_table, title = "Observations Summary", show.rownames = FALSE)


# recode institution
inst.code <- xlsx::read.xlsx("../codebook_project_22944_2024_05_13.xlsx",2, header = F)
dat$inst <- dat$institution
for(i in 1:nrow(inst.code)){
  dat[dat$institution %in% i,"inst"] <- inst.code[inst.code$X1 %in% i,"X2"]
}

## remove participants without an institution 
nrow(dat[dat$inst %in% c("Other"),])
nrow(dat[dat$institution %in% c(NA),])
nrow(dat[dat$institution %in% 0,])
nrow(dat[dat$inst %in% c("Prefer not to say"),])
dat <- dat[!(dat$inst %in% c("Other", NA, "Prefer not to say")) ,]
dat <- dat[!(dat$institution %in% 0) ,]

### TODO: count number of NA's and number of 0s per person and show distribution
# Count the number of NA's and 0's per person
dat$na_count <- apply(dat, 1, function(x) sum(is.na(x)))
dat$zero_count <- apply(dat, 1, function(x) sum(x == 0, na.rm = TRUE))

# Convert counts to a long format for easier plotting
count_data <- data.frame(
  id = seq_len(nrow(dat)),
  na_count = dat$na_count,
  zero_count = dat$zero_count
)

# Plot the distribution of NA counts
ggplot(count_data, aes(x = na_count)) +
  geom_bar() +
  labs(title = "Distribution of NA Counts per Person",
       x = "Number of NAs",
       y = "Frequency") +
  theme_minimal()

# Plot the distribution of 0 counts
ggplot(count_data, aes(x = zero_count)) +
  geom_bar() +
  labs(title = "Distribution of Zero Counts per Person",
       x = "Number of Zeros",
       y = "Frequency") +
  theme_minimal()


##  detangling strategy: Mutate all meaningful 0 to x and then mutate all other 0 (skipped questions) to an NA, then mutate the x back to 0
# for all variables that do contain the level "quoted" and "not quoted", replace 0s with NAs

# # Mutate consent variables to change 1 to "x"
# dat <- dat %>%
#   mutate(
#     consent1 = if_else(consent1 == 0, "x", as.character(consent1)),
#     consent2 = if_else(consent2 == 0, "x", as.character(consent2)),
#     consent3 = if_else(consent3 == 0, "x", as.character(consent3))
#   )
# 
# # Mutate gender variables to change 0 to "x"
# dat <- dat %>%
#   mutate(
#     female = if_else(female == 0, "x", as.character(female)),
#     male = if_else(male == 0, "x", as.character(male)),
#     nonbinary = if_else(nonbinary == 0, "x", as.character(nonbinary)),
#     genderNA = if_else(genderNA == 0, "x", as.character(genderNA))
#   )
# 
# # Mutate care variables to change 0 to "x"
# dat <- dat %>%
#   mutate(
#     careKids = if_else(careKids == 0, "x", as.character(careKids)),
#     careFamily = if_else(careFamily == 0, "x", as.character(careFamily)),
#     careNo = if_else(careNo == 0, "x", as.character(careNo))
#   )
# 
# # Mutate addhours variables to change 0 to "x"
# dat <- dat %>%
#   mutate(
#     researchwitharAddhours = if_else(researchwitharAddhours == 0, "x", as.character(researchwitharAddhours)),
#     researchwithoutarAddhours = if_else(researchwithoutarAddhours == 0, "x", as.character(researchwithoutarAddhours)),
#     fundingapplicAddhours = if_else(fundingapplicAddhours == 0, "x", as.character(fundingapplicAddhours)),
#     teachingAddhours = if_else(teachingAddhours == 0, "x", as.character(teachingAddhours)),
#     supervisingAddhours = if_else(supervisingAddhours == 0, "x", as.character(supervisingAddhours)),
#     adminAddhours = if_else(adminAddhours == 0, "x", as.character(adminAddhours)),
#     politicalworkAddhours = if_else(politicalworkAddhours == 0, "x", as.character(politicalworkAddhours))
#   )
# 
# # Mutate discrimination variables to change 0 to "x"
# dat <- dat %>%
#   mutate(
#     discrAge = if_else(discrAge == 0, "x", as.character(discrAge)),
#     discrSES = if_else(discrSES == 0, "x", as.character(discrSES)),
#     discrDisability = if_else(discrDisability == 0, "x", as.character(discrDisability)),
#     discrSex = if_else(discrSex == 0, "x", as.character(discrSex)),
#     discrGender = if_else(discrGender == 0, "x", as.character(discrGender)),
#     discrSexualorientation = if_else(discrSexualorientation == 0, "x", as.character(discrSexualorientation)),
#     discrPhysical = if_else(discrPhysical == 0, "x", as.character(discrPhysical)),
#     discrLanguage = if_else(discrLanguage == 0, "x", as.character(discrLanguage)),
#     discrNationality = if_else(discrNationality == 0, "x", as.character(discrNationality)),
#     discrEthnicity = if_else(discrEthnicity == 0, "x", as.character(discrEthnicity)),
#     discrReligion = if_else(discrReligion == 0, "x", as.character(discrReligion)),
#     discrCompetency = if_else(discrCompetency == 0, "x", as.character(discrCompetency)),
#     discrHierarchy = if_else(discrHierarchy == 0, "x", as.character(discrHierarchy)),
#     discrUnknown = if_else(discrUnknown == 0, "x", as.character(discrUnknown)),
#     discrOther = if_else(discrOther == 0, "x", as.character(discrOther)),
#     discrNA = if_else(discrNA == 0, "x", as.character(discrNA)),
#     discrfundingSNF = if_else(discrfundingSNF == 0, "x", as.character(discrfundingSNF)),
#     discrfundingnonSNF = if_else(discrfundingnonSNF == 0, "x", as.character(discrfundingnonSNF)),
#     discrfundingUniversity = if_else(discrfundingUniversity == 0, "x", as.character(discrfundingUniversity)),
#     discrother1 = if_else(discrother1 == 0, "x", as.character(discrother1)),
#     discrother2 = if_else(discrother2 == 0, "x", as.character(discrother2)),
#     dupl1_discrUnknown = if_else(dupl1_discrUnknown == 0, "x", as.character(dupl1_discrUnknown)),
#     dupl1_discrNA = if_else(dupl1_discrNA == 0, "x", as.character(dupl1_discrNA))
#   )
# 
# # Mutate pressure variables to change 0 to "x"
# dat <- dat %>%
#   mutate(
#     pressureqrpFunder = if_else(pressureqrpFunder == 0, "x", as.character(pressureqrpFunder)),
#     pressureqrpStakeholder = if_else(pressureqrpStakeholder == 0, "x", as.character(pressureqrpStakeholder)),
#     pressureqrpSupervisor = if_else(pressureqrpSupervisor == 0, "x", as.character(pressureqrpSupervisor)),
#     pressureqrpCollegueIn = if_else(pressureqrpCollegueIn == 0, "x", as.character(pressureqrpCollegueIn)),
#     pressureqrpManagerIn = if_else(pressureqrpManagerIn == 0, "x", as.character(pressureqrpManagerIn)),
#     pressureqrpCollegueOut = if_else(pressureqrpCollegueOut == 0, "x", as.character(pressureqrpCollegueOut)),
#     pressureqrpManagerOut = if_else(pressureqrpManagerOut == 0, "x", as.character(pressureqrpManagerOut)),
#     pressureqrpCompetitiveenviron = if_else(pressureqrpCompetitiveenviron == 0, "x", as.character(pressureqrpCompetitiveenviron)),
#     pressureqrpMyself = if_else(pressureqrpMyself == 0, "x", as.character(pressureqrpMyself)),
#     pressureqrpOther1 = if_else(pressureqrpOther1 == 0, "x", as.character(pressureqrpOther1)),
#     pressureqrpOther2 = if_else(pressureqrpOther2 == 0, "x", as.character(pressureqrpOther2)),
#     pressureNo = if_else(pressureNo == 0, "x", as.character(pressureNo))
#   )
# 
# # Mutate institution variable to change 0 to "x"
# # dat <- dat %>%
# #   mutate(
# #     institution = if_else(institution == 0, "x", as.character(institution))
# #   )
# # 
# # Replace 0s with NA in all other variables where 0 indicates a skipped question
# dat <- dat %>%
#   mutate(across(everything(), ~ if_else(. == 0, NA, .)))

# # Convert all columns with "x" factors to 0 in numeric form
# dat <- dat %>%
#   mutate(across(where(is.factor), ~ {
#     # Convert factor to character for processing
#     . <- as.character(.)
#     # Replace "x" with 0, keeping other values as they are
#     . <- if_else(. == "x", "0", .)
#     # Convert to numeric, coercing only valid numbers, and leaving others as NA
#     as.numeric(.)
#   }))

# Identify columns that contain levels "quoted" and "not quoted"
columns_to_update <- sapply(dat, function(col) {
  is.factor(col) && !(all(c("quoted", "not quoted") %in% levels(col)))
})

# Replace 0s with NA in the identified columns
dat[columns_to_update] <- lapply(dat[columns_to_update], function(col) {
  levels(col)[levels(col) == "0"] <- NA     # Replace level "0" with NA
  col[col == "0"] <- NA                     # Replace values "0" with NA
  droplevels(col)                         # Drop the unused "0" level
})


# Create the new 'gender' variable
dat <- dat %>%
  mutate(gender = case_when(
    male == "quoted" & female == "not quoted" & nonbinary == "not quoted" ~ "male",
    female == "quoted" & male == "not quoted" & nonbinary == "not quoted" ~ "female",
    (nonbinary == "quoted") | (male == "quoted" & female == "quoted")  ~ "other",
    genderNA == "quoted" ~ NA_character_,
    TRUE ~ NA_character_  # Any other combinations are assigned as NA
  ))

# Create the new 'carework' variable
dat <- dat %>%
  mutate(carework = case_when(
    careNo == "quoted"  ~ "no care work",
    careFamily == "quoted" | careKids == "quoted" & nonbinary == "not quoted" ~ "care work"
  ))


## some codebook processing
codebook <- openxlsx::read.xlsx("SWiMS_Codebook_v5.xlsx")
codebook$question <- gsub(pattern = " \\(q_.*$", replacement = "", codebook$Item)

## data on institutions
dat.meta <- openxlsx::read.xlsx("SWiMS_institutions.xlsx")


## save data as rdata object
save(dat, dat.meta,  codebook, file = paste0("../data/SWiMS2024_Data_",Sys.Date(),".RData"))
