#### Data cleaning of the SWiMS 2024 data #####

# Load necessary libraries
library(ggplot2)
library(foreign)
library(dplyr)

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

# TODO: How many observations were removed due to less than 10 observations per institution?


# remove responses before 2024-05-13 (the official start of the survey)
table(dat$date >= as.Date("2024-05-13"))

dat <- dat %>%
  filter(as.Date(date, format = "%d.%m.%Y") >= as.Date("2024-05-13"))


# TODO: why are there data values 0, for example jobsatisfaciton, should be between 1 and 7
table(dat$jobSatisfaction)

# TODO: determine which participants just clicked through and are thus invalid
# TODO: generate report of how many observations were excluded due to the various reasons.

## save data as rdata object
codebook <- openxlsx::read.xlsx("SWiMS_Codebook_v2.xlsx")
save(dat, codebook, file = paste0("../data/SWiMS2024_Data_",Sys.Date(),".RProj"))
