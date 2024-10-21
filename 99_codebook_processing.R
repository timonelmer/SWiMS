### SWiMS 2024: Codebook processing ###
### /TE, Oct 2024 ###

library(readxl)
library(gtranslate)
library(tidyverse)
library(psych)
library(ggplot2)
rm(list = ls())
#source("99_helpers.R")

c <- read_xlsx(path = "SWiMS_Codebook_v1.xlsx",sheet = 1) 
c.long <- read_xlsx("codebook_project_22944_2024_10_18.xlsx",sheet = 1) 
colnames(c.long) <- c("var","varExt","type","levels")

# Function to insert empty rows above rows where var starts with "v_" and the line above in varExt is NA
insert_empty_rows <- function(df, column_name = "var", prefix = "v_", check_column = "varExt", num_rows = 2) {
  new_df <- data.frame(var = NA,varExt = NA,type =NA, levels =NA)  # Initialize an empty dataframe with the same column names
  #colnames(new_df) <- colnames(df)
  
  for (i in seq_len(nrow(df))) {
    # Check if the current row's value starts with the specified prefix
    starts_with_prefix <- !is.na(df[i, column_name]) && startsWith(as.character(df[i, column_name]), prefix)
    
    # Check if the previous row's value in check_column is NA
    previous_is_na <- if (i == 1) FALSE else is.na(df[i - 1, check_column])
    
    if (starts_with_prefix && previous_is_na) {
      # Create empty rows with the same column names
      empty_rows <- data.frame(matrix(NA, nrow = num_rows, ncol = ncol(df)))
      colnames(empty_rows) <- colnames(df)
      
      # Add empty rows and the current row to new_df
      new_df <- rbind(new_df, empty_rows, df[i, ])
    } else {
      # Add the current row to new_df
      new_df <- rbind(new_df, df[i, ])
    }
  }
  
  return(new_df)
}


c.long <- insert_empty_rows(c.long) # add two empty rows above each variable for better parsing later

var.starts <- grep("^v_",c.long$var)
for(var in var.starts){
  # extract relevant information per variable
  var.name <- c.long[var,"var"]
  cat(paste0("\r",var," ",var.name))
  item <- c.long[var-3,"var"]
  item.label <- c.long[var,"levels"]
  type <- c.long[var,"type"]
  var.name.new <- c.long[c.long$var %in% var.name,"varExt"]
  
  var.end <- var+(which(is.na(c.long[var:nrow(c.long),"levels"]))[1]-2)
  if(var == 1361) var.end = var
  if(var == var.end){ # if only one row of data
    values <- paste(c.long[(var):var.end,"type"], collapse = "//")
    labels <- paste(c.long[(var):var.end,"levels"], collapse = "//")
  }else{
    values <- paste(c.long[(var+1):var.end,"type"], collapse = "//")
    labels <- paste(c.long[(var+1):var.end,"levels"], collapse = "//")
  }
  # feed it back into the general codebook file
  c.var <- which(c$InternalName == var.name.new)
  #c[c.var,"VarName"] <- var.name
  c[c.var,"Item"] <- item
  c[c.var,"Label"] <- item.label
  c[c.var,"Type"] <- type
  c[c.var,"Values"] <- values
  c[c.var,"Labels"] <- labels
}

openxlsx::write.xlsx(c,"SWiMS_Codebook_v2.xlsx")
