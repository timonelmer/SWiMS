# Helper functions and color palettes


# Define Palette for categorical  – Okabe-Ito / Color Universal Design (CUD)
cols_categorical <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")
# Define Palette for oridnal variables

cols_ordinal <- c("#DD8452", "#F1A719", "#afd4e3","#67A9CF", "#3c75b0")
cols_ordinal <- c("#DD8452", "#F1A719", "#f5d8a2","#67A9CF", "#638eba")
cols_ordinal <- c("#DD8452", "#F1A719", "#f5d8a2","#67A9CF", "#638eba", "#3c75b0")

# 
# palette1 <- c("#B2182B", "#EF8A62", "#F7F7F7", "#67A9CF", "#2166AC")
# 
# palette2 <- c("#CA562C", "#E08214", "#F5F5F5", "#80CDC1", "#018571")
# 
# palette3 <- c("#A50026", "#D73027", "#F0F0F0", "#4575B4", "#313695")
# 
# palette4 <- c("#C44E52", "#DD8452", "#E6E6E6", "#7EBDC2", "#4C72B0")
# 
# palette5 <- c("#B35806", "#F1A340", "#F7F7F7", "#998EC3", "#542788")



# Function to add line breaks to x-axis labels
break_labels <- function(labels, Z = 20) {
  sapply(labels, function(label) {
    # Insert line breaks after every Z characters at the closest space or punctuation
    gsub(paste0("(.{1,", Z, "}\\b)(?=\\S)"), "\\1\n", label, perl = TRUE)
  })
}

# Function to cut after first ","
remove_before_and_comma <- function(x) {
  sub("^[^,]*, *", "", x)
}

# Function to add an fake level for space
insert_level <- function(fct, new_level, position) {
  old_levels <- levels(fct)
  if (new_level %in% old_levels) {
    warning("Level already exists. Returning original factor.")
    return(fct)
  }
  
  new_levels <- append(old_levels, new_level, after = position - 1)
  factor(fct, levels = new_levels)
}

# Function to formulate the question of a variable
swims.formulation <- function(var, codeb = codebook, what = "all"){
  Item <- codeb[codeb$VarName %in% var,"Item"]
  Item <- gsub(pattern = ' \\(q_.*',"",Item)
  
  Labels <- codeb[codeb$VarName %in% var,"Labels"]
  Labels <- gsub("//"," ; ",Labels)
  
  if(what == "item") return(paste0("Question: '",Item,"'"))
  if(what == "labels") return(Labels)
  if(what == "all") return(paste0("Question: '",Item,"', with answer options: ", Labels,"."))
}

# Add watermark 
#swims.watermark <- annotate("text", x =1, y = 1, label = "SWiMS24", size = 20, alpha = 0.1, fontface = "bold", color = "gray80") 

# Filter function 
# Throw out every bar that has less than x observations
swims.filter <- function(data, cutter, lower.limit = NULL, divider = FALSE){
  
  if(divider){
    
    
    
  } else {
    
    data %>%
      filter(get(cutter) > lower.limit) # more than lower.limit
    
  }
}

