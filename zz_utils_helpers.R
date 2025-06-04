# Helper functions and color palettes


# Define Palette for categorical  – Okabe-Ito / Color Universal Design (CUD)
#cols_categorical <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")
#cols_categorical <- c("#009E73","#56B4E9","#E69F00",)
#cols_categorical <- c("#DF6963","#007D3D","#CF74A7","#E1CDB0" ) #new actionuni colors
#cols_categorical <- c("#009E73",  "#7570B3", "#56B4E9","#F0E442",)
cols_categorical <- c("#009E73","#CC79A7","#F0E442", "#56B4E9")


# Define Palette for oridnal variables

# cols_ordinal <- c("#DD8452", "#F1A719", "#afd4e3","#67A9CF", "#3c75b0")
# cols_ordinal <- c("#DD8452", "#F1A719", "#f5d8a2","#67A9CF", "#638eba")
# cols_ordinal <- c("#DD8452", "#F1A719", "#f5d8a2","#67A9CF", "#638eba", "#3c75b0")
# cols_ordinal <- c("#DD8452", "#F1A719", "#afd4e3","#67A9CF", "#3c75b0")
# cols_ordinal <- c("#DD8452", "#F1A719", "#a2bf8f","#67A9CF", "#638eba")
# cols_ordinal <- c("#E88C00", "#FFBF00", "#00A896","#007BFF", "#4A00B8")
# cols_ordinal <- c("#D67C30", "#FFB74D", "#6EC4A6","#2196F3", "#42A5F5")
# cols_ordinal <- c("#D67C30", "#FFB74D", "#6EC4A6", "#4CAF50", "#2196F3", "#42A5F5")
# cols_ordinal <- c("#DD8452", "#FFBF00", "#f5d8a2","#42A5F5", "#007BFF", "#3c75b0")
# cols_ordinal <- c("#A50026", "#D73027", "#FDAE61","#A6D96A", "#1A9850", "#006837")
cols_ordinal <- c("#D73027", "#DD8452", "#FFBF00","#AFD4E3", "#5EB7FF", "#2D7DE6")
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

