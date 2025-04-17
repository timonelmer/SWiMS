# Helper functions 
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