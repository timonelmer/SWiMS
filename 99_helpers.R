# Helper functions


### other helpers ###

# Function to add line breaks to x-axis labels
break_labels <- function(labels, Z = 20) {
  sapply(labels, function(label) {
    # Insert line breaks after every Z characters at the closest space or punctuation
    gsub(paste0("(.{1,", Z, "}\\b)(?=\\S)"), "\\1\n", label, perl = TRUE)
  })
}


#### SWIMS Plots #####

swims.plot.distribution <- function(var, institution = NULL,  data = dat, codeb = codebook){
  
  # Ensure var exists in the codeb
  if (!var %in% codeb$VarName) {
    stop(paste("Variable", var, "not found in the codeb."))
  }
  
  # Use variable label if available
  x_label <- strsplit(codeb[codeb$VarName %in% var,"Labels"],"//")[[1]]
  
  
  if(!is.null(institution)){
  # Prepare data for plotting
  plot_data <- data %>%
    mutate(group = ifelse(institution == target_institution, paste0(target_institution), "Other Institutions")) %>%
    filter(!is.na(!!sym(var)), !is.na(group)) %>%
    group_by(group, !!sym(var)) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(group) %>%
    mutate(proportion = count / sum(count))
  
  # Create plot
  g <- ggplot(plot_data, aes_string(x = var, y = "proportion", fill = "group")) +
    geom_bar(stat = "identity", position = "dodge", width = 0.8) +
    scale_y_continuous(labels = scales::percent, limits = c(0,max(plot_data$proportion+.05))) +
    labs(
      x = var,
      y = "Proportion",
      fill = "Institution",
      title = paste("Distribution of", var)
    ) +
    scale_fill_manual(values = c(myblue, myorange)) +
    geom_text(aes(label = count), position = position_dodge(width = 0.8), vjust = -0.5, size = 4) +
    theme_minimal() +
    theme(
      text = element_text(size = 12),
      axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 10),
      axis.title = element_text(size = 12),
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 10),
      plot.title = element_text(size = 14, hjust = 0.5)
    ) + 
    scale_x_discrete(labels = function(x) str_wrap(x, width = 30))   # Apply text wrapping
    
  
  plot(g)
  }else{ #plot for no intitution specified
    
    # Prepare data for plotting
    plot_data <- data %>%
      filter(!is.na(!!sym(var))) %>%
      group_by(!!sym(var)) %>%
      summarise(count = n(), .groups = "drop") %>%
      mutate(proportion = count / sum(count))
    
    # Create plot
    g <- ggplot(plot_data, aes_string(x = var, y = "proportion")) +
      geom_bar(stat = "identity", fill = myblue, width = 0.8) +
      scale_y_continuous(labels = scales::percent, limits = c(0, max(plot_data$proportion + 0.05))) +
      labs(
        x = "",
        y = "Proportion"#,
        #title = paste("Distribution of", var)
      ) +
      geom_text(aes(label = count), vjust = -0.5, size = 4) +
      theme_minimal() +
      theme(
        text = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 14, hjust = 0.5)
      )  + 
      scale_x_discrete(labels = function(x) str_wrap(x, width = 30))   # Apply text wrapping
      
    
    plot(g)
  }
}



#### colors ####

myblue <- "#1f77b4"
myorange <- "#ff7f0e"
mygreen <- "forestgreen"
myyellow <- "yellow"
mypurple <- "purple"