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

# Add watermark 
#swims.watermark <- annotate("text", x =1, y = 1, label = "SWiMS24", size = 20, alpha = 0.1, fontface = "bold", color = "gray80") 

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


swims.plot.multibar <- function(var, 
                                institution = NULL, 
                                data = dat, 
                                codeb = codebook, 
                                fill_colors = NULL,
                                colors_set = "Set1"
){
  
  # Define variable
  var_org <- var
  var <- codeb[codeb$Category %in% var,"VarName"]
  var_text <- data.frame("variable" = var,"text" = codeb[codeb$VarName %in% var,"Label"])
  
  # Ensure var exists in the codeb
  if (!all(var %in% codeb$VarName)) {
    stop(paste("Variable", var, "not found in the codeb."))
  }
  
  # Use variable label if available
  x_label <- strsplit(codeb[codeb$VarName %in% var,"Labels"],"//")[[1]]
  
  
  if(!is.null(institution)){ # for institution specific analyses
        # Prepare data for plotting
        plot_data <- data %>%
          mutate(group = ifelse(institution == target_institution, paste0(target_institution), "Other Institutions")) %>%
          mutate(group = factor(group, levels = c("Other Institutions", setdiff(levels(factor(group)), "Other Institutions")))) %>%
          filter(if_all(all_of(var), ~ !is.na(.)), !is.na(group)) %>%
          pivot_longer(cols = all_of(var), names_to = "variable", values_to = "value") %>%
          group_by(group, variable, value) %>%
          summarise(count = n(), .groups = "drop") %>%
          group_by(group, variable) %>%
          mutate(proportion = count / sum(count)) %>% 
          ungroup() 
        
        # Change values for varName
        plot_data <- plot_data %>%
          left_join(var_text, by = "variable") %>%
          select(-variable) %>% 
          mutate(text = str_wrap(text, width = 20))
        
        
        # Compute the range of the sum of counts per text variable
        range_values <- plot_data %>%
          group_by(text) %>%
          summarise(total_count = sum(count)) %>%
          summarise(min_obs = min(total_count), max_obs = max(total_count))
        
        # Store as a formatted text for annotation
        if(range_values$min_obs == range_values$max_obs){
          range_text <- paste0("Observations per item: ", range_values$min_obs)
        }else{
          range_text <- paste0("Observations per item: ", range_values$min_obs, " to ", range_values$max_obs)
        }
        
        
        if(is.null(fill_colors)){
          fill_colors <- RColorBrewer::brewer.pal(n = min(length(unique(plot_data$value)), 9), name = colors_set)
        } 
        
        
        # Create plot
        plot_function <- function(plot_data) {
          
          # Plot erstellen
          g <- ggplot(plot_data, aes(x = group, y = proportion, fill = value)) +  
            geom_bar(stat = "identity", position = "fill", width = 0.6) +  # Stacked Bar Chart
            scale_y_continuous(labels = scales::percent) +  
            labs(
              x = NULL,   # Entferne x-Achsen-Beschriftung
              y = "Proportion",
              fill = "Response",
              title = paste0("Comparison of Responses by Institution: ", var_org)
            ) +
            scale_fill_manual(values = fill_colors) +  
            facet_wrap(~ text, ncol = 2, strip.position = "left") +  # Zwei-Spalten-Layout
            coord_flip() +  # Dreht das Diagramm (horizontal)
            # Add text annotation for observation range at bottom right
            annotate("text", x = Inf, y = Inf, label = range_text, size = 4, hjust = 1, vjust = 1) +  
            #swims_watermark +
            
            # Füge Gruppenlabels für beide Balken korrekt hinzu
            geom_text(data = plot_data %>% distinct(text, group, .keep_all = TRUE), 
                      aes(x = group, y = 0.05, label = group),  
                      inherit.aes = FALSE, 
                      size = 5, fontface = "bold",
                      hjust = 0,
                      position = position_nudge(x = -0.5)) +
            
            theme_minimal() +
            theme(
              text = element_text(size = 12),
              strip.text.y.left = element_text(size = 14, face = "bold", angle = 0),  # Lesbare Facet-Titel
              axis.text.y = element_blank(),  # Entferne die ursprünglichen Gruppen-Namen links
              axis.text.x = element_blank(),  # Entferne die x-Achsen-Beschriftungen
              axis.title.x = element_blank(), # Entferne x-Achsen-Titel komplett
              axis.title = element_text(size = 12),
              legend.text = element_text(size = 10),
              legend.title = element_text(size = 10),
              plot.title = element_text(size = 14, hjust = 0.5)
            )
          
          return(g)
        }
        plot_function(plot_data)
    }
    
    
  else{ # for national level analyses
    # Prepare data for plotting (without institution differentiation)
    plot_data <- data %>%
      filter(if_all(all_of(var), ~ !is.na(.))) %>%  # Remove NAs in selected variables
      pivot_longer(cols = all_of(var), names_to = "variable", values_to = "value") %>%
      group_by(variable, value) %>%
      summarise(count = n(), .groups = "drop") %>%
      group_by(variable) %>%
      mutate(proportion = count / sum(count)) %>%
      ungroup()
    
    # Add labels for variables
    plot_data <- plot_data %>%
      left_join(var_text, by = "variable") %>%
      select(-variable) %>%
      mutate(text = str_wrap(text, width = 40))
    
    # Compute the range of the sum of counts per text variable
    range_values <- plot_data %>%
      group_by(text) %>%
      summarise(total_count = sum(count)) %>%
      summarise(min_obs = min(total_count), max_obs = max(total_count))
    
    # Store as a formatted text for annotation
    if(range_values$min_obs == range_values$max_obs){
      range_text <- paste0("Observations per item: ", range_values$min_obs)
    }else{
      range_text <- paste0("Observations per item: ", range_values$min_obs, " to ", range_values$max_obs)
    }
    
    
    # Set colors if not provided
    if (is.null(fill_colors)) {
      fill_colors <- RColorBrewer::brewer.pal(n = min(length(unique(plot_data$value)), 9), name = colors_set)
    }
    
    # Function to create the plot
    plot_function <- function(plot_data) {
      
      g <- ggplot(plot_data, aes(x = text, y = proportion, fill = value)) +   
        geom_bar(stat = "identity", position = "fill", width = 0.6) +  # Stacked bar chart  
        scale_y_continuous(labels = scales::percent) +   
        labs(
          x = NULL,   
          y = "Proportion",
          fill = "Response"#,
          #title = "Overall Distribution of Responses"
        ) + 
        scale_fill_manual(values = fill_colors) +   
        #facet_wrap(~ text, ncol = 2, strip.position = "left") +  # Two-column layout  
        
        coord_flip() +  # Horizontal bars  
        
        # Add text annotation for observation range at bottom right
        annotate("text", x = Inf, y = Inf, label = range_text, size = 4, hjust = 1, vjust = 1) +  
        #swims_watermark +
        theme_minimal() +  
        theme(
          text = element_text(size = 12),
          strip.text.y.left = element_text(size = 14, face = "bold", angle = 0),  
          axis.text.y = element_text(size = 12),  
          #axis.text.x = element_blank(),
          axis.text.x = element_text(size = 12),  
          #axis.title.x = element_blank(),  
          axis.title = element_text(size = 12),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          plot.title = element_text(size = 14, hjust = 0.5)
        )
      
      return(g)
    }
    
    # Generate the plot
    plot_function(plot_data)
    
  }
  
}



#### colors ####

myblue <- "#1f77b4"
myorange <- "#ff7f0e"
mygreen <- "forestgreen"
myyellow <- "yellow"
mypurple <- "purple"