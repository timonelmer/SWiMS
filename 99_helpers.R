# Helper functions


### other helpers ###

# Function to add line breaks to x-axis labels
break_labels <- function(labels, Z = 20) {
  sapply(labels, function(label) {
    # Insert line breaks after every Z characters at the closest space or punctuation
    gsub(paste0("(.{1,", Z, "}\\b)(?=\\S)"), "\\1\n", label, perl = TRUE)
  })
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


#### SWIMS Plots #####

# Add watermark 
#swims.watermark <- annotate("text", x =1, y = 1, label = "SWiMS24", size = 20, alpha = 0.1, fontface = "bold", color = "gray80") 

swims.plot.distribution <- function(var, institution = NULL, divider = NULL, 
                                    annoFontSize = 4,  # font size for counts on top of bar
                                    font_size = 12,
                                    data = dat, codeb = codebook){

  # Ensure var exists in the codeb
  if (!var %in% codeb$VarName) {
    stop(paste("Variable", var, "not found in the codeb."))
  }

  # Check for divider
  if(!is.null(divider)){
    
    # Ensure divider exists in the codeb
    if (!all(divider %in% codeb$VarName)) {
      stop(paste("Divider", divider, "not found in the codeb."))
    }
    
    # Use divider label if available
    if(!is.na(codeb[codeb$VarName %in% divider,"Labels"])){
      divider_label <- strsplit(codeb[codeb$VarName %in% divider,"Labels"],"//")[[1]]
      dat[,divider] <- factor(dat[,divider], levels = divider_label)
    }else{
      divider_label <- unique(dat[,divider])
    }
  }
  
  if(!is.null(institution) & is.null(divider)){ # with institution, no divider
    
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
      fill = "Institution"#,
      #title = paste("Distribution of", var)
    ) +
    scale_fill_manual(values = c(our_color[1], our_color[2])) +
    geom_text(aes(label = count), position = position_dodge(width = 0.8), vjust = -0.5, size = annoFontSize) +
    theme_minimal() +
    theme(
      text = element_text(size = font_size),
      axis.text.x = element_text(size = font_size, angle = 45, hjust = 1),
      axis.text.y = element_text(size = font_size),
      axis.title = element_text(size = font_size),
      legend.text = element_text(size = font_size),
      legend.title = element_text(size = font_size),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      plot.title = element_text(size = 14, hjust = 0.5) 
    ) + 
    scale_x_discrete(labels = function(x) str_wrap(x, width = 30))   # Apply text wrapping
    
  
  plot(g)
  
  } else if(is.null(institution) & is.null(divider)){ #plot for no institution and divider specified
    
    # Prepare data for plotting
    plot_data <- data %>%
      filter(!is.na(!!sym(var))) %>%
      group_by(!!sym(var)) %>%
      summarise(count = n(), .groups = "drop") %>%
      mutate(proportion = count / sum(count))
    
    # Create plot
    g <- ggplot(plot_data, aes_string(x = var, y = "proportion")) +
      geom_bar(stat = "identity", fill = our_color[1], width = 0.8) +
      scale_y_continuous(labels = scales::percent, limits = c(0, max(plot_data$proportion + 0.05))) +
      labs(
        x = "",
        y = "Proportion"#,
        #title = paste("Distribution of", var)
      ) +
      geom_text(aes(label = count), vjust = -0.5, size = annoFontSize) +
      theme_minimal() +
      theme(
        text = element_text(size = font_size),
        axis.text.x = element_text(size = font_size, angle = 45, hjust = 1),
        axis.text.y = element_text(size = font_size),
        axis.title = element_text(size = font_size),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(size = 14, hjust = 0.5) 
      )  + 
      scale_x_discrete(labels = function(x) str_wrap(x, width = 30))   # Apply text wrapping
      
    
    plot(g)
    
  } else if(is.null(institution) & !is.null(divider)){ # no institution, but divider
    
    # Prepare data for plotting
    plot_data <- data %>%
      filter(if_all(all_of(var), ~ !is.na(.)), !is.na(get(divider))) %>%  # Remove NAs in selected variables
      pivot_longer(cols = all_of(var), names_to = "variable", values_to = "value") %>%
      group_by(variable, value, divider = get(divider)) %>%
      summarise(count = n(), .groups = "drop") %>%
      group_by(variable, divider) %>%
      mutate(proportion = count / sum(count),
             n_total = sum(count)) %>%
      ungroup()
    
    plot_data$divider <- factor(plot_data$divider, levels = divider_label)
    
    # Create plot
    g <- ggplot(plot_data, aes_string(x = "value", y = "proportion", fill = "divider")) +
      geom_bar(stat = "identity", position = "dodge", width = 0.8) +
      scale_y_continuous(labels = scales::percent, limits = c(0,max(plot_data$proportion+.05))) +
      labs(
        x = "",
        y = "Proportion within Group",
        fill = divider,#,
        #title = paste("Distribution of", var)
      ) +
      scale_fill_manual(values = our_color[1:length(divider_label)]) +
      geom_text(aes(label = count), position = position_dodge(width = 0.8), vjust = -0.5, size = annoFontSize) +
      theme_minimal() +
      theme(
        text = element_text(size = font_size),
        axis.text.x = element_text(size = font_size, angle = 45, hjust = 1),
        axis.text.y = element_text(size = font_size),
        axis.title = element_text(size = font_size),
        legend.text = element_text(size = font_size),
        legend.title = element_text(size = font_size),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(size = 14, hjust = 0.5) 
      ) + 
      scale_x_discrete(labels = function(x) str_wrap(x, width = 30))   # Apply text wrapping
    
    
    plot(g)
    
  } else if(!is.null(institution) & !is.null(divider)){ # institution, and divider
    
    # Prepare data for plotting
    plot_data <- data %>%
      mutate(group = ifelse(institution == target_institution, paste0(target_institution), "Other Institutions")) %>%
      filter(if_all(all_of(var), ~ !is.na(.)), !is.na(group), !is.na(get(divider))) %>%  # Remove NAs in selected variables
      pivot_longer(cols = all_of(var), names_to = "variable", values_to = "value") %>%
      group_by(group, variable, value, divider = get(divider)) %>%
      summarise(count = n(), .groups = "drop") %>%
      group_by(variable, divider, group) %>%
      mutate(proportion = count / sum(count),
             n_total = sum(count)) %>%
      ungroup()
    
    plot_data$divider <- factor(plot_data$divider, levels = divider_label)

    alpha_labs <- unique(plot_data$group)
    
    # Create plot
    g <- ggplot(plot_data, aes_string(x = "value", y = "proportion", fill = "divider", alpha = "group")) +
      geom_bar(stat = "identity", position = "dodge", width = 0.8) +
      scale_y_continuous(labels = scales::percent, limits = c(0,max(plot_data$proportion+.05))) +
      scale_alpha_manual(values = setNames(c(1, 0.6, 0), alpha_labs),
                         guide = guide_legend(reverse = TRUE)) +
      labs(
        x = "",
        y = "Proportion within Group",
        fill = divider,#,
        #title = paste("Distribution of", var)
      ) +
      scale_fill_manual(values = our_color[1:length(divider_label)]) +
      geom_text(aes(label = count), position = position_dodge(width = 0.8), vjust = -0.5, size = annoFontSize) +
      facet_wrap(~ group, ncol = 1) +
      theme_minimal() +
      theme(
        text = element_text(size = font_size),
        axis.text.x = element_text(size = font_size, angle = 45, hjust = 1),
        axis.text.y = element_text(size = font_size),
        axis.title = element_text(size = font_size),
        legend.text = element_text(size = font_size),
        legend.title = element_text(size = font_size),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(size = 14, hjust = 0.5) 
      ) + 
      guides(alpha = "none") +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) # Apply text wrapping
      
    
  }
}


swims.plot.multibar <- function(
    var = NULL,
    institution = NULL,
    divider = NULL,
    data = dat,
    codeb = codebook,
    ncol_plot = 1,
    fill_color_set = NULL,
    fontSize = 12,
    fontsize_inplot_text = 4,
    colors_set = "RdYlGn",
    space4comp = F
){

  # Preparations ####
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
  
  # Set colors if not provided
  if (is.null(fill_color_set)) {
    fill_colors <- RColorBrewer::brewer.pal(n = length(x_label), name = colors_set)
  } else {
    fill_colors <- fill_color_set[1:length(x_label)]
  }
  
  # Check for divider
  if(!is.null(divider)){
    
    # Ensure divider exists in the codeb
    if (!all(divider %in% codeb$VarName)) {
      stop(paste("Divider", divider, "not found in the codeb."))
    }
    
    # Use divider label if available
    divider_label <- strsplit(codeb[codeb$VarName %in% divider,"Labels"],"//")[[1]]
    
  }
  
  # Prepare data ####
  if(!is.null(institution) & !is.null(divider)){ # Instituion and divider
    
    plot_data <- data %>%
      mutate(group = ifelse(institution == target_institution, paste0(target_institution), "Other Institutions")) %>%
      mutate(group = factor(group, levels = c("Other Institutions", setdiff(levels(factor(group)), "Other Institutions")))) %>%
      filter(if_all(all_of(var), ~ !is.na(.)), !is.na(group), !is.na(get(divider))) %>%
      pivot_longer(cols = all_of(var), names_to = "variable", values_to = "value") %>%
      group_by(group, variable, value, divider = get(divider)) %>%
      summarise(count = n(), .groups = "drop") %>%
      group_by(group, variable, divider) %>%
      mutate(proportion = count / sum(count),
             n_total = sum(count)) %>% 
      ungroup() 
    
  } else if(!is.null(institution) & is.null(divider)){ # Instituion and no-divider
    
    plot_data <- data %>%
      mutate(group = ifelse(institution == target_institution, paste0(target_institution), "Other Institutions")) %>%
      mutate(group = factor(group, levels = c("Other Institutions", setdiff(levels(factor(group)), "Other Institutions")))) %>%
      filter(if_all(all_of(var), ~ !is.na(.)), !is.na(group)) %>%
      pivot_longer(cols = all_of(var), names_to = "variable", values_to = "value") %>%
      group_by(group, variable, value) %>%
      summarise(count = n(), .groups = "drop") %>%
      group_by(group, variable) %>%
      mutate(proportion = count / sum(count),
             n_total = sum(count)) %>% 
      ungroup() 
    
  } else if(is.null(institution) & !is.null(divider)){ # no-Instituion and divider
    
    plot_data <- data %>%
      filter(if_all(all_of(var), ~ !is.na(.)), !is.na(get(divider))) %>%  # Remove NAs in selected variables
      pivot_longer(cols = all_of(var), names_to = "variable", values_to = "value") %>%
      group_by(variable, value, divider = get(divider)) %>%
      summarise(count = n(), .groups = "drop") %>%
      group_by(variable, divider) %>%
      mutate(proportion = count / sum(count),
             n_total = sum(count)) %>%
      ungroup()
    
  } else if(is.null(institution) & is.null(divider)){ # no-Instituion and no-divider
    
    plot_data <- data %>%
      filter(if_all(all_of(var), ~ !is.na(.))) %>%  # Remove NAs in selected variables
      pivot_longer(cols = all_of(var), names_to = "variable", values_to = "value") %>%
      group_by(variable, value) %>%
      summarise(count = n(), .groups = "drop") %>%
      group_by(variable) %>%
      mutate(proportion = count / sum(count),
             n_total = sum(count)) %>%
      ungroup()
    
  }
  
  cut_width <- ifelse("group" %in% colnames(plot_data), 40, 20)
  
  plot_data <- plot_data %>%
    left_join(var_text, by = "variable") %>%
    select(-variable) %>%
    mutate(text = str_wrap(text, width = cut_width))
  
  if(!is.null(divider) & !is.null(institution)){
    plot_data$interaction_lab <- interaction(plot_data$group, plot_data$divider)
    plot_data <- plot_data %>%
      mutate(
        group_order = if_else(group == "Other Institutions", 1, 2),
        
        # Reordne interaction_lab entsprechend
        interaction_lab = fct_reorder(interaction_lab, group_order)
      )
    
    alpha_labs <- unique(plot_data$group)
  }
  
  # Compute the range of the sum of counts per text variable
  if(!is.null(institution)){
    range_values <- plot_data %>%
      filter(group == target_institution) %>%
      group_by(text) %>%
      summarise(total_count = sum(count)) %>%
      summarise(min_obs = min(total_count), max_obs = max(total_count))    
  } else {
    range_values <- plot_data %>%
      group_by(text) %>%
      summarise(total_count = sum(count)) %>%
      summarise(min_obs = min(total_count), max_obs = max(total_count))    
  }
  
  # Store as a formatted text for annotation
  if(!is.null(institution)){
    if(range_values$min_obs == range_values$max_obs){
      range_text <- paste0("Institutional observations per item: ", range_values$min_obs)
    }else{
      range_text <- paste0("Institutional observations per item: ", range_values$min_obs, " to ", range_values$max_obs)
    } 
  } else {
    if(range_values$min_obs == range_values$max_obs){
      range_text <- paste0("Observations per item: ", range_values$min_obs)
    }else{
      range_text <- paste0("Observations per item: ", range_values$min_obs, " to ", range_values$max_obs)
    } 
  }
  
  # Plotting ####
  if(!is.null(institution) & !is.null(divider)){ # Institution and divider
    
    if(space4comp){
      plot_data$x_pos <- plot_data$interaction_lab
      
      plot_data$x_pos <- as.numeric(plot_data$x_pos)
      
      plot_data$x_pos[
        plot_data$x_pos > length(levels(plot_data$interaction_lab))/2] <- plot_data$x_pos[
          plot_data$x_pos > length(levels(plot_data$interaction_lab))/2] + 1
      
      g <- ggplot(plot_data, aes(x = x_pos, y = proportion, fill = value, alpha = group)) + 
        geom_bar(stat = "identity", position = "fill", width = 0.6) +
        # Aussehen
        scale_alpha_manual(values = setNames(c(0.6, 1, 0), alpha_labs),
                           guide = guide_legend(reverse = TRUE)) +
        scale_y_continuous(labels = scales::percent) + 
        scale_x_continuous(breaks = plot_data$x_pos,
                           labels = plot_data$interaction_lab) +
        scale_fill_manual(values = fill_colors,
                          guide = guide_legend(reverse = TRUE)) + 
        facet_wrap(~ text, ncol = ncol_plot, strip.position = "left") +  # Zwei-Spalten-Layout
        coord_flip(clip = "off") +  # Dreht das Diagramm (horizontal)
        # Text
        labs(
          x = NULL,   # Entferne x-Achsen-Beschriftung
          y = "Proportion",
          fill = "Response",
          alpha = "Institution Type (Transparency)",
          #title = paste0("Comparison of Responses to ", var_org, " by ", divider, " of specific Instition"),
          subtitle = range_text
        ) +
        theme_minimal() +
        theme(
          text = element_text(size = fontSize),
          strip.text.y.left = element_text(size = fontSize, angle = 0),  # Lesbare Facet-Titel
          axis.text.y = element_blank(),  # Entferne die ursprünglichen Gruppen-Namen links
          axis.title = element_text(size = fontSize),
          legend.text = element_text(size = fontSize),
          legend.position = "bottom",
          legend.title = element_text(size = fontSize),
          plot.title = element_text(size = fontSize, hjust = 0.5),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.spacing.x = unit(2, "lines")
        ) +
        #swims_watermark +
        geom_text(data = plot_data,
                  aes(x = x_pos, y = 0.01, label = divider),
                  inherit.aes = FALSE,
                  size = fontsize_inplot_text,
                  hjust = 0) +
        geom_text(data = plot_data,
                  aes(x = x_pos, y = 1.01, 
                      label = paste0("N = ", n_total)),
                  inherit.aes = FALSE,
                  size = fontsize_inplot_text,
                  hjust = 0)
      
    } else {
    
    g <- ggplot(plot_data, aes(x = interaction_lab, y = proportion, fill = value, alpha = group)) + 
      geom_bar(stat = "identity", position = "fill", width = 0.6) +
      # Aussehen
      scale_alpha_manual(values = setNames(c(0.6, 1, 0), alpha_labs),
                         guide = guide_legend(reverse = TRUE)) +
      scale_y_continuous(labels = scales::percent) + 
      scale_fill_manual(values = fill_colors,
                        guide = guide_legend(reverse = TRUE)) + 
      facet_wrap(~ text, ncol = ncol_plot, strip.position = "left") +  # Zwei-Spalten-Layout
      coord_flip(clip = "off") +  # Dreht das Diagramm (horizontal)
      # Text
      labs(
        x = NULL,   # Entferne x-Achsen-Beschriftung
        y = "Proportion",
        fill = "Response",
        alpha = "Institution Type (Transparency)",
        #title = paste0("Comparison of Responses to ", var_org, " by ", divider, " of specific Instition"),
        subtitle = range_text
      ) +
      theme_minimal() +
      theme(
        text = element_text(size = fontSize),
        strip.text.y.left = element_text(size = fontSize, angle = 0),  # Lesbare Facet-Titel
        axis.text.y = element_blank(),  # Entferne die ursprünglichen Gruppen-Namen links
        axis.title = element_text(size = fontSize),
        legend.text = element_text(size = fontSize),
        legend.position = "bottom",
        legend.title = element_text(size = fontSize),
        plot.title = element_text(size = fontSize, hjust = 0.5),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.spacing.x = unit(2, "lines")
      ) +
      #swims_watermark +
      geom_text(data = plot_data,
                aes(x = interaction_lab, y = 0.01, label = divider),
                inherit.aes = FALSE,
                size = fontsize_inplot_text,
                hjust = 0) +
      geom_text(data = plot_data,
                aes(x = interaction_lab, y = 1.01, 
                    label = paste0("N = ", n_total)),
                inherit.aes = FALSE,
                size = fontsize_inplot_text,
                hjust = 0)
    
    }
    
  } else if (!is.null(institution) & is.null(divider)){ # Institution and no-divider
    
    g <- ggplot(plot_data, aes(x = group, y = proportion, fill = value, alpha = ifelse(group == "Other Institutions", "TRUE", "FALSE"))) + 
      geom_bar(stat = "identity", position = "fill", width = 0.6) +
      # Aussehen
      scale_alpha_manual(values = c("TRUE" = 0.6, "FALSE" = 1)) +
      scale_y_continuous(labels = scales::percent) + 
      scale_fill_manual(values = fill_colors,
                        guide = guide_legend(reverse = TRUE)) +  
      facet_wrap(~ text, ncol = ncol_plot, strip.position = "left") +  # Zwei-Spalten-Layout
      coord_flip(clip = "off") +  # Dreht das Diagramm (horizontal)
      #swims_watermark +
      # Text
      labs(
        x = NULL,   # Entferne x-Achsen-Beschriftung
        y = "Proportion",
        fill = "Response",
        #title = paste0("Comparison of Responses to ", var_org, " by specific Institution"),
        subtitle = range_text
      ) +
      # Füge Gruppenlabels für beide Balken korrekt hinzu
      geom_text(data = plot_data %>% distinct(text, group, .keep_all = TRUE), 
                aes(x = group, y = 0.05, label = group),  
                inherit.aes = FALSE, 
                size = fontsize_inplot_text, #fontface = "bold",
                hjust = 0,
                position = position_nudge(x = -0.5)) +
      theme_minimal() +
      theme(
        text = element_text(size = fontSize),
        strip.text.y.left = element_text(size = fontSize, angle = 0),  # Lesbare Facet-Titel
        axis.text.y = element_blank(),  # Entferne die ursprünglichen Gruppen-Namen links
        axis.title = element_text(size = fontSize),
        legend.text = element_text(size = fontSize),
        legend.position = "bottom",
        legend.title = element_text(size = fontSize),
        plot.title = element_text(size = fontSize, hjust = 0.5),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.spacing.x = unit(2, "lines")
      ) +
      guides(alpha = "none") +
      geom_text(data = plot_data,
                aes(x = group, y = 1.01, 
                    label = paste0("N = ", n_total)),
                inherit.aes = FALSE,
                size = fontsize_inplot_text,
                hjust = 0)
    
  } else if (is.null(institution) & !is.null(divider)){ # no-Institution and divider
    
    g <- ggplot(plot_data, aes(x = divider, y = proportion, fill = value)) + 
      geom_bar(stat = "identity", position = "fill", width = 0.6) +
      # Aussehen
      scale_y_continuous(labels = scales::percent) + 
      scale_fill_manual(values = fill_colors,
                        guide = guide_legend(reverse = TRUE)) +  
      facet_wrap(~ text, ncol = ncol_plot, strip.position = "left") +  # Zwei-Spalten-Layout
      coord_flip(clip = "off") +  # Dreht das Diagramm (horizontal)
      #swims_watermark +
      # Text
      labs(
        x = NULL,   # Entferne x-Achsen-Beschriftung
        y = "Proportion",
        fill = "Response",
        #title = paste0("Comparison of Responses to ", var_org, " by ", divider),
        subtitle = range_text
      ) +
      # Füge Gruppenlabels für beide Balken korrekt hinzu
      geom_text(data = plot_data %>% distinct(text, divider, .keep_all = TRUE), 
                aes(x = divider, y = 0.05, label = divider),  
                inherit.aes = FALSE, 
                size = fontsize_inplot_text, #fontface = "bold",
                hjust = 0,
                position = position_nudge(x = -0.5)) +
      theme_minimal() +
      theme(
        text = element_text(size = fontSize),
        strip.text.y.left = element_text(size = fontSize, angle = 0),  # Lesbare Facet-Titel
        axis.text.y = element_blank(),  # Entferne die ursprünglichen Gruppen-Namen links
        axis.title = element_text(size = fontSize),
        legend.text = element_text(size = fontSize),
        legend.position = "bottom",
        legend.title = element_text(size = fontSize),
        plot.title = element_text(size = fontSize, hjust = 0.5),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.spacing.x = unit(2, "lines")
      ) +
      geom_text(data = plot_data,
                aes(x = divider, y = 1.01, 
                    label = paste0("N = ", n_total)),
                inherit.aes = FALSE,
                size = fontsize_inplot_text,
                hjust = 0)
    
  } else if (is.null(institution) & is.null(divider)){ # no-Institution and no-divider
    
    g <- ggplot(plot_data, aes(x = text, y = proportion, fill = value)) +   
      geom_bar(stat = "identity", position = "fill", width = 0.6) +  # Stacked bar chart  
      # Aussehen
      scale_y_continuous(labels = scales::percent) + 
      scale_fill_manual(values = fill_colors,
                        guide = guide_legend(reverse = TRUE)) +   
      coord_flip(clip = "off") +  # Horizontal bars  
      #swims_watermark +
      # Text 
      labs(
        x = NULL,   
        y = "Proportion",
        fill = "Response"
      ) + 
      # Add text annotation for observation range at bottom right
      annotate("text", x = Inf, y = Inf, label = range_text, size = 4, hjust = 1, vjust = 1) +  
      theme_minimal() +  
      theme(
        text = element_text(size = fontSize),
        strip.text.y.left = element_text(size = fontSize, face = "bold", angle = 0),  
        axis.text.y = element_text(size = fontSize),  
        axis.text.x = element_text(size = fontSize),  
        axis.title = element_text(size = fontSize),
        legend.text = element_text(size = fontSize),
        legend.position = "bottom",
        legend.title = element_text(size = fontSize),
        plot.title = element_text(size = fontSize, hjust = 0.5),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.spacing.x = unit(2, "lines")
      )
    
  }
  
  # Generate the plot ####
  return(g)
  
}

## aggregation plot


swims.plot.aggregation <- function(var, 
                                   institution = target_institution, 
                                   data = dat, 
                                   codeb = codebook, 
                                   fill_colors = NULL,
                                   colors_set = "Set1", 
                                   showCount = F
){
  
  # 
  # vars <- "discrimination_type"
  # data = dat
  # codeb = codebook
  # fill_colors = NULL
  # colors_set = "Set1"
  # showCount = T
  # 
  # Define variable
  var_org <- var
  var <- codeb[codeb$Category %in% var,"VarName"]
  var_text <- data.frame("variable" = var,"text" = codeb[codeb$VarName %in% var,"Label"])
  
  # Ensure var exists in the codeb
  if (!all(var %in% codeb$VarName)) {
    stop(paste("Variable", var, "not found in the codeb."))
  }
  
  data.m <- reshape2::melt(data[,c("institution",var)], id.vars = "institution")
  
  # Use variable label if available
  x_label <- strsplit(codeb[codeb$VarName %in% var,"Labels"],"//")[[1]]
  
  # Prepare data for plotting
  plot_data <- data %>%
    mutate(group = ifelse(institution == target_institution, paste0(target_institution), "Other Institutions")) %>%
    mutate(group = factor(group, levels = c(setdiff(levels(factor(group)), "Other Institutions"), "Other Institutions"))) %>%
    filter(if_all(all_of(var), ~ !is.na(.)), !is.na(group)) %>%
    pivot_longer(cols = all_of(var), names_to = "variable", values_to = "value") %>%
    group_by(group, variable, value) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(group, variable) %>%
    ungroup() 
  
  # Change values for varName
  plot_data <- plot_data %>%
    left_join(var_text, by = "variable") %>%
    select(-variable) %>% 
    mutate(text = factor(text, levels = var_text$text)) %>%# Order responses correctly
    mutate(text = str_wrap(text, width = 20)) %>% 
    filter(value %in% "quoted") %>% 
    group_by(group) %>%
    mutate(Percentage = count / sum(count))
  
  
  if(is.null(fill_colors)){
    fill_colors <- RColorBrewer::brewer.pal(n = min(length(unique(plot_data$text)), 9), name = colors_set)
    if (length(unique(plot_data$text)) > 9) {
      fill_colors <- colorRampPalette(fill_colors)(length(unique(plot_data$text)))
    }
  } 
  #c(myblue, myorange, myyellow, mypurple, mygreen)
  
  
  # Create plot
  plot_function <- function(plot_data) {
    
    # Plot erstellen
    g <- ggplot(plot_data[plot_data$value %in% "quoted",], aes(x = group, y = count, fill = text)) +  
      geom_bar(stat = "identity", position = "fill", width = 0.6) +  # Stacked Bar Chart
      labs(
        x = NULL,   # Entferne x-Achsen-Beschriftung
        y = "Percentage of Among All Named Categories",
        fill = "Category",
        title = ""
        # title = paste0("Comparison of Responses by Institution: ", var_org
        #)
      ) +
      scale_fill_manual(values = fill_colors) +  
      scale_y_continuous(labels = scales::percent)  +
      
      theme_minimal() +
      theme(
        text = element_text(size = 12),
        strip.text.y.left = element_text(size = 14, face = "bold", angle = 0),  # Lesbare Facet-Titel
        #axis.text.y = element_blank(),  # Entferne die ursprünglichen Gruppen-Namen links
        #axis.text.x = element_blank(),  # Entferne die x-Achsen-Beschriftungen
        #axis.title.x = element_blank(), # Entferne x-Achsen-Titel komplett
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        plot.title = element_text(size = 14, hjust = 0.5)
      )
    
    if(showCount){return(g+
                           geom_text(aes(label = count),  position = position_fill(vjust = 0.5), size = 2, color = "white")) 
    }else{
      return(g)
    }
  }
  
  plot_function(plot_data)
  
}



#### colors ####
our_color <- c("#1f77b4", "#ff7f0e", "forestgreen", "yellow", "purple", "grey", "red")

