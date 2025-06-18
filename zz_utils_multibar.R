#### Multibar Plot - SWIMS 
swims.plot.multibar <- function(
    var = NULL,
    institution_prov = NULL,
    divider = NULL,
    data = dat,
    codeb = codebook,
    ncol_plot = 1,
    #fill_color_set = NULL,
    fill_color_set = cols_ordinal,
    font_size = 12,
    fontsize_inplot = 4,
    #colors_set = "RdBu", # or "RdBu"?
    colors_set = NULL,
    rev_colors_set = F,
    space4comp = F,
    legend.nrow = 1,
    wrap_legend = FALSE,
    alpha_plot = 0.6,
    width_bar = 0.7,
    proportion.label = F,
    cut.small.groups = NULL, # Remove groups with less or equal to this number of responses
    small.group.delete = FALSE # TRUE = deletion of the groups, FALSE = keep the groups but alpha is set to 0
){
  # Example 
  # var <- "discrimination1"
  # institution_prov <- NULL
  # divider <- NULL
  # data <- dat
  # codeb <- codebook
  # ncol_plot <- 1
  # fill_color_set <- NULL
  # font_size <- 12
  # fontsize_inplot <- 4
  # colors_set <- "RdBu"
  # space4comp <- FALSE
  # legend.nrow <- 3
  # alpha_plot <- 0.6
  # width_bar <- 0.8
  # wrap_legend = TRUE
  # cut.small.groups <- 10
  # small.group.delete <- FALSE
  # rev_colors_set <- FALSE
  
  # PREPARATIONS ####
  # Define variable 
  var_org <- var
  var <- codeb[codeb$Category %in% var,"VarName"]
  var_text <- data.frame("variable" = var,"text" = codeb[codeb$VarName %in% var,"Label"])
  
  # Modify institution_prov
  if(!is.null(institution_prov)){
    institution_prov <- remove_before_and_comma(institution_prov)
    dat$institution <- remove_before_and_comma(dat$institution)
  }
  
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
    if(length(x_label) == 2){
      
    }
    fill_colors <- fill_color_set[1:length(x_label)]
    
    if(length(x_label) == 2){
      fill_colors <- RColorBrewer::brewer.pal(n = 3, name = colors_set)[c(1,3)]
    }
  }
  
  if(rev_colors_set) fill_colors <- rev(fill_colors)
  
  # Check for divider
  if(!is.null(divider)){
    
    # Ensure divider exists in the codeb
    if (!all(divider %in% codeb$VarName)) {
      stop(paste("Divider", divider, "not found in the codeb."))
    }
    
    # # Use divider label if available
    # divider_label <- strsplit(codeb[codeb$VarName %in% divider,"Labels"],"//")[[1]]
    # Use divider label if available
    if(!is.na(codeb[codeb$VarName %in% divider,"Labels"])){
      divider_label <- strsplit(codeb[codeb$VarName %in% divider,"Labels"],"//")[[1]]
      dat[,divider] <- factor(dat[,divider], levels = divider_label)
    }else{
      divider_label <- unique(dat[,divider])
    }
  }
  
  if(!is.null(cut.small.groups) || !is.null(small.group.delete)){
    
    if(cut.small.groups > 0 || small.group.delete == F){
      imp_mess <<- paste("Groups with less than", cut.small.groups, "responses are not visible in the plot.")
    } else if(cut.small.groups == 0 || small.group.delete == T){
      imp_mess <<- paste("Groups with less than", cut.small.groups, "responses are deleted from the plot.")
    }
  } else {
    stop("cut.small.groups and small.group.delete must be always defined together.")
  }

  # PREPROCESSING ####
  if(!is.null(institution_prov) & !is.null(divider)){ # Instituion and divider
    
    # Reference tibble
    tibble_obj <- expand_grid(
      group = c(institution_prov, "Other Institutions"),
      divider = divider_label,
      variable = var,
      value = x_label
    )
    
    obj <- lapply(var, function(x){
      data %>%
        mutate(group = ifelse(institution == institution_prov, paste0(institution_prov), "Other Institutions")) %>%
        mutate(group = factor(group, levels = c("Other Institutions", setdiff(levels(factor(group)), "Other Institutions")))) %>%
        filter(if_all(all_of(x), ~ !is.na(.)), !is.na(group), !is.na(get(divider))) %>%
        pivot_longer(cols = all_of(x), names_to = "variable", values_to = "value") %>%
        group_by(group, variable, value, divider = get(divider)) %>%
        summarise(count = n(), .groups = "drop") %>%
        group_by(group, variable, divider) %>%
        mutate(proportion = count / sum(count),
               n_total = sum(count)) %>% 
        ungroup() 
      
    })
    
    if(!is.null(cut.small.groups) && cut.small.groups > 0){
      
      obj <- lapply(1:length(obj), function(sub_plot_data){
        
        side_obj <- obj[[sub_plot_data]] %>%
          filter(group == institution_prov) %>%
          group_by(divider) %>% 
          summarise(count = sum(count))
        
        if(any(side_obj$count <= cut.small.groups)){
          
          problem_divider <- which(side_obj$count <= cut.small.groups)
          
          if(!all(unique(obj[[sub_plot_data]]$divider) %in% side_obj$divider)){
            problem_divider <- as.character(side_obj$divider[problem_divider])
            problem_divider <- c(problem_divider, setdiff(unique(obj[[sub_plot_data]]$divider), side_obj$divider))
          } else {
            problem_divider <- as.character(side_obj$divider[problem_divider])
          }

          if(length(problem_divider) == length(divider_label)){
            
            mess <- paste("No data for this variable in", institution_prov, "after filtering for group size under", cut.small.groups, "responses.")
            
            return(mess) 
            
          } else if (length(problem_divider) > 0){
            
            obj[[sub_plot_data]]$drop <- ifelse(obj[[sub_plot_data]]$divider %in% problem_divider, "drop", "keep")
            
          }
        } else {
          
          obj[[sub_plot_data]]$drop <- "keep"
          
        }
        
        return(obj[[sub_plot_data]])
      }) 
    }
    
  } else if(!is.null(institution_prov) & is.null(divider)){ # Instituion and no-divider
    
    # Reference tibble
    tibble_obj <- expand_grid(
      group = c(institution_prov, "Other Institutions"),
      variable = var,
      value = x_label
    )
    
    obj <- lapply(var, function(x){
      data %>%
        mutate(group = ifelse(institution == institution_prov, paste0(institution_prov), "Other Institutions")) %>%
        mutate(group = factor(group, levels = c("Other Institutions", setdiff(levels(factor(group)), "Other Institutions")))) %>%
        filter(if_all(all_of(x), ~ !is.na(.)), !is.na(group)) %>%
        pivot_longer(cols = all_of(x), names_to = "variable", values_to = "value") %>%
        group_by(group, variable, value) %>%
        summarise(count = n(), .groups = "drop") %>%
        group_by(group, variable) %>%
        mutate(proportion = count / sum(count),
               n_total = sum(count)) %>% 
        ungroup() 
      
    })
    
    if(!is.null(cut.small.groups) && cut.small.groups > 0){
      
      obj <- lapply(1:length(obj), function(sub_plot_data){
          
          side_obj <- obj[[sub_plot_data]] %>%
            filter(group == institution_prov)
          
          if(sum(side_obj$count) < cut.small.groups){
            
            mess <- paste("No data for this variable in", institution_prov, "after filtering for group size under", cut.small.groups, "responses.")
            
            return(mess) 
          } else {
            
            obj[[sub_plot_data]]$drop <- "keep"
            
          } 
        
        return(obj[[sub_plot_data]])
      }) 
    }

  } else if(is.null(institution_prov) & !is.null(divider)){ # no-Instituion and divider
    
    # Reference tibble
    tibble_obj <- expand_grid(
      divider = divider_label,
      variable = var,
      value = x_label
    )
    
    obj <- lapply(var, function(x){
      data %>%
        filter(if_all(all_of(x), ~ !is.na(.)), !is.na(get(divider))) %>%  # Remove NAs in selected variables
        pivot_longer(cols = all_of(x), names_to = "variable", values_to = "value") %>%
        group_by(variable, value, divider = get(divider)) %>%
        summarise(count = n(), .groups = "drop") %>%
        group_by(variable, divider) %>%
        mutate(proportion = count / sum(count),
               n_total = sum(count)) %>%
        ungroup()
      
    })
    
    if(!is.null(cut.small.groups) && cut.small.groups > 0){
      
      obj <- lapply(1:length(obj), function(sub_plot_data){
        
        side_obj <- obj[[sub_plot_data]] %>%
          group_by(divider) %>% 
          summarise(count = sum(count))
        
        if(any(side_obj$count <= cut.small.groups)){
          
          problem_divider <- which(side_obj$count <= cut.small.groups)
          
          if(!all(unique(obj[[sub_plot_data]]$divider) %in% side_obj$divider)){
            problem_divider <- as.character(side_obj$divider[problem_divider])
            problem_divider <- c(problem_divider, setdiff(unique(obj[[sub_plot_data]]$divider), side_obj$divider))
          } else {
            problem_divider <- as.character(side_obj$divider[problem_divider])
          }
          
          if(length(problem_divider) == length(divider_label)){
            
            mess <- paste("No data for this variable in", institution_prov, "after filtering for group size under", cut.small.groups, "responses.")
            
            return(mess) 
            
          } else if (length(problem_divider) > 0){
            
            obj[[sub_plot_data]]$drop <- ifelse(obj[[sub_plot_data]]$divider %in% problem_divider, "drop", "keep")
            
          }
        } else {
          
          obj[[sub_plot_data]]$drop <- "keep"
          
        }  
        
        return(obj[[sub_plot_data]])
      }) 
    } else {
      
      obj <- lapply(1:length(obj), function(sub_plot_data){
        
        obj[[sub_plot_data]]$drop <- "keep"
        
        return(obj[[sub_plot_data]])
      })
      
    }
    
  } else if(is.null(institution_prov) & is.null(divider)){ # no-Instituion and no-divider
    
    # Reference tibble
    tibble_obj <- expand_grid(
      variable = var,
      value = x_label
    )
    
    obj <- lapply(var, function(x){
      data %>%
        filter(if_all(all_of(x), ~ !is.na(.))) %>%  # Remove NAs in selected variables
        pivot_longer(cols = all_of(x), names_to = "variable", values_to = "value") %>%
        group_by(variable, value) %>%
        summarise(count = n(), .groups = "drop") %>%
        group_by(variable) %>%
        mutate(proportion = count / sum(count),
               n_total = sum(count)) %>%
        ungroup()
      
    })
    
    if(!is.null(cut.small.groups) && cut.small.groups > 0){
      
      obj <- lapply(1:length(obj), function(sub_plot_data){
        
        side_obj <- obj[[sub_plot_data]] %>%
          summarise(count = sum(count))
        
      if(sum(side_obj$count) < cut.small.groups){
        
        mess <- paste("No data for this variable in", institution_prov, "after filtering for group size under", cut.small.groups, "responses.")
        
        return(mess) 
      } else {
        
        obj[[sub_plot_data]]$drop <- "keep"
        
      }
        
        return(obj[[sub_plot_data]])
      })
    } else {
      
      obj <- lapply(1:length(obj), function(sub_plot_data){
        
        obj[[sub_plot_data]]$drop <- "keep"
        
        return(obj[[sub_plot_data]])
      })
      
    }

  }
  
  # Combine all objects
  if(all(grepl("No data for this variable in", obj))){
    
    return(obj[[1]])
    
  } else if(any(grepl("No data for this variable in", obj))){
    
    obj <- obj[!grepl("No data for this variable in", obj)]
    
  }
  
  plot_data <- bind_rows(obj)
  
  if(!is.null(divider) && !is.null(institution_prov)){
    
    plot_data <- left_join(tibble_obj, plot_data, by = c("group", "divider", "variable", "value"))
    
    plot_data$divider <- factor(plot_data$divider, levels = rev(divider_label))
    
  } else if(!is.null(divider) && is.null(institution_prov)){
    
    plot_data <- left_join(tibble_obj, plot_data, by = c("divider", "variable", "value"))
    
    plot_data$divider <- factor(plot_data$divider, levels = rev(divider_label))

  } else if(is.null(divider) && !is.null(institution_prov)){
    
    plot_data <- left_join(tibble_obj, plot_data, by = c("group", "variable", "value"))

  } else if(is.null(divider) && is.null(institution_prov)){
    
    plot_data <- left_join(tibble_obj, plot_data, by = c("variable", "value"))

  }
  
  # MODIFICATIONS FOR PLOT ####
  # Ordering of groups and alpha_labels
  if("group" %in% colnames(plot_data)){
    plot_data$group <- factor(plot_data$group, levels = unique(plot_data$group))
    
    if(is.null(divider)){
    plot_data <- plot_data %>%
      arrange(group == institution_prov) %>% 
      mutate(group = factor(group, levels = unique(group)))
    }
    
    
    
    alpha_labels <- unique(plot_data$group)
    
  }
  
  # Length of institute names
  if(wrap_legend){
    if("group" %in% colnames(plot_data)){
    levels(plot_data$group) <- str_wrap(levels(plot_data$group), width = 20)
    } 
  }
  
  # Add Item text to plot_data and cut it
  cut_width <- ifelse("group" %in% colnames(plot_data), 40, 20)
  plot_data <- plot_data %>%
    left_join(var_text, by = "variable") %>%
    select(-variable) %>%
    mutate(text = str_wrap(text, width = cut_width))

  
  # Compute the range of the sum of counts per text variable
  if(!is.null(institution_prov)){

    range_values <- plot_data %>%
      filter(group == institution_prov) %>%
      group_by(text) %>%
      summarise(total_count = sum(count, na.rm = T)) %>%
      summarise(min_obs = min(total_count, na.rm = T), max_obs = max(total_count, na.rm = T)) 

  } else {
    
    range_values <- plot_data %>%
      group_by(text) %>%
      summarise(total_count = sum(count, na.rm = T)) %>%
      summarise(min_obs = min(total_count, na.rm = T), max_obs = max(total_count, na.rm = T)) 
    
  }
  
  # Interaction helper for divder & instituion
  if(!is.null(divider) & !is.null(institution_prov)){
    plot_data$interaction_lab <- interaction(plot_data$group, plot_data$divider)
    plot_data <- plot_data %>%
      mutate(
        group_order = if_else(group == "Other Institutions", 1, 2),
        
        # Reordne interaction_lab entsprechend
        interaction_lab = fct_reorder(interaction_lab, group_order)
      )
  }
  
  
  # Store as a formatted text for annotation
  # if(!is.null(institution_prov)){
  #   if(range_values$min_obs == range_values$max_obs){
  #     range_text <- paste0("Institutional observations per item: ", range_values$min_obs)
  #   }else{
  #     range_text <- paste0("Institutional observations per item: ", range_values$min_obs, " to ", range_values$max_obs)
  #   }
  # } else {
  #   if(range_values$min_obs == range_values$max_obs){
  #     range_text <- paste0("Observations per item: ", range_values$min_obs)
  #   }else{
  #     range_text <- paste0("Observations per item: ", range_values$min_obs, " to ", range_values$max_obs)
  #   }
  # }
  
  # Prepare drops and change fill based on drops
  plot_data$drop <- ifelse(is.na(plot_data$count), "drop", plot_data$drop)
  plot_data$value_fill <- factor(plot_data$value, levels = x_label)
  plot_data$value_fill[which(plot_data$drop == "drop")] <- NA
  plot_data$proportion <- ifelse(is.na(plot_data$proportion), 0, plot_data$proportion)
  if("n_total" %in% colnames(plot_data)){
    plot_data$n_total <- ifelse(is.na(plot_data$n_total), "", plot_data$n_total)
  }
  if("count" %in% colnames(plot_data)){
    plot_data$count <- ifelse(is.na(plot_data$count), "", plot_data$count)
  }
  
  
  
  # PLOT ####
  if(!is.null(institution_prov) & !is.null(divider)){ # Institution and divider
    
    if(space4comp){
      
      # Additional preparement
      plot_data$x_pos <- plot_data$interaction_lab
      
      plot_data$x_pos <- as.numeric(plot_data$x_pos)
      
      plot_data$x_pos[
        plot_data$x_pos > length(levels(plot_data$interaction_lab))/2] <- plot_data$x_pos[
          plot_data$x_pos > length(levels(plot_data$interaction_lab))/2] + 1
      
      # Plot
      g <- ggplot(plot_data, aes(x = x_pos, y = proportion, fill = value_fill, alpha = group)) + 
        geom_bar(stat = "identity", position = "fill", width = width_bar) +
        # Aussehen
        scale_alpha_manual(values = setNames(c(1, alpha_plot), alpha_labels)) +
        scale_y_continuous(labels = scales::percent) + 
        scale_x_continuous(breaks = plot_data$x_pos,
                           labels = plot_data$interaction_lab) +
        scale_fill_manual(values = fill_colors,
                          na.value = "transparent",
                          breaks = unique(na.omit(plot_data$value))
                          ) + 
        facet_wrap(~ text, ncol = ncol_plot, strip.position = "left") +  # Zwei-Spalten-Layout
        coord_flip(clip = "off") +  # Dreht das Diagramm (horizontal)
        # Text
        labs(
          x = NULL,   # Entferne x-Achsen-Beschriftung
          y = "Proportion",
          fill = "",
          alpha = "Transparency"
          #title = paste0("Comparison of Responses to ", var_org, " by ", divider, " of specific Instition"),
          # subtitle = range_text
        ) +
        theme_minimal() +
        theme(
          text = element_text(size = font_size),
          strip.text.y.left = element_text(size = font_size, angle = 0),  # Lesbare Facet-Titel
          axis.text.y = element_blank(),  # Entferne die ursprünglichen Gruppen-Namen links
          axis.title = element_text(size = font_size),
          legend.text = element_text(size = font_size),
          legend.position = "bottom",
          legend.title = element_text(size = font_size),
          plot.title = element_text(size = font_size, hjust = 0.5),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.spacing.x = unit(4, "lines"),
          # clip = "off",
          plot.margin = unit(c(0,3,0,0), "cm")
        ) +
        # swims_watermark +
        # geom_text(data = plot_data,
        #           aes(x = x_pos, y = 0.01, label = divider),
        #           inherit.aes = FALSE,
        #           size = fontsize_inplot,
        #           hjust = 0) +
        geom_text(data = plot_data,
                  aes(x = x_pos, y = 1.01, 
                      label = ifelse(paste0("N = ", n_total) == "N = ", "", paste0("N = ", n_total))),
                  inherit.aes = FALSE,
                  size = fontsize_inplot,
                  hjust = 0) +
        guides(fill = guide_legend(nrow = legend.nrow, reverse = TRUE),
               alpha = guide_legend(nrow = 2, reverse = TRUE,
                                    override.aes = list(
                                      fill = "gray",      # just to ensure something visible
                                      color = "black",    # this adds the black frame!
                                      size = 0.5          # optional: thinner border
                                    )
                                    ))
      
    } else {
      
      g <- ggplot(plot_data, aes(x = interaction_lab, y = proportion, fill = value_fill, alpha = group)) + 
        geom_bar(stat = "identity", position = "fill", width = width_bar) +
        # Aussehen
        scale_alpha_manual(values = setNames(c(1, alpha_plot), alpha_labels)) +
        scale_y_continuous(labels = scales::percent) + 
        scale_fill_manual(values = fill_colors,
                          na.value = "transparent",
                          breaks = unique(na.omit(plot_data$value))
        ) + 
        facet_wrap(~ text, ncol = ncol_plot, strip.position = "left") +  # Zwei-Spalten-Layout
        coord_flip(clip = "off") +  # Dreht das Diagramm (horizontal)
        # Text
        labs(
          x = NULL,   # Entferne x-Achsen-Beschriftung
          y = "Proportion",
          fill = "",
          alpha = "Transparency"
          #title = paste0("Comparison of Responses to ", var_org, " by ", divider, " of specific Instition"),
          # subtitle = range_text
        ) +
        theme_minimal() +
        theme(
          text = element_text(size = font_size),
          strip.text.y.left = element_text(size = font_size, angle = 0),  # Lesbare Facet-Titel
          axis.text.y = element_blank(),  # Entferne die ursprünglichen Gruppen-Namen links
          axis.title = element_text(size = font_size),
          legend.text = element_text(size = font_size),
          legend.position = "bottom",
          legend.title = element_text(size = font_size),
          plot.title = element_text(size = font_size, hjust = 0.5),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.spacing.x = unit(4, "lines"),
          # clip = "off",
          plot.margin = unit(c(0,3,0,0), "cm")
        ) +
        # swims_watermark +
        # geom_text(data = plot_data,
        #           aes(x = interaction_lab, y = 0.01, label = divider),
        #           inherit.aes = FALSE,
        #           size = fontsize_inplot,
        #           hjust = 0) +
        geom_text(data = plot_data,
                  aes(x = interaction_lab, y = 1.01, 
                      label = ifelse(paste0("N = ", n_total) == "N = ", "", paste0("N = ", n_total))),
                  inherit.aes = FALSE,
                  size = fontsize_inplot,
                  hjust = 0) +
        guides(fill = guide_legend(nrow = legend.nrow, reverse = TRUE),
               alpha = guide_legend(nrow = 2, reverse = FALSE,
                                    override.aes = list(
                                      fill = "gray",      # just to ensure something visible
                                      color = "black",    # this adds the black frame!
                                      size = 0.5          # optional: thinner border
                                    )
               )) 
      
    }
    
  } else if (!is.null(institution_prov) & is.null(divider)){ # Institution and no-divider

    g <- ggplot(plot_data, aes(x = group, y = proportion, fill = value_fill, alpha = ifelse(group == "Other Institutions", "TRUE", "FALSE"))) + 
      geom_bar(stat = "identity", position = "fill", width = width_bar) +
      # Aussehen
      scale_alpha_manual(values = c("TRUE" = alpha_plot, "FALSE" = 1)) +
      scale_y_continuous(labels = scales::percent) + 
      scale_fill_manual(values = fill_colors,
                        na.value = "transparent",
                        breaks = unique(na.omit(plot_data$value))
      ) +  
      facet_wrap(~ text, ncol = ncol_plot, strip.position = "left") +  # Zwei-Spalten-Layout
      coord_flip(clip = "off") +  # Dreht das Diagramm (horizontal)
      # swims_watermark +
      # Text
      labs(
        x = NULL,   # Entferne x-Achsen-Beschriftung
        y = "Proportion",
        fill = ""
        #title = paste0("Comparison of Responses to ", var_org, " by specific Institution"),
        #subtitle = range_text
      ) +
      # Füge Gruppenlabels für beide Balken korrekt hinzu
      # geom_text(data = plot_data %>% distinct(text, group, .keep_all = TRUE), 
      #           aes(x = group, y = 0.05, label = group),  
      #           inherit.aes = FALSE, 
      #           size = fontsize_inplot, #fontface = "bold",
      #           hjust = 0,
      #           position = position_nudge(x = 0)) +
      theme_minimal() +
      theme(
        text = element_text(size = font_size),
        strip.text.y.left = element_text(size = font_size, angle = 0),  # Lesbare Facet-Titel
        axis.text.y = element_blank(),  # Entferne die ursprünglichen Gruppen-Namen links
        axis.title = element_text(size = font_size),
        legend.text = element_text(size = font_size),
        legend.position = "bottom",
        legend.title = element_text(size = font_size),
        plot.title = element_text(size = font_size, hjust = 0.5),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.spacing.x = unit(4, "lines"),
        # clip = "off",
        plot.margin = unit(c(0,3,0,0), "cm"),
        legend.text.align = 0
      ) +
      geom_text(data = plot_data,
                aes(x = group, y = 1.01, 
                    label = ifelse(paste0("N = ", n_total) == "N = ", "", paste0("N = ", n_total))),
                inherit.aes = FALSE,
                size = fontsize_inplot,
                hjust = 0) +
      guides(fill = guide_legend(nrow = legend.nrow, reverse = TRUE),
             alpha = "none") 
    
  } else if (is.null(institution_prov) & !is.null(divider)){ # no-Institution and divider
    
    g <- ggplot(plot_data, aes(x = divider, y = proportion, fill = value_fill)) + 
      geom_bar(stat = "identity", position = "fill", width = width_bar) +
      # Aussehen
      scale_y_continuous(labels = scales::percent) + 
      scale_fill_manual(values = fill_colors,
                        na.value = "transparent",
                        breaks = unique(na.omit(plot_data$value))
      ) +  
      facet_wrap(~ text, ncol = ncol_plot, strip.position = "left") +  # Zwei-Spalten-Layout
      coord_flip(clip = "off") +  # Dreht das Diagramm (horizontal)
      # swims_watermark +
      # Text
      labs(
        x = NULL,   # Entferne x-Achsen-Beschriftung
        y = "Proportion",
        fill = ""
        #title = paste0("Comparison of Responses to ", var_org, " by ", divider),
        #subtitle = range_text
      ) +
      # Füge Gruppenlabels für beide Balken korrekt hinzu
      # geom_text(data = plot_data %>% distinct(text, divider, .keep_all = TRUE), 
      #           aes(x = divider, y = 0.05, label = divider),  
      #           inherit.aes = FALSE, 
      #           size = fontsize_inplot, #fontface = "bold",
      #           hjust = 0,
      #           position = position_nudge(x = 0)) +
      theme_minimal() +
      theme(
        text = element_text(size = font_size),
        strip.text.y.left = element_text(size = font_size, angle = 0),  # Lesbare Facet-Titel
        axis.text.y = element_blank(),  # Entferne die ursprünglichen Gruppen-Namen links
        axis.title = element_text(size = font_size),
        legend.text = element_text(size = font_size),
        legend.position = "bottom",
        legend.title = element_text(size = font_size),
        plot.title = element_text(size = font_size, hjust = 0.5),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.spacing.x = unit(4, "lines"),
        # clip = "off",
        plot.margin = unit(c(0,3,0,0), "cm"),
        legend.text.align = 0
      ) +
      geom_text(data = plot_data,
                aes(x = divider, y = 1.01, 
                    label = ifelse(paste0("N = ", n_total) == "N = ", "", paste0("N = ", n_total))),
                inherit.aes = FALSE,
                size = fontsize_inplot,
                hjust = 0) +
      guides(fill = guide_legend(nrow = legend.nrow, reverse = TRUE)) 
    
  } else if (is.null(institution_prov) & is.null(divider)){ # no-Institution and no-divider
    
    plot_data$text <- fct_rev(as.factor(plot_data$text))
  
    g <- ggplot(plot_data, aes(x = text, y = proportion, fill = value_fill)) +   
      geom_bar(stat = "identity", position = "fill", width = width_bar) +  # Stacked bar chart  
      # Aussehen
      scale_y_continuous(labels = scales::percent) + 
      scale_fill_manual(values = fill_colors,
                        na.value = "transparent",
                        breaks = unique(na.omit(plot_data$value))
      ) +   
      coord_flip(clip = "off") +  # Horizontal bars  
      #swims_watermark +
      # Text 
      labs(
        x = NULL,   
        y = "Proportion",
        fill = ""
      ) + 
      # Add text annotation for observation range at bottom right
      # annotate("text", x = Inf, y = Inf, label = range_text, size = 4, hjust = 1, vjust = 1) +  
      theme_minimal() +  
      theme(
        text = element_text(size = font_size),
        strip.text.y.left = element_text(size = font_size, angle = 0),  
        axis.text.y = element_text(size = font_size),  
        axis.text.x = element_text(size = font_size),  
        axis.title = element_text(size = font_size),
        legend.text = element_text(size = font_size),
        legend.position = "bottom",
        legend.title = element_text(size = font_size),
        plot.title = element_text(size = font_size, hjust = 0.5),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.spacing.x = unit(4, "lines"),
        # clip = "off",
        plot.margin = unit(c(0,3,0,0), "cm"),
        legend.text.align = 0
      ) +
      guides(fill = guide_legend(nrow = legend.nrow, reverse = TRUE)) 
    
    # Add white percentage labels if flag is set
    if (proportion.label) {
      g <- g + geom_text(
        aes(label = ifelse(proportion > 0.00, paste0(round(proportion * 100), "%"), "")),
        position = position_fill(vjust = 0.5),
        color = "white"
      )
    }
  }
  
  # Check for group label color
  if(rev_colors_set == T){
    if(!is.null(institution_prov) & !is.null(divider)){
      
      if(space4comp){
        
        # g <- g + geom_shadowtext(data = plot_data,
        #                        aes(x = x_pos, y = 0.01, label = divider),
        #                        inherit.aes = FALSE,
        #                        size = fontsize_inplot,
        #                        hjust = 0)
        
        g <- g + geom_text(data = plot_data,
                                 aes(x = x_pos, y = 0.01, label = divider),
                                 inherit.aes = FALSE,
                                 size = fontsize_inplot,
                                 hjust = 0)
        
      } else {
        
        # g <- g + geom_shadowtext(data = plot_data,
        #                     aes(x = interaction_lab, y = 0.01, label = divider),
        #                     inherit.aes = FALSE,
        #                     size = (fontsize_inplot),
        #                     hjust = 0)
        
        g <- g + geom_text(data = plot_data,
                                 aes(x = interaction_lab, y = 0.01, label = divider),
                                 inherit.aes = FALSE,
                                 size = (fontsize_inplot),
                                 hjust = 0)
        
      }
      
    } else if(!is.null(institution_prov) & is.null(divider)){
      
      # g <- g +  geom_shadowtext(data = plot_data %>% distinct(text, group, .keep_all = TRUE), 
      #                     aes(x = group, y = 0.05, label = group),  
      #                     inherit.aes = FALSE, 
      #                     size = fontsize_inplot, #fontface = "bold",
      #                     hjust = 0,
      #                     position = position_nudge(x = 0))
      
      g <- g +  geom_text(data = plot_data %>% distinct(text, group, .keep_all = TRUE), 
                                aes(x = group, y = 0.05, label = group),  
                                inherit.aes = FALSE, 
                                size = fontsize_inplot, #fontface = "bold",
                                hjust = 0,
                                position = position_nudge(x = 0))
        
    } else if(is.null(institution_prov) & !is.null(divider)){
      
      # g <- g + geom_shadowtext(data = plot_data %>% distinct(text, divider, .keep_all = TRUE), 
      #                  aes(x = divider, y = 0.05, label = divider),  
      #                  inherit.aes = FALSE, 
      #                  size = fontsize_inplot, #fontface = "bold",
      #                  hjust = 0,
      #                  position = position_nudge(x = 0))
      
      g <- g + geom_text(data = plot_data %>% distinct(text, divider, .keep_all = TRUE), 
                               aes(x = divider, y = 0.05, label = divider),  
                               inherit.aes = FALSE, 
                               size = fontsize_inplot, #fontface = "bold",
                               hjust = 0,
                               position = position_nudge(x = 0))
      
    } else if(is.null(institution_prov) & is.null(divider)){
      
      # Empty 
      
    }
  } else {
    if(!is.null(institution_prov) & !is.null(divider)){
      
      if(space4comp){
        
        g <- g + geom_text(data = plot_data,
                                 aes(x = x_pos, y = 0.01, label = divider),
                                 inherit.aes = FALSE,
                                 size = fontsize_inplot,
                                 hjust = 0)

      } else {
        g <- g + geom_text(data = plot_data,
                                 aes(x = interaction_lab, y = 0.01, label = divider),
                                 inherit.aes = FALSE,
                                 size = (fontsize_inplot),
                                 hjust = 0)
      }
      
    } else if(!is.null(institution_prov) & is.null(divider)){
      
      g <- g + geom_text(data = plot_data %>% distinct(text, group, .keep_all = TRUE), 
                         aes(x = group, y = 0.05, label = group),  
                         inherit.aes = FALSE, 
                         size = fontsize_inplot, #fontface = "bold",
                         hjust = 0,
                         position = position_nudge(x = 0))
      
    } else if(is.null(institution_prov) & !is.null(divider)){
      
      g <- g + geom_text(data = plot_data %>% distinct(text, divider, .keep_all = TRUE), 
                         aes(x = divider, y = 0.05, label = divider),  
                         inherit.aes = FALSE, 
                         size = fontsize_inplot, #fontface = "bold",
                         hjust = 0,
                         position = position_nudge(x = 0))
      
    } else if(is.null(institution_prov) & is.null(divider)){
      
      # Empty 
      
    }
  }

  # RETURN ####
  return(g)
  
}
