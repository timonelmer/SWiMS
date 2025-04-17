# Distribution Plot - SWIMS 
swims.plot.distribution <- function(var, 
                                    institution_prov = NULL, 
                                    divider = NULL, 
                                    annoFontSize = 4,  # font size for counts on top of bar
                                    font_size = 12,   
                                    data = dat, 
                                    codeb = codebook,
                                    alpha_plot = 0.6,
                                    colors_set = "RdYlBu",
                                    width_bar = 0.8,
                                    width_text = 25,
                                    fct_ord = NULL,
                                    cut.small.groups = NULL, # Remove groups with less or equal to this number of responses
                                    small.group.delete = NULL # TRUE = deletion of the groups, FALSE = keep the groups but alpha is set to 0
){
  # Example
  # var <- "age"
  # institution_prov <- target_institution
  # divider <- NULL
  # annoFontSize <- 4
  # font_size <- 12
  # data <- dat
  # codeb <- codebook
  # alpha_plot <- 0.6
  # colors_set <- "Set2"
  # width_bar <- 0.8
  # width_text <- 25
  # cut.small.groups <- 10
  # small.group.delete <- F
  # fct_ord <- NULL

  # PREPARATION ####
  # Ensure var exists in the codeb
  if (!var %in% codeb$VarName) {
    stop(paste("Variable", var, "not found in the codeb."))
  }
  
  # Modify institution_prov
  if(!is.null(institution_prov)){
    institution_prov <- remove_before_and_comma(institution_prov)
    dat$institution <- remove_before_and_comma(dat$institution)
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

  if(!is.null(cut.small.groups) && !is.null(small.group.delete)){
    
    if(cut.small.groups > 0 || small.group.delete == F){
      imp_mess <<- paste("If overall groups with less than", cut.small.groups, "responses exists, they are not visible in the plot.")
    } else if(cut.small.groups > 0 || small.group.delete == T){
      imp_mess <<- paste("If overall groups with less than", cut.small.groups, "responses exists, they are deleted from the plot.")
    }
  } else if((is.null(cut.small.groups) && !is.null(small.group.delete)) ||
            (!is.null(cut.small.groups) && is.null(small.group.delete))){
    stop("cut.small.groups and small.group.delete must be always defined together.")
  }
  
  # Check for levles of var
  if(is.null(levels(unique(data[[var]])))){
    input <- unique(data[[var]])
    input <- na.omit(input)
    
    if(is.null(fct_ord)){
      input <- factor(input, levels = c(input))
    } else {
      input <- factor(input, levels = c(fct_ord))
    }
    
  } else {
    input <- levels(unique(data[[var]]))
    input <- na.omit(input)
    
    if(is.null(fct_ord)){
      input <- factor(input, levels = c(input))
    } else {
      input <- factor(input, levels = c(fct_ord))
    }
  }
  
  # PREPROCESSING ####
  # Prepare tibble
  if(!is.null(institution_prov) & !is.null(divider)){ # with institution, no divider
   
    # Reference tibble
    tibble_obj <- expand_grid(
      group = c(institution_prov, "Other Institutions"),
      divider = divider_label,
      variable = var,
      value = input
        )
    
    # Prepare data for plotting
    plot_data <- data %>%
      mutate(group = ifelse(institution == institution_prov, paste0(institution_prov), "Other Institutions")) %>%
      filter(if_all(all_of(var), ~ !is.na(.)), !is.na(group), !is.na(get(divider))) %>%  # Remove NAs in selected variables
      pivot_longer(cols = all_of(var), names_to = "variable", values_to = "value") %>%
      group_by(group, variable, value, divider = get(divider)) %>%
      summarise(count = n(), .groups = "drop") %>%
      group_by(variable, divider, group) %>%
      mutate(proportion = count / sum(count),
             n_total = sum(count)) %>%
      ungroup()
    
    # Left join
    plot_data <- left_join(tibble_obj, plot_data, by = c("group", "divider", "variable", "value"))

    # Filter small groups
    if(!is.null(cut.small.groups) && cut.small.groups > 0){
      
      side_obj <- plot_data %>%
        filter(group == institution_prov) %>%
        group_by(divider) %>% 
        summarise(count = sum(count, na.rm = TRUE))
      
      if(any(side_obj$count <= cut.small.groups)){
        
        problem_divider <- which(side_obj$count <= cut.small.groups)
        
        if(length(problem_divider) == length(divider_label)){
          
          mess <- paste("No data for this variable in", institution_prov, "after filtering for group size under", cut.small.groups, "responses.")
          
          return(mess) 
          
        } else if (length(problem_divider) > 0){
          
          drop_divider <- side_obj$divider[problem_divider]
          
          plot_data$drop <- ifelse(plot_data$divider %in% drop_divider, "drop", "keep")
          
        }
      } else {
        
        plot_data$drop <- "keep"
        
      } 
    } else {
      
      plot_data$drop <- "keep"
      
    }
    
  } else if(!is.null(institution_prov) & is.null(divider)){
    
    # Reference tibble
    tibble_obj <- expand_grid(
      group = c(institution_prov, "Other Institutions"),
      !!var := input
    )
    
    # Prepare data for plotting
    plot_data <- data %>%
      mutate(group = ifelse(institution == institution_prov, paste0(institution_prov), "Other Institutions")) %>%
      filter(!is.na(!!sym(var)), !is.na(group)) %>%
      group_by(group, !!sym(var)) %>%
      summarise(count = n(), .groups = "drop") %>%
      group_by(group) %>%
      mutate(proportion = count / sum(count)) %>% 
      mutate(!!var := as.factor(.data[[var]]))
    
    # Left join
    plot_data <- left_join(tibble_obj, plot_data, by = c("group", var))

    if(!is.null(cut.small.groups) && cut.small.groups > 0){
      
      side_obj <- plot_data %>%
        filter(group == institution_prov)
      
      if(sum(side_obj$count, na.rm = T) < cut.small.groups){
        
        mess <- paste("No data for this variable in", institution_prov, "after filtering for group size under", cut.small.groups, "responses.")
        
        return(mess) 
      } else {
        
        plot_data$drop <- "keep"
        
      } 
    } else {
      
      plot_data$drop <- "keep"
      
    }
    
  } else if(is.null(institution_prov) & !is.null(divider)){
    
    # Reference tibble
    tibble_obj <- expand_grid(
      divider = divider_label,
      variable = var,
      value = input
    )
    
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
    
    # Left join
    plot_data <- left_join(tibble_obj, plot_data, by = c("divider", "variable", "value"))
    
    if(!is.null(cut.small.groups) && cut.small.groups > 0){
      
      side_obj <- plot_data %>%
        group_by(divider) %>% 
        summarise(count = sum(count))
      
      if(any(side_obj$count <= cut.small.groups)){
        
        problem_divider <- which(side_obj$count <= cut.small.groups)
        
        if(length(problem_divider) == length(divider_label)){
          
          mess <- paste("No data for this variable in", institution_prov, "after filtering for group size under", cut.small.groups, "responses.")
          
          return(mess) 
          
        } else if (length(problem_divider) > 0){
          
          drop_divider <- side_obj$divider[problem_divider]
          
          plot_data$drop <- ifelse(plot_data$divider %in% drop_divider, "drop", "keep")
          
        }
      } else {
        
        plot_data$drop <- "keep"
        
      }  
    } else {
      
      plot_data$drop <- "keep"
      
    }
    
  } else if(is.null(institution_prov) & is.null(divider)){
    
    # Reference tibble
    tibble_obj <- expand_grid(
      !!var := input
    )
    
    # Prepare data for plotting
    plot_data <- data %>%
      filter(!is.na(!!sym(var))) %>%
      group_by(!!sym(var)) %>%
      summarise(count = n(), .groups = "drop") %>%
      mutate(proportion = count / sum(count))
    
    # Left join
    #plot_data <- left_join(tibble_obj, plot_data, by = var)
    
    if(!is.null(cut.small.groups) && cut.small.groups > 0){
      
      if(sum(plot_data$count) < cut.small.groups){
        
        mess <- paste("No data for this variable in", institution_prov, "after filtering for group size under", cut.small.groups, "responses.")
        
        return(mess) 
      } else {
        
        plot_data$drop <- "keep"
      
      }
    } else {
      
      plot_data$drop <- "keep"
      
    }
    
  }
  
  # MODIFICATIONS FOR PLOT ####

  # Ordering groups
  if("group" %in% colnames(plot_data)[1]){
    plot_data$group <- factor(plot_data$group, levels = c(institution_prov, "Other Institutions"))
  }
  
  # # Fill the NA values
  plot_data$proportion[is.na(plot_data$proportion)] <- 0
  
  # Specials
  if(!is.null(divider) && !is.null(institution_prov)){
    
    plot_data$divider <- factor(plot_data$divider, levels = divider_label)
    fill_colors <- RColorBrewer::brewer.pal(n = length(divider_label), name = colors_set)
    
    
  } else if(!is.null(institution_prov) && is.null(divider)){
    
    fill_colors <- RColorBrewer::brewer.pal(n = 3, name = colors_set)[1:2]
    
  } else if(is.null(institution_prov) && !is.null(divider)){
    
    plot_data$divider <- factor(plot_data$divider, levels = divider_label)
    fill_colors <- RColorBrewer::brewer.pal(n = length(divider_label), name = colors_set)
    
  } else if(is.null(institution_prov) && is.null(divider)){
    
    fill_colors <- RColorBrewer::brewer.pal(n = 3, name = colors_set)[1]
    
  }
  
  # Alpha labels
  if("group" %in% colnames(plot_data)){
  alpha_labels <- unique(plot_data$group)
  }
  
  # Modify filling
  if(!is.null(divider)){
    
    plot_data$value_fill <- ifelse(plot_data$drop == "drop", NA, as.character(plot_data$divider))
    plot_data$value_fill <- factor(plot_data$value_fill, levels = levels(plot_data$divider)) 
    
  } else if (!is.null(institution_prov)){
    
    plot_data$value_fill <- ifelse(plot_data$drop == "drop", NA, as.character(plot_data$group))
    plot_data$value_fill <- factor(plot_data$value_fill, levels = levels(plot_data$group)) 
  
  }
  
  # Drop everything with "drop" if small.group.delete is TRUE
  if(!is.null(small.group.delete) && small.group.delete == T){
    plot_data <- plot_data %>%
      filter(drop == "keep")
  }
  
  # Modificaiton of plot 
  if("n_total" %in% colnames(plot_data)){
    plot_data$n_total <- ifelse(is.na(plot_data$n_total), "", plot_data$n_total)
    
    plot_data$n_total <- ifelse(plot_data$drop == "drop", "", plot_data$n_total)
  }
  
  if("count" %in% colnames(plot_data)){
    plot_data$count <- ifelse(is.na(plot_data$count), "", plot_data$count)
    
    plot_data$count <- ifelse(plot_data$drop == "drop", "", plot_data$count)
  }
  
  # PLOT ####
  if(!is.null(institution_prov) & !is.null(divider)){ # divider and institution

    # Create plot
    g <- ggplot(plot_data, aes_string(x = "value", y = "proportion", fill = "value_fill", alpha = "group")) +
      geom_bar(stat = "identity", position = "dodge", width = width_bar) +
      scale_y_continuous(labels = scales::percent, 
                         limits = c(0,max(plot_data$proportion+.05))) +
      scale_alpha_manual(values = setNames(c(1,alpha_plot), alpha_labels)) +
      labs(
        x = "",
        y = "Proportion within Group",
        fill = divider,#,
        #title = paste("Distribution of", var)
      ) +
      scale_fill_manual(values = setNames(fill_colors, na.omit(unique(plot_data$value_fill))),
                        na.value = NA,
                        breaks = unique(na.omit(plot_data$divider))
      ) +
      geom_text(aes(label = count), position = position_dodge(width = 1), vjust = -0.5, size = annoFontSize) +
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
        plot.title = element_text(size = font_size, hjust = 0.5) ,
        strip.text = element_text(size = font_size)
      ) + 
      guides(alpha = "none") +
      scale_x_discrete(labels = function(x) str_wrap(x, width = width_text)) # Apply text wrapping
    
  } else if(!is.null(institution_prov) & is.null(divider)){ # no-divider and institution
    # Create plot
    g <- ggplot(plot_data, aes_string(x = var, y = "proportion", fill = "value_fill")) +
      geom_bar(stat = "identity", position = "dodge", width = width_bar) +
      scale_y_continuous(labels = scales::percent, 
                         limits = c(0,max(plot_data$proportion+.05))) +
      labs(
        x = "",
        y = "Proportion",
        fill = "Institution"#,
        #title = paste("Distribution of", var)
      ) +
      scale_fill_manual(values = setNames(fill_colors, na.omit(unique(plot_data$value_fill))),
                        breaks = unique(na.omit(plot_data$value_fill))
      ) +
      geom_text(aes(label = count), position = position_dodge(width = 1), vjust = -0.5, size = annoFontSize) +
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
        plot.title = element_text(size = font_size, hjust = 0.5),
        strip.text = element_text(size = font_size)
      ) + 
      scale_x_discrete(labels = function(x) str_wrap(x, width = width_text))   # Apply text wrapping
    
  } else if(is.null(institution_prov) & !is.null(divider)){ # divider and no-institution
    
    # Create plot
    g <- ggplot(plot_data, aes_string(x = "value", y = "proportion", fill = "value_fill")) +
      geom_bar(stat = "identity", position = "dodge", width = width_bar) +
      scale_y_continuous(labels = scales::percent, 
                         limits = c(0,max(plot_data$proportion+.05))) +
      labs(
        x = "",
        y = "Proportion within Group",
        fill = divider,#,
        #title = paste("Distribution of", var)
      ) +
      scale_fill_manual(values = setNames(fill_colors, na.omit(unique(plot_data$value_fill))),
                        na.value = "transparent",
                        breaks = unique(na.omit(plot_data$divider))
      ) +
      geom_text(aes(label = count), position = position_dodge(width = 1), vjust = -0.5, size = annoFontSize) +
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
        plot.title = element_text(size = font_size, hjust = 0.5) ,
        strip.text = element_text(size = font_size)
      ) + 
      scale_x_discrete(labels = function(x) str_wrap(x, width = width_text))   # Apply text wrapping
    
  } else if(is.null(institution_prov) & is.null(divider)){ # no-divider and no-institution
    
    plot_data[[var]] <- as.factor(plot_data[[var]])
    
    # Create plot
    g <- ggplot(plot_data, aes_string(x = var, y = "proportion")) +
      geom_bar(stat = "identity", fill = fill_colors, width = width_bar) +
      scale_y_continuous(labels = scales::percent, 
                         limits = c(0, max(plot_data$proportion + 0.05))) +
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
        plot.title = element_text(size = font_size, hjust = 0.5) ,
        strip.text = element_text(size = font_size)
      )  + 
      scale_x_discrete(labels = function(x) str_wrap(x, width = width_text))   # Apply text wrapping
    
  }
  
  # RETURN ####
  return(g)
  
}

