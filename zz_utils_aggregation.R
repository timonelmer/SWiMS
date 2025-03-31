# Aggregation Plot - SWIMS 
swims.plot.aggregation <- function(var, 
                                   institution_prov = NULL, 
                                   data = dat, 
                                   codeb = codebook, 
                                   fill_colors = NULL,
                                   colors_set = "Set2", 
                                   showCount = F,
                                   font_size = 12,
                                   width_bar = 0.5,
                                   annoFontSize = 8,
                                   count_votes_at_all = FALSE # Creates plot only showing how many did something at all
){
  
  # Example 
  # var <- "qrp_pressure"
  # institution_prov <- target_institution
  # data <- dat
  # codeb <- codebook
  # fill_colors <- NULL
  # colors_set <- "Accent"
  # showCount <- F
  # font_size <- 12
  # width_bar <- 0.5
  # annoFontSize <- 8
  # count_votes_at_all <- F
   
  # PREPARATION ####
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
  
  # SUB-PART: COUNT VOTES AT ALL ####
  if(count_votes_at_all){
    side_obj <- data %>%
      mutate(n_quoted = rowSums(across(all_of(var), ~ . == "quoted", .names = "check_{.col}"), na.rm = TRUE)) 
    
    if(is.null(institution_prov)){
      
      # How many quoted something
      quot_dat <- side_obj %>% 
        filter(n_quoted > 0) %>% 
        nrow()
      
      # How many did not quote something
      non_quot_dat <- side_obj %>% 
        filter(n_quoted == 0) %>% 
        nrow()
      
      # Mean quoted
      mean_quot_dat <- side_obj %>% 
        filter(n_quoted > 0) 
      
      mean_quot_dat <- mean(mean_quot_dat$n_quoted)
      
      # plot
      plot_data <- data.frame(
        group = c("At least one dimension reported", "No dimension reported"),
        count = c(quot_dat, non_quot_dat),
        Percentage = c(quot_dat / sum(quot_dat, non_quot_dat), non_quot_dat / sum(quot_dat, non_quot_dat))
      )
      
      plot_function <- function(plot_data) {
        g <- ggplot(plot_data, aes(x = group, y = Percentage)) +  
          geom_bar(stat = "identity", width = width_bar, fill = "darkgreen") +  
          labs(
            x = paste("Mean number of dimensions reported:", round(mean_quot_dat, 2)),   
            y = "Percentage"
            #title = "In cases where you have experienced or witnessed bullying, discrimination, harassment, or other unfair treatment, were such behaviors related to…"
          ) +
          scale_y_continuous(labels = scales::percent) +
          theme_minimal() +
          theme(
            text = element_text(size = font_size),
            axis.ticks.x = element_blank(),
            axis.text.y = element_text(size = font_size),
            axis.title = element_text(size = font_size),
            legend.position = "right",
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            plot.title = element_text(size = font_size, hjust = 0.5)
          )  +
          guides(fill = "none")
          
        
        
        if(showCount){
          return(g + geom_text(aes(label = paste0("N = ", count)), vjust = -0.5, size = annoFontSize, color = "black"))
        } else {
          return(g)
        }
      }
      
      g <- plot_function(plot_data)
      
      return(g)
      
    } else if(!is.null(institution_prov)){
      
      side_obj <- side_obj %>% 
        filter(institution == institution_prov)
      
      # How many quoted something
      quot_dat <- side_obj %>% 
        filter(n_quoted > 0) %>% 
        nrow()
      
      # How many did not quote something
      non_quot_dat <- side_obj %>% 
        filter(n_quoted == 0) %>% 
        nrow()
      
      # Mean quoted
      mean_quot_dat <- side_obj %>% 
        filter(n_quoted > 0) 
      
      mean_quot_dat <- mean(mean_quot_dat$n_quoted)
      
      # plot
      plot_data <- data.frame(
        group = c("At least one dimension reported", "No dimension reported"),
        count = c(quot_dat, non_quot_dat),
        Percentage = c(quot_dat / sum(quot_dat, non_quot_dat), non_quot_dat / sum(quot_dat, non_quot_dat))
      )
      
      plot_function <- function(plot_data) {
        g <- ggplot(plot_data, aes(x = group, y = Percentage)) +  
          geom_bar(stat = "identity", width = width_bar, fill = "darkgreen") +  
          labs(
            x = paste("Mean number of dimensions reported:", round(mean_quot_dat, 2)),   
            y = "Percentage"
            #title = "In cases where you have experienced or witnessed bullying, discrimination, harassment, or other unfair treatment, were such behaviors related to…"
          ) +
          scale_y_continuous(labels = scales::percent) +
          theme_minimal() +
          theme(
            text = element_text(size = font_size),
            axis.ticks.x = element_blank(),
            axis.text.y = element_text(size = font_size),
            axis.title = element_text(size = font_size),
            legend.position = "right",
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            plot.title = element_text(size = font_size, hjust = 0.5)
          ) +
          guides(fill = "none")
        
        
        
        if(showCount){
          return(g + geom_text(aes(label = paste0("N = ", count)), vjust = -0.5, size = annoFontSize, color = "black"))
        } else {
          return(g)
        }
      }
        
      g <- plot_function(plot_data)
      
      return(g)         
               
    }
  }
  
  # PREPROCESSING ####
  if(!is.null(institution_prov)){ # with institution
    
    # Prepare data for plotting
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
    
    plot_data <- bind_rows(obj)
    
    plot_data <- plot_data %>%
      left_join(var_text, by = "variable") %>%
      select(-variable) %>% 
      mutate(text = factor(text, levels = var_text$text)) %>%# Order responses correctly
      mutate(text = str_wrap(text, width = 20)) %>% 
      filter(value %in% "quoted") %>% 
      group_by(group) %>%
      mutate(Percentage = count / sum(count))
    
    
    # Rearrange group
    plot_data <- plot_data %>%
      arrange(group == "Other Institutions")
    plot_data$group <- factor(plot_data$group, levels = c(institution_prov, "Other Institutions"))
    

  } else if(is.null(institution_prov)){ # without institution)
    
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
    
    plot_data <- bind_rows(obj)
    
    # Merge labels for variables
    plot_data <- plot_data %>%
      left_join(var_text, by = "variable") %>%
      select(-variable) %>% 
      mutate(text = factor(text, levels = var_text$text)) %>%
      mutate(text = str_wrap(text, width = 20)) %>%
      filter(value %in% "quoted") %>%
      mutate(Percentage = count / sum(count))
    
  }
  
  # Define fill colors if not provided
  if(is.null(fill_colors)){
    fill_colors <- RColorBrewer::brewer.pal(n = min(length(unique(plot_data$text)), 9), name = colors_set)
    if (length(unique(plot_data$text)) > 9) {
      fill_colors <- colorRampPalette(fill_colors)(length(unique(plot_data$text)))
    }
  }
  
  # PLOTTING ####
  if(!is.null(institution_prov)){
    
    plot_function <- function(plot_data) {

      # Order levels
      plot_data$text <- as.factor(plot_data$text)
      plot_data$text <- fct_relevel(plot_data$text, "Unknown", "Prefer not to say", "No, no one", after = Inf)
      
      # Plot erstellen
      g <- ggplot(plot_data[plot_data$value %in% "quoted",], aes(x = group, y = count, fill = text)) +  
        geom_bar(stat = "identity", position = "fill", width = width_bar) +  # Stacked Bar Chart
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
          text = element_text(size = font_size),
          axis.text.x = element_text(size = font_size, angle = 45, hjust = 1),
          axis.text.y = element_text(size = font_size),
          axis.title = element_text(size = font_size),
          legend.text = element_text(size = font_size),
          legend.title = element_text(size = font_size),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.title = element_text(size = font_size, hjust = 0.5) 
        )  +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) # Apply text wrapping
      
      if(showCount){return(g+
                             geom_text(aes(label = count),  position = position_fill(vjust = 0.5), size = annoFontSize, color = "white")) 
      }else{
        return(g)
      }
    }
    
  } else if(is.null(institution_prov)){
    
    plot_function <- function(plot_data) {
      g <- ggplot(plot_data, aes(x = "", y = Percentage, fill = text)) +  
        geom_bar(stat = "identity", width = width_bar) +  
        labs(
          x = NULL,   
          y = "Percentage",
          fill = "Category",
          title = ""
        ) +
        scale_fill_manual(values = fill_colors) +  
        scale_y_continuous(labels = scales::percent) +
        theme_minimal() +
        theme(
          text = element_text(size = font_size),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(size = font_size),
          axis.title = element_text(size = font_size),
          legend.position = "right",
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.title = element_text(size = font_size, hjust = 0.5)
        ) 
      
      
      if(showCount){
        return(g + geom_text(aes(label = count), vjust = -0.5, size = annoFontSize, color = "black"))
      } else {
        return(g)
      }
    }
  }
  
  # RETURN ####
  plot_function(plot_data)

}

