
#### R Shiny app for visualizing the SWiMS 2024 data #####

# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)

load("../data/SWiMS2024_Data_2024-11-07.RData")

# Extract variable labels
var_labels <- attr(dat, "variable.labels")

# Get institution labels, ensure they are characters
institution_labels <- as.character(unique(dat$institution))

# UI part of the Shiny App
ui <- fluidPage(
  titlePanel("Variable Distribution by Institution"),
  
  sidebarLayout(
    sidebarPanel(
      # Dropdown for selecting the variable
      selectInput("variable", "Select a Variable:", choices = names(dat)),
      
      # Dropdown for selecting the institution
      selectInput("institution", "Select an Institution:", choices = institution_labels)
    ),
    
    mainPanel(
      # Plot output
      plotOutput("distributionPlot")
    )
  )
)

# Server part of the Shiny App
server <- function(input, output) {
  
  # Plot the distribution of the selected variable
  output$distributionPlot <- renderPlot({
    
    # Get selected variable and institution
    selected_var <- input$variable
    selected_inst <- input$institution
    
    # Use variable labels for axis labels and title
    x_label <- ifelse(!is.null(var_labels[selected_var]), var_labels[selected_var], selected_var)
    title_label <- ifelse(!is.null(var_labels[selected_var]), paste("Distribution of", var_labels[selected_var]), paste("Distribution of", selected_var))
    
    # Prepare data for the plot
    plot_data <- dat %>%
      mutate(institution_group = ifelse(institution == selected_inst, selected_inst, "Other Institutions")) %>%
      group_by(institution_group, !!sym(selected_var)) %>%
      summarise(count = n()) %>%
      group_by(institution_group) %>%
      mutate(proportion = count / sum(count)) # Calculate relative proportions
    
    # Plot relative proportions
    ggplot(plot_data, aes_string(x = selected_var, y = "proportion", fill = "institution_group")) +
      geom_bar(stat = "identity", position = "dodge", width = 0.8) +
      
      # Show proportions as percentages
      scale_y_continuous(labels = scales::percent) +
      
      # Add count above the bar
      geom_text(aes(label = count), position = position_dodge(width = 0.8), vjust = -0.5, size = 4) +
      
      # Customize bar colors
      scale_fill_manual(values = c("#1f77b4","#ff7f0e")) + 
      
      # Labels for axes and title
      labs(x = x_label, y = "Proportion", fill = "Institution", title = title_label) +
      
      # Minimal theme and adjust text sizes
      theme_minimal() +
      theme(
        text = element_text(size = 11),
        axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 11),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 11),
        plot.title = element_text(size = 11, hjust = 0.5)
      )
  })
}


# Run the Shiny App
shinyApp(ui = ui, server = server)

#TODO: check if the numbers in the plots correspond to the actual numbers (i.e., whether the numbers for "other institutions" are computed correctly) 
#TODO: Add features to the shiny app, such as: differenting the results not by institution but also by gender category (male, female, nonbinary, genderNA), research discipline, ...#
#... phd status (dat$phd), position type (dat$mainPosition). each of these variables can be activated with a checkbox and then the comparison is made per selected variable just like with the institution now
# only one checkbox can then be active at the same time. 
#TODO: add item formulation above the ggplot as a text (not within the plot but as a text in RShiny)


