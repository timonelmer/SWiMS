# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)

load("../data/SWiMS2024_Data_2024-11-18.RData")

# Extract variable labels
var_labels <- attr(dat, "variable.labels")

# Get institution labels, ensure they are characters
institution_labels <- as.character(unique(dat$institution))

# UI part of the Shiny App
ui <- fluidPage(
  titlePanel("SWiMS 2024 Data: Variable Distribution"),
  
  sidebarLayout(
    sidebarPanel(
      # Dropdown for selecting the variable
      selectInput("variable", "Select a Variable:", choices = names(dat), selected = "stress"),
      
      # Checkboxes to differentiate by other categories
      checkboxGroupInput("category", "Differentiate by:", 
                         choices = list(
                           "Institution" = "institution",
                           "Gender" = "gender",
                           "Research Discipline" = "researchDiscipline",
                           "PhD Status" = "phd",
                           "Position Type" = "mainPosition"
                         )),
      
      # Dropdown for selecting the institution or other category levels
      uiOutput("levelSelect")
    ),
    
    mainPanel(
      # Plot output
      plotOutput("distributionPlot"),
      
      # Display the item formulation below the plot
      uiOutput("itemText")
    )
  )
)

# Server part of the Shiny App
server <- function(input, output, session) {
  
  # Update the UI for selecting levels of the chosen category
  output$levelSelect <- renderUI({
    selected_category <- input$category
    
    # Ensure at most one category is selected
    if (length(selected_category) > 1) {
      showNotification("Please select only zero or one category for comparison at a time.", type = "error")
      return(NULL)
    }
    
    if (length(selected_category) == 1) {
      category_levels <- unique(na.omit(dat[[selected_category]]))  # Remove NAs from category levels
      selectInput("category_level", paste("Select a Level of", selected_category, ":"), 
                  choices = category_levels, selected = category_levels[1])
    }
  })
  
  # Render the item formulation text below the plot
  output$itemText <- renderUI({
    selected_var <- input$variable
    item_text <- codebook[codebook$VarName %in% input$variable,"question"]
    
    
    
    if (!is.null(item_text) && item_text != "") {
      HTML(paste("<b>Item Formulation:</b>", item_text))
    } else {
      HTML("<b>Item Formulation:</b> No label available for this variable.")
    }
  })
  
  # Plot the distribution of the selected variable
  output$distributionPlot <- renderPlot({
    
    # Get selected variable and category for comparison
    selected_var <- input$variable
    selected_category <- input$category
    
    # Ensure only one category is selected at a time
    if (length(selected_category) > 1) {
      return(NULL)  # If more than one category is selected, do not render the plot
    }
    
    # Determine if a specific category level is selected
    if (length(selected_category) == 1) {
      selected_level <- input$category_level
      plot_data <- dat %>%
        mutate(group = ifelse(!!sym(selected_category) == selected_level, as.character(selected_level), "Other")) %>%
        filter(!is.na(!!sym(selected_var)), !is.na(group)) %>%  # Remove NAs from x-axis and fill variable
        group_by(group, !!sym(selected_var)) %>%
        summarise(count = n()) %>%
        group_by(group) %>%
        mutate(proportion = count / sum(count)) # Calculate relative proportions
    } else {
      # If no category is selected, show the overall distribution
      plot_data <- dat %>%
        filter(!is.na(!!sym(selected_var))) %>%  # Remove NAs from x-axis variable
        group_by(!!sym(selected_var)) %>%
        summarise(count = n()) %>%
        mutate(proportion = count / sum(count)) # Calculate relative proportions
    }
    
    # Use variable labels for axis labels and title
    x_label <- ifelse(!is.null(var_labels[selected_var]), var_labels[selected_var], selected_var)
    title_label <- paste("Distribution of", x_label)
    
    # Plot relative proportions
    ggplot(plot_data, aes_string(x = selected_var, y = "proportion", fill = ifelse(length(selected_category) == 1, "group", "selected_var"))) +
      geom_bar(stat = "identity", position = "dodge", width = 0.8) +
      
      # Show proportions as percentages
      scale_y_continuous(labels = scales::percent) +
      
      # Add count above the bar
      geom_text(aes(label = count), position = position_dodge(width = 0.8), vjust = -0.5, size = 4) +
      
      # Customize bar colors
      scale_fill_manual(values = c("#1f77b4", "#ff7f0e")) + 
      
      # Labels for axes and title
      labs(x = x_label, y = "Proportion", fill = ifelse(length(selected_category) == 1, "Group", "Value"), title = title_label) +
      
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
