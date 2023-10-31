
#install and load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, rio, here, DT, shiny, plotly)

#import cleaned data
cleaned_df <- import(here("data", "4dsw_app_data.xlsx"))

#transform for data table
td <- cleaned_df %>% 
  mutate(linked_title = ifelse(link != "Not Reported", paste0("<a href='", link, "' target='_blank'>", title, "</a>"), title),
         corresponding_author = ifelse(email != "Not Reported", paste0("<a href='mailto:", email, "'>", corr_author_name, "</a>"), corr_author_name),
         publication_year = as.numeric(publication_year)) %>% 
  arrange(desc(publication_year), title) %>% 
  select(-title) %>% 
  rename(title = linked_title,
         study_design = effectiveness_approach,
         outcome_domain_studied = effectiveness,
         equity_domain_studied = equity,
         `Student Race/Ethnicity` = race_ethnicity,
         school_type = school_level,
         location = community) %>% 
  select(publication_year, title, corresponding_author, data_years, location, school_type, grade_level, `Student Race/Ethnicity`,
         state, fifth_day_activities, study_design, outcome_domain_studied, equity_domain_studied,
         publication_type) 


#define filter choices
states <- state.abb
grades <- c("K", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
effect_choices <- c("Approach", "Achievement", "Attainment", "Attendance", "Crime", "Health", "Housing",
                    "Incidents", "Retention")
fifthday_choices <- c("Extracurriculars (clubs/sports)", "Student Instruction", "Teacher In-Service",
                      "Nothing (shut down school)", "Child Care", "Field/Educational Trips")
race_ethnicity_choices <- c("American Indian and/or Alaska Native", "Asian", "Black or African American",
                            "Hispanic, Latino, or Spanish", "Native Hawaiian/Other Pacific Islander",
                            "White", "Other")

#function to capitalize column names
capitalize_colnames <- function(df) {
  new_names <- colnames(df) %>%
    lapply(function(name) {
      name <- str_replace_all(name, "_", " ")
      name <- str_to_title(name)
      return(name)
    })
  return(unlist(new_names))
}
#extract cleaned column names
cap_names <- capitalize_colnames(td)


# Define UI for application that will show table
ui <- fluidPage(
  # Application title
  tags$head(
    tags$style(
      HTML('
        .title-panel {
          text-align: left;
          padding-left: 10px; 
          font-size: 28px;
          /*font-weight: bold; */
          font-family: "Open Sans", sans-serif;
        }
        
         .dt-center.dt-body-center.column-Title {
           width: 700px !important; 
         }
         
         body {
           font-family: "Open Sans", sans-serif; 
         }
         
         .reset-button {
      padding-left: 5px; 
         }
         
    table {
        border-collapse: collapse;
        width: 100%;
        border: 1px solid #ddd;
      }
      th, td {
        text-align: left;
        padding: 8px;
        border-bottom: 1px solid #ddd;
      }
      
      .table-container {
        display: grid;
        grid-template-columns: repeat(2, 1fr); /* Adjust 2 to the desired number of columns */
        gap: 10px;
      }
      .table {
        padding: 5px;
      }
      
     /* .table-container {*/
     /*   display: flex;*/
     /* }
    /*  .table-container .table {*/
    /*    width: 50%;*/
    /*    padding: 10px;*/
      }
         
         $(document).ready(function() {
            $("[data-toggle=tooltip]").tooltip();
         });
        
      ')
    ),
    tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css?family=Open+Sans")
  ),
  div(class = "title-panel", "Four-Day School Week Research Database"),
  
  # Instructions
  div("If you'd like to look at studies in a specific state(s), select that state here:", style = "text-align: left; font-size: 16px; margin-top: 10px; padding-left: 10px;"),
  
  # US Map
  plotlyOutput("us_map"),
  
  # Instructions
  div("Select any additional filters", style = "text-align: left; font-size: 16px; margin-top: 10px; padding-left: 10px;"),
  
 # Top panel with div and filters
  fluidRow(
    div(
      selectizeInput("community_filter", "Location:", choices = c("Rural", "Suburban", "Urban"), multiple = TRUE),
      style = "display:inline-block; width:12%; margin-left: 25px"
    ),
    div(
      selectizeInput("school_filter", "School Type:", choices = c("Elementary", "Middle", "High"), multiple = TRUE),
      style = "display:inline-block; width:12%;"
    ),
    div(
      selectizeInput("grade_filter", "Grade:", choices = grades, multiple = TRUE),
      style = "display:inline-block; width:10%;"
    ),
    div(
      selectizeInput("race_filter", "Student Race/Ethnicity:", choices = race_ethnicity_choices, multiple = TRUE),
      style = "display:inline-block; width:15%;"
    ),
    div(
      selectizeInput("fifthday_filter", "Fifth Day Activity:", choices = fifthday_choices, multiple = TRUE),
      style = "display:inline-block; width:15%;"
    ),
    div(
      selectizeInput("effectiveness_filter", "Outcome Domain Studied:", choices = effect_choices, multiple = TRUE),
      style = "display:inline-block; width:15%;"
    ),
    
    fluidRow(div(
      actionButton("resetFilters", "Reset Filters", class = "reset-button"),
      style = "padding-left: 30px;")),
    
  ),
 
  # Instructions
  div("All studies meeting your criteria:", style = "text-align: left; font-size: 16px; margin-top: 10px; padding-left: 10px;"),
 
  # TabsetPanel for Data Table and Summary Statistics
  tabsetPanel(
    type = "tabs",
    tabPanel("Data Table",
             # Show table
             mainPanel(h2("Empirical Studies"),
               DTOutput("datatable")
             )
    ),
    tabPanel("Summary Statistics",
             h2("Summary Statistics"),
             uiOutput("summary_stats_table")
    ),
    #verbatimTextOutput("results_text"),
    #downloadButton("downloadData", "Download Data")
   
    )
 )


server <- function(input, output, session) {
  
  # Create a reactive variable to store selected states
  selected_states <- reactiveVal(character(0))
  
  clicked_state <- reactiveVal(character(0))
  
  data_available <- reactiveVal(TRUE)
  
  # Create base map
  base_map <- plot_geo() %>%
    add_trace(
      z = rep(1, 50),
      locations = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"),
      colorscale = 'Viridis',
      type = 'choropleth',
      locationmode = 'USA-states',
      marker = list(line = list(color = 'rgb(255,255,255)', width = 2)),
      hoverinfo = 'location',
      text = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"),
      customdata = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"),
      showscale = FALSE, # Hide the color scale
      colorscale = list(c(0, 1), c("rgb(255, 255, 255)", "rgb(255, 255, 255)"))
    ) %>%
    layout(
      geo = list(scope = 'usa')
    )

  # Create a US map
  output$us_map <- renderPlotly({

    us_states <- plot_geo() %>%
      add_trace(
        z = rep(1, 50),
        locations = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"),
        colorscale = 'Viridis',
        #colors = custom_color_scale[selected_states],
        type = 'choropleth',
        locationmode = 'USA-states',
        marker = list(line = list(color = 'rgb(255,255,255)', width = 2)),
        hoverinfo = 'location',
        text = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"),
        customdata = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"),
        showscale = FALSE, # Hide the color scale
        colorscale = list(c(0, 1), c("rgb(255, 255, 255)", "rgb(255, 255, 255)")) # Set the color scale to be transparent

      ) %>%
      layout(
        geo = list(scope = 'usa')
      )

    # Register the 'plotly_click' event
    event_register(us_states, 'plotly_click')


    us_states
  })
  
  # Listen to map click events and update selected states
  observeEvent(event_data("plotly_click"), {
    event <- event_data("plotly_click")
    if (!is.null(event)) {
      clicked_state <- event$customdata
      if (clicked_state %in% selected_states()) {
        # If the state is already selected, remove it
        selected_states(selected_states()[selected_states() != clicked_state])
      } else {
        # If the state is not selected, add it to the list
        selected_states(c(selected_states(), clicked_state))
      }
      # Update the map based on selected states or revert to the base map
      if (length(selected_states()) > 0) {
        # Update the map with selected states
        output$us_map <- renderPlotly({
            us_states <- plot_geo() %>%

              # Highlight selected states with custom color
              add_trace(
                z = length(selected_states),
                locations = selected_states(),
                colorscale = 'Viridis',
                type = 'choropleth',
                locationmode = 'USA-states',
                marker = list(line = list(color = 'rgb(255,255,255)', width = 2)),
                hoverinfo = 'location',
                customdata = selected_states(),
                showscale = FALSE,
                colorscale = list(c(0, 1), c("rgb(255, 255, 255)", "rgb(255, 255, 255)"))
              ) %>%
              layout(
                geo = list(scope = 'usa')
              )

            us_states
        })
      } else {
        # Revert to the base map
        output$us_map <- renderPlotly({
          base_map
        })
      }
    }
    
  })
  



  
  
   # Create a reactive filtered dataset based on user selections
  filtered_dataset <- reactive({
    filtered_data <- td
    
    # if (nrow(filtered_data) == 0) {
    #   # Display a message to the user
    #   shinyjs::html("No matching data found.")
    #   return(NULL)
    # }
  
    
    # Filter by selected states (FILTER WHEN MULTIPLE = OR)
    if (length(selected_states()) > 0) {
      selected_states_regex <- paste(selected_states(), collapse = "|")
      filtered_data <- filtered_data %>%
        filter(grepl(selected_states_regex, state, ignore.case = TRUE))
    } 

    
 
    # Filter by community
    if (!is.null(input$community_filter) && length(input$community_filter) > 0) {
      filter_expr_community <- sapply(input$community_filter, function(community_filter) {
        grepl(community_filter, filtered_data$location, ignore.case = TRUE)
      })
      filtered_data <- filtered_data %>%
        filter(rowSums(filter_expr_community) > 0)
    } 
    
    # Filter by school type
    if (!is.null(input$school_filter) && length(input$school_filter) > 0) {
      filter_expr_school <- sapply(input$school_filter, function(school_filter) {
        grepl(school_filter, filtered_data$school_type, ignore.case = TRUE)
      })
      filtered_data <- filtered_data %>%
        filter(rowSums(filter_expr_school) > 0)
    } 
    
    # Filter by grade level
    if (!is.null(input$grade_filter) && length(input$grade_filter) > 0) {
      grade_filter_expr <- sapply(input$grade_filter, function(grade_filter) {
        grepl(paste0("\\b", grade_filter, "\\b"), filtered_data$grade_level, ignore.case = TRUE)
      })
      filtered_data <- filtered_data %>%
        filter(rowSums(grade_filter_expr) > 0)
    } 
    
   # Filter by effectiveness
    # Filter by effectiveness
    # if (!is.null(input$effectiveness_filter) && length(input$effectiveness_filter) > 0) {
    #   filter_expr_effectiveness <- sapply(input$effectiveness_filter, function(effectiveness_filter) {
    #     grepl(effectiveness_filter, filtered_data$outcome_domain_studied, ignore.case = TRUE)
    #   })
    #   filtered_data <- filtered_data %>%
    #     filter(rowSums(filter_expr_effectiveness) > 0)
    # } 
    
    if (!is.null(input$effectiveness_filter) && length(input$effectiveness_filter) > 0) {
      filter_expr_effectiveness <- sapply(input$effectiveness_filter, function(effectiveness_filter) {
        grepl(effectiveness_filter, filtered_data$outcome_domain_studied, ignore.case = TRUE)
      })
      filtered_data <- filtered_data %>%
        filter(rowSums(filter_expr_effectiveness) > 0)
    } 


    # Filter by fifth day
    
    # if (!is.null(input$fifthday_filter) && length(input$fifthday_filter) > 0) {
    #   filter_expr_fifthday <- sapply(input$fifthday_filter, function(fifthday_filter) {
    #     grepl(fifthday_filter, filtered_data$fifth_day_activities, ignore.case = TRUE)
    #   })
    #   filtered_data <- filtered_data %>%
    #     filter(rowSums(filter_expr_fifthday) > 0)
    # } 
    # 
    cat("Selected Fifth Day Filters:")
    print(input$fifthday_filter)
    
    # if (!is.null(input$fifthday_filter) && length(input$fifthday_filter) > 0) {
    #   filtered_data <- filtered_data[grepl(
    #     paste(input$fifthday_filter, collapse = "|"),
    #     filtered_data$fifth_day_activities, ignore.case = TRUE
    #   ), ]
    # } 

    
    # # Filter by fifth day
    # if (!is.null(input$fifthday_filter) && length(input$fifthday_filter) > 0) {
    #   # Create an empty logical vector of the same length as the data
    #   filter_expr_fifthday <- logical(nrow(filtered_data))
    # 
    #   # Loop through each filter term and combine the results using OR
    #   for (fifthday_filter in input$fifthday_filter) {
    #     filter_expr_fifthday <- filter_expr_fifthday | grepl(fifthday_filter, filtered_data$fifth_day_activities, ignore.case = TRUE)
    #   }
    # 
    #   # Apply the combined filter
    #   filtered_data <- filtered_data %>%
    #     filter(filter_expr_fifthday)
    # 
    #   # Check if there is no data after filtering
    #   if (nrow(filtered_data) == 0) {
    #     return("No data matches the selected filters.")
    #   }
    # }
    
    if (!is.null(input$fifthday_filter) && length(input$fifthday_filter) > 0) {
      # Create a regular expression pattern by escaping special characters
      filter_pattern <- sapply(input$fifthday_filter, function(filter) {
        filter <- gsub("([\\[\\]{}()+*^$|\\\\.?])", "\\\\\\1", filter, perl = TRUE)
        paste(filter, collapse = "|")
      })
      
      # Use grepl with the regular expression pattern
      filtered_data <- filtered_data[grepl(filter_pattern, filtered_data$fifth_day_activities, ignore.case = TRUE), ]
    } 

    # Filter by race
    
    # if (!is.null(input$race_filter) && length(input$race_filter) > 0) {
    #   filter_expr_race <- sapply(input$race_filter, function(race_filter) {
    #     grepl(race_filter, filtered_data$`Student Race/Ethnicity`, ignore.case = TRUE)
    #   })
    #   filtered_data <- filtered_data %>%
    #     filter(rowSums(filter_expr_race) > 0)
    # } 
    
    # # Filter by race/ethnicity
    # if (!is.null(input$race_filter) && length(input$race_filter) > 0) {
    #   # Create an empty logical vector of the same length as the data
    #   filter_expr_race <- logical(nrow(filtered_data))
    # 
    #   # Loop through each filter term and combine the results using OR
    #   for (race_filter in input$race_filter) {
    #     filter_expr_race <- filter_expr_race | grepl(race_filter, filtered_data$`Student Race/Ethnicity`, ignore.case = TRUE)
    #   }
    # 
    #   # Apply the combined filter
    #   filtered_data <- filtered_data %>%
    #     filter(filter_expr_race)
    # 
    #   # Check if there is no data after filtering
    #   if (nrow(filtered_data) == 0) {
    #     return("No data matches the selected race/ethnicity filters.")
    #   }
    # }
    
    # Filter by race/ethnicity
    if (!is.null(input$race_filter) && length(input$race_filter) > 0) {
      filter_expr_race <- sapply(input$race_filter, function(race_filter) {
        grepl(race_filter, td$`Student Race/Ethnicity`, ignore.case = TRUE)
      })
      # Check if there are rows to filter
      if (nrow(td) > 0) {
        filtered_data <- td %>%
          filter(rowSums(filter_expr_race) > 0)
      } else {
        filtered_data <- data.frame()
      }
    } else {
      filtered_data <- td
    }

    return(filtered_data)
  })
  
  # # Render the text based on the number of rows in the filtered dataset
  # output$data_summary <- renderText({
  #   filtered_data <- filtered_dataset()
  #   num_rows <- nrow(filtered_data)
  #   if (num_rows == 0) {
  #     return("No data matches the selected filters.")
  #   } else {
  #     return(paste("Number of matching rows: ", num_rows))
  #   }
  # })  
  
  

  # Render the filtered dataset as a table and specify column names
  output$datatable <- DT::renderDataTable({
    filtered_data <- filtered_dataset()

    # Check if data is NULL (no matching data)
    if (is.null(filtered_data) || nrow(filtered_data) == 0) {
      return(DT::datatable(data.frame(Message = "No data matches your filters.")) %>% formatStyle("Message", fontWeight = "bold")) # or handle it appropriately
    }
    
    if (nrow(filtered_data) > 0) {
      colnames(filtered_data) <- cap_names
      datatable(filtered_data, 
                escape = FALSE,
                rownames = FALSE,
                options = list(
                  dom = 'lBfrtipC',
                  columnDefs = list(list(width = "1000px", targets = "Title")),
                  pageLength = 20
                )) %>% 
        formatStyle("Publication Year", cursor = "pointer", `data-toggle` = "tooltip", title = "Description")
    } else {
      # Handle the case when filtered_data is empty
      return(HTML("No data matches your filters."))
    }
  })
  
  
  
  observeEvent(input$resetFilters, {
    updateSelectizeInput(session, "community_filter", selected = character(0))
    updateSelectizeInput(session, "school_filter", selected = character(0))
    updateSelectizeInput(session, "grade_filter", selected = character(0))
    updateSelectizeInput(session, "effectiveness_filter", selected = character(0))
    updateSelectizeInput(session, "fifthday_filter", selected = character(0))
    updateSelectizeInput(session, "race_filter", selected = character(0))
    selected_states(character(0))  # Reset selected states
    data_available(TRUE) 
    output$us_map <- renderPlotly({ #Reset Map
      base_map
    })
  })
  
  observeEvent(input$community_filter, {
    data_available(nrow(filtered_dataset()) > 0)
  })
  
  observeEvent(input$school_filter, {
    data_available(nrow(filtered_dataset()) > 0)
  })
  
  observeEvent(input$grade_filter, {
    data_available(nrow(filtered_dataset()) > 0)
  })
  
  observeEvent(input$effectiveness_filter, {
    data_available(nrow(filtered_dataset()) > 0)
  })
  
  observeEvent(input$fifthday_filter, {
    data_available(nrow(filtered_dataset()) > 0)
  })
  
  observeEvent(input$race_filter, {
    data_available(nrow(filtered_dataset()) > 0)
  })

  
  # Render the summary statistics table in the "Summary Statistics" tab
  output$summary_stats_table <- renderUI({
    #filtered data to use filters (td if no filters)
    filtered_data <- filtered_dataset()
    
    if (is.null(filtered_data) || nrow(filtered_data) == 0) {
      return(HTML("No data matches your filters."))
    }

    
    if (nrow(filtered_data) > 0) {
      colnames(filtered_data) <- cap_names
      community_table <- filtered_data %>%
        mutate(Location = str_remove_all(Location, "; -999|-999; ")) %>%
        separate_rows(Location, sep = "; ") %>%
        count(Location) %>%
        mutate(
          Location = ifelse(Location == "-999", "Not Reported", Location),
          percent = paste0(round(n / nrow(td) * 100, 2), "%")
        ) %>%
        arrange(desc(Location != "Not Reported"), desc(n))
      
      school_table <- filtered_data %>%
        mutate(`School Type` = str_remove_all(`School Type`, "; -999|-999; ")) %>%
        separate_rows(`School Type`, sep = "; ") %>%
        count(`School Type`) %>%
        mutate(
          `School Type` = ifelse(`School Type` == "-999", "Not Reported", `School Type`),
          percent = paste0(round(n / nrow(td) * 100, 2), "%")
        ) %>%
        arrange(desc(`School Type` != "Not Reported"), desc(n))
      
      design_table <- filtered_data %>%
        mutate(`Study Design` = str_remove_all(`Study Design`, "; -999|-999; ")) %>%
        separate_rows(`Study Design`, sep = "; ") %>%
        count(`Study Design`) %>%
        mutate(
          `Study Design` = ifelse(`Study Design` == "-999", "Not Reported", `Study Design`),
          percent = paste0(round(n / nrow(td) * 100, 2), "%")
        ) %>%
        arrange(desc(`Study Design` != "Not Reported"), desc(n))
      
      #render tables with concise formatting 
      community_table_html <- renderTable(community_table)
      design_table_html <- renderTable(design_table)
      school_table_html <- renderTable(school_table)
      
      #show tables in two columns (with css code)
      div(
        class = "table-container",
        div(class = "table",
            h3("Location Table"), # Add a title for the community table
            community_table_html
        ),
        div(class = "table",
            h3("School Type Table"), 
            school_table_html
        ), 
        div(class = "table",
            h3("Study Design Table"), # Add a title for the study design table
            design_table_html
        ),
      )
    } else {
      # Handle the case when filtered_data is empty
      return(HTML("No data matches your filters."))
    }
    
    
  })

  # output$downloadData <- downloadHandler(
  #   filename = function() {
  #     # Define the filename for the downloaded file
  #     "4dsw_app_data.xlsx"
  #   },
  #   content = function(file) {
  #     openxlsx::write.xlsx(as.data.frame(td), path= file)
  #   }
  # )
    
}
  
   #}
  
  

shinyApp(ui, server)

###TODO
#MAKE TITLE COLUMN WIDER

#add description/notes section - and note that at the top to state if no email there is no link for author. Same for title links - no public full text link
#add note describing what categories are (esp. effectiveness categories e.g., approach)