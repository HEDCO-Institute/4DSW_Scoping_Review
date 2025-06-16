

#install and load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, rio, here, DT, shiny, plotly, openxlsx)

# Specify update dates
last_search <- "April 2025"
next_search <- "April 2026"

# Import cleaned data
cleaned_df <- import(here("data", "4dsw_app_data.xlsx"))

# Transform data for dashboard
cleaned_dashboard_df <- cleaned_df %>% 
  rename(study_design = effectiveness_approach,
         outcome_domain_studied = effectiveness,
         equity_domain_studied = equity,
         `Student Race/Ethnicity` = race_ethnicity,
         school_type = school_level,
         `Community Type (Rurality)` = community) %>% 
  mutate(study_design = ifelse(study_design == "Not Reported", "Not Applicable", study_design),
         outcome_domain_studied = str_replace_all(outcome_domain_studied, c("Retention" = "Staff/Teacher Retention",
                                                                            "Climate" = "School Climate",
                                                                            "Incidents" = "School Disciplinary Incidents",
                                                                            "Households" = "Household Impacts",
                                                                            "Crime" = "Criminal Activity")),
         outcome_domain_studied = if_else(!str_detect(evidence_domain, "(?i)Effectiveness"), "Not Applicable", outcome_domain_studied),
         equity_domain_studied = str_replace_all(equity_domain_studied, c("Age" = "Age or Grade Level",
                                                                           "Rurality" = "Community Type (Rurality)",
                                                                           "ELL" = "English Language Learner",
                                                                           "Gifted" = "Gifted Student Status",
                                                                           "Immigrant" = "Immigration Status")),
         equity_domain_studied = if_else(!str_detect(evidence_domain, "(?i)Equity"), "Not Applicable", equity_domain_studied),
         # Extract implementation-related terms and create implementation_domain
         implementation_domain = str_extract_all(evidence_domain, "(?i)(acceptability|feasibility|resource)") %>%
           lapply(function(x) str_replace_all(x, "Resource", "Resource Use")) %>%
           sapply(paste, collapse = "; "),
         # Create new_evidence_domain excluding extracted terms
         new_evidence_domain = str_remove_all(evidence_domain, "(?i)(acceptability|feasibility|resource)") %>%
           str_replace_all(";;", ";") %>%  
           str_trim()) %>%
   mutate(implementation_domain = str_remove(implementation_domain, "^;\\s*"),
          new_evidence_domain = str_remove(new_evidence_domain, "^;\\s*"))

# Clean up empty implementation_domain values to NA
cleaned_dashboard_df <- cleaned_dashboard_df %>%
  mutate(across(
    everything(), 
    ~ . %>%
      str_replace_all(";;+", ";") %>%    # Replace multiple semicolons with a single semicolon
      str_remove("^;\\s*") %>%           # Remove leading semicolons
      str_remove("\\s*;\\s*$") %>%       # Remove trailing semicolons, even if followed by a space
      str_squish(),                      # Remove extra spaces
    .names = "{.col}"                   # Keeps the original column names
  )) %>%
  mutate(across(ends_with("domain"), ~ifelse(. == "", "Not Applicable", .))) %>% 
  rename(implementation_domain_studied = implementation_domain)
  

  
# Create subset for users to download
cleaned_df_to_export <- cleaned_dashboard_df %>% 
  select(publication_year, title, corr_author_name, data_years, `Community Type (Rurality)`, school_type, grade_level, `Student Race/Ethnicity`,
         state, fifth_day_activities, outcome_domain_studied, equity_domain_studied, implementation_domain_studied, #evidence_domain,
         citation, citation_link, corr_author_link) #publication_type, publisher, study_design, 
  

# Transform for data table
td <- cleaned_dashboard_df %>% 
  mutate(linked_title = ifelse(citation_link != "Not Reported", paste0("<a href='", citation_link, "' target='_blank'>", title, "</a>"), title),
         corresponding_author = ifelse(corr_author_link != "Not Reported", paste0("<a href='", corr_author_link, "' target='_blank'>", corr_author_name, "</a>"), corr_author_name),
         publication_year = as.numeric(publication_year)) %>% 
  arrange(desc(publication_year), title) %>% 
  rename(title_text = title, 
         author_text = corr_author_name,
         title_link = citation_link,
         title = linked_title) %>% 
  select(publication_year, title, corresponding_author, data_years, `Community Type (Rurality)`, school_type, grade_level, `Student Race/Ethnicity`,
         state, fifth_day_activities, outcome_domain_studied, equity_domain_studied, implementation_domain_studied, #evidence_domain, #INTERNAL ONLY
         citation, title_text, author_text, title_link, corr_author_link) #study_design, publication_type, publisher, citation) #INTERNAL ONLY 


# Define filter choices
states <- state.abb
grades <- c("K", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
effect_choices <- c("Approach", "Achievement", "Attainment", "Attendance", "Criminal Activity", 
                    "Health", "Household Impacts", "School Climate", 
                     "School Disciplinary Incidents", "Staff/Teacher Retention")
fifthday_choices <- c("Child Care", "Extracurriculars (clubs, sports)", "Nothing (shut down school)",
                      "Student Instruction", "Teacher In-Service", "Trips (educational/field)")
race_ethnicity_choices <- c("American Indian and/or Alaska Native", "Asian", "Black or African American",
                            "Hispanic, Latino, or Spanish", "Native Hawaiian/Other Pacific Islander",
                            "White", "Other")

# Create function to capitalize column names
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
#cap_names <- capitalize_colnames(td)


# Define UI for application that will show table
ui <- fluidPage(
  # HTML Customization
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
             font-family: "Source Sans", sans-serif; 
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
             grid-template-columns: repeat(3, 1fr); /* Adjust 2 to the desired number of columns */
             gap: 5px;
           }
           .table {
             padding: 5px;
           }
         
           /* This block of commented-out styles is closed properly now */
           /*
           .table-container {
             display: flex;
           }
           .table-container .table {
             width: 50%;
             padding: 10px;
           }
           */
           
           $(document).ready(function() {
             $("[data-toggle=tooltip]").tooltip();
           });
         ')
      
    ),
    tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css?family=Open+Sans"),
  HTML('<!-- Google tag (gtag.js) -->
          <script async src="https://www.googletagmanager.com/gtag/js?id=G-8W2N0L5B8P"></script>
          <script>
            window.dataLayer = window.dataLayer || [];
            function gtag(){dataLayer.push(arguments);}
            gtag("js", new Date());
            gtag("config", "G-8W2N0L5B8P");
          </script>')
),
# Title
  div(class = "title-panel", "Four-Day School Week Research Database"),
  
  # Instructions
  div("Step 1 - If you'd like to look at studies in a specific state, select that state below:", 
      style = "text-align: left; font-size: 16px; margin-top: 10px; padding-left: 10px; color: #007030; font-weight: bold;"),
  div("Tip: You can only select one state at a time. Once a state is selected, click it again to de-select it. There is no data available for states that are not shown in green.", 
      style = "text-align: left; font-size: 12px; margin-top: 2px; padding-left: 10px;"),
  
  # US Map
  plotlyOutput("us_map"),
  
  # Instructions
  div("Step 2 - Select any additional filters:", 
      style = "text-align: left; font-size: 16px; margin-top: 10px; padding-left: 10px; color: #007030; font-weight: bold;"),
  div("Tip: Filters will show all studies that include, but are not limited to, your selected filter(s). ", 
      style = "text-align: left; font-size: 12px; margin-top: 2px; padding-left: 10px; margin-bottom: 5px;"),
  
 # Top panel with div and filters
  fluidRow(
    div(
      selectizeInput("community_filter", "Community Type (Rurality):", choices = c("Rural", "Suburban", "Urban"), multiple = TRUE),
      style = "display:inline-block; width:25%; margin-left: 25px;"
    ),
    div(
      selectizeInput("school_filter", "School Type:", choices = c("Elementary", "Middle", "High"), multiple = TRUE),
      style = "display:inline-block; width:25%;"
    ),
    div(
      selectizeInput("race_filter", "Student Race/Ethnicity:", choices = race_ethnicity_choices, multiple = TRUE),
      style = "display:inline-block; width:25%;"
    )
   ),
    
  fluidRow(
    div(
      selectizeInput("grade_filter", "Grade:", choices = grades, multiple = TRUE),
      style = "display:inline-block; width:25%; margin-left: 25px;"
    ),
    div(
      selectizeInput("fifthday_filter", "Fifth Day Activity:", choices = fifthday_choices, multiple = TRUE),
      style = "display:inline-block; width:25%;"
    ),
    div(
      selectizeInput("effectiveness_filter", "Outcome Domain Studied:", choices = effect_choices, multiple = TRUE),
      style = "display:inline-block; width:25%;"
    ),
    
    fluidRow(div(
      actionButton("resetFilters", "Reset Filters", class = "reset-button"),
      style = "padding-left: 30px;"))
    ),
 
  # Instructions
  div("Step 3 - Results that meet your criteria are below:", 
      style = "text-align: left; font-size: 16px; margin-top: 10px; padding-left: 10px; margin-bottom: 5px; color: #007030; font-weight: bold;"),
 
 
  # TabsetPanel data table, summary statistics, and glossary
 tabsetPanel(
   type = "tabs",
   tabPanel("Data Table",
            mainPanel(
              h2("Research Articles", style = "display: inline-block; margin-right: 20px;"),
              div(style = "display: flex; justify-content: space-between; align-items: center; ", 
                  p("All studies meeting your criteria:"),
                  div(
                    downloadButton("downloadData", "Download All Data", style = "display: inline-block; margin-right: 10px; margin-bottom: 5px; margin-left: 10px;"),
                    downloadButton("downloadFilteredData", "Download Filtered Data", style = "display: inline-block; margin-bottom: 5px; margin-left: 10px;")
                  )
              ),
              DTOutput("datatable")
            )
   ),
   tabPanel("Summary Statistics",
            h2("Summary Statistics"),
            p("The tables below present frequencies based on the filters you have selected above. The tables will automatically update as you change filters. "),
            uiOutput("summary_stats_table")
   ),
   tabPanel("Glossary",
            
            h3("Glossary of Terms"),
            
            h4("Publication Year:"),
            p("Year the study was published."),
            
            h4("Title:"),
            p("Title of the article this study is based on. If a link is available, it will link to the full article."),
            
            h4("Corresponding Author:"),
            p("Author listed in article as corresponding author. If a link is available, it will link to their public website."),
            
            h4("Data Years:"),
            p("The years from which data originated."),
            
            h4("Community Type (Rurality):"),
            p("Rural, Suburban, and/or Urban."),
            
            h4("School Type"),
            p("Educational level of schools (e.g., Elementary)."),
            
            h4("Grade Level:"),
            p("Grade level of students."),
            
            h4("Student Race/Ethnicity:"),
            p("This column lists the race/ethnicities of students in the study."),
            
            h4("State:"),
            p("State(s) where the study took place."),
            
            h4("Fifth Day Activity:"),
            p("What researchers reported regarding what schools did during the fifth day of the week when classes were not formally in session."),
            
            tags$ul(
              tags$li(HTML("<strong>Child care:</strong> School-provided childcare")),
              tags$li(HTML("<strong>Extracurriculars:</strong> Extracurricular activities such as clubs or sports")),
              tags$li(HTML("<strong>Nothing (shut down school):</strong> Schools that were not open to students on the fifth day")),
              tags$li(HTML("<strong>Student instruction:</strong> Schools that offered student instruction for at least one off-day per school year (full or partial day)")),
              tags$li(HTML("<strong>Teacher in-service:</strong> Schools that offered teacher in-service on at least one off-day per school year (full or partial day)")),
              tags$li(HTML("<strong>Trips:</strong> School outings such as educational or field trips"))
            ),
            
            # h4("Study Design:"),
            # p("The statistical approach or design of the study"),
            # 
            # tags$ul(
            #   tags$li(HTML("<strong>Before-after study:</strong> Studies that test differences in outcomes before and after a school switches from a five-day to a four-day week")),
            #   tags$li(HTML("<strong>Between-group – with controls:</strong> Studies that used multilevel regression or multiple linear regression to examine the relationship between four-day school weeks and outcomes interest while controlling for confounding variables.")),
            #   tags$li(HTML("<strong>Between-group – without controls:</strong> Studies that used ANOVA, MANOVA, Mann-Whitney U tests, or t-tests to test differences in outcomes between four-day school week and five-day school week outcomes without controlling for covariates")),
            #   tags$li(HTML("<strong>Computational model:</strong> Use of mathematical models and simulated data.")),
            #   tags$li(HTML("<strong>Descriptive statistics only:</strong> Studies that presented descriptive information and did not perform inferential statistical analyses")),
            #   tags$li(HTML("<strong>Difference-in-differences:</strong> Studies that analyze the difference in outcomes before and after a school switches to a four-day week compared to outcomes for schools that remained on a five-day week")),
            #   tags$li(HTML("<strong>Matched pair design:</strong> Studies that match samples based on characteristics such as demographic information, school location or school type before testing outcomes between four-day and five-day week schools."))
            # ),
            
            h4("Outcome Domain Studied:"),
            p("Variables researchers reported as outcomes. For this review, we grouped outcomes that were similar. For example, “Health” refers to any kind of health status or health-related behaviors."),
            
            tags$ul(
              tags$li(HTML("<strong>Achievement:</strong> Academic achievement of any kind (standardized tests, grades)")),
              tags$li(HTML("<strong>Attainment:</strong> Indicators of level of education completed (passing a grade level, graduation)")),
              tags$li(HTML("<strong>Attendance:</strong> Number of days a student attended school or was absent")),
              tags$li(HTML("<strong>Criminal Activity:</strong> Juvenile crime or juvenile justice involvement rates in a community")),
              tags$li(HTML("<strong>Disciplinary Incidents:</strong> Bullying, fighting, truancy, suspensions, expulsions")),
              tags$li(HTML("<strong>Health:</strong> Health status or health-related behaviors")),
              tags$li(HTML("<strong>Household Impacts:</strong> Family resources, home prices, parental employment, parental stress, time with family, travel.")),
              tags$li(HTML("<strong>School Climate:</strong> School climate such as belongingness or feelings about school")),
              tags$li(HTML("<strong>Staff/Teacher Retention:</strong> Staff/teacher retention rates or determinants of staff/teacher retention"))
            ),
            
            h4("Equity Domain Studied:"),
            p("Variables that researchers used to test differences across outcomes. For example, a study that is labeled with “age” is a study in which researchers tested differences in outcomes across student age groups."),
            
               tags$ul(
                 tags$li(HTML("<strong>Age or Grade Level:</strong> Student age or grade level")),
                 tags$li(HTML("<strong>Community Type (Rurality):</strong> School rurality status")),
                 tags$li(HTML("<strong>English Language Learner:</strong> Student English-language-learner status")),
                 tags$li(HTML("<strong>Gifted Student Status:</strong> Student gifted status")),
                 tags$li(HTML("<strong>Immigration Status:</strong> Student immigration status")),
                 tags$li(HTML("<strong>Race/ethnicity:</strong> Student’s race/ethnicity")),
                 tags$li(HTML("<strong>Socioeconomic Status:</strong> Student and family socioeconomic status")),
                 tags$li(HTML("<strong>Sex/gender:</strong> Student sex or gender identity")),
                 tags$li(HTML("<strong>Special Education:</strong> Student special education status"))
                 ),
            
            h4("Implementation Domain Studied:"),
            
               tags$ul(
                 tags$li(HTML("<strong>Acceptability:</strong> Examined the extent to which the 4DSW is acceptable to key stakeholder groups")),
                 tags$li(HTML("<strong>Feasibility:</strong> Examined how the intervention was implemented (e.g., barriers/facilitators to implementation, degree to which it was implemented as designed within the intended context and its existing resources)")),
                 tags$li(HTML("<strong>Resource Use:</strong>  Examined the resource requirements/implications of implementing the intervention (e.g., inputs/costs, cost-effectiveness/benefit)"))
               )
             )
  ),
# div(
#   style = "text-align: left; padding: 10px; background-color: #f1f1f1; width: 100%; font-size: 14px; margin-top: 15px;",
#   p(
#     HTML("<i class='fa fa-calendar'></i> Last search of the literature: <b>", last_search, "</b>"),
#     HTML("<br><i class='fa fa-refresh'></i> Updated search expected: <b>", next_search, "</b>")
#   ))
fluidRow(
  column(12,  # Makes it span the full width
         div(
           style = "text-align: left; padding: 10px; background-color: #f4f4f4; width: 100%; font-size: 14px; margin-top: 15px;",
           p(
             HTML("<i class='fa fa-calendar'></i> Last search of the literature: <b>", last_search, "</b>"),
             HTML("<br><i class='fa fa-refresh'></i> Updated search expected: <b>", next_search, "</b>")
           )
         )
  ))
)


server <- function(input, output, session) {
  
  # Create base map
  base_map <- plot_geo() %>%
    add_trace(
      z = rep(1, 33),
      locations = c("AK", "AZ",  "CA", "CO",  "FL", "GA", "HI", "ID", "IL", "IA", "KS", "KY", "LA", "ME", "MA", "MI", "MN", "MO", "MT", "NE", "NV", "NH", "NM", "NC", "ND", "OK", "OR", "PA", "SD", "TX", "UT", "WA", "WY"),
      colorscale = list(c(0, 1), c("#007030", "#007030")),
      #colors = custom_color_scale[selected_states],
      type = 'choropleth',
      locationmode = 'USA-states',
      marker = list(line = list(color = 'rgb(255,255,255)', width = 2)),
      hoverinfo = 'location',
      text = c("Alaska", "Arizona", "California", "Colorado", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Massachusetts", "Michigan", "Minnesota", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Mexico", "North Carolina", "North Dakota", "Oklahoma", "Oregon", "Pennsylvania", "South Dakota", "Texas", "Utah", "Washington", "Wyoming"),
      customdata = c("AK", "AZ", "CA", "CO", "FL", "GA", "HI", "ID", "IL",  "IA", "KS", "KY", "LA", "ME", "MA", "MI", "MN", "MO", "MT", "NE", "NV", "NH", "NM", "NC", "ND", "OK", "OR", "PA", "SD", "TX", "UT", "WA", "WY"),
      showscale = FALSE, # Hide the color scale
      colorscale = list(c(0, 1), c("rgb(255, 255, 255)", "rgb(255, 255, 255)")) # Set the color scale to be transparent
      
    ) %>%
    layout(
      geo = list(scope = 'usa')
    )

  # Create a US map
  output$us_map <- renderPlotly({



    # Register the 'plotly_click' event on map
    event_register(base_map, 'plotly_click')


    # us_states
    base_map
  })
  
  # Create a reactive variable to store selected states
  selected_states <- reactiveVal(character(0))
  
  clicked_state <- reactiveVal(character(0))

  
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
                colorscale = list(c(0, 1), c("#007030", "#007030")),
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
    
    # Filter by selected states 
    if (!is.null(selected_states()) && length(selected_states()) > 0) {
      selected_states_regex <- paste(selected_states(), collapse = "|")
      filtered_data <- filtered_data %>%
        filter(grepl(selected_states_regex, state, ignore.case = TRUE))
    }
    
    # Filter by community type
    if (!is.null(input$community_filter) && length(input$community_filter) > 0) {
      filter_expr_community <- do.call(cbind, lapply(input$community_filter, function(community_filter) {
        grepl(community_filter, filtered_data$`Community Type (Rurality)`, ignore.case = TRUE)
      }))
      filtered_data <- filtered_data %>%
        filter(rowSums(filter_expr_community) > 0)
    }
    
    # Filter by school type
    if (!is.null(input$school_filter) && length(input$school_filter) > 0) {
      filter_expr_school <- do.call(cbind, lapply(input$school_filter, function(school_filter) {
        grepl(school_filter, filtered_data$school_type, ignore.case = TRUE)
      }))
      filtered_data <- filtered_data %>%
        filter(rowSums(filter_expr_school) > 0)
    }
    

    # Filter by grade level
    if (!is.null(input$grade_filter) && length(input$grade_filter) > 0) {
      grade_filter_expr <- do.call(cbind, lapply(input$grade_filter, function(grade_filter) {
        grepl(paste0("\\b", grade_filter, "\\b"), filtered_data$grade_level, ignore.case = TRUE)
      }))
      filtered_data <- filtered_data %>%
        filter(rowSums(grade_filter_expr) > 0)
    }
    
    
    # Filter by effectiveness
    if (!is.null(input$effectiveness_filter) && length(input$effectiveness_filter) > 0) {
      filter_expr_effectiveness <- do.call(cbind, lapply(input$effectiveness_filter, function(effectiveness_filter) {
        grepl(effectiveness_filter, filtered_data$outcome_domain_studied, ignore.case = TRUE)
      }))
      filtered_data <- filtered_data %>%
        filter(rowSums(filter_expr_effectiveness) > 0)
    }

    # Filter by fifth day activity
    # if (!is.null(input$fifthday_filter) && length(input$fifthday_filter) > 0) {
    #   filter_expr_fifthday <- do.call(cbind, lapply(input$fifthday_filter, function(fifthday_filter) {
    #     grepl(fifthday_filter, filtered_data$fifth_day_activities, ignore.case = TRUE)
    #   }))
    #   filtered_data <- filtered_data %>%
    #     filter(rowSums(filter_expr_fifthday) > 0)
    # }
    
    # Fix to include parentheses 
    if (!is.null(input$fifthday_filter) && length(input$fifthday_filter) > 0) {
      # Escape any special characters in the filter options
      escaped_filters <- sapply(input$fifthday_filter, function(x) {
        stringr::str_replace_all(x, "([\\(\\)\\[\\]\\{\\}\\|\\^\\$\\.\\*\\+\\?\\\\])", "\\\\\\1")
      })
      
      # Combine the filters into a single regex pattern
      filter_pattern <- paste(escaped_filters, collapse = "|")
      
      # Apply the filter
      filtered_data <- filtered_data %>%
        filter(stringr::str_detect(fifth_day_activities, regex(filter_pattern, ignore_case = TRUE)))
    }
    


    # # Filter by race/ethnicity
    if (!is.null(input$race_filter) && length(input$race_filter) > 0) {
      # Create an empty logical vector of the same length as the data
      filter_expr_race <- logical(nrow(filtered_data))

      # Loop through each filter term and combine the results using OR
      for (race_filter in input$race_filter) {
        filter_expr_race <- filter_expr_race | grepl(race_filter, filtered_data$`Student Race/Ethnicity`, ignore.case = TRUE)
      }

      # Apply the combined filter
      filtered_data <- filtered_data %>%
        filter(filter_expr_race)

    }

    
 
    return(filtered_data)
  })
  

  # Render the filtered dataset as a table and specify column names
  output$datatable <- DT::renderDataTable({
    filtered_data <- filtered_dataset()


    if (nrow(filtered_data) > 0) {
      filtered_data <- filtered_data %>% 
        select(-author_text, -title_text, -corr_author_link, -title_link, -citation)
      
      colnames(filtered_data) <- capitalize_colnames(filtered_data)
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
      empty_data <- as.data.frame(matrix(NA, nrow = 0, ncol = ncol(filtered_data)))
      colnames(empty_data) <- colnames(filtered_data)
      colnames(empty_data) <- capitalize_colnames(filtered_data)
      datatable(empty_data, options = list(
        language = list(
          emptyTable = "No data matches your filters."
        )
      ))
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
    output$us_map <- renderPlotly({ #Reset Map
      base_map
    })
  })
  

  # Render the summary statistics table in the "Summary Statistics" tab
  output$summary_stats_table <- renderUI({
    #filtered data to use filters 
    filtered_data <- filtered_dataset()
    
   
     if (nrow(filtered_data) > 0) {
      colnames(filtered_data) <- capitalize_colnames(filtered_data)
      
    
    grade_text_table <- filtered_data %>%
      mutate(`Grade Level` = str_remove_all(`Grade Level`, "; -999|-999; ")) %>%
      separate_rows(`Grade Level`, sep = "; ") %>%
      mutate(`Grade Level` = case_when(`Grade Level` == "K"  ~ "Kindergarten",
                                     `Grade Level` == "1"  ~ "1st Grade",
                                     `Grade Level` == "2"  ~ "2nd Grade",
                                     `Grade Level` == "3"  ~ "3rd Grade",
                                     `Grade Level` == "4"  ~ "4th Grade",
                                     `Grade Level` == "5"  ~ "5th Grade",
                                     `Grade Level` == "6"  ~ "6th Grade",
                                     `Grade Level` == "7"  ~ "7th Grade",
                                     `Grade Level` == "8"  ~ "8th Grade",
                                     `Grade Level` == "9"  ~ "9th Grade",
                                     `Grade Level` == "10" ~ "10th Grade",
                                     `Grade Level` == "11" ~ "11th Grade",
                                     `Grade Level` == "12" ~ "12th Grade",
                                     TRUE ~ `Grade Level`)) %>% 
      count(`Grade Level`) %>%
      mutate(
        `Grade Level` = ifelse(`Grade Level` == "-999", "Not Reported", `Grade Level`),
        percent = paste0(round(n / nrow(td) * 100, 2), "%")
      ) %>%
      arrange(desc(`Grade Level` != "Not Reported"), desc(n))
  grade_text_table_render <- renderTable(grade_text_table)
    
   
    # function to create summary tables
    process_summary_tables <- function(var_name, data) {
      result <- data %>%
        mutate_at(vars(all_of(var_name)), list(~str_remove_all(.,"; -999|-999; "))) %>%
        separate_rows(all_of(var_name), sep = "; ") %>%
        count(!!sym(var_name)) %>%
        mutate(
          !!var_name := ifelse(!!sym(var_name) == "-999", "Not Reported", !!sym(var_name)),
          percent = paste0(round(n / nrow(data) * 100, 2), "%")
        ) %>%
        arrange(
          desc(!!sym(var_name) != "Not Applicable"),  # Push "Not Applicable" to the end
          desc(!!sym(var_name) != "Not Reported"),    # Push "Not Reported" to the end
          desc(n))
      
      return(result)
    }
    
    
    # List of variables
    vars_list <- c("Community Type (Rurality)", "School Type", "Grade Level", "Student Race/Ethnicity", 
                   "Fifth Day Activities", "Outcome Domain Studied", 
                   "Equity Domain Studied", "Implementation Domain Studied")
    
    # Use lapply to process data for each variable
    tables_list <- lapply(vars_list, process_summary_tables, data = filtered_data)
    
    #render tables with concise formatting 
    rendered_tables_list <- lapply(tables_list, function(tbl) {
      renderTable(tbl)
    })
    
    

    #show tables in two columns (with css code)
    div(
      class = "table-container",
      div(class = "table",
          h3("Community Type (Rurality) Table"), 
          rendered_tables_list[[1]]
      ),
      div(class = "table",
          h3("School Type Table"), 
          rendered_tables_list[[2]]
      ), 
      div(class = "table",
          h3("Race/Ethnicity Table"), 
          rendered_tables_list[[4]]
      ),
      div(class = "table",
          h3("Grade Level Table"), 
          grade_text_table_render
      ),
      div(class = "table",
          h3("Fifth Day Activities Table"), 
          rendered_tables_list[[5]]
      ),
      # div(class = "table",
      #     h3("Study Design Table"), 
      #     rendered_tables_list[[6]]
      # ),
      div(class = "table",
          h3("Outcome Domain Table"), 
          rendered_tables_list[[6]]
      ),
      div(class = "table",
          h3("Equity Domain Table"), 
          rendered_tables_list[[7]]
      ),
      div(class = "table",
          h3("Implementation Domain Table"), 
          rendered_tables_list[[8]]
      ),
    )
    
     } else {
       # Display a message when there are no matching results
       div(
         p("No data matches your filters."),
         style = "text-align: center; font-size: 16px; margin-top: 10px; font-weight: bold;"
       )
     }
    
    
 
  })
  
  #download buttons
  
  output$downloadData <- downloadHandler(
    filename = "4dsw_data.xlsx",
    content = function(file) {
      write.xlsx(cleaned_df_to_export, file)  # all data
    }
  )
  
  # output$downloadFilteredData <- downloadHandler(
  #   filename = "4dsw_filtered_data.xlsx",
  #   content = function(file) {
  #     filtered_data_export <- filtered_dataset() %>% 
  #     mutate(title_text = str_extract(title, "(?<=>)[^<]+"))
  #     
  #     colnames(filtered_data_export) <- cap_names
  #     
  #     write.xlsx(filtered_data_export, file)  # filtered data
  #   }
  # )
  
  output$downloadFilteredData <- downloadHandler(
    filename = "4dsw_filtered_data.xlsx",
    content = function(file) {
      filtered_data <- filtered_dataset() 
      
      filtered_data_export <- filtered_data %>% 
        select(-corresponding_author, -title) %>% 
        rename(title = title_text,
               corr_author_name = author_text,
               citation_link = title_link) %>% 
        select(publication_year, title, corr_author_name, everything())
      
      write.xlsx(filtered_data_export, file)  # Write the filtered data
    }
  )
  
}
  

  
  

shinyApp(ui, server)

#rsconnect::deployApp(appDir = "outputs/data_dashboard", appName = "4dsw_data_dashboard")#file needs to be .R; files need to be in data_dashboard folder; account needs to be setup
