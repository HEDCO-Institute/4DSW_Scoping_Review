library(shiny)
#library(shinydashboard)
library(tidyverse)
library(rio)
library(here)
library(RefManageR)
library(DT)


#import data
raw_df <- import(here("data", "4DSW_Data_copy.xlsx")) 

#list of single variables (to aggregate across id only)
single_cat_vars <- c("author", "publication_year", "publication_type", "publisher", "student_type", "student_discipline", 
                 "evidence_domain", "data_years", "effectiveness_approach")


#aggregate non-numeric variables
agg_df <- raw_df %>% 
  select(-starts_with("n")) %>% 
  group_by(refid) %>% 
  summarize(across(everything(), ~paste(unique(.), collapse = "; "))) %>% 
  ungroup() 

#subset of single single aggregated variables
sagg_df <- agg_df %>% 
  select(refid, all_of(single_cat_vars))

#create dataframe to marge state abbreviations
state_abbrev_df <- data.frame(category = state.name,
                              abbrev = state.abb)

#transform wide variables to categorical variables  
df_long <- agg_df %>%
  select(refid, starts_with("community"), starts_with("state"), starts_with("grade"),
         starts_with("school"), starts_with("effectiveness"), starts_with("equity"), 
         -effectiveness_approach) %>% 
  pivot_longer(cols = -refid, names_to = "variable", values_to = "response") %>%
  mutate(
    value = sub(".*_", "", variable),
    value = str_to_title(value),
    new_var_name = case_when(str_detect(variable, "^community") ~ "community", 
                            str_detect(variable, "^state") ~ "state",
                            str_detect(variable, "^grade") ~ "grade_level",
                            str_detect(variable, "^school") ~ "school_level",
                            str_detect(variable, "^effectiveness") ~ "effectiveness",
                            str_detect(variable, "^equity") ~ "equity",
                             TRUE ~ variable),
    category = case_when(str_detect(variable, "^community") & response == "Yes" ~  value, 
                         str_detect(variable, "^state") & response == "Yes" ~  value,
                         str_detect(variable, "^grade") & response == "Yes" ~  value,
                         str_detect(variable, "^school") & response == "Yes" ~  value,
                         str_detect(variable, "^effectiveness") & response == "Yes" ~  value,
                         str_detect(variable, "^equity") & response == "Yes" ~  value,
                         TRUE ~ NA)) %>% 
  mutate(category = case_when(category == "Newmexico" ~ "New Mexico",
                              category == "Southdakota" ~ "South Dakota",
                              category == "Northdakota" ~ "North Dakota",
                              TRUE ~ category)) %>% 
  left_join(state_abbrev_df, by = "category") %>% 
  mutate(abbrev = ifelse(is.na(abbrev), category, abbrev)) %>% 
  select(-category) %>% 
  rename(category = abbrev)

td_longvar <- df_long %>%
  group_by(refid, new_var_name) %>%
  summarise(category_value = paste(category[!is.na(category)], collapse = "; "), .groups = "drop_last") %>%
  pivot_wider(names_from = new_var_name, values_from = category_value) %>% 
  ungroup()

#import reference info from Zotero
ref_df <- import(here("data", "4dsw_eligible_citations.xlsx")) %>% 
  janitor::clean_names() %>% 
  rename(citation = bibliography)

#import citation links
link_df <- import(here("data", "4dsw_eligible_citations_links.xlsx")) %>% 
  janitor::clean_names() %>% 
  rename(link = link_to_public_full_text) %>% 
  select(refid, link)

current_included_refids <- raw_df$refid

#merge data together
jd <- sagg_df %>% 
  left_join(td_longvar) %>% 
  left_join(ref_df) %>% 
  left_join(link_df)
  


#transform for data table
td <- jd %>% 
  mutate_all(~ ifelse(is.na(.) | .x == -999 | . == "", "Not Reported", .)) %>% 
  mutate_all(~ str_remove_all(as.character(.), "-999; |; -999")) %>% 
  mutate(equity = str_replace_all(equity, c("Raceethnicity" = "Race/Ethnicity", "Sexgender" = "Sex/Gender", "Ell" = "ELL", 
                                            "Ses" = "SES", "Specialeducation" = "Special Education")),
         linked_title = paste0("<a href='", link, "' target='_blank'>", title, "</a>")) %>% 
  select(-title) %>% 
  rename(title = linked_title) %>% 
  select(publication_year, title, author, data_years, community, school_level, grade_level, state, 
         evidence_domain, effectiveness_approach, effectiveness, equity,
         publication_type, publisher, citation) %>% 
  arrange(desc(publication_year), title)


#
#paste0("[", title, "]", "(", link, ")")

#define filter choices
states <- state.abb
grades <- c("K", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
ev_domain <- unique(raw_df$evidence_domain)
effect_choices <- c("Approach", "Achievement", "Attainment", "Attendance", "Crime", "Health", "Housing",
                    "Incidents", "Retention")
equity_choices <- c("Age", "ELL", "Gifted", "Immigrant", "Race/Ethnicity", "Rurality", "SES", "Sex/Gender", "Special Education")

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
      HTML("
        .title-panel {
          text-align: left;
          padding-left: 10px; 
          font-size: 28px;
          /*font-weight: bold;
          font-family: Helvetica, sans-serif;
        }
        
         .dt-center.dt-body-center.column-Title {
           width: 700px !important; /* Adjust the width as needed */
        }
      ")
    )
  ),
  div(class = "title-panel", "Empirical Studies on the Four Day School Week"),
  
 # Top panel with div and filters
  fluidRow(
    div(
      selectizeInput("state_filter", "State:", choices = states, multiple = TRUE),
      style = "display:inline-block; width:10%; margin-left: 25px"
    ),
    div(
      selectizeInput("community_filter", "Location:", choices = c("Rural", "Suburban", "Urban"), multiple = TRUE),
      style = "display:inline-block; width:12%;"
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
      selectizeInput("evidence_filter", "Evidence Type:", choices = ev_domain, multiple = TRUE),
      style = "display:inline-block; width:15%;"
    ),
    div(
      selectizeInput("effectiveness_filter", "Effectiveness:", choices = effect_choices, multiple = TRUE),
      style = "display:inline-block; width:15%;"
    ),
    div(
      selectizeInput("equity_filter", "Equity:", choices = equity_choices, multiple = TRUE),
      style = "display:inline-block; width:15%;"
    ),
    
  ),
  
  # Top panel with filters
  # fluidRow(
  #   column(6, selectizeInput("state_filter", "State:", choices = states, multiple = TRUE)),
  #   column(6, selectizeInput("community_filter", "Location:", choices = c("Rural", "Suburban", "Urban"), multiple = TRUE)),
  #   column(6, selectizeInput("school_filter", "School Type:", choices = c("Elementary", "Middle", "High"), multiple = TRUE)),
  #   column(6, selectizeInput("grade_filter", "Grade", choices = grades, multiple = TRUE)),
  #   column(12, actionButton("reset", "Reset Filters"))
  # ),
  
  # Sidebar with filters
  # sidebarLayout(
  #   sidebarPanel(
  #     selectizeInput("state_filter", "State:", choices = states, multiple = TRUE),
  #     selectizeInput("community_filter", "Location:", choices = c("Rural", "Suburban", "Urban"), multiple = TRUE),
  #     selectizeInput("school_filter", "School Type:", choices = c("Elementary", "Middle", "High"), multiple = TRUE),
  #     selectizeInput("grade_filter", "Grade", choices = grades, multiple = TRUE),
  #     actionButton("reset", "Reset Filters")
  #   ),
    
    # Show table
    mainPanel(
      DTOutput("datatable")
    )
 )


server <- function(input, output, session) {
  # Create a reactive filtered dataset based on user selections
  filtered_dataset <- reactive({
    filtered_data <- td
    
    # Filter by state
    if (!is.null(input$state_filter) && length(input$state_filter) > 0) {
      filter_expr_state <- sapply(input$state_filter, function(state_filter) {
        grepl(state_filter, filtered_data$state, ignore.case = TRUE)
      })
      filtered_data <- filtered_data %>%
        filter(rowSums(filter_expr_state) > 0)
    }
    
    # Filter by community
    if (!is.null(input$community_filter) && length(input$community_filter) > 0) {
      filter_expr_community <- sapply(input$community_filter, function(community_filter) {
        grepl(community_filter, filtered_data$community, ignore.case = TRUE)
      })
      filtered_data <- filtered_data %>%
        filter(rowSums(filter_expr_community) > 0)
    }
    
    # Filter by school type
    if (!is.null(input$school_filter) && length(input$school_filter) > 0) {
      filter_expr_school <- sapply(input$school_filter, function(school_filter) {
        grepl(school_filter, filtered_data$school_level, ignore.case = TRUE)
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
    
    # Filter by evidence
    if (!is.null(input$evidence_filter) && length(input$evidence_filter) > 0) {
      filter_expr_evidence <- sapply(input$evidence_filter, function(evidence_filter) {
        grepl(evidence_filter, filtered_data$evidence_domain, ignore.case = TRUE)
      })
      filtered_data <- filtered_data %>%
        filter(rowSums(filter_expr_evidence) > 0)
    }
    
    # Filter by effectiveness
    if (!is.null(input$effectiveness_filter) && length(input$effectiveness_filter) > 0) {
      filter_expr_effectiveness <- sapply(input$effectiveness_filter, function(effectiveness_filter) {
        grepl(effectiveness_filter, filtered_data$effectiveness, ignore.case = TRUE)
      })
      filtered_data <- filtered_data %>%
        filter(rowSums(filter_expr_effectiveness) > 0)
    }
    
    # Filter by equity
    if (!is.null(input$equity_filter) && length(input$equity_filter) > 0) {
      filter_expr_equity <- sapply(input$equity_filter, function(equity_filter) {
        grepl(equity_filter, filtered_data$equity, ignore.case = TRUE)
      })
      filtered_data <- filtered_data %>%
        filter(rowSums(filter_expr_equity) > 0)
    }

    return(filtered_data)
  })
  

  # Render the filtered dataset as a table and specify column names
  output$datatable <- DT::renderDataTable({
    filtered_data <- filtered_dataset()
    colnames(filtered_data) <- cap_names
    
    datatable(filtered_data, 
              escape = FALSE,
              rownames = FALSE,
              options = list(
                dom = 'lBfrtipC',
                columnDefs = list(list(width = "1000px", targets = "Title")),
                pageLength = 20)) %>% 
      formatStyle("Title", width = "1000px")
  })

  observeEvent(input$reset, {
    updateSelectizeInput(session, "state_filter", selected = character(0))
    updateSelectizeInput(session, "community_filter", selected = character(0))
    updateSelectizeInput(session, "school_filter", selected = character(0))
    updateSelectizeInput(session, "grade_filter", selected = character(0))
    updateSelectizeInput(session, "evidence_filter", selected = character(0))
    updateSelectizeInput(session, "effectiveness_filter", selected = character(0))
    updateSelectizeInput(session, "equity_filter", selected = character(0))
  })
}

shinyApp(ui, server)

rsconnect::deployApp(appDir = "outputs/data_dashboard", appName = "4dsw_data_dashboard") #file needs to be named app.R
