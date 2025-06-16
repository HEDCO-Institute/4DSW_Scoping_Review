# This script imports, cleans, and analyzes data needed to reproduce the technical report

# Install and load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(janitor, tidyverse, rio, here, readxl, gt)

# Import data
raw_df <- import(here("data", "4dsw_study_data.xlsx")) %>% 
  janitor::clean_names() %>% 
  distinct(across(-user), .keep_all = TRUE) 

elig_df <- read_excel(here("data", "4dsw_eligibility_data.xlsx")) %>% 
  clean_names()

linked_df <- import(here("data", "4dsw_linked_references.csv")) %>% 
  janitor::clean_names()

cit_df <- import(here("data", "4dsw_all_citations.xlsx")) %>% 
  janitor::clean_names()

dupe_df <- import(here("data", "4dsw-duplicate-refs.csv"))

#### TODO: Figure out how to export search date from Distiller data -------
our_search_date <- "April 2025"

## Custom Functions -----------------------------------

# Function to clean citation text
clean_citation <- function(citation_column) {
  citation_column %>%
    gsub("#[^#]*#", "", .) %>%            # Remove all text between and including ##
    gsub("\\s+", " ", .) %>%              # Replace multiple spaces with a single space
    gsub("\\.(?![^.]*$)", "", ., perl = TRUE) %>% # Remove periods except the last one
    gsub("\\s+$", "", .) %>%              # Remove trailing spaces
    gsub("\\(\\),\\s*", "", .) %>%        # Remove instances of () and (), 
    gsub("\\(\\)", "", .) %>%             # Remove empty parentheses
    gsub(",\\s*\\.", ".", .) %>%          # Replace ", ." with "."
    gsub("\\s*\\.", ".", .) %>%           # Replace " ." with "."
    gsub("\\s*\\,", ",", .)               # Replace " ," with ","
}


# Define function to summarize checkbox variables with majority category on top
summarize_checkboxes <- function(data, prefix, filter_condition = NULL) {
  filtered_data <- if (!is.null(filter_condition)) {
    data %>% filter(!!rlang::parse_expr(filter_condition))
  } else {
    data
  }
  
  filtered_data %>%
    select(starts_with(prefix)) %>%
    summarise(across(everything(), ~ sum(!is.na(.)), .names = "N_{col}")) %>%
    pivot_longer(cols = everything(), names_to = "Category", values_to = "N") %>%
    mutate(
      Category = str_remove(Category, paste0("^N_", prefix)), # Remove prefix
      Percentage = round((N / nrow(filtered_data)) * 100, 1),
      Denominator = nrow(filtered_data),  # Add the denominator
      SortOrder = case_when( # Create a custom sort order
        Category %in% c("not reported", "unknown") ~ 1,
        TRUE ~ 0
      )
    ) %>%
    arrange(SortOrder, desc(N)) %>% # Arrange by SortOrder, then by N
    select(-SortOrder) # Remove the SortOrder column
}



# Create function to summarize single-response categorical variables
categorical_summary <- function(data, var, filter_condition = NULL) {
  filtered_data <- if (!is.null(filter_condition)) {
    data %>% filter(!!rlang::parse_expr(filter_condition))
  } else {
    data
  }
  
  filtered_data %>%
    count(!!sym(var), name = "N") %>%
    mutate(
      Category = tolower(as.character(!!sym(var))), # Convert Category to lowercase
      Percentage = round((N / nrow(filtered_data)) * 100, 1), # Calculate percentage
      Denominator = nrow(filtered_data),  # Add denominator
      SortOrder = case_when( # Create a custom sort order
        Category %in% c("not reported", "unknown") ~ 1,
        TRUE ~ 0
      )
    ) %>%
    arrange(SortOrder, desc(N)) %>% # Arrange by SortOrder, then by N
    select(Category, N, Percentage, Denominator, -SortOrder) # Reorder and drop SortOrder
}


# Summarize numerical variables (median and range)
numerical_summary <- function(data, var) {
  data %>%
    summarise(
      Median = median(!!sym(var), na.rm = TRUE),
      Min = min(!!sym(var), na.rm = TRUE),
      Max = max(!!sym(var), na.rm = TRUE),
      Range = paste0(min(!!sym(var), na.rm = TRUE), "-", max(!!sym(var), na.rm = TRUE))
    )
}

# Create a lookup table for proper state names
state_lookup <- c(
  "alabama" = "Alabama", "alaska" = "Alaska", "arizona" = "Arizona", "arkansas" = "Arkansas",
  "california" = "California", "colorado" = "Colorado", "connecticut" = "Connecticut", "delaware" = "Delaware",
  "florida" = "Florida", "georgia" = "Georgia", "hawaii" = "Hawaii", "idaho" = "Idaho",
  "illinois" = "Illinois", "indiana" = "Indiana", "iowa" = "Iowa", "kansas" = "Kansas",
  "kentucky" = "Kentucky", "louisiana" = "Louisiana", "maine" = "Maine", "maryland" = "Maryland",
  "massachusetts" = "Massachusetts", "michigan" = "Michigan", "minnesota" = "Minnesota", "mississippi" = "Mississippi",
  "missouri" = "Missouri", "montana" = "Montana", "nebraska" = "Nebraska", "nevada" = "Nevada",
  "newhampshire" = "New Hampshire", "newjersey" = "New Jersey", "newmexico" = "New Mexico", "newyork" = "New York",
  "northcarolina" = "North Carolina", "northdakota" = "North Dakota", "ohio" = "Ohio", "oklahoma" = "Oklahoma",
  "oregon" = "Oregon", "pennsylvania" = "Pennsylvania", "rhodeisland" = "Rhode Island", "southcarolina" = "South Carolina",
  "southdakota" = "South Dakota", "tennessee" = "Tennessee", "texas" = "Texas", "utah" = "Utah",
  "vermont" = "Vermont", "virginia" = "Virginia", "washington" = "Washington", "westvirginia" = "West Virginia",
  "wisconsin" = "Wisconsin", "wyoming" = "Wyoming"
)

## Reproduce Results ------------------------------- 

### Search & Eligibility ------

# Number of quarantined references in Distiller
number_of_duplicates <- nrow(dupe_df)

# Number of search results
number_search_results <- nrow(cit_df) + number_of_duplicates

# Number of citations excluded
number_dropped <- sum(elig_df$study_screening_decision == "Drop")
number_ineligible <- sum(elig_df$study_eligibility_decision == "Not Eligible", na.rm = TRUE)

# Number of citations included
number_kept<- sum(elig_df$study_screening_decision == "Keep")

# Number of citations not retrieved
number_not_retrieved <- elig_df %>%
  filter(pdf_retrieved == "No") %>% 
  nrow()

# Calculate exclude reasons
# 1) Main studies marked “Not Eligible”
excluded_main <- elig_df %>%
  filter(study_eligibility_decision == "Not Eligible") %>%
  transmute(
    id     = as.character(refid),
    reason = study_exclude_reason
  )

# 2) Any linked reports for those same studies
excluded_linked <- linked_df %>%
  #rename(refid = ref_id) %>% 
  filter(refid %in% excluded_main$id) %>%
  transmute(
    id     = as.character(linked_refid),
    reason = excluded_main$reason[match(refid, excluded_main$id)]
  )

# 3) Combine and dedupe so each citation counts once
all_excluded <- bind_rows(excluded_main, excluded_linked) %>%
  distinct(id, reason)

# 4) Tally and map to your parenthetical labels
exclusion_reasons_summary <- all_excluded %>%
  count(reason, name = "n") %>%
  arrange(desc(n)) %>%
  mutate(
    label = case_when(
      reason == "Ineligible concept - not four-day school week"     ~ "Ineligible concept (not 4DSW)",
      reason == "Ineligible context - not K-12 schools"             ~ "Ineligible context (not K-12)",
      reason == "Ineligible context - not in United States"         ~ "Ineligible context (not US)",
      reason == "Ineligible study design - not empirical research"  ~ "Ineligible design (not empirical research)",
      reason == "Ineligible publication - not in English"           ~ "Ineligible publication (not English)",
      TRUE                                                           ~ reason
    )
  )

# Number of reports of included studies
## Pull out the IDs of studies that survived full-text eligibility
eligible_refids <- elig_df %>%
  filter(study_eligibility_decision == "Eligible") %>%
  pull(refid)
## For each of those studies, grab all of its linked reports
eligible_linked_refids <- linked_df %>%
  #rename(refid = ref_id) %>% 
  filter(refid %in% eligible_refids) %>%
  pull(linked_refid)
##Combine and dedupe
all_eligible_reports <- unique(c(eligible_refids, eligible_linked_refids))
## Your count is just the length of that vector
number_eligible_reports <- length(all_eligible_reports)


# Number of eligible studies
number_eligible <- nrow(raw_df)


### Study Data -------------

# Create table of results
data <- raw_df %>% 
  mutate(across(where(is.character), ~ na_if(., "")),
         implementability_acceptability = evidence_acceptability,
         implementability_feasibility = evidence_feasibility,
         implementability_resource = evidence_resource)

# Apply custom function to variable groups
community_summary <- summarize_checkboxes(data, "community_")
grade_summary <- summarize_checkboxes(data, "grade_")
school_summary <- summarize_checkboxes(data, "school_")
fifthday_summary <- summarize_checkboxes(data, "fifthday_") %>% 
  mutate(Category = str_replace_all(Category, "_", " "))
evidence_summary <- summarize_checkboxes(data, "evidence_")

stakeholder_summary <- summarize_checkboxes(data, "stakeholder_groups_select_", filter_condition = "!is.na(evidence_acceptability)") %>% #demon = studies reporting acceptability
  mutate(Category = str_replace_all(Category, "_", " "))
outcome_summary <- summarize_checkboxes(data, "effectiveness_", filter_condition = "!is.na(evidence_effectiveness)") %>% #demon = studies reporting effectiveness
  filter(Category != "approach") %>% 
  mutate(Category = case_when(Category == "achievement" ~ "student achievement",
                              Category == "attendance" ~ "student attendance",
                              Category == "retention" ~ "teacher and staff recruitment and retention",
                              Category == "health" ~ "student health status or behaviors",
                              Category == "attainment" ~ "student attainment",
                              Category == "climate" ~ "school climate",
                              Category == "incidents" ~ "disciplinary incidents during school",
                              Category == "households" ~ "household impacts",
                              Category == "crime" ~ "juvenile crime",
                              TRUE ~ Category))
equity_summary <- summarize_checkboxes(data, "equity_", filter_condition = "!is.na(evidence_equity)") %>% #demon = studies reporting equity
  mutate(Category = case_when(Category == "age" ~ "student age",
                              Category == "rurality" ~ "community type or rurality", 
                              Category == "ses" ~ "student socioeconomic status", 
                              Category == "raceethnicity" ~ "student race or ethnicity",
                              Category == "sexgender" ~ "student gender identity or sex", 
                              Category == "specialeducation" ~ "student special education status", 
                              Category == "ell" ~ "student English language learner status", 
                              Category == "gifted" ~ "gifted student status", 
                              Category == "immigrant" ~ "student immigration status", 
                              Category == "domain_none" ~ "not reported",
                              TRUE ~ Category))
implementability_summary <- summarize_checkboxes(data, "implementability_", filter_condition = "if_any(starts_with('implementability'), ~ !is.na(.))") #demon = studies reporting implementability

# Fix wording for inline code
publication_summary_specific <- categorical_summary(data, "publication_type") %>% 
  mutate(Category = case_when(Category == "student" ~ "student dissertation or thesis",
                              Category == "journal" ~ "peer-reviewed journal article",
                              TRUE ~ Category))

# Apply the lookup table to fix state names
state_summary <- summarize_checkboxes(data, "state_") %>%
  mutate(Category = recode(Category, !!!state_lookup))

grey_lit_summary <- publication_summary_specific %>% 
  filter(Category != "peer-reviewed journal article")

journal_summary <- publication_summary_specific %>% 
  filter(Category == "peer-reviewed journal article")

# Categorical variables custom function
student_type_summary <- categorical_summary(data, "student_type", filter_condition = "publication_type == 'Student'")
student_disc_summary <- categorical_summary(data, "student_discipline", filter_condition = "publication_type == 'Student'")
race_summary <- categorical_summary(data, "raceethnicity_majority")
approach_summary <- categorical_summary(data, "effectiveness_approach", filter_condition = "!is.na(evidence_effectiveness)")

# Combine categories for publication type and summarize
publication_summary <- categorical_summary(data, "publication_type") %>% 
  mutate(
    CombinedCategory = case_when(
      Category %in% c("student", "report", "preprint") ~ "grey literature",
      TRUE ~ Category # Keep "journal" as-is
    )
  ) %>%
  group_by(CombinedCategory) %>%
  summarise(
    N = sum(N),
    Percentage = round((N / sum(N)) * 100, 1), # Use the sum of the current N
    Denominator = sum(Denominator) # The denominator remains unchanged
  ) %>%
  rename(Category = CombinedCategory) %>% 
  arrange(desc(N))



# Numeric variables custom function
publication_year_summary <- numerical_summary(data, "publication_year")

#For data_years, as checkbox variable
# Combine all year columns into a single column
years_summary <- data %>%
  select(starts_with("data_years_select_")) %>%  # Select all year-related columns
  pivot_longer(cols = everything(), names_to = "year_variable", values_to = "year") %>%  # Reshape to long format
  mutate(year = as.numeric(year)) %>%  # Ensure the years are numeric
  filter(!is.na(year) & year != -999)  # Remove NA and -999 values

# Calculate median and range
summary_data_years <- years_summary %>%
  summarise(
    Median = median(year, na.rm = TRUE),
    Min = min(year, na.rm = TRUE),
    Max = max(year, na.rm = TRUE),
    Range = paste0(min(year, na.rm = TRUE), "-", max(year, na.rm = TRUE))
  )



## Print Table 1 info --------------------------------

print(publication_year_summary)
print(publication_summary)
print(student_type_summary)
print(student_disc_summary) 
print(summary_data_years)
print(community_summary)
print(state_summary, n = 50)
print(grade_summary)
print(school_summary)
print(race_summary)
print(fifthday_summary)


##  Print Table 2 info ---------------------------------
print(evidence_summary[1,])
print(evidence_summary[2,])
print(stakeholder_summary, n = 30) 
print(approach_summary) 
print(outcome_summary) 
print(transform(evidence_summary[3,], Percentage = round(N / nrow(data %>% filter(!is.na(evidence_effectiveness))) * 100, 0))) 
print(equity_summary)
print(slice(evidence_summary, -1:-3))


## Tidy data for Appendices ----------

# Clean citations
elig_td <- elig_df %>% 
  mutate(citation_cleaned = citation %>%
           # Remove all text between and including ##
           gsub("#[^#]*#", "", .) %>%
           # Replace multiple spaces with a single space
           gsub("\\s+", " ", .) %>%
           # Remove periods except the last one
           gsub("\\.(?![^.]*$)", "", ., perl = TRUE) %>%
           # Remove trailing spaces
           gsub("\\s+$", "", .) %>%
           # Remove instances of () and (), 
           gsub("\\(\\),\\s*", "", .) %>%
           gsub("\\(\\)", "", .) %>%
           # Replace ", ." with "."
           gsub(",\\s*\\.", ".", .))

# Reference data
cit_td <- cit_df %>% 
  mutate(citation_cleaned = bibliography %>%
           # Remove all text between and including ##
           gsub("#[^#]*#", "", .) %>%
           # Replace multiple spaces with a single space
           gsub("\\s+", " ", .) %>%
           # Remove periods except the last one
           gsub("\\.(?![^.]*$)", "", ., perl = TRUE) %>%
           # Remove trailing spaces
           gsub("\\s+$", "", .) %>%
           # Remove instances of () and (), 
           gsub("\\(\\),\\s*", "", .) %>%
           gsub("\\(\\)", "", .) %>%
           # Replace ", ." with "."
           gsub(",\\s*\\.", ".", .) %>% 
           # Replace " ." with "."
           gsub("\\s*\\.", ".", .) %>% 
           # Replace " ," with ","
           gsub("\\s*\\,", ",", .)) %>% 
  select(-bibliography)

### Appendix 1: studies “Not Eligible” ------------

a1_info <- elig_td %>%
  filter(study_eligibility_decision == "Not Eligible") %>%
  select(citation_cleaned, study_exclude_reason) %>%
  arrange(citation_cleaned)

### Appendix 2: studies with pdf_retrieved == "No" ----------
a2_info <- elig_td %>%
  filter(pdf_retrieved == "No") %>%
  mutate(
    reason = if_else(
      pdf_retrieved == "No",
      "Could not locate document",
      study_exclude_reason
    )
  ) %>%
  select(citation_cleaned, reason) %>%
  arrange(citation_cleaned)

### Appendix 3: long characteristics table ----------------

# Tidy study data (from Analysis_Script.Rmd)
#transform linked references to combine multiple reports per study
linked_td <- linked_df %>% 
  select(refid, linked_refid) %>% 
  rowwise() %>% 
  mutate(main_refid = refid) %>%
  reframe(main_refid = main_refid,
          refid = c(refid, linked_refid)) %>%
  ungroup() %>% 
  distinct() %>% 
  left_join(cit_td) %>% 
  group_by(main_refid) %>% 
  summarize(all_reports = paste(citation_cleaned, collapse = " !!! "),
            all_refids = paste(refid, collapse = ", ")) %>% 
  ungroup() %>% 
  mutate(all_reports = str_replace_all(all_reports, "!!! ", "<br><br>")) %>% 
  rename(refid = main_refid) %>% 
  mutate(refid = as.character(refid))


df_long <- raw_df %>%
  # Select relevant columns
  select(refid, starts_with("community"), starts_with("state"), starts_with("grade"),
         starts_with("school"), starts_with("effectiveness"), starts_with("equity"), 
         starts_with("evidence"), -effectiveness_approach, starts_with("stakeholder_groups_select"), starts_with("data_years_select")) %>% 
  # Pivot to long format
  pivot_longer(cols = -refid, names_to = "variable", values_to = "response") %>%
  # Extract and clean the variable categories
  mutate(
    value = sub(".*_", "", variable), # Extract the suffix of the variable name
    value = str_to_title(value), # Capitalize for consistency
    value = ifelse(value == "Unknown", "Not Reported", value), # Rename "Unknown" to "Not Reported"
    response = str_remove_all(response, "; -999|-999;"), # Clean invalid codes
    response = str_squish(response), # Remove unnecessary spaces
    # If a response exists (non-NA), use the response itself as the category
    category = case_when(
      !is.na(response) & response != "" ~ value,
      TRUE ~ NA_character_
    ),
    # Create new variable names for grouping
    new_var_name = case_when(
      str_detect(variable, "^community") ~ "community", 
      str_detect(variable, "^state") ~ "state",
      str_detect(variable, "^grade") ~ "grade_level",
      str_detect(variable, "^school") ~ "school_level",
      str_detect(variable, "^effectiveness") ~ "effectiveness",
      str_detect(variable, "^equity") ~ "equity",
      str_detect(variable, "^evidence") ~ "evidence_domain",
      str_detect(variable, "^data_years_select") ~ "data_years",
      str_detect(variable, "^stakeholder_groups_select") ~ "stakeholder_groups",
      TRUE ~ variable
    ),
    # Fix specific state naming issues
    category = case_when(
      category == "Newmexico" ~ "New Mexico",
      category == "Southdakota" ~ "South Dakota",
      category == "Northdakota" ~ "North Dakota",
      category == "Northcarolina" ~ "North Carolina",
      category == "Southcarolina" ~ "South Carolina",
      category == "Newhampshire" ~ "New Hampshire",
      TRUE ~ category
    )
  )

td_longvar <- df_long %>%
  group_by(refid, new_var_name) %>%
  summarise(category_value = paste(category[!is.na(category)], collapse = "; "), .groups = "drop_last") %>%
  pivot_wider(names_from = new_var_name, values_from = category_value) %>% 
  ungroup() %>% 
  mutate(data_years = ifelse(data_years == "999", "Not Reported", data_years))



#list of single variables (to aggregate across id only)
single_cat_vars <- c("author_last_name", "publication_year", "publication_type", "publisher", "student_type", "student_discipline",
                     "effectiveness_approach")

#subset of single single aggregated variables
single_df <- raw_df %>%
  select(refid, all_of(single_cat_vars))

#transform fifth day data for table
fif_td <- raw_df %>% 
  pivot_longer(cols = fifthday_child_care:fifthday_not_reported, 
               names_to = "variable", 
               values_to = "response") %>% 
  mutate(response = str_replace_all(response, 
                                    c("Not reported" = "Not Reported"))) %>% 
  group_by(refid) %>%
  summarise(fifth_day_activities = paste(response[!is.na(response)], collapse = "; "), .groups = "drop_last") %>%
  ungroup()

#transform additional data for table
# add_td <- add_df %>% 
#   mutate(email = ifelse(grepl("@", corr_author_contact), corr_author_contact, NA))

race_td <- raw_df %>% 
  mutate(raceethnicity_checkbox_other_please_specify = str_remove_all(raceethnicity_checkbox_other_please_specify, "\\s*\\(please specify\\)")) %>% 
  pivot_longer(cols = raceethnicity_checkbox_american_indian_and_or_alaska_native:raceethnicity_checkbox_other_please_specify, 
               names_to = "variable", 
               values_to = "response") %>% 
  group_by(refid) %>%
  summarise(race_ethnicity = paste(response[!is.na(response)], collapse = "; "), .groups = "drop_last") %>%
  ungroup() %>% 
  mutate(race_ethnicity = ifelse(race_ethnicity == "", "Not Reported", race_ethnicity))

# #extract title from reference df
# ref_td <- ref_df %>% 
#   dplyr::select(refid, title)

#extract cleaned citation 
elig_subset <- elig_td %>% 
  dplyr::select(refid, citation_cleaned)

#merge all study data together
jd <- single_df %>% 
  left_join(td_longvar) %>% 
  left_join(fif_td) %>% 
  # left_join(add_td) %>% 
  left_join(race_td) %>% 
  # left_join(ref_td) %>% 
  left_join(elig_subset) %>% 
  left_join(select(cit_df, refid, title), by = "refid")

#correct formatting
td_study <- jd %>% 
  mutate_all(~ ifelse(is.na(.) | .x == -999 | . == "", "Not Reported", .)) %>% 
  mutate_all(~ str_remove_all(as.character(.), "-999; |; -999")) %>% 
  mutate_all(~ str_remove_all(as.character(.), "Not Reported; |; Not Reported")) %>% 
  mutate(equity = str_replace_all(equity, c("Raceethnicity" = "Race/Ethnicity", "Sexgender" = "Sex/Gender", "Ell" = "ELL", 
                                            "Ses" = "SES", "Specialeducation" = "Special Education"))) %>% 
  mutate(study_design = case_when(effectiveness_approach == "Hierarchical Linear Modeling" ~ "Between Groups - With Controls",
                                  effectiveness_approach == "Regression Adjustment" ~ "Between Groups - With Controls",
                                  effectiveness_approach == "ANOVA" ~ "Between Groups - Without Controls", 
                                  effectiveness_approach == "MANOVA" ~ "Between Groups - Without Controls", 
                                  effectiveness_approach == "Mann-Whitney U test" ~ "Between Groups - Without Controls", 
                                  effectiveness_approach == "T-Test" ~ "Between Groups - Without Controls", 
                                  effectiveness_approach == "No" ~ "Not Reported",
                                  TRUE ~ effectiveness_approach)) %>% 
  mutate(study_design = ifelse(study_design == "Not Reported", "Not Applicable", study_design),
         stakeholder_groups = ifelse(stakeholder_groups == "Not Reported", "Not Applicable", stakeholder_groups),
         outcome_domain_studied = str_replace_all(effectiveness, c("Retention" = "Staff/Teacher Retention",
                                                                   "Climate" = "School Climate",
                                                                   "Incidents" = "School Disciplinary Incidents",
                                                                   "Households" = "Household Impacts",
                                                                   "Crime" = "Criminal Activity")),
         equity_domain_studied = str_replace_all(equity, c("Age" = "Age or Grade Level",
                                                           "Rurality" = "Community Type (Rurality)",
                                                           "ELL" = "English Language Learner",
                                                           "Gifted" = "Gifted Student Status",
                                                           "Immigrant" = "Immigration Status",
                                                           "Not Reported" = "Not Reported or Applicable")),
         study_author_year = paste(author_last_name, publication_year),
         pub_type = ifelse(publication_type == "Student", paste0(publication_type, ": ", student_type), publication_type)) 

# Process `data_years` into ranges for table
td_dy <- td_study %>%
  separate_rows(data_years, sep = ";\\s*") %>%
  filter(data_years != "Not Reported") %>%
  mutate(data_years = as.numeric(data_years)) %>%
  arrange(refid, data_years) %>%
  group_by(refid) %>%
  mutate(
    gap         = data_years - lag(data_years, default = first(data_years)) > 1,
    range_group = cumsum(gap)
  ) %>%
  group_by(refid, range_group) %>%
  summarise(
    range = ifelse(
      n() == 1,
      as.character(data_years),
      paste0(min(data_years), "-", max(data_years))
    ),
    .groups = "drop"
  ) %>%
  group_by(refid) %>%
  summarise(data_years = paste(range, collapse = "; "), .groups = "drop")

td_study_tbl <- td_study %>%
  select(-data_years) %>%
  left_join(td_dy, by = "refid") %>%
  mutate(data_years = if_else(is.na(data_years), "Not Reported", data_years))

# Process `grade_level` into ranges for table
td_gl <- td_study_tbl %>%
  separate_rows(grade_level, sep = ";\\s*") %>%
  mutate(
    grade_level = if_else(grade_level == "K", "0", grade_level),
    grade_level = if_else(grade_level == "Not Reported", NA, grade_level)
  ) %>%
  filter(!is.na(grade_level)) %>%
  mutate(grade_level = as.numeric(grade_level)) %>%
  arrange(refid, grade_level) %>%
  group_by(refid) %>%
  mutate(
    gap         = grade_level - lag(grade_level, default = first(grade_level)) > 1,
    range_group = cumsum(gap)
  ) %>%
  group_by(refid, range_group) %>%
  summarise(
    range = ifelse(
      n() == 1,
      if_else(first(grade_level) == 0, "K", as.character(first(grade_level))),
      paste0(
        if_else(min(grade_level) == 0, "K", as.character(min(grade_level))),
        "-",
        as.character(max(grade_level))
      )
    ),
    .groups = "drop"
  ) %>%
  group_by(refid) %>%
  summarise(grade_level_range = paste(range, collapse = "; "), .groups = "drop") %>%
  right_join(td_study_tbl, by = "refid") %>%
  mutate(grade_level = if_else(is.na(grade_level_range), "Not Reported", grade_level_range)) %>%
  select(-grade_level_range) %>%
  mutate(refid = as.character(refid))

# Merge with linked references and select variables
a3_info <- td_gl %>% 
  left_join(linked_td) %>%
  mutate(all_reports = ifelse(is.na(all_reports), citation_cleaned, all_reports)) %>% 
  dplyr::select(study_author_year, title, pub_type, data_years, community, school_level, grade_level, race_ethnicity,
                state, fifth_day_activities, study_design, outcome_domain_studied, equity_domain_studied, evidence_domain,
                stakeholder_groups, all_reports) %>% 
  arrange(study_author_year)


# Build the final wide‐to‐long for Appendix 3
a3_dflist <- map(1:nrow(a3_info), ~ a3_info[.x, ])

df_list_long <- map(a3_dflist, ~ .x %>% 
                      pivot_longer(cols = everything(), 
                                   names_to = "variable", 
                                   values_to = "value") %>% 
                      mutate(variable = case_when(variable == "study_author_year" ~ "Study",
                                                  variable == "title" ~ "Title",
                                                  variable == "pub_type" ~ "Publication Type",
                                                  variable == "data_years" ~ "Data Years",
                                                  variable == "community" ~ "Community Type (Rurality)",
                                                  variable == "school_level" ~ "Education Level of Schools",
                                                  variable == "grade_level" ~ "Grade Level",
                                                  variable == "race_ethnicity" ~ "Student Race/Ethnicity",
                                                  variable == "state" ~ "State(s)",
                                                  variable == "fifth_day_activities" ~ "Fifth Day Activity",
                                                  variable == "study_design" ~ "Study Design", 
                                                  variable == "outcome_domain_studied" ~ "Outcome Domain Studied",
                                                  variable == "equity_domain_studied" ~ "Equity Domain Studied",
                                                  variable == "evidence_domain" ~ "Evidence Domain Studied",
                                                  variable == "stakeholder_groups" ~ "<span style='font-size:15px;'>Stakeholders</span><br><span style='font-size:12px;'>(if acceptability studied)</span>",
                                                  variable == "all_reports" ~ "Citation(s)",
                                                  TRUE ~ variable)))

combined_a3 <- bind_rows(df_list_long, .id = "source") %>%
  mutate(source = as.integer(source))

