
#install and load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(janitor, tidyverse, rio, here)

#import data
raw_df <- import(here("data", "4DSW_Data.xlsx")) 


#create dataframe to merge state abbreviations
state_abbrev_df <- data.frame(category = state.name,
                              abbrev = state.abb)

#transform wide variables to categorical variables  
df_long <- raw_df %>%
  select(refid, starts_with("community"), starts_with("state"), starts_with("grade"),
         starts_with("school"), starts_with("effectiveness"), starts_with("equity"), 
         starts_with("evidence"), -effectiveness_approach) %>% 
  pivot_longer(cols = -refid, names_to = "variable", values_to = "response") %>%
  mutate(
    value = sub(".*_", "", variable),
    value = str_to_title(value),
    value = ifelse(value == "Unknown", "Not Reported", value),
    response = str_remove_all(response, "; -999|-999;"),
    response = str_squish(response),
    response = case_when(response == "Yes; No" ~ "Yes",
                         response == "No; Yes" ~ "Yes",
                         TRUE ~ response),
    new_var_name = case_when(str_detect(variable, "^community") ~ "community", 
                             str_detect(variable, "^state") ~ "state",
                             str_detect(variable, "^grade") ~ "grade_level",
                             str_detect(variable, "^school") ~ "school_level",
                             str_detect(variable, "^effectiveness") ~ "effectiveness",
                             str_detect(variable, "^equity") ~ "equity",
                             str_detect(variable, "^evidence") ~ "evidence_domain",
                             TRUE ~ variable),
    category = case_when(str_detect(variable, "^community") & response == "Yes" ~  value, 
                         str_detect(variable, "^state") & response == "Yes" ~  value,
                         str_detect(variable, "^grade") & response == "Yes" ~  value,
                         str_detect(variable, "^school") & response == "Yes" ~  value,
                         str_detect(variable, "^effectiveness") & response == "Yes" ~  value,
                         str_detect(variable, "^equity") & response == "Yes" ~  value,
                         str_detect(variable, "^evidence") & response == "Yes" ~ value,
                         TRUE ~ NA)) %>% 
  mutate(category = case_when(category == "Newmexico" ~ "New Mexico",
                              category == "Southdakota" ~ "South Dakota",
                              category == "Northdakota" ~ "North Dakota",
                              category == "Northcarolina" ~ "North Carolina",
                              category == "Southcarolina" ~ "South Carolina",
                              category == "Newhampshire" ~ "New Hampshire",
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

#list of single variables (to aggregate across id only)
single_cat_vars <- c("author", "publication_year", "publication_type", "publisher", "student_type", "student_discipline",
                     "data_years", "effectiveness_approach")

#subset of single single aggregated variables
single_df <- raw_df %>%
 select(refid, all_of(single_cat_vars))


#import reference info citation links
ref_df <- import(here("data", "4dsw_eligible_citations_links.xlsx")) %>% 
  janitor::clean_names() %>% 
  rename(link = link_to_public_full_text,
         citation = bibliography)

#manually add links 
ref_td <- ref_df %>% 
  mutate(link = ifelse(is.na(link), other_public_link, link))

#import fifth day additional coding
fif_df <- import(here("data", "4dsw_fifthday.xlsx")) %>% 
  janitor::clean_names() 

#transform fifth day data for table
fif_td <- fif_df %>% 
 # mutate(fifth_day_recode_extracuriculars_clubs_sports = ifelse(fifth_day_recode_extracuriculars_clubs_sports == "Extracuriculars (clubs, sports)",
  #                                                              "Extracurriculars (clubs/sports)",
   #                                                             fifth_day_recode_extracuriculars_clubs_sports))  %>% 
  pivot_longer(cols = fifth_day_recode_child_care:fifth_day_recode_not_reported, 
               names_to = "variable", 
               values_to = "response") %>% 
  mutate(response = str_replace_all(response, 
                                    c("Not reported" = "Not Reported"))) %>% 
  group_by(refid) %>%
  summarise(fifth_day_activities = paste(response[!is.na(response)], collapse = "; "), .groups = "drop_last") %>%
  ungroup()

#import author contact and student race/ethnicity additional coding
add_df <- import(here("data", "4dsw_corrauth_studrace.xlsx")) %>% 
  janitor::clean_names()

#transform additional data for table
add_td <- add_df %>% 
  mutate(email = ifelse(grepl("@", corr_author_contact), corr_author_contact, NA))

race_td <- add_df %>% 
  mutate(race_ethnicity_other_please_specify = str_remove_all(race_ethnicity_other_please_specify, "\\s*\\(please specify\\)")) %>% 
  pivot_longer(cols = race_ethnicity_american_indian_and_or_alaska_native:race_ethnicity_other_please_specify, 
               names_to = "variable", 
               values_to = "response") %>% 
  group_by(refid) %>%
  summarise(race_ethnicity = paste(response[!is.na(response)], collapse = "; "), .groups = "drop_last") %>%
  ungroup() %>% 
  mutate(race_ethnicity = ifelse(race_ethnicity == "", "Not Reported", race_ethnicity))

#import author webpage links
auli_df <- import(here("data", "4dsw_author_webpage_links.xlsx")) 

auli_td <- auli_df %>%    
  distinct(corr_author_name, .keep_all = TRUE) %>% 
  rename(author_link = `Web Link`) %>% 
  select(-refid)

#merge data together
jd <- single_df %>% 
  left_join(td_longvar) %>% 
  left_join(ref_td) %>% 
  left_join(fif_td) %>% 
  left_join(add_td) %>% 
  left_join(race_td) %>% 
  left_join(auli_td)

#correct formatting
td <- jd %>% 
  mutate_all(~ ifelse(is.na(.) | .x == -999 | . == "", "Not Reported", .)) %>% 
  mutate_all(~ str_remove_all(as.character(.), "-999; |; -999")) %>% 
  mutate_all(~ str_remove_all(as.character(.), "Not Reported; |; Not Reported")) %>% 
  mutate(equity = str_replace_all(equity, c("Raceethnicity" = "Race/Ethnicity", "Sexgender" = "Sex/Gender", "Ell" = "ELL", 
                                            "Ses" = "SES", "Specialeducation" = "Special Education"))) %>% 
  select(-user, -level)

#export to outputs/data folder to deploy app
rio::export(td, here("outputs", "data_dashboard", "data", "4dsw_app_data.xlsx"))

#SUMMARY STATS

#DOESNT WORK - MANUALLY CREATING TABLES WITHIN SHINY APP
# freq_agg_var <- function(data, variable_name) {
#   data %>%
#     mutate(!!variable_name := str_remove_all(!!variable_name, "; -999|-999; ")) %>%
#     separate_rows(!!variable_name, sep = "; ") %>%
#     count(!!variable_name) %>% 
#     mutate(!!variable_name := ifelse(!!variable_name == "-999", "Not Reported", !!variable_name),
#            percent = paste0(round(n/nrow(data) * 100, 2), "%")) %>% 
#     arrange(desc(!!variable_name != "Not Reported"), desc(n))
# }
# #then kable after with the object name (kable(approach_sum))

#Create table for each variable 


# freq_design <- jd %>% 
#   mutate(effectiveness_approach = str_remove_all(effectiveness_approach, "; -999|-999; ")) %>% 
#   separate_rows(effectiveness_approach, sep = "; ") %>% 
#   count(effectiveness_approach) %>% 
#   mutate(effectiveness_approach = ifelse(effectiveness_approach == "-999", "Not Reported", effectiveness_approach),
#          percent = paste0(round(n/nrow(jd) * 100, 2), "%")) %>% 
#   arrange(desc(effectiveness_approach != "Not Reported"), desc(n)) #%>% 
# #knitr::kable()
# 
# 
# 
# #create list of variables to create count summary table for
# sum_cat_vars <- c("community", "school_level", "grade_level", "`Student Race/Ethnicity`", 
#                   "fifth_day_activities", "study_design", "outcome_domain_studied",
#                   "equity_domain_studied", "publication_type")
# 
# # test <- lapply(sum_cat_vars, function(variable_name) {
# #   freq_agg_var(td, variable_name)
# # })
# 
# 
# 
# testtyy <- agg_df %>%
#   separate_rows(effectiveness_approach, sep = "; ") %>%
#   group_by(effectiveness_approach) %>%
#   summarise(n = sum(effectiveness_approach == "-999"),
#             percent = paste0(round(sum(effectiveness_approach == "-999") / n() * 100, 2), "%"))


