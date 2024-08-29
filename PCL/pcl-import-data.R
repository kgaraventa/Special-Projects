# Load Packages -----------------------------------------------------------

library(tidyverse)
library(janitor)
library(googlesheets4)

# Authentication ----------------------------------------------------------

gs4_auth(Sys.getenv("GOOGLE_SHEETS_EMAIL"))

# Create Data Folder ------------------------------------------------------

if (!(file.exists("PCL/data"))) {
  dir.create("PCL/data")
}

# Import File -------------------------------------------------------------

pcl_staff_data <- read_sheet("https://docs.google.com/spreadsheets/d/1RTGtuK2MOd25weO1LRCZ88t-X-N-Vf6eF8jOyxZApds/edit?gid=0#gid=0",
                               sheet = "data") %>% 
  clean_names()

# Program Areas -----------------------------------------------------------

program_areas <- pcl_staff_data %>% 
  select(identifier:program_area) %>% 
  separate_longer_delim(program_area,
                        delim = ", ")

write_rds(program_areas, "PCL/data/program_areas.rds")

# Topic Priority ----------------------------------------------------------

priority <- pcl_staff_data %>% 
  select(identifier, fiscal_year, program_area, need_edcm:need_dv_ipv) %>% 
  pivot_longer(cols = starts_with("need"),
               names_to = "topic",
               values_to = "priority") %>% 
  mutate(topic = str_remove(topic, "need_")) %>% 
  mutate(topic = case_match(topic,
                            "edcm" ~ "Evaluation/Data Collecting and/or Management",
                            "dei" ~ "Diversity, Equity, and Inclusion",
                            "team_staff" ~ "Team Building/Staff Engagement",
                            "frontline" ~ "Frontline Worker Well-being",
                            "mand_report" ~ "Mandatory Reporting",
                            "tic" ~ "Trauma-Informed Care",
                            "sel" ~ "Social Emotional Learning",
                            "mi" ~ "Motivational Interviewing",
                            "behavior" ~ "Behavior Management",
                            "conflict_deesc" ~ "Conflict Management/De-Escalation",
                            "engagement" ~ "Engagement Strategies",
                            "parent_ed" ~ "Parent Education",
                            "gov_services" ~ "Governmental Services",
                            "cw" ~ "Child Welfare",
                            "infant_child_mh" ~ "Infant/Child Mental Health",
                            "youth_teen_mh" ~ "Youth/Teen Mental Health",
                            "child_dev" ~ "Child Development",
                            "adult_dev" ~ "Adult Development",
                            "su" ~ "Substance Abuse",
                            "dv_ipv" ~ "Domestic Violence/Intimate Partner Violence")) %>% 
  filter(priority != 1) %>% 
  mutate(priority = case_match(priority,
                               2 ~ "Low",
                               3 ~ "Medium",
                               4 ~ "High"),
         priority = fct_relevel(priority,
                                "Low",
                                "Medium",
                                "High"))

write_rds(priority, "PCL/data/priority.rds")

priority_count <- priority %>% 
  count(fiscal_year, topic, priority) %>% 
  group_by(fiscal_year, topic) %>% 
  ungroup()

write_rds(priority_count, "PCL/data/priority_count.rds")

# Priority - Other --------------------------------------------------------

priority_other_rating <- pcl_staff_data %>% 
  select(identifier, fiscal_year, program_area, need_other1, need_other2, need_other3) %>% 
  pivot_longer(cols = starts_with("need"),
               names_to = "topic",
               values_to = "priority") %>% 
  mutate(topic = str_remove(topic, "need_")) %>% 
  mutate(priority = case_match(priority,
                               3 ~ "Medium",
                               4 ~ "High"),
         priority = fct_relevel(priority,
                                "Medium",
                                "High"))

priority_other_topic <- pcl_staff_data %>% 
  select(identifier, fiscal_year, program_area, ends_with("response")) %>% 
  pivot_longer(cols = starts_with("need"),
               names_to = "topic",
               values_to = "topic_name") %>% 
  mutate(topic = str_remove(topic, "need_")) %>% 
  mutate(topic = str_remove(topic, "_response")) 

priority_other <-
  left_join(priority_other_rating,
            priority_other_topic,
            join_by(identifier, topic, fiscal_year, program_area)) %>% 
  mutate(topic = case_match(topic,
                            "other1" ~ "Other 1",
                            "other2" ~ "Other 2",
                            "other3" ~ "Other 3"))

write_rds(priority_other, "PCL/data/priority_other.rds")

# Topic Specific Info -----------------------------------------------------

topic_specific_know_skill <- pcl_staff_data %>% 
  select(identifier, fiscal_year, program_area, starts_with("know_skill")) %>%
  select(-contains("other")) %>% 
  pivot_longer(cols = starts_with("know_skill"),
               names_to = "question",
               values_to = "response") %>% 
  separate_wider_delim(question,
                       delim = "_skill_",
                       names = c("question", "topic")) %>% 
  mutate(question = case_match(question,
                               "know" ~ "Knowledge/Skill Level Needed")) %>% 
  mutate(topic = case_match(topic,
                            "edcm" ~ "Evaluation/Data Collecting and/or Management",
                            "dei" ~ "Diversity, Equity, and Inclusion",
                            "team_staff" ~ "Team Building/Staff Engagement",
                            "frontline" ~ "Frontline Worker Well-being",
                            "mand_report" ~ "Mandatory Reporting",
                            "tic" ~ "Trauma-Informed Care",
                            "sel" ~ "Social Emotional Learning",
                            "mi" ~ "Motivational Interviewing",
                            "behavior" ~ "Behavior Management",
                            "conflict_deesc" ~ "Conflict Management/De-Escalation",
                            "engagement" ~ "Engagement Strategies",
                            "parent_ed" ~ "Parent Education",
                            "gov_services" ~ "Governmental Services",
                            "cw" ~ "Child Welfare",
                            "infant_child_mh" ~ "Infant/Child Mental Health",
                            "youth_teen_mh" ~ "Youth/Teen Mental Health",
                            "child_dev" ~ "Child Development",
                            "adult_dev" ~ "Adult Development",
                            "su" ~ "Substance Abuse",
                            "dv_ipv" ~ "Domestic Violence/Intimate Partner Violence")) %>% 
  mutate(response = case_match(response,
                               1 ~ "Beginner",
                               2 ~ "Intermediate",
                               3 ~ "Advanced"))

topic_specific_area <- pcl_staff_data %>% 
  select(identifier, fiscal_year, program_area, starts_with("area")) %>%
  select(-contains("other")) %>% 
  pivot_longer(cols = starts_with("area"),
               names_to = "question",
               values_to = "response") %>% 
  separate_wider_delim(question,
                       delim = "rea_",
                       names = c("question", "topic")) %>% 
  mutate(question = case_match(question,
                               "a" ~ "Specific Area")) %>% 
  mutate(topic = case_match(topic,
                            "edcm" ~ "Evaluation/Data Collecting and/or Management",
                            "dei" ~ "Diversity, Equity, and Inclusion",
                            "team_staff" ~ "Team Building/Staff Engagement",
                            "frontline" ~ "Frontline Worker Well-being",
                            "mand_report" ~ "Mandatory Reporting",
                            "tic" ~ "Trauma-Informed Care",
                            "sel" ~ "Social Emotional Learning",
                            "mi" ~ "Motivational Interviewing",
                            "behavior" ~ "Behavior Management",
                            "conflict_deesc" ~ "Conflict Management/De-Escalation",
                            "engagement" ~ "Engagement Strategies",
                            "parent_ed" ~ "Parent Education",
                            "gov_services" ~ "Governmental Services",
                            "cw" ~ "Child Welfare",
                            "infant_child_mh" ~ "Infant/Child Mental Health",
                            "youth_teen_mh" ~ "Youth/Teen Mental Health",
                            "child_dev" ~ "Child Development",
                            "adult_dev" ~ "Adult Development",
                            "su" ~ "Substance Abuse",
                            "dv_ipv" ~ "Domestic Violence/Intimate Partner Violence"))

topic_specific_trainer <- pcl_staff_data %>% 
  select(identifier, fiscal_year, program_area, starts_with("trainer")) %>%
  select(-contains("other")) %>% 
  pivot_longer(cols = starts_with("trainer"),
               names_to = "question",
               values_to = "response") %>% 
  separate_wider_delim(question,
                       delim = "ner_",
                       names = c("question", "topic")) %>% 
  mutate(question = case_match(question,
                               "trai" ~ "Trainer Information")) %>% 
  mutate(topic = case_match(topic,
                            "edcm" ~ "Evaluation/Data Collecting and/or Management",
                            "dei" ~ "Diversity, Equity, and Inclusion",
                            "team_staff" ~ "Team Building/Staff Engagement",
                            "frontline" ~ "Frontline Worker Well-being",
                            "mand_report" ~ "Mandatory Reporting",
                            "tic" ~ "Trauma-Informed Care",
                            "sel" ~ "Social Emotional Learning",
                            "mi" ~ "Motivational Interviewing",
                            "behavior" ~ "Behavior Management",
                            "conflict_deesc" ~ "Conflict Management/De-Escalation",
                            "engagement" ~ "Engagement Strategies",
                            "parent_ed" ~ "Parent Education",
                            "gov_services" ~ "Governmental Services",
                            "cw" ~ "Child Welfare",
                            "infant_child_mh" ~ "Infant/Child Mental Health",
                            "youth_teen_mh" ~ "Youth/Teen Mental Health",
                            "child_dev" ~ "Child Development",
                            "adult_dev" ~ "Adult Development",
                            "su" ~ "Substance Abuse",
                            "dv_ipv" ~ "Domestic Violence/Intimate Partner Violence"))

topic_specific <-
  bind_rows(topic_specific_know_skill,
            topic_specific_area,
            topic_specific_trainer) %>% 
  relocate(topic, .after = program_area)

write_rds(topic_specific, "PCL/data/topic_specific.rds")

# Align Topic & Specific --------------------------------------------------

topic_alignment <-
  left_join(priority,
            topic_specific_know_skill,
            join_by(identifier, topic, fiscal_year, program_area)) %>% 
  left_join(topic_specific_area,
            join_by(identifier, topic, fiscal_year, program_area)) %>% 
  left_join(topic_specific_trainer,
            join_by(identifier, topic, fiscal_year, program_area)) %>% 
  select(-starts_with("question")) %>% 
  rename(level = response.x,
         subtopic = response.y,
         trainer_info = response)

write_rds(topic_alignment, "PCL/data/topic_alignment.rds")

# Topic Specific "Other" Info ---------------------------------------------

topic_specific_other_know_skill <- pcl_staff_data %>% 
  select(identifier, fiscal_year, program_area, starts_with("know_skill")) %>%
  select(identifier, fiscal_year, program_area, contains("other")) %>% 
  pivot_longer(cols = starts_with("know_skill"),
               names_to = "question",
               values_to = "response") %>% 
  separate_wider_delim(question,
                       delim = "_skill_",
                       names = c("question", "topic")) %>% 
  mutate(question = case_match(question,
                               "know" ~ "Knowledge/Skill Level Needed")) %>% 
  mutate(topic = case_match(topic,
                            "other1" ~ "Other 1",
                            "other2" ~ "Other 2",
                            "other3" ~ "Other 3")) %>% 
  mutate(response = case_match(response,
                               1 ~ "Beginner",
                               2 ~ "Intermediate",
                               3 ~ "Advanced"))

topic_specific_other_area <- pcl_staff_data %>% 
  select(identifier, fiscal_year, program_area, starts_with("area")) %>%
  select(identifier, fiscal_year, program_area, contains("other")) %>% 
  pivot_longer(cols = starts_with("area"),
               names_to = "question",
               values_to = "response") %>% 
  separate_wider_delim(question,
                       delim = "rea_",
                       names = c("question", "topic")) %>% 
  mutate(question = case_match(question,
                               "a" ~ "Specific Area")) %>% 
  mutate(topic = case_match(topic,
                            "other1" ~ "Other 1",
                            "other2" ~ "Other 2",
                            "other3" ~ "Other 3"))

topic_specific_other_trainer <- pcl_staff_data %>% 
  select(identifier, fiscal_year, program_area, starts_with("trainer")) %>%
  select(identifier, fiscal_year, program_area, contains("other")) %>% 
  pivot_longer(cols = starts_with("trainer"),
               names_to = "question",
               values_to = "response") %>% 
  separate_wider_delim(question,
                       delim = "ner_",
                       names = c("question", "topic")) %>% 
  mutate(question = case_match(question,
                               "trai" ~ "Trainer Information")) %>% 
  mutate(topic = case_match(topic,
                            "other1" ~ "Other 1",
                            "other2" ~ "Other 2",
                            "other3" ~ "Other 3"))
topic_specific_other <-
  bind_rows(topic_specific_other_know_skill,
            topic_specific_other_area,
            topic_specific_other_trainer) %>% 
  relocate(topic, .after = program_area)

write_rds(topic_specific_other, "PCL/data/topic_specific_other.rds")

# Alignment Topic & Specific for Other ------------------------------------

topic_alignment_other <-
  left_join(priority_other,
            topic_specific_other_know_skill,
            join_by(identifier, topic, fiscal_year, program_area)) %>% 
  left_join(topic_specific_other_area,
            join_by(identifier, topic, fiscal_year, program_area)) %>% 
  left_join(topic_specific_other_trainer,
            join_by(identifier, topic, fiscal_year, program_area)) %>% 
  select(-starts_with("question")) %>% 
  rename(level = response.x,
         subtopic = response.y,
         trainer_info = response)

write_rds(topic_alignment_other, "PCL/data/topic_alignment_other.rds")
