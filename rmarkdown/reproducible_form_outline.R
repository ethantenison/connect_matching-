library(rmarkdown)
library(readr)
library(tidyverse)
library(janitor)
library(qualtRics)  #For connecting to Qualtrics API
library(Hmisc)      #Helpful for annotations and labels

# Connecting to the Qualtrics API
qualtrics_api_credentials(
  api_key = Sys.getenv("qualtrics_key"), 
  base_url = "ca1.qualtrics.com", 
  install = FALSE
)

#Pulling the organization and students surveys 
surveys <- all_surveys() 

#Finding the row index for organization and student surveys
stu_number <- which(surveys$name=="08.CONNECT- Fall 2021", arr.ind=TRUE)
org_number <- which(surveys$name=="Matching Form_Fall 2021", arr.ind=TRUE)

#Fetching the Survey Contents 
org_raw <- fetch_survey(surveyID = surveys$id[org_number], force_request = TRUE)
stu_raw <- fetch_survey(surveyID = surveys$id[stu_number], force_request = TRUE)


#Project Summaries 
#org adendum 
library(readxl)
Fall_2021_Project_Cohort_info <- read_excel("data/raw/Fall 2021 Project Cohort info.xlsx") |>
  clean_names()

org <- org_raw
colnames(org) <- label(org)
org <- org |>
  clean_names() |> 
  filter(which_connect_program_is_this_project_affiliated_with == "RGK Center") |> 
  select(-c(1:17)) |> 
  left_join(Fall_2021_Project_Cohort_info, by = c("project_name" = "org_project")) |> 
  mutate(does_your_candidate_need_to_have_access_to_transportation = case_when(
    does_your_candidate_need_to_have_access_to_transportation == "Yes" ~ "Transporation Required, ",
    TRUE ~ ""
  ),
  will_your_project_permit_flexible_work_hours = case_when(
    will_your_project_permit_flexible_work_hours == "Yes" ~ "Flexible Hours, ",
    TRUE ~ "Fixed Hours, "
  ),
  will_your_candidate_be_able_to_work_remotely = case_when(
    will_your_candidate_be_able_to_work_remotely == "Yes" ~ "Remote",
    TRUE ~ "Must work from site"
  ),
  what_other_relevant_skills_would_be_helpful_for_your_candidate_to_have_i_e_other_languages_spoken_coding_analytical_software_professional_skills_etc_list_them_here =
    replace_na(what_other_relevant_skills_would_be_helpful_for_your_candidate_to_have_i_e_other_languages_spoken_coding_analytical_software_professional_skills_etc_list_them_here, "")

  ) |> 
  mutate(work_environment = paste0(does_your_candidate_need_to_have_access_to_transportation, 
                                   will_your_project_permit_flexible_work_hours,
                                   will_your_candidate_be_able_to_work_remotely))

#Getting the slices from project name so that organization with duplicates aren't deleted 
slices = unique(org$project_name)[!is.na(unique(org$project_name))]


for(v in slices){
  render("rmarkdown/project_summaries.Rmd",
         output_file=paste0("C:/Users/tenis/Desktop/Data_Projects/connect_matching/reports/rgk_project_summaries/", v, ".pdf")
         )
}


#Student Profiles 


raw_students <- read_csv("data/raw/student_summaries_summer2021.csv")

raw_students <- raw_students %>% janitor::row_to_names(row_number = 1) %>% clean_names() %>% select(-c(1:17)) 

raw_students <- raw_students[-1,]

raw_students <- raw_students %>% mutate(name = paste0(first_name," ", last_name))

raw_students$linked_in_preferred_but_not_required[is.na(raw_students$linked_in_preferred_but_not_required)] <- "UNAVAILABLE"

raw_students$do_you_have_access_to_transportation[raw_students$do_you_have_access_to_transportation == "No"] <- "DOES NOT HAVE ACCESS TO TRANSPORTATION"
raw_students$do_you_have_access_to_transportation[raw_students$do_you_have_access_to_transportation == "Yes"] <- "HAS ACCESS TO TRANSPORTATION"
raw_students$do_you_need_flexible_work_hours[raw_students$do_you_need_flexible_work_hours == "No"] <- ""
raw_students$do_you_need_flexible_work_hours[raw_students$do_you_need_flexible_work_hours == "Yes"] <- "REQUIRES FLEXIBLE WORK HOURS"
raw_students$do_you_need_the_ability_to_work_remotely[raw_students$do_you_need_the_ability_to_work_remotely == "No"] <- "CAN WORK IN PERSON OR REMOTELY"
raw_students$do_you_need_the_ability_to_work_remotely[raw_students$do_you_need_the_ability_to_work_remotely == "Yes"] <- "REQUIRES ABILITY TO WORK REMOTELY"


previous_connect <- grepl("previously participated", raw_students$please_select_each_of_the_following_that_applies_to_you)
raw_students$previous_connect <- previous_connect

raw_students$connect <- ""

for (i in 1:length(raw_students$previous_connect)){
  if (raw_students$previous_connect[i] == TRUE){
    raw_students$connect[i] <- "PEVIOUS CONNECT FELLOW"
  }
  else if (raw_students$previous_connect[i] == FALSE) {
    raw_students$connect[i] <- "NEW TO CONNECT"
  }
}

raw_students$please_select_each_of_the_following_that_applies_to_you[raw_students$please_select_each_of_the_following_that_applies_to_you == "I am pursuing a doctoral degree."] <- "DOCTORAL CANDIDATE"

raw_students$please_select_each_of_the_following_that_applies_to_you[raw_students$please_select_each_of_the_following_that_applies_to_you == "I am pursuing a doctoral degree.,I have previously participated in the CONNECT Program."] <- "DOCTORAL CANDIDATE"

raw_students$please_select_each_of_the_following_that_applies_to_you[raw_students$please_select_each_of_the_following_that_applies_to_you == "I am pursuing a Master's degree."] <- "MASTER CANDIDATE"

raw_students$please_select_each_of_the_following_that_applies_to_you[raw_students$please_select_each_of_the_following_that_applies_to_you == "I am pursuing a Master's degree.,I am in the Nonprofit Portfolio Studies Program.,I have previously participated in the CONNECT Program."] <- "MASTER CANDIDATE"

raw_students$please_select_each_of_the_following_that_applies_to_you[raw_students$please_select_each_of_the_following_that_applies_to_you == "I am pursuing a Master's degree.,I have previously participated in the CONNECT Program."] <- "MASTER CANDIDATE"

raw_students$please_select_each_of_the_following_that_applies_to_you[raw_students$please_select_each_of_the_following_that_applies_to_you == "I am pursuing a Master's degree.,I am pursuing a doctoral degree."] <- "DOCTORAL CANDIDATE"

raw_students$please_select_each_of_the_following_that_applies_to_you[raw_students$please_select_each_of_the_following_that_applies_to_you == "I am pursuing a Master's degree.,I am pursuing a doctoral degree.,I am in the Nonprofit Portfolio Studies Program.,I have previously participated in the CONNECT Program."] <- "DOCTORAL CANDIDATE"

raw_students$please_select_each_of_the_following_that_applies_to_you[raw_students$please_select_each_of_the_following_that_applies_to_you == "I am pursuing a Master's degree.,I am in the Nonprofit Portfolio Studies Program."] <- "MASTER CANDIDATE"


#Fixing school information 
for (i in 1:length(
  raw_students$which_ut_schools_colleges_are_you_affiliated_with_check_all_that_apply_selected_choice
)) {
  if (raw_students$which_ut_schools_colleges_are_you_affiliated_with_check_all_that_apply_selected_choice[i] == "Other:") {
    raw_students$which_ut_schools_colleges_are_you_affiliated_with_check_all_that_apply_selected_choice[i] <-
      raw_students$which_ut_schools_colleges_are_you_affiliated_with_check_all_that_apply_other_text[i]
  }
}

raw_students$why_are_you_interested_in_working_on_a_project_check_all_that_apply <- toupper(
  raw_students$why_are_you_interested_in_working_on_a_project_check_all_that_apply)

raw_students$which_ut_schools_colleges_are_you_affiliated_with_check_all_that_apply_selected_choice <- toupper(
  raw_students$which_ut_schools_colleges_are_you_affiliated_with_check_all_that_apply_selected_choice)

slices = unique(raw_students$name)[!is.na(unique(raw_students$name))]


for(v in slices){
  render("notebooks/summer2021_student_summaries.Rmd",
         output_file=paste0("C:/Users/tenis/Desktop/Data_Projects/CONNECT_Eval/reports/student_summaries/summer2021/", v, ".pdf")#,
         #params=list(new_title=paste(v))
  )
}
