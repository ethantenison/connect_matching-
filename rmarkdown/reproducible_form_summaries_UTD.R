library(rmarkdown)  #For rmarkdown functions 
library(readr)      #Read data
library(tidyverse)  #Data manipulation and visualization package
library(janitor)    #Data cleaning package 
library(qualtRics)  #For connecting to Qualtrics API
library(Hmisc)      #Helpful for annotations and labels

#------------------- Getting Data from Qualtrics ------------------------------#
qualtrics_api_credentials(
  api_key = Sys.getenv("qualtrics_key"), 
  base_url = "ca1.qualtrics.com", 
  install = FALSE
)

surveys <- all_surveys() 

stu_number <- which(surveys$name=="CONNECT - Student Form", arr.ind=TRUE)
org_number <- which(surveys$name=="Matching Form_Fall 2021", arr.ind=TRUE)

org_raw <- fetch_survey(surveyID = surveys$id[org_number], force_request = TRUE)
stu_raw <- fetch_survey(surveyID = surveys$id[stu_number], force_request = TRUE)


#---------------------------- Project Summaries ------------------------------#

#Reading additional org data. This section will be changed next semester when
#org form is improved
library(readxl)
Fall_2021_Project_Cohort_info <-
  read_excel("data/raw/Fall 2021 Project Cohort info.xlsx", sheet = "UTD") |>
  clean_names()


org <- org_raw
colnames(org) <- label(org)
org <- org |>
  clean_names() |>
  filter(which_connect_program_is_this_project_affiliated_with == "UT-Dallas") |>
  select(-c(1:17)) |>
  left_join(Fall_2021_Project_Cohort_info,
            by = c("project_name" = "org_project")) |>
  mutate(
    does_your_candidate_need_to_have_access_to_transportation = case_when(
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
      replace_na(
        what_other_relevant_skills_would_be_helpful_for_your_candidate_to_have_i_e_other_languages_spoken_coding_analytical_software_professional_skills_etc_list_them_here,
        ""
      )
    
  ) |>
  mutate(
    work_environment = paste0(
      does_your_candidate_need_to_have_access_to_transportation,
      will_your_project_permit_flexible_work_hours,
      will_your_candidate_be_able_to_work_remotely
    )
  )

#Special Data manipulation for ICDC because of incorrect data 
org$project_goal[org$project_name == "ICDC"] <-
  "To document the program delivery and performance metrics for ICDC, including both existing data management processes and potential ways forward"
org$project_deliverables[org$project_name == "ICDC"] <-
  "1) Design an organization-level logic model that specifies strategies, activities, outputs, outcomes, impact, assumptions, and causal links
2) Provide a written inventory of the data management tools and information collected at various points throughout the organization
3) Provide recommendations on what data to collect and how to efficiently collect, consolidate, and manage data for the purposes of documenting impact
"
org$realistically_how_much_time_do_you_expect_your_student_to_commit_per_week_working_on_your_assigned_project[org$project_name == "ICDC"] <-
  "5 - 10 hours per week"

#Automated Process
#Each slice is run through the project_summaries.rmd file
slices = unique(org$project_name)[!is.na(unique(org$project_name))]

for (v in slices) {
  render(
    "rmarkdown/project_summaries.Rmd",
    output_file = paste0(
      "C:/Users/tenis/Desktop/Data_Projects/connect_matching/reports/utd_project_summaries/",
      v,
      ".pdf"
    )
  )
}


#---------------------------- Student Profiles --------------------------------# 


stu <- stu_raw
colnames(stu) <- label(stu)

stu <- stu |>
  clean_names() |>
  filter(progress == 100) |> 
  mutate(name = paste0(first_name," ", last_name),
         across(everything(), as.character)) |> 
  select(-c(1:17))

#Fixing responses for profile 
stu$linked_in_preferred_but_not_required[is.na(stu$linked_in_preferred_but_not_required)] <- "UNAVAILABLE"
stu$do_you_have_access_to_transportation[stu$do_you_have_access_to_transportation == "No"] <- "DOES NOT HAVE ACCESS TO TRANSPORTATION"
stu$do_you_have_access_to_transportation[stu$do_you_have_access_to_transportation == "Yes"] <- "HAS ACCESS TO TRANSPORTATION"
stu$do_you_need_flexible_work_hours[stu$do_you_need_flexible_work_hours == "No"] <- ""
stu$do_you_need_flexible_work_hours[stu$do_you_need_flexible_work_hours == "Yes"] <- "REQUIRES FLEXIBLE WORK HOURS"
stu$do_you_need_the_ability_to_work_remotely[stu$do_you_need_the_ability_to_work_remotely == "No"] <- "CAN WORK IN PERSON OR REMOTELY"
stu$do_you_need_the_ability_to_work_remotely[stu$do_you_need_the_ability_to_work_remotely == "Yes"] <- "REQUIRES ABILITY TO WORK REMOTELY"

#Redsigning how to previous fellows, masters, nonprofit studies, and doctoral candidates appear

stu <- stu |>
  mutate(
    please_select_each_of_the_following_that_applies_to_you_selected_choice_i_am_pursuing_a_masters_degree =
      case_when(
        please_select_each_of_the_following_that_applies_to_you_selected_choice_i_am_pursuing_a_masters_degree == "I am pursuing a Master's degree." ~ "MASTERS CANDIDATE",
        TRUE ~ ""
      ),
    please_select_each_of_the_following_that_applies_to_you_selected_choice_i_am_pursuing_a_doctoral_degree =
      case_when(
        please_select_each_of_the_following_that_applies_to_you_selected_choice_i_am_pursuing_a_doctoral_degree == "I am pursuing a doctoral degree." ~ "DOCTORAL CANDIDATE",
        TRUE ~ ""
      ),
  ) |> 
  mutate(degree = paste0(please_select_each_of_the_following_that_applies_to_you_selected_choice_i_am_pursuing_a_masters_degree,
                         please_select_each_of_the_following_that_applies_to_you_selected_choice_i_am_pursuing_a_doctoral_degree)) |> 
  mutate(degree = case_when(
    degree == "MASTERS CANDIDATEDOCTORAL CANDIDATE" ~ "DOCTORAL CANDIDATE",
    TRUE ~ degree
  ),
  across(everything(), as.character),
  across(everything(), ~replace_na(.x, ""))
  )




slices = unique(stu$name)[!is.na(unique(stu$name))]

for(v in slices){
  render("rmarkdown/student_summaries_utd.Rmd",
         output_file=paste0("C:/Users/tenis/Desktop/Data_Projects/connect_matching/reports/student_profiles_utd/", v, ".pdf")
  )
}

