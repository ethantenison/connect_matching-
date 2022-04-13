library(rmarkdown)
library(readr)
library(tidyverse)
library(janitor)
library(qualtRics)  #For connecting to Qualtrics API
library(Hmisc)      #Helpful for annotations and labels
library(tidygeocoder)

#------------------- Getting Data from Qualtrics ------------------------------#
qualtrics_api_credentials(
  api_key = Sys.getenv("qualtrics_key"),
  base_url = "ca1.qualtrics.com",
  install = FALSE
)

surveys <- all_surveys()

stu_number <- which(surveys$name=="10.CONNECT- Summer 2022", arr.ind=TRUE)
org_number <- which(surveys$name=="Matching Form_Summer 2022", arr.ind=TRUE)

#Fetching the Survey Contents 
org_raw <- fetch_survey(surveyID = surveys$id[org_number], force_request = TRUE)
stu_raw <- fetch_survey(surveyID = surveys$id[stu_number], force_request = TRUE)


#---------------------------- Project Summaries ------------------------------#

#Reading additional org data. This section will be changed next semester when
#org form is improved

org <- org_raw
colnames(org) <- label(org)
org <- org |>
  clean_names() |> 
  rename_with(~ gsub('please_rate_how_relevant_the_following_technical_skills_are_to_your_project_', '', .x)) |> 
  rename_with(~ gsub('please_rate_how_relevant_the_following_evaluation_consulting_and_language_skills_are_to_your_project_', '', .x)) |> 
  rename(nonprofit_experience = "how_much_experience_working_or_volunteering_with_nonprofits_should_your_candidate_have_working_or_volunteering_for_nonprofits",
         time_commitment = realistically_how_much_time_do_you_expect_your_student_to_commit_per_week_working_on_your_assigned_project,
         transportation = does_your_candidate_need_to_have_access_to_transportation,
         flexible_hours = will_your_project_permit_flexible_work_hours,
         remote = will_your_candidate_be_able_to_work_remotely) |> 
  rename("spanish" = "please_indicate_how_relevant_the_proficiency_with_the_following_languages_is_to_your_project_note_1_language_proficiency_not_required_and_5_native_speaker_required_spanish") |> 
  mutate(transportation = case_when(
          transportation == "Yes" ~ "Transporation Required, ",
          TRUE ~ ""
        ),
        flexible_hours = case_when(
          flexible_hours == "Yes" ~ "Flexible Hours, ",
          TRUE ~ "Fixed Hours, "
        ),
        remote = case_when(
          remote == "Yes" ~ "Remote",
          TRUE ~ "Must work from site"
        ),
        work_environment = paste0(transportation, 
                                   flexible_hours,
                                   remote),
          project_deliverables = gsub('(\\s\\d)\\)', '\n\n \\1\\)', project_deliverables),
         organization_address = case_when(
           organization_address == "N/A" ~ "",
           TRUE ~ organization_address
         )) |> 
  mutate(across(45:69,~gsub("1 - Not relevant", "1", .)),
         across(45:69,~gsub("5 - Extremely relevant", "5", .)),
         across(45:69, as.numeric))


#write.csv(org, "data/processed/werk.csv", row.names = FALSE)
library(readr)
org2 <- read_csv("data/processed/werk.csv")

#Getting the slices from project name so that organization with duplicates aren't deleted 
slices = unique(org2$project_name)


#Make sure the object is called org2 or whatever you chang it to 
for(v in slices){
  render("rmarkdown/project_summaries.Rmd",
         output_file=paste0("C:/Users/tenis/Desktop/Data_Projects/connect_matching/reports/rgk_project_summaries/Summer 2022/", v, ".pdf")
         )
}


#---------------------------- Student Profiles --------------------------------# 


stu <- stu_raw
colnames(stu) <- label(stu)

stu <- stu |>
  clean_names() |>
  filter(progress == 100) |> 
  mutate(name = paste0(first_name," ", last_name)) |> 
  select(name, everything(),-c(1:19)) |> 
  rename(pronouns = preferred_pronouns_selected_choice,
         spanish = please_rate_your_proficiency_with_the_following_language_note_1_no_proficiency_and_5_extremely_proficient_native_speaker_spanish,
       phone = phone_number_please_do_not_include_hyphens_or_parentheses,
       relevant_skills = do_you_have_other_relevant_skills_or_previous_experience_that_may_be_helpful_for_us_to_know_about_i_e_other_languages_spoken_courses_youvetaken_or_will_be_enrolled_in_this_summer_list_them_here_note_separate_each_skill_with_a_comma,
       target_population = every_semester_the_connect_program_works_with_organizations_that_serve_many_different_target_populations_are_there_any_specific_populations_that_youre_interested_in_working_with,
       nonprofit_experience = over_the_past_5_years_approximately_how_much_experience_have_you_had_working_or_directly_volunteering_with_nonprofit_organizations,
       international = to_comply_with_university_rules_and_regulations_please_indicate_if_you_are_an_international_student_please_note_that_this_does_not_prohibit_participation_in_this_program) |> 
  rename_with(~ gsub('please_rate_your_experience_with_the_following_technical_skills_note_1_not_experienced_and_5_extremely_experienced_', '', .x)) |> 
  rename_with(~ gsub('please_rate_your_experience_with_the_following_consulting_and_evaluation_skills_note_1_not_experienced_and_5_extremely_experienced_', '', .x)) |>
  rename_with(~ gsub('which_of_the_following_best_describes_your_race_ethnicity_select_all_that_apply_selected_choice_', '', .x) ) 

#Fixing responses for profile 
stu$linked_in_preferred_but_not_required[is.na(stu$linked_in_preferred_but_not_required)] <- "UNAVAILABLE"
stu$do_you_have_access_to_transportation[stu$do_you_have_access_to_transportation == "No"] <- "DOES NOT HAVE ACCESS TO TRANSPORTATION"
stu$do_you_have_access_to_transportation[stu$do_you_have_access_to_transportation == "Yes"] <- "HAS ACCESS TO TRANSPORTATION"
stu$do_you_need_flexible_work_hours[stu$do_you_need_flexible_work_hours == "No"] <- ""
stu$do_you_need_flexible_work_hours[stu$do_you_need_flexible_work_hours == "Yes"] <- "REQUIRES FLEXIBLE WORK HOURS"
stu$do_you_need_the_ability_to_work_remotely[stu$do_you_need_the_ability_to_work_remotely == "No"] <- "CAN WORK IN PERSON OR REMOTELY"
stu$do_you_need_the_ability_to_work_remotely[stu$do_you_need_the_ability_to_work_remotely == "Yes"] <- "REQUIRES ABILITY TO WORK REMOTELY"

#Redsigning how to previous fellows, masters, nonprofit studies, and doctoral candidates appear
previous_connect <- grepl("I have previously participated in the CONNECT Program.",
                          stu$please_select_each_of_the_following_that_applies_to_you_i_have_previously_participated_in_the_connect_program)
stu$previous_connect <- previous_connect
stu$connect <- ""

for (i in 1:length(stu$previous_connect)){
  if (stu$previous_connect[i] == TRUE){
    stu$connect[i] <- "PEVIOUS CONNECT FELLOW"
  }
  else if (stu$previous_connect[i] == FALSE) {
    stu$connect[i] <- "NEW TO CONNECT"
  }
}

stu <- stu |>
  mutate(
    please_select_each_of_the_following_that_applies_to_you_i_am_pursuing_a_masters_degree =
      case_when(
        please_select_each_of_the_following_that_applies_to_you_i_am_pursuing_a_masters_degree == "I am pursuing a Master's degree." ~ "MASTERS CANDIDATE",
        TRUE ~ ""
      ),
    please_select_each_of_the_following_that_applies_to_you_i_am_pursuing_a_doctoral_degree =
      case_when(
        please_select_each_of_the_following_that_applies_to_you_i_am_pursuing_a_doctoral_degree == "I am pursuing a doctoral degree." ~ "DOCTORAL CANDIDATE",
        TRUE ~ ""
      ),
    is_there_any_additional_information_regarding_your_availability_that_we_should_know_about = case_when(
      is_there_any_additional_information_regarding_your_availability_that_we_should_know_about == "" ~ "NONE",
      TRUE ~ is_there_any_additional_information_regarding_your_availability_that_we_should_know_about
    )
  ) |> 
  mutate(degree = paste0(please_select_each_of_the_following_that_applies_to_you_i_am_pursuing_a_masters_degree,
                         please_select_each_of_the_following_that_applies_to_you_i_am_pursuing_a_doctoral_degree)) |> 
  mutate(degree = case_when(
    degree == "MASTERS CANDIDATEDOCTORAL CANDIDATE" ~ "DOCTORAL CANDIDATE",
    TRUE ~ degree
  ),
  across(everything(), as.character),
  across(everything(), ~replace_na(.x, ""))
  ) |> 
  unite(col = "school",
        which_ut_schools_colleges_are_you_affiliated_with_check_all_that_apply_selected_choice_lbj_school_of_public_affairs:which_ut_schools_colleges_are_you_affiliated_with_check_all_that_apply_other_text) |> 
  unite(col = "interests",
        identify_each_of_the_project_categories_you_are_interested_in_check_all_that_apply_research:identify_each_of_the_project_categories_you_are_interested_in_check_all_that_apply_website_design)

#Uniting columns   
stu$school <- gsub("_","",stu$school)
stu$school <- gsub("Other:", "", stu$school)
stu$school <- gsub("([a-z])([A-Z])","\\1 & \\2",stu$school)
stu$school <- gsub("Mc & Combs","McCombs",stu$school)

stu$interests <- gsub("_","",stu$interests)
stu$interests <- gsub("([a-z])([A-Z])","\\1, \\2",stu$interests)

stu$target_population[stu$target_population == ""] <- "Open to all"
stu$target_population[stu$target_population == "no"] <- "Open to all"
stu$relevant_skills[stu$relevant_skills == ""] <- "Additional not listed"
stu$is_there_any_additional_information_regarding_your_availability_that_we_should_know_about[stu$is_there_any_additional_information_regarding_your_availability_that_we_should_know_about == ""] <- "None listed"


slices = unique(stu$name)[!is.na(unique(stu$name))]

#The next line just for testing a few students 
slices2 <- slices[1:3]

for(v in slices){
  render("rmarkdown/student_summaries.Rmd",
         output_file=paste0("C:/Users/tenis/Desktop/Data_Projects/connect_matching/reports/student_profiles/rgk/summer_2022/", v, ".pdf")
  )
}
