library(rmarkdown)
library(readr)
library(tidyverse)
library(janitor)
library(qualtRics)  #For connecting to Qualtrics API
library(Hmisc)      #Helpful for annotations and labels
library(ggmap)



#---------------------------- Project Summaries ------------------------------#

#Reading additional org data. This section will be changed next semester when
#org form is improved

org <- read_csv("data/processed/hobby_spring_2022.csv")

org <- org |>
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
  what_other_relevant_skills_would_be_helpful_for_your_candidate_to_have_i_e_other_languages_spoken_coding_analytical_software_professional_skills_etc_list_them_here =
    replace_na(what_other_relevant_skills_would_be_helpful_for_your_candidate_to_have_i_e_other_languages_spoken_coding_analytical_software_professional_skills_etc_list_them_here, "None")

  ) |> 
  mutate(work_environment = paste0(transportation, 
                                   flexible_hours,
                                   remote),
         project_deliverables = gsub('(\\s\\d)\\)', '\n\n \\1\\)', project_deliverables),
         organization_address = case_when(
           organization_address == "N/A" ~ "",
           TRUE ~ organization_address
         )) |> 
  mutate(across(23:47,~gsub("1 - Not relevant", "1", .)),
         across(23:47,~gsub("5 - Extremely relevant", "5", .)),
         across(23:47, as.numeric))



register_google(Sys.getenv("google_geocode_key"))
org <- mutate_geocode(org, organization_address)




#Getting the slices from project name so that organization with duplicates aren't deleted 
slices = unique(org$project_name)[!is.na(unique(org$project_name))]

slice = slices[3]


for(v in slices){
  render("rmarkdown/project_summaries.Rmd",
         output_file=paste0("C:/Users/tenis/Desktop/Data_Projects/connect_matching/reports/hobby_project_summaries/Spring 2022/", v, ".pdf")
         )
}


#---------------------------- Student Profiles --------------------------------# 


stu <- stu_raw
colnames(stu) <- label(stu)

stu <- stu |>
  clean_names() |>
  filter(progress == 100) |> 
  mutate(name = paste0(first_name," ", last_name)) |> 
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

stu$interests <- gsub("_","",stu$interests)
stu$interests <- gsub("([a-z])([A-Z])","\\1, \\2",stu$interests)

stu$every_semester_the_connect_program_works_with_organizations_that_serve_many_different_target_populations_are_there_any_specific_populations_that_youre_interested_in_working_with[stu$every_semester_the_connect_program_works_with_organizations_that_serve_many_different_target_populations_are_there_any_specific_populations_that_youre_interested_in_working_with == ""] <- "Open to all"
stu$every_semester_the_connect_program_works_with_organizations_that_serve_many_different_target_populations_are_there_any_specific_populations_that_youre_interested_in_working_with[stu$every_semester_the_connect_program_works_with_organizations_that_serve_many_different_target_populations_are_there_any_specific_populations_that_youre_interested_in_working_with == "no"] <- "Open to all"



slices = unique(stu$name)[!is.na(unique(stu$name))]

for(v in slices){
  render("rmarkdown/student_summaries.Rmd",
         output_file=paste0("C:/Users/tenis/Desktop/Data_Projects/connect_matching/reports/student_profiles_rgk/", v, ".pdf")
  )
}
