library(rmarkdown)
library(readr)
library(tidyverse)
library(janitor)
library(qualtRics)  #For connecting to Qualtrics API
library(Hmisc)      #Helpful for annotations and labels
library(ggmap)

#---------------------------- Student Profiles --------------------------------# 


stu <- read_csv("data/processed/utd_students_spring_2022.csv")
 

#Fixing responses for profile 
stu$linked_in_preferred_but_not_required[is.na(stu$linked_in_preferred_but_not_required)] <- "UNAVAILABLE"
stu$do_you_have_access_to_transportation[stu$do_you_have_access_to_transportation == "No"] <- "DOES NOT HAVE ACCESS TO TRANSPORTATION"
stu$do_you_have_access_to_transportation[stu$do_you_have_access_to_transportation == "Yes"] <- "HAS ACCESS TO TRANSPORTATION"
stu$do_you_need_flexible_work_hours[stu$do_you_need_flexible_work_hours == "No"] <- ""
stu$do_you_need_flexible_work_hours[stu$do_you_need_flexible_work_hours == "Yes"] <- "REQUIRES FLEXIBLE WORK HOURS"
stu$do_you_need_the_ability_to_work_remotely[stu$do_you_need_the_ability_to_work_remotely == "No"] <- "CAN WORK IN PERSON OR REMOTELY"
stu$do_you_need_the_ability_to_work_remotely[stu$do_you_need_the_ability_to_work_remotely == "Yes"] <- "REQUIRES ABILITY TO WORK REMOTELY"

stu2 <- stu |>
  mutate(
    please_select_each_of_the_following_that_applies_to_you_selected_choice_selected_choice =
      case_when(
        please_select_each_of_the_following_that_applies_to_you_selected_choice == "I am pursuing a Master's degree." ~ "MASTERS CANDIDATE",
        please_select_each_of_the_following_that_applies_to_you_selected_choice == "I am pursuing a doctoral degree." ~ "DOCTORAL CANDIDATE",
        please_select_each_of_the_following_that_applies_to_you_selected_choice == "I am pursuing a Master's degree.,I am pursuing a certificate in Nonprofit Management."  ~ "MASTERS CANDIDATE, CERTIFICATE IN NONPROFIT MANAGEMENT",
        please_select_each_of_the_following_that_applies_to_you_selected_choice == "I am pursuing a doctoral degree.,I have previously participated in the CONNECT Program." ~ "DOCTORAL CANDIDATE",
        please_select_each_of_the_following_that_applies_to_you_selected_choice == "I am pursuing a Master's degree.,I am pursuing a doctoral degree." ~ "DOCTORAL CANDIDATE",
        please_select_each_of_the_following_that_applies_to_you_selected_choice == "I am pursuing another certificate. Please indicate which one." ~ "",
        TRUE ~ ""
      ),
    target_population = replace_na(target_population,""),
    relevant_skills = replace_na(relevant_skills,"None listed"),
    please_select_each_of_the_following_that_applies_to_you_i_am_pursuing_another_certificate_please_indicate_which_one_text =
      replace_na(please_select_each_of_the_following_that_applies_to_you_i_am_pursuing_another_certificate_please_indicate_which_one_text, ""), 
    is_there_any_additional_information_about_your_availability_that_we_should_know =
      replace_na(is_there_any_additional_information_about_your_availability_that_we_should_know, "None listed"),
    across(1:29, as.character),
    across(1:29, ~ replace_na(.x, "")),
    name = paste0(first_name," ", last_name),
    identify_each_of_the_project_categories_you_are_interested_in_check_all_that_apply = 
      gsub("([a-z])([A-Z])","\\1, \\2",identify_each_of_the_project_categories_you_are_interested_in_check_all_that_apply),
    target_population = case_when(
      target_population == "" ~ "Open to all",
      target_population == "no" ~ "Open to all",
      TRUE ~ target_population
    ),
    please_select_each_of_the_following_that_applies_to_you_selected_choice = paste0(
      please_select_each_of_the_following_that_applies_to_you_selected_choice, "",
      please_select_each_of_the_following_that_applies_to_you_i_am_pursuing_another_certificate_please_indicate_which_one_text)
  ) |> 
  rename(degree = please_select_each_of_the_following_that_applies_to_you_selected_choice) |> 
  mutate(
    across(30:55,~gsub("1 (not experienced)", "1", .)),
    across(30:55,~gsub("5 (extremely experienced)", "5", .)),
    across(30:55, as.numeric)
  )
 

slices = unique(stu2$name)[!is.na(unique(stu2$name))]

slice = slices[1]

for(v in slices){
  render("rmarkdown/student_summaries_utd.Rmd",
         output_file=paste0("C:/Users/tenis/Desktop/Data_Projects/connect_matching/reports/student_profiles/utd/spring_2022/", v, ".pdf")
  )
}
