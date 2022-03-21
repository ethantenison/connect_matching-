library(rmarkdown)
library(readr)
library(tidyverse)
library(janitor)
library(qualtRics)  #For connecting to Qualtrics API
library(Hmisc)      #Helpful for annotations and labels
library(ggmap)

#---------------------------- Student Profiles --------------------------------# 


stu <- read_csv("data/processed/hobby_students_spring_2022.csv")
 

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
    please_select_each_of_the_following_that_applies_to_you =
      case_when(
        please_select_each_of_the_following_that_applies_to_you == "I am pursuing a Master's degree." ~ "MASTERS CANDIDATE",
        please_select_each_of_the_following_that_applies_to_you == "I am pursuing a doctoral degree." ~ "DOCTORAL CANDIDATE",
        please_select_each_of_the_following_that_applies_to_you == "I am pursuing a Master's degree.,I have previously participated in the CONNECT Program." ~ "MASTERS CANDIDATE",
        please_select_each_of_the_following_that_applies_to_you == "I am pursuing a doctoral degree.,I have previously participated in the CONNECT Program." ~ "DOCTORAL CANDIDATE",
        please_select_each_of_the_following_that_applies_to_you == "I am pursuing a Master's degree.,I am pursuing a doctoral degree." ~ "DOCTORAL CANDIDATE",
        please_select_each_of_the_following_that_applies_to_you == "I am pursuing a Master's degree.,I am in the Nonprofit Portfolio Studies Program.,I have previously participated in the CONNECT Program." ~ "MASTERS CANDIDATE, NONPROFIT STUDIES PORTFOLIO",
        TRUE ~ ""
      ),
    schools = gsub('Other (please specify)', '', schools),
    schools_other = replace_na(schools_other,""),
    target_population = replace_na(target_population,""),
    relevant_skills = replace_na(relevant_skills,"None listed"),
    is_there_any_additional_information_regarding_your_availability_that_we_should_know_about =
      replace_na(is_there_any_additional_information_regarding_your_availability_that_we_should_know_about, "None listed"),
    schools = paste0(schools," ",schools_other),
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
    across(26:50,~gsub("1-Not Experienced", "1", .)),
    across(26:50,~gsub("5-Extremely Experienced", "5", .)),
    across(26:50, as.numeric)
  ) |> 
  rename(degree = please_select_each_of_the_following_that_applies_to_you)
 

slices = unique(stu2$name)[!is.na(unique(stu2$name))]

slice = slices[15:19]

for(v in slices){
  render("rmarkdown/student_summaries_hobby.Rmd",
         output_file=paste0("C:/Users/tenis/Desktop/Data_Projects/connect_matching/reports/student_profiles/hobby/spring_2022/", v, ".pdf")
  )
}
