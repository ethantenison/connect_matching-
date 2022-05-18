# CONNECT Matching Algorithm

This algorithm was developed to streamline the student-nonprofit matching process of the CONNECT Program. Originally developed by CS students at the University of Texas as a stand alone website using a variety of languages, this version uses strictly the R programming language and `Qualtrics`.


### Directory Structure

```
+-- README.md       <- The top-level readme file
+-- data
|    +-- external   <- Data from third party sources
|    +-- interim    <- Intermediate data that has been transformed
|    +-- processed  <- The final datasets 
|    +-- raw        <- Original data if it needs to be saved
|
+-- Scripts_and_Rmarkdown     
|    +-- images                 <- logos for reports and matching
|    +-- matching_RGK.Rmd       <- Used for matching
|    +-- matching_RGK_NLP.Rmd   <- Experimental matching with NLP
|    +-- project_summaries.Rmd  <- project summary template
|    +-- reproducible_script.R  <- script to create automated reports
|    +-- resume_analysis.Rmd    <- Testing NLP
|    +-- student_summaries.Rmd  <- student summary template
|    +-- theme.css              <- automated report styling
|
+-- reports         <- Generated analysis and automated reports
|
+-- models          <- Saved NLP model 
```

All the coding work takes places in the Scripts_and_Rmarkdown folder. For the matching process, use matching_RGK.Rmd. For automated reports you will use three files: reproducible_script.R, project_summaries.Rmd, and student_summaries.Rmd. The first script is used to data prep for both student and project summaries. The last two are both report templates. Once the prep work is done, reproducible_script.R can loop over both of the templates to create the reports. 

**NOTE** The NLP work has been separated into separate files because it is experimental and needs to be improved before fully integrating with the system. 