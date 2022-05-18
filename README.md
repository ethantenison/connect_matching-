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
