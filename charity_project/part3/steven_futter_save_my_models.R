########################################
## PREDICT 422
## Charity Project - Part 3 (The Mailing List Problem)
##
## based upon SaveYourModels.R
########################################

# note that fit2 and glm.stepwise are my two models for predicting DAMT and DONR, respectively. These
# variables were loaded into memory prior to running the code below. See steven_futter_charity_project_part1.R
# and steven_futter_charity_project_part2.R for the creation of these two models. 

# Save Part 1 Model: 
# Note that for future reference the save command accepts multiple objects. For example, the Lasso 
# model my include cvLasso$lambda.min. 
outPath = file.path("/Users","stevenfutter","Dropbox","NU","MACHINE_LEARNING","charity_project","part3")
modelPart1 = fit2
save(modelPart1,file=file.path(outPath,"modelPart1.RData"))
         
# Save Part 2 Model: 
outPath = file.path("/Users","stevenfutter","Dropbox","NU","MACHINE_LEARNING","charity_project","part3")
modelPart2 = glm.stepwise
modelPart2_prob_threshold = threshA2
save(modelPart2,modelPart2_prob_threshold,file=file.path(outPath,"modelPart2.RData"))

# To load the models from Part 1 and Part 2, we have the following commands:

# Specify the file path where your .RData files are located
modelPath = file.path("/Users","stevenfutter","Dropbox","NU","MACHINE_LEARNING","charity_project","part3")

# Use the load command to load objects from your .RData files.
load(file.path(modelPath,"modelPart1.RData"))
load(file.path(modelPath,"modelPart2.RData"))

# We can now see modelPart1 and modelPart2 (plus any hyper-parameters) in the R environment memory.