########################################
## PREDICT 422
## Charity Project - Part 3 (The Mailing List Problem)
##
## SaveYourModels.R
########################################

# In order to use your chosen models from Part 1 and Part 2 of the project to predict
# values for the Validation data and Test data for Part 3 of the project, you need a
# means to bring your chosen models into the R Environment. There are several ways to 
# accomplish this (a few possibilities listed here). Note that you may want to 
# alter the model naming convention to distinguish between Part 1 (Regression Problem)
# models and Part 2 (Classification Problem) models.
#   - Run code from Part 1 and Part 2 in the current R session. If you leave the R
#     session running for your Part 3 code, then those models will still be in memory 
#     as your work on Part 3.
#   - Save your chosen models from Part 1 and Part 2 to .RData files. This is a means
#     of saving R objects to a file that is able to be opened again in R.
#
# I'm going with the second approach. 

# Save Part 1 Model: 
#   0. For the sake of discussion, let's use modelC1 from SampleCodePart1.R for this
#      part. While modelC1 wasn't the top model on the Part 1 test data, it was close,
#      and it will be more illustrative for me to use a Lasso model here than a
#      regression model.
#   1. Go to your Part 1 code and execute the commands that you need so that your
#      chosen model is in memory (or you can execute ALL of your code from Part 1).
#      You want the model to be trained on the Part 1 training set, as before, so you
#      don't need to make any changes to your Part 1 code.
#   2. While your chosen Part 1 model is in memory, execute the following commands
#      (modified for your particular situation):
outPath = file.path("/Users","stevenfutter","Dropbox","NU","MACHINE_LEARNING","charity_project","part3")
modelPart1 = fit3
modelPart1_bestlam = bestlam
save(modelPart1,modelPart1_bestlam,file=file.path(outPath,"modelPart1.RData"))

#      - The first command defines the file path where you want to save the file. It
#        can be set the same as your inPath or not. You need to define a valid file
#        path for your file system.
#      - The second command renames my modelC1 to be called modelPart1. I do this only
#        for the reason that my chosen Part 2 model might also be modelC1. Renaming
#        the model distinguishes my Part 1 model from my Part 2 model. Also, note that
#        you would specify your chosen model here in place of "modelC1".
#      - The third command performs the saving action, specifying which R objects to
#        save (modelPart1 in this case) and the file name (with file path) to save to.
#        You can save more than one object in this command (as you will see in the
#        Part 2 example). For example, the Lasso model for modelC1 was built for the 
#        single value cvLasso$lambda.min. If modelC1 had been trained over a range of
#        lambda values, then we would want to save the chosen optimal value of 
#        cvLasso$lambda.min along with the model.
#   3. You now have your chosen Part 1 model saved to your computer. You can close or
#      terminate your R session, and you will still be able to come back later and 
#      load the model from the .RData file. You can go to the file location that you
#      specified above, and you should see a file named modelPart1.RData.

# Save Part 2 Model: 
#   0. I'll use modelC1 from SampleCodePart2.R for this part. Based on the results
#      obtained in Part 2, modelC1 had the highest TP rate. While the "accuracy" for
#      modelC1 wasn't as high as for some of the other models, it is really the TP
#      rate that will drive the mailing list selection.
#   1. Go to your Part 2 code and execute the commands that you need so that your
#      chosen model is in memory. It is a good idea to start with a fresh R session
#      here. That way you don't have any unintentional carry-over from your Part 1 
#      code.
#   2. While your chosen Part 2 model is in memory, execute the following commands
#      (modified for your particular situation):
outPath = file.path("/Users","stevenfutter","Dropbox","NU","MACHINE_LEARNING","charity_project","part3")
modelPart2 = glm.stepwise
modelPart2_prob_threshold = threshA2
save(modelPart2,modelPart2_prob_threshold,file=file.path(outPath,"modelPart2.RData"))

#      - Note that my modelC1 does not need any additional hyper-parameters to be
#        saved separately from the model itself. The parameter bestCp is encoded in
#        the model object, since we pruned the model to match that Cp value.
#      - For a model that requires additional parameters to be saved (such as a
#        threshold for a logistic regression model), you can modify the command as
#        follows (naming is altered in this example):
#          save(modelB1,optIdxB1,file=file.path(outPath,"modelPart2.RData"))
#      - When saving to a .RData file, you can list as many objects and values as
#        you want. Follow those objects with the file specification. R will bundle 
#        everything up and save it to one .RData file. For example, you could save
#        multiple models and the data used to train the models as follows:
#           save(classData2,modelA1,optIdxA1,modelB1,optIdxB1,file=file.path(outPath,"exampleSave.RData"))
#   3. You now have your chosen Part 2 model saved to your computer. You can close or
#      terminate your R session, and you will still be able to come back later and 
#      load the model from the .RData file. You can go to the file location that you
#      specified above, and you should see a file named modelPart2.RData.

# Once you have saved your Part 1 and Part 2 models, I recommend you start with a 
# fresh R session again. To load the models from Part 1 and Part 2, we have the 
# following commands:

# Specify the file path where your .RData files are located
modelPath = file.path("/Users","stevenfutter","Dropbox","NU","MACHINE_LEARNING","charity_project","part3")

# Use the load command to load objects from your .RData files.
load(file.path(modelPath,"modelPart1.RData"))
load(file.path(modelPath,"modelPart2.RData"))

# You should now see modelPart1 and modelPart2 (plus any hyper-parameters that you
# saved) in your R Environment (i.e. in memory).