#### MR SIZE VALIDATION ####

# DESCRIPTION
# this exercise will try to use the alternative pipe sizes to 
# confirm or overrule the pipe size entered in the mr data
# using ml, having first confirmed pipe sizes for a training set

# creating the training set
# goal is to find some mr for which we trust the entered size
# 
# Method 1: use the mr2p data
# mr2p already finds the closest pipe AND gives the distance and match quality
# select pipes with a good match quality:
# - M1 or M2
# - within +/- 0.1 metres
# RESULT: only 3 M1/M2 with 1 metre matchedMainDistance are in the mr_sizes data

# Method 2: Find MRs which give the GISID


#### SET UP ####
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
source("C:/Users/dylan/Documents/R/scripts/import_functions.R")
#### DATA PREP ####
# import the mains repair data
importMR()
mr$sizeInches <- as.double(mr$sizeInches)

# import the mr alternative sizes
mr_sizes <- read_excel("F:/2/mr pipe size validation.xlsx")

# import the mains repairs geodatabase
mr2p <- rgdal::readOGR("C:/Users/Dylan/Documents/work/maps/MainsRepairs.gdb", 
                        layer = "MainsRepair")
mr2p$CompletedDate <- ymd_hms(mr2p$CompletedDate)
mr2p@data$MatchedMainGISID <- as.character(mr2p@data$MatchedMainGISID)
mr2p <- mr2p[!is.na(mr2p$CompletedDate), ]
#mr2p <- mr2p[mr2p@data$CompletedDate >= min(mr$dateCompleted), ]


# SAP jobs with an asset reference
asset_refs <- xli()
#### GET THE CORRECT PIPE SIZE: METHOD 1 ####
mr_sizes <- left_join(mr_sizes, 
                      mr2p@data[c("Activity", "MatchedMainGISID", "MatchedMainDistance_Num", "MatchType")],
                      by = c("operation" = "Activity"))
mr_sizes <- left_join(mr_sizes, mains, by = c("MatchedMainGISID" = "GISID"))

mr_sizes$sapGisMatch <- ifelse(mr_sizes$sizeInches == mr_sizes$size_inches, 1, 0)

names(mr_sizes) <- c("operation", "sizeBand", "sizeLetter", "sizeInches", "sizeVsLetter", 
                     "jobTextSize", "jobSize", "siSize", "ptwSize", "swSize", "altSizeCount",
                     "hits", "material", "jobPipeMaterial", "mainTypeAIM", "ptwPipeType",
                     "jobPipeSize", "dateCreated", "dateCompleted", "x", "y", "address",
                     "MatchedMainGISID", "MatchedMainDistance", "MatchType", "sizeGIS", "sapGisMatch")

# add the gisid of the main identified in the mr
mr_sizes <- left_join(mr_sizes, mr[c("operation", "gisid")], by = "operation")

# add the match columns
mr_size_matches <- mr_sizes$sizeInches == mr_sizes[c("jobSize", "siSize", "ptwSize")]
# mr_size_matches[is.na(mr_size_matches)] <- "UNKNOWN"
attr(mr_sizeMatches, "dimnames")[[2]] <- c("jobSizeMatch", "siSizeMatch", "ptwSizeMatch")
mr_sizes <- cbind(mr_sizes, mr_sizeMatches)

#### GET THE CORRECT PIPE SIZE: METHOD 2 ####
mains <- xli()  # from team charts
mains$GISID <- as.character(mains$GISID)

mr_mains <- mr %>% filter(nchar(gisid) > 4)

mr_mains <- left_join(mr_mains, mains, by = c("gisid" = "GISID"))
mr_mains$sizeMatch <- ifelse(mr_mains$size_inches == mr_mains$sizeInches, 1, 0)

#### VISUALISING THE DATA ####
mr_sizes %>% 
  filter(!is.na(sapGisMatch),
         size_inches < 100, 
         job < 100) %>% 
ggplot(aes(sizeInches, size_inches)) + 
  geom_point(alpha = 0.3) + 
  facet_wrap(~ sapGisMatch) + 
  labs(x = "SAP entry pipe size", y = "Matched Main size from GIS")

mr_sizes %>% 
  ggplot(aes(material, jobPipeMaterial)) + 
  geom_count()

#### MODELLING DATA ####
library(caret)

# create modelling data 
mr_sizes_modelling <- mr_sizes %>% 
  filter(MatchedMainDistance < .5, 
         !is.na(sizeRight),
         MatchedMainGISID == gisid) %>% 
  select(sizeInches, jobSize, siSize, ptwSize, 
         jobSizeMatch, siSizeMatch, ptwSizeMatch,
         material, jobPipeMaterial, 
         mainTypeAIM, ptwPipeType, MatchedMainDistance, 
         sizeGIS, sizeRight)

mr_sizes_modelling$ptwPipeType2 <- ifelse(mr_sizes_modelling$ptwPipeType == "Distribution", "DM",
                                          ifelse(mr_sizes_modelling$ptwPipeType == "Transmission", "TM",
                                                 "Unknown"))

mr_sizes_modelling$pipeTypeMatch <- ifelse(mr_sizes_modelling$mainTypeAIM == mr_sizes_modelling$ptwPipeType2, 1, 0)
mr_sizes_modelling$materiaMatch <- ifelse(mr_sizes_modelling$material == mr_sizes_modelling$jobPipeMaterial, 1, 0)


# create the train/test data
inTrain <- createDataPartition(y = mr_sizes_modelling$sizeRight, p = 0.8, list = FALSE)
train <- mr_sizes_modelling[inTrain, ]
train[is.na(train)] <- "unknown"
train$sizeRight <- as.factor(train$sizeRight)

test <- mr_sizes_modelling[-inTrain, ]
test[is.na(test)] <- "unknown"
test$sizeRight <- as.factor(test$sizeRight)

myTrainControl <- trainControl(
  method = "cv",
  number = 10,
  verboseIter = TRUE
)

myTrainControl2 <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 2,
  verboseIter = TRUE
)

#### MODELS ####
# RANDOM FOREST
fit.rf <- train(
  sizeRight ~ jobSizeMatch + siSizeMatch + ptwSizeMatch,
  train,
  method = 'ranger',
  trControl = myTrainControl
)

fit.rf.repeated <- train(
  sizeRight ~ jobSizeMatch + siSizeMatch + ptwSizeMatch,
  train,
  method = 'ranger',
  trControl = myTrainControl2
)

confusionMatrix(test$sizeRight, predict(fit.rf, test))
results <- resamples(list(fit.rf, fit.glmnet))
summary(results)



# glmnet
# extra sizes only
fit.glmnet <- train(
  sizeRight ~ jobSizeMatch + siSizeMatch + ptwSizeMatch,
  train,
  method = 'glmnet',
  trControl = myTrainControl
)

#### RESULTS ####
# out of sample confusion matrix
confusionMatrix(test$sizeRight, predict(fit.rf, test))
confusionMatrix(test$sizeRight, predict(fit.rf.repeated, test))


results <- resamples(list(rf = fit.rf, glmnet = fit.glmnet))
summary(results)
