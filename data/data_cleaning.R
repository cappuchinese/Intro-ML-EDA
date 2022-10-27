## ---------------------------
##
## Name: EDA.R
##
## Author: Lisa Hu
##
## Purpose: Script to produce the clean dataset
##
## Email: l.j.b.hu@st.hanze.nl
##
## Copyright (c) 2022 Lisa Hu
## Licensed under GPLv3. See LICENSE
##
## ---------------------------

# Set working directory to this folder
setwd("data")
# Read dataset
dataset <- read.csv("Data.csv")
# Change the empty strings to NA
dataset[dataset == ""] <- NA

# Group the samples
dataset <- dataset %>%
  mutate(
    ## Factor for order of age
    diagnosis_group = factor(
      dplyr::case_when(
        diagnosis == 1 ~ "Control",
        diagnosis == 2 ~ "Benign",
        stage == "I" ~ "I-II",
        stage == "IA" ~ "I-II",
        stage == "IB" ~ "I-II",
        stage == "II" ~ "I-II",
        stage == "IIA" ~ "I-II",
        stage == "IIB" ~ "I-II",
        stage == "III" ~ "III-IV",
        stage == "IV" ~ "III-IV" ),
      level = c("Control", "Benign", "I-II", "III-IV")
    )
  )

# Drop unnecessary columns
drop <- c("sample_id", "patient_cohort", "sample_origin", "benign_sample_diagnosis",
          "REG1A", "age", "sex", "stage")
dataset <- dataset[,!(names(dataset) %in% drop)]

# Log transform and meann centering
log.data <- log(dataset[2:6] +1)
dataset[2:6] <- log.data

# Create set column for training and test sets (50/50)
dataset$set <- factor( rep( c("Training", "Test"), times = nrow(dataset)/2 ),
                       levels = c("Training", "Test"))

# Subset training and test
training <- subset(dataset[2:7], dataset$set == "Training")
test <- subset(dataset[2:7], dataset$set == "Test")

# Create Control+PDAC training and test data
control.train <- subset(dataset[2:8], dataset$diagnosis == 1 | dataset$diagnosis == 3)
control.test <- subset(control.train[1:6], control.train$set == "Test")
control.train <- control.train[control.train$set == "Training",]
control.train <- control.train[-7]

# Create Benign+PDAC training and test data
benign.train <- subset(dataset[2:8], dataset$diagnosis == 2 | dataset$diagnosis == 3)
benign.test <- subset(benign.train[1:6], benign.train$set == "Test")
benign.train <- benign.train[benign.train$set == "Training",]
benign.train <- benign.train[-7]

# Export dataset
write.csv(training, "training.csv", row.names = F, quote = F, na="")
write.csv(test, "test.csv", row.names = F, quote = F, na="")
write.csv(control.train, "control_train.csv", row.names = F, quote = F, na="")
write.csv(control.test, "control_test.csv", row.names = F, quote = F, na="")
write.csv(benign.train, "benign_train.csv", row.names = F, quote = F, na="")
write.csv(benign.test, "benign_test.csv", row.names = F, quote = F, na="")

# Set working directory back to root
setwd("..")
