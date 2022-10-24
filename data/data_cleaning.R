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

control <- subset(dataset, dataset$diagnosis == 1 | dataset$diagnosis == 3)
benign <- subset(dataset, dataset$diagnosis == 2 | dataset$diagnosis == 3)

control <- control[-1]
benign <- benign[-1]
dataset <- dataset[-1]

# Export dataset
write.csv(dataset, "cleaned_data.csv", row.names = F, quote = F, na="")
write.csv(control, "control_data.csv", row.names = F, quote = F, na="")
write.csv(benign, "benign_data.csv", row.names = F, quote = F, na="")

# CODE TO CREATE TRAINING AND TEST SETS
# # Create set column for training and test sets
# dataset$set <- factor( rep( c("Training", "Test"), times = nrow(dataset)/2 ),
#                       levels = c("Training", "Test"))
# # Subset accordingly
# training <-  subset(dataset, dataset$set == "Training")
# test.data <- subset(dataset, dataset$set == "Test")
# # Drop the set column is the subsets
# training <- training[-7]
# test.data <- test.data[-7]
# write.csv(training, "training_set.csv", row.names = F, quote = F, na = "")
# write.csv(test.data, "test_set.csv", row.names = F, quote = F, na = "")

# Set working directory back to root
setwd("..")
