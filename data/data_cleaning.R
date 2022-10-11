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
          "diagnosis", "REG1A")
dataset <- dataset[,!(names(dataset) %in% drop)]

# Log transform
log.data <- log(dataset[4:8] +1)
dataset[4:8] <- log.data

# Factorize sex
dataset$sex <- factor(dataset$sex, levels = c("F", "M"))

# Export dataset
write.csv(dataset, "cleaned_data.csv", row.names = FALSE, quote = FALSE)

## CODE TO CREATE TRAINING AND TEST SETS
# # Create set column for training and test sets
# dataset$set <- factor( rep( c("Training", "Test"), times = nrow(dataset)/2 ),
#                       levels = c("Training", "Test"))
# # Subset accordingly
# training <-  subset(dataset, dataset$set == "Training")
# test.data <- subset(dataset, dataset$set == "Test")
# # Drop the set column is the subsets
# training <- training[-10]
# test.data <- test.data[-10]
