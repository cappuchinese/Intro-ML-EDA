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
dataset$sex <- factor(dataset$sex)

# Drop unnecessary columns
drop <- c("sample_id", "patient_cohort", "sample_origin", "benign_sample_diagnosis",
          "REG1A", "stage")
dataset <- dataset[,!(names(dataset) %in% drop)]

# Log transform and meann centering
log.data <- log(dataset[4:8] +1)
dataset[4:8] <- log.data

# Random split for training and test sets (50/50)
set.seed(391)
train.rows <- sort(sample(seq_len(nrow(dataset)), size = floor(0.7*nrow(dataset))))

training <- dataset[train.rows,]
test <- dataset[-train.rows,]

# Remove diagnosis column
training <- training[,-3]
test <- test[,-3]

# Export dataset
write.csv(training, "training.csv", row.names = F, quote = F, na="")
write.csv(test, "test.csv", row.names = F, quote = F, na="")

# Set working directory back to root
setwd("..")
