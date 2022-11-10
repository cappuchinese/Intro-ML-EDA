## ---------------------------
##
## Name: data_cleaning.R
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

## ---- cleaning ----
# Set working directory to this folder
setwd("../data")
# Read dataset
dataset <- read.csv("../data/Data.csv")
# Change the empty strings to NA
dataset[dataset == ""] <- NA

# Group the samples
dataset$sex <- factor(dataset$sex)
dataset <- dataset %>%
  mutate(
    diagnosis = factor(
      dplyr::case_when(
        diagnosis == 1 ~ "Control",
        diagnosis == 2 ~ "Benign",
        stage == "I" ~ "PDAC",
        stage == "IA" ~ "PDAC",
        stage == "IB" ~ "PDAC",
        stage == "II" ~ "PDAC",
        stage == "IIA" ~ "PDAC",
        stage == "IIB" ~ "PDAC",
        stage == "III" ~ "PDAC",
        stage == "IV" ~ "PDAC"),
      level = c("Control", "Benign", "PDAC")
    ),
    stage = factor(
      dplyr::case_when(
        stage == "I" ~ "early",
        stage == "IA" ~ "early",
        stage == "IB" ~ "early",
        stage == "II" ~ "early",
        stage == "IIA" ~ "early",
        stage == "IIB" ~ "early",
        stage == "III" ~ "late",
        stage == "IV" ~ "late"),
      level = c("early", "late")
    )
  )

# Drop unnecessary columns
drop <- c("sample_id", "patient_cohort", "sample_origin", "benign_sample_diagnosis",
          "REG1A")
dataset <- dataset[,!(names(dataset) %in% drop)]

# Move diagnosis and stage to last column
dataset <- dataset %>% select(-c(3,4), everything())

# Log transform and meann centering
log.data <- log(dataset[3:7] +1)
dataset[3:7] <- log.data

## ---- splitting ----
# Random split for training and test sets (50/50)
set.seed(391)
train.rows <- sort(sample(seq_len(nrow(dataset)), size = floor(0.7*nrow(dataset))))

training <- dataset[train.rows,]
test <- dataset[-train.rows,1:7]

control <- subset(training,
                  training$diagnosis == "Control" | training$diagnosis == "PDAC")
benign <- subset(training,
                       training$diagnosis == "Benign" | training$diagnosis == "PDAC")
pdac <- subset(training, !is.na(training$stage))

# Export dataset
write.csv(dataset, "../data/wekafiles/cleaned_data.csv", row.names = F, quote = F, na="")
write.csv(control, "../data/wekafiles/control_train.csv", row.names = F, quote = F, na="")
write.csv(benign, "../data/wekafiles/benign_train.csv", row.names = F, quote = F, na="")
write.csv(pdac, "../data/wekafiles/pdac_train.csv", row.names = F, quote = F, na="")
write.csv(test, "../data/wekafiles/test.csv", row.names = F, quote = F, na="")

# Set working directory back to log folder
setwd("../log")
