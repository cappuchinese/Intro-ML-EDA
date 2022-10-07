#' All the variables to be used in this file
# Read the data
dataset <- read.csv("../data/Data.csv")
codebook <- read_delim("../data/codebook.txt", delim = "|")

# Change the empty strings to NA
dataset[dataset == ""] <- NA

# Remove unnecessary columns
drop <- c("sample_id", "patient_cohort", "sample_origin", "benign_sample_diagnosis")
dataset <- dataset[,!(names(dataset) %in% drop)]

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

REG1A <- dunnTest(dataset$REG1A ~ dataset$diagnosis_group)
REG1B <- dunnTest(dataset$REG1B ~ dataset$diagnosis_group)
dataset <- dataset[,!(names(dataset) %in% "REG1A")]

# Log transform
log.data <- log(dataset[5:9] +1)
dataset[5:9] <- log.data

# Different diagnosis and blood groups
control <- subset(dataset, diagnosis == 1)
benign <- subset(dataset, diagnosis == 2)
pdac <- subset(dataset, diagnosis == 3)
blood <- subset(dataset, plasma_CA19_9 >= 0)

# Drop the "plasma" column
dataset <- dataset[,-5]

# PCA data
pca <- prcomp(dataset[,c(1,3,5:8)], center = TRUE, scale. = TRUE)
