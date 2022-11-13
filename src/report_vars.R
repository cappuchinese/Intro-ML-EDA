## ---------------------------
##
## Name: report_vars.R
##
## Author: Lisa Hu
##
## Purpose: Script everything surrounding the EDA
##
## Email: l.j.b.hu@st.hanze.nl
##
## Copyright (c) 2022 Lisa Hu
## Licensed under GPLv3. See LICENSE
##
## ---------------------------

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
        stage == "I" ~ "I-IIA",
        stage == "IA" ~ "I-IIA",
        stage == "IB" ~ "I-IIA",
        stage == "II" ~ "I-II",
        stage == "IIA" ~ "I-IIA",
        stage == "IIB" ~ "I-II",
        stage == "III" ~ "III-IV",
        stage == "IV" ~ "III-IV" ),
      level = c("Control", "Benign", "I-IIA", "I-II", "III-IV")
    )
  )

# Perform the tests
REG1A <- dunnTest(dataset$REG1A ~ dataset$diagnosis_group)
REG1B <- dunnTest(dataset$REG1B ~ dataset$diagnosis_group)

# Create a nice format to show the correct comparisons
comparison <- t(cbind(REG1A$res[c(2:5,7,8),c(1,4)], REG1B$res[c(2:5,7,8),4]))
colnames(comparison) <- comparison[1,]
comparison <- comparison[-1,]
comparison <- apply(comparison, 2, as.numeric)
rownames(comparison) <- c("REG1A", "REG1B")
comparison[comparison > 0.05] <- "ns"

# Drop the REG1A
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

# Read the weka results
result <- read_csv("../data/weka_out/base.csv")
# Make algorithm names readable
result <- algorithm.names(result)
# Wanted results
result <- result %>%
  group_by(Key_Scheme) %>%
  summarise_at(vars(Percent_correct, True_positive_rate, False_positive_rate,
                    Area_under_ROC), list(mean = weighted.mean))
names(result) <- c("Algorithm", "Accuracy", "Sensitivity", "FPR", "AUROC")
cap <-  "\\label{tab:weka}Results of the different algorithms from Weka"

# Read the optimization results
opt.res <- read_csv("../data/weka_out/optimization.csv")
# Make algorithm names readable
opt.res <- algorithm.names(opt.res)
opt.res <- opt.res %>%
  mutate(
    Options = factor(opt.res$Key_Scheme_options,
                     level = unique(opt.res$Key_Scheme_options),
                     labels = c("-", "0.550", "0.575", "0.600", "0.625", "0.650", "0.675"))
  )
opt.res$Options <- paste(opt.res$Key_Scheme, paste0("(", opt.res$Options, ")"))
# Call the wanted results
x <- opt.res %>%
  group_by(Key_Dataset, Options) %>%
  summarise_at(vars(Percent_correct, True_positive_rate, False_positive_rate),
               list(mean = weighted.mean))
names(x) <- c("Dataset", "Algorithm (threshold)", "Accuracy", "TPR", "FPR")
cap <-  "Results of the ThresholdSelector with different thresholds "

# Read the ROC files
roc.control <- read.arff("../data/weka_out/roc_control.arff")
roc.benign <- read.arff("../data/weka_out/roc_benign.arff")
