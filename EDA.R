## ---------------------------
##
## Name: EDA.R
##
## Author: Lisa Hu
##
## Purpose: Script everything surrounding the EDA
##
## Email: l.j.b.hu@st.hanze.nl
##
## ---------------------------

## ---- read-data ----
dataset <- read.csv("Data.csv")
codebook <- read_delim("codebook.txt", delim = "|")
knitr::kable(codebook[1:4], booktabs = T, caption = "Data values")
knitr::kable(codebook[c(1,5)], booktabs = T, caption = "Description")

## ---- change-data ----
# Change the empty strings to NA
dataset[dataset == ""] <- NA

# Remove unnecessary columns
drop <- c("sample_id", "benign_sample_diagnosis")
dataset <- dataset[,!(names(dataset) %in% drop)]

# Different diagnosis groups
control <- subset(dataset, diagnosis == 1)
benign <- subset(dataset, diagnosis == 2)
pdac <- subset(dataset, diagnosis == 3)

# Demographics
demograph <- data.frame(c(sum(control$sex == "F"), sum(control$sex == "M")),
                        c(sum(benign$sex == "F"), sum(benign$sex == "M")),
                        c(sum(pdac$sex == "F"), sum(pdac$sex == "M")))
colnames(demograph) <- c("Control", "Benign", "PDAC")
rownames(demograph) <- c("Female", "Male")

knitr::kable(demograph)

# Amount of blood plasma samples
nrow(dataset) - sum(length(which(is.na(dataset$plasma_CA19_9))))
blood <- subset(dataset, plasma_CA19_9 >= 0)

## ---- EDA

## Assign age categories
# dataset <- dataset %>%
#   mutate(
#     ## Factor for order of age
#     age_group = factor(
#       dplyr::case_when(
#         age <= 30 ~ "<30",
#         age > 30 & age <= 40 ~ "31-40",
#         age > 40 & age <= 50 ~ "41-50",
#         age > 50 & age <= 60 ~ "51-60",
#         age > 60 & age <= 70 ~ "61-70",
#         age > 70 ~ ">70" ),
#       level = c("<30", "31-40", "41-50", "51-60", "61-70", ">70")
#     )
#   )
