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

# Change the empty strings to NA
dataset[dataset == ""] <- NA

## ---- change-data ----
# Remove unnecessary columns
drop <- c("sample_id", "benign_sample_diagnosis")
dataset <- dataset[,!(names(dataset) %in% drop)]

# Fill in the NAs with the average
dataset$plasma_CA19_9 <- replace_na(dataset$plasma_CA19_9,
                                    mean(dataset$plasma_CA19_9, na.rm=T))
dataset$REG1A <- replace_na(dataset$REG1A, mean(dataset$REG1A, na.rm=T))

## ---- EDA
## Assign age categories
dataset <- dataset %>%
  mutate(
    ## Factor for order of age
    age_group = factor(
      dplyr::case_when(
        age <= 30 ~ "<30",
        age > 30 & age <= 40 ~ "31-40",
        age > 40 & age <= 50 ~ "41-50",
        age > 50 & age <= 60 ~ "51-60",
        age > 60 & age <= 70 ~ "61-70",
        age > 70 ~ ">70" ),
      level = c("<30", "31-40", "41-50", "51-60", "61-70", ">70")
    )
  )
