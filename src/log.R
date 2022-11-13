## ---------------------------
##
## Name: log.R
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

## ---- read-data ----
dataset <- read_csv("../data/Data.csv")
codebook <- read_delim("../data/codebook.txt", delim = "|")
pander(codebook[1:4], booktabs = T, split.tables = 100,
       caption = "Codebook explaining each column in the given data (Continues below)")
pander(codebook[c(1,5)], booktabs = T, caption = "Further explanation of the columns",
       justify = c("right", "left"), split.tables = 100)

## ---- change-data ----
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
    ),
    ## Factor if there's a blood sample or not
    blood = factor(
      dplyr::case_when(
        plasma_CA19_9 >= 0 ~ "yes",
        TRUE ~ "no"),
      level = c("yes", "no")
    )
  )

## ---- summary ----
pander(summary(dataset), booktabs = T, split.tables = 100,
       caption = "A quick summary of the dataset")

## ---- reg-vs ----
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
pander(comparison[,c(2,4,6)], split.tables = 100, booktabs = T,
       caption = "\\label{tab:comp}Adjusted p-values of Kruskal-Wallis test,
       Dunn's multiple comparisons; ns - not significant.
       The header shows the groups that were compared. (Table continues below)")
pander(comparison[,c(1,3,5)], split.tables = 100, booktabs = T)

dataset <- dataset[,!(names(dataset) %in% "REG1A")]

## ---- log-trans ----
log.data <- log(dataset[5:9] +1)
dataset[5:9] <- log.data
pander(summary(dataset), booktabs = T, split.tables = 100,
       caption = "Summary of the data after the log transformation")

## ---- demograph ----
# Different diagnosis and blood groups
control <- subset(dataset, diagnosis == 1)
benign <- subset(dataset, diagnosis == 2)
pdac <- subset(dataset, diagnosis == 3)

# Demographics
demograph <- dataset %>%
  group_by(sex, diagnosis, stage) %>% tally()
demograph.blood <- dataset %>%
  group_by(sex, blood) %>% tally()

pander(demograph, booktabs = T,
       caption = "\\label{tab:demo}Demographic of the samples")
pander(demograph.blood, booktabs = T,
       caption = "\\label{tab:blood}Demographic of the blood samples")

## ---- explore-var ----
# Summary dataset
pander(summary(dataset), split.tables = 100)

## ---- exp-box ----
# Create the boxplots for the different columns
y.values <- names(dataset[5:9])
y.labs <- c("log(CA19.9) (U/ml)", "log(Creatinine) (mg/ml)", "log(LYVE1) (ng/ml)", "log(REG1B) (ng/ml)",
            "log(TFF1) (ng/ml)")
plt.tag <- c("a", "b", "c", "d", "e")
plts <- mapply(create.plots, y.values, y.labs, plt.tag)

# Grid and print the plots
p1 <- ggarrange(plotlist = plts[1:2], ncol = 2,
                common.legend = TRUE, legend = "bottom")
p2 <- ggarrange(plotlist = plts[3:5], ncol = 3,
                common.legend = TRUE, legend = "none")
my.grid <- ggarrange(p1, p2, nrow = 2)
print(annotate_figure(my.grid, top = text_grob("Boxplots visualizing the biomarkers",
                                               face = "bold")))

## ---- correlation ----
cor_matrix <- cor(dataset[,c(1,6:9)])
heatmap(cor_matrix, scale = "column", Colv = NA, Rowv = NA, main = "Correlation heatmap")

## ---- pca ----
pca <- prcomp(dataset[,c(1,6:9)], center = TRUE, scale. = TRUE)
ggbiplot(pca, obs.scale = 1, var.scale = 1, groups = factor(dataset$diagnosis),
         ellipse = TRUE, circle = FALSE) +
  ggtitle("PCA of different biomarkers") +
  guides(color = guide_legend(title = "Diagnosis")) +
  theme(legend.position = "bottom")

## ---- weka-base ----
# Read the results
result <- read_csv("../data/weka_out/base.csv")
# Make algorithm names readable
result <- algorithm.names(result)
# Results
x <- result %>%
  group_by(Key_Scheme) %>%
  summarise_at(vars(Percent_correct, True_positive_rate, False_positive_rate,
                    Area_under_ROC), list(mean = weighted.mean))
names(x) <- c("Algorithm", "Accuracy", "Sensitivity", "FPR", "AUROC")
cap <-  "\\label{tab:weka}Results of the different algorithms from Weka"
pander(x, booktabs = T, split.tables = 100, caption = cap)

## ---- optimization ----
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
pander(x[8:14,2:5], booktabs = T, split.tables = 100,
       caption = paste0("\\label{tab:control}", cap, "(Control vs. PDAC)."))
pander(x[1:7,2:5], booktabs = T, split.tables = 100,
       caption = paste0("\\label{tab:benign}", cap, "(Benign vs. PDAC)."))

## ---- roc ----
# Read the ROC files
roc.control <- read.arff("../data/weka_out/roc_control.arff")
roc.benign <- read.arff("../data/weka_out/roc_benign.arff")

# Plot FPR against TPR + gradient of Threshold
ggplot(data = roc.control, aes(`False Positive Rate`, `True Positive Rate`)) +
  geom_point(aes(colour = Threshold)) +
  scale_color_gradientn(colors = c('#3f5efb', '#fc466b'), name = "Control-PDAC") +
  new_scale_color() +
  geom_point(data = roc.benign, aes(colour = Threshold)) +
  scale_color_gradientn(colors = c('#22c1c3', '#fdbb2d'), name = "Benign-PDAC") +
  ggtitle("ROC curves of the two models")
