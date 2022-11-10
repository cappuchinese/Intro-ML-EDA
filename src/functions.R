## ---------------------------
##
## Name: functions.R
##
## Author: Lisa Hu
##
## Purpose: Script with functions for readability
##
## Email: l.j.b.hu@st.hanze.nl
##
## Copyright (c) 2022 Lisa Hu
## Licensed under GPLv3. See LICENSE
##
## ---------------------------

# List of ggplot boxplots
create.plots <- function(y.values, y.label, plt.tag) {
  list(ggplot(data = control, aes(x = diagnosis, y = !!sym(y.values))) +
    geom_boxplot(outlier.color = "red", outlier.shape = 1, aes(fill = "Control")) +
    geom_boxplot(data = benign, outlier.color = "red", outlier.shape = 1,
                 aes(fill = "Benign")) +
    geom_boxplot(data = pdac, outlier.color = "red", outlier.shape = 1,
                 aes(fill = "PDAC")) +
    labs(x = "Diagnosis", y = y.label, tag = plt.tag) +
    scale_fill_manual(values = c("red", "green", "blue"),
                      limits = c("Control", "Benign", "PDAC"),
                      name = ""))
}

# Make algorithm names readable
algorithm.names <- function(weka.res){
  labels <- unlist(strsplit(unique(weka.res$Key_Scheme), "\\."))
  labels <- labels[seq(0, length(labels), 4)]
  weka.res$Key_Scheme <- factor(weka.res$Key_Scheme, levels = unique(weka.res$Key_Scheme),
                                labels = labels)
  return(weka.res)
}
