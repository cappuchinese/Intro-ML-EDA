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

# Unfinished
significance.check <- function(important) {
  y <- important$control_train %>%
    select(-1) %>%
    gather(key = type, value = value, Percent_correct, True_positive_rate, True_negative_rate, False_positive_rate, False_negative_rate) %>%
    group_by(Options, type) %>%
    summarise(value = list(value)) %>%
    spread(Options, value) %>%
    group_by(type)

  for (type in seq(nrow(y))){
    for (algorithm in seq(3,8)){
      p.value <- t.test(unlist(y[type,2]), unlist(y[type,algorithm]))$p.value
      if (p.value < 0.05) {
        cat(type,"-", algorithm, "* ")
      }
    }
  }
}
