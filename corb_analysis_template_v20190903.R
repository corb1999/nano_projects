# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# INTRO ================================
# v20190829
# Project: 
# Notes: 
# INTRO ^^^ ============================
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# LOAD LIBRARIES ------------------------------------------------------------
library(tidyverse)
library(tidylog)
library(lubridate)
library(DataExplorer)
library(reshape2)
library(grid)
library(caTools)
clock_a <- now()
# LOAD LIBRARIES ^^^ ----------------------------------------------------------

# MULTIPLOT =================================================================
# Multiple plot function
# via http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


# MULTIPLOT ^^^ ================================================================

# LOAD DATA -----------------------------------------------------------------

clock_1 <- now()
xx <- read.csv("train.csv")
clock_2 <- now()
time_load_train <- clock_2 - clock_1
time_load_train

clock_1 <- now()
yy <- read.csv("test.csv")
clock_2 <- now()
time_load_test <- clock_2 - clock_1
time_load_test

gc(verbose = TRUE)

# LOAD DATA ^^^ -------------------------------------------------------------

# EXPLORE 1 ---------------------------------------------------------------------



# EXPLORE 1 ^^^ -----------------------------------------------------------------

# END ====================================================================
clock_b <- now()
time_run_script <- clock_b - clock_a
time_run_script
time_load_train
time_load_test
ls()
gc(verbose = TRUE)

#rm(list = ls())

# END ^^^ ====================================================================

# APPENDIX =====================================================

#### _____ -----> BAUXITE <----- _____ #####
# AN R SCRIPT TO TAKE CONFUSION MATRIX VALUES (ORE) 
# TO MAKE A CONFUSION MATRIX (BAUXITE)
library(ggplot2)
library(dplyr)

# USE THIS TO DERIVE MATRIX VALUES FROM A DF
ore <- c( 
  sum(1 == 1 & 1 == 1), # true positives
  sum(1 == 1 & 1 == 1), # false negatives
  sum(1 == 1 & 1 == 1), # false positives
  sum(1 == 1 & 1 == 1)  # true negatives
)

# USE THIS TO MANUALLY ENTER MATRIX VALUES
ore <- c(20, 10, 180, 1820) 

# GENERATE RANDOM VALUES TO TEST
ore <- c(sample(1:500, 4, replace = FALSE)) 

# CREATIN THE MATRIX AND CALCULATING THE METRICS
bauxite <- matrix(ore, 
                  nrow = 2, ncol = 2, 
                  dimnames = list(c("Positive Test", "Negative Test"), 
                                  c("Event Present", "Event Absent")))
nn <- sum(bauxite)
accuracy <- (bauxite[1, 1] + bauxite[2, 2]) / nn
sensitivity <- bauxite[1, 1] / sum(bauxite[, 1])
specificity <- bauxite[2, 2] / sum(bauxite[, 2])
ppv <- bauxite[1, 1] / sum(bauxite[1, ])
npv <- bauxite[2, 2] / sum(bauxite[2, ])
presence <- sum(bauxite[, 1]) / nn
absence <- sum(bauxite[, 2]) / nn
fpr <- bauxite[1, 2] / sum(bauxite[, 2])
fnr <- bauxite[2, 1] / sum(bauxite[, 1])
measure <- c(accuracy, 
             sensitivity, specificity, 
             ppv, npv, 
             fpr, fnr, 
             presence, absence)
metric <- c("accuracy", 
            "sensitivity", "specificity", 
            "ppv", "npv", 
            "fpr", "fnr", 
            "presence", "absence")
bresult <- data.frame(metric = as.factor(metric), measure, yorder = c(9:1))
baux_plt1 <- bresult %>% 
  ggplot(aes(x = measure, y = reorder(metric, yorder))) +   
  geom_point(size = 10, color = "#bc204b") + 
  geom_label(aes(label = paste0(floor(measure * 100), "%")), 
             hjust = "left") + 
  scale_x_continuous(limits = c(0, 1), 
                     breaks = c(seq(0, 1, 0.1)), 
                     name = "", 
                     labels = scales::percent, 
                     position = "top") + 
  theme_bw() + 
  labs(title = "BAUXITE Confusion Matrix Results")

# OUTPUTS
baux_plt1
nn
bauxite
prop.table(bauxite)
bresult[, 1:2]
#### _____ ^^^^^ BAUXITE ^^^^^ _____ #####


# APPENDIX ^^^ =====================================================

# ps ============================================================
# 