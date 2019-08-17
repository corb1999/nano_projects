#### _____ -----> BAUXITE <----- _____ #####
# AN R SCRIPT TO TAKE CONFUSION MATRIX VALUES (ORE) TO MAKE A CONFUSION MATRIX (BAUXITE)
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