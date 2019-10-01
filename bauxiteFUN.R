# BAUXITE =======================================================
# Takes two 0/1 vectors of targets/predictions to create
#   a confusion matrix and calculates key metrics
bauxite <- function(targetV, predictionV) {
  if (max(targetV) > 1 | max(predictionV) > 1 | 
      min(targetV) < 0 | min(predictionV) < 0 | 
      length(targetV) != length(predictionV)
      ) {
    print("Error: Invalid vectors for 2x2 confusion matrix")
  } else {
    # RAW MATERIAL FOR CONFUSION MATRIX
    ore <- c( 
      sum(targetV == 1 & predictionV == 1), # true positives
      sum(targetV == 1 & predictionV == 0), # false negatives
      sum(targetV == 0 & predictionV == 1), # false positives
      sum(targetV == 0 & predictionV == 0)  # true negatives
    )
    # CONFUSION MATRIX
    bauxite <- matrix(ore, 
                      nrow = 2, ncol = 2, 
                      dimnames = list(c("Positive Test", "Negative Test"), 
                                      c("Event Present", "Event Absent")))
    # MEASUREMENTS
    nn <- sum(bauxite)
    accuracy <- (bauxite[1, 1] + bauxite[2, 2]) / nn
    precision <- bauxite[1, 1] / sum(bauxite[1, ])
    sensitivity <- bauxite[1, 1] / sum(bauxite[, 1])
    specificity <- bauxite[2, 2] / sum(bauxite[, 2])
    F1score <- (2 * precision * sensitivity) / (precision + sensitivity)
    ppv <- bauxite[1, 1] / sum(bauxite[1, ])
    npv <- bauxite[2, 2] / sum(bauxite[2, ])
    presence <- sum(bauxite[, 1]) / nn
    absence <- sum(bauxite[, 2]) / nn
    fpr <- bauxite[1, 2] / sum(bauxite[, 2])
    fnr <- bauxite[2, 1] / sum(bauxite[, 1])
    measure <- c(accuracy, precision, 
                 sensitivity, specificity, 
                 F1score, 
                 ppv, npv, 
                 fpr, fnr, 
                 presence, absence)
    metric <- c("Accuracy", "Precision",  
                "Sensitivity", "Specificity", 
                "F1score", 
                "PPV", "NPV", 
                "FPR", "FNR", 
                "Presence", "Absence")
    bresult <- data.frame(metric = as.factor(metric), 
                          measure, yorder = c(11:1))
    # OUTPUT
    return(bresult)
  }
}

# Plot function for the metrics above ::::::::::::::::::::::::::::::
bauxitePLT <- function(bresult) {
  bplt1 <- bresult %>% 
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
    labs(title = "BAUXITE Confusion Matrix Results") + 
    ylab("Confusion Matrix Measurement")
  return(bplt1)
}
# BAUXITE ^^^ =======================================================
