# http://ethen8181.github.io/machine-learning/unbalanced/unbalanced.html
ROCInfo <- function(df, predict, actual, cost.fp, cost.fn) {
  require(ROCR)
  require(gridExtra)
  require(grid)
  
  pred <- prediction(df[[predict]], df[[actual]])
  perf <- performance(pred, "tpr", "fpr")
  roc_dt <- data.frame(fpr = perf@x.values[[1]], tpr = perf@y.values[[1]])
  
  # cost with the specified false positive and false negative cost 
  # false postive rate * number of negative instances * false positive cost + 
  # false negative rate * number of positive instances * false negative cost
  cost <- perf@x.values[[1]] * cost.fp * sum(df[[actual]] == 'bad') + 
    (1 - perf@y.values[[1]]) * cost.fn * sum(df[[actual]] == 'good')
  
  cost_dt <- data.frame(cutoff = pred@cutoffs[[1]], cost = cost)
  
  # optimal cutoff value, and the corresponding true positive and false positive rate
  best_index  <- which.min(cost)
  best_cost   <- cost_dt[best_index, "cost"]
  best_tpr    <- roc_dt[best_index, "tpr"]
  best_fpr    <- roc_dt[best_index, "fpr"]
  best_cutoff <- pred@cutoffs[[1]][best_index]
  
  # area under the curve
  auc <- performance(pred, "auc")@y.values[[1]]
  
  # normalize the cost to assign colors to 1
  normalize <- function(v) {
    (v - min(v)) / diff(range(v))
  }
  
  # create color from a palette to assign to the 100 generated threshold between 0 ~ 1
  # then normalize each cost and assign colors to it, the higher the blacker
  # don't times it by 100, there will be 0 in the vector
  col_ramp <- colorRampPalette(c( "green", "orange", "red", "black" ))(100)   
  col_by_cost <- col_ramp[ceiling(normalize(cost) * 99) + 1]
  
  roc_plot <- ggplot(roc_dt, aes(fpr, tpr)) + 
    geom_line(color = rgb(0, 0, 1, alpha = 0.3)) +
    geom_point(color = col_by_cost, size = 4, alpha = 0.2) + 
    geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), alpha = 0.8, color = "royalblue") + 
    labs(title = "ROC", x = "False Postive Rate", y = "True Positive Rate") +
    geom_hline(yintercept = best_tpr, alpha = 0.8, linetype = "dashed", color = "steelblue4") +
    geom_vline(xintercept = best_fpr, alpha = 0.8, linetype = "dashed", color = "steelblue4")				
  
  cost_plot <- ggplot(cost_dt, aes(cutoff, cost)) +
    geom_line(color = "blue", alpha = 0.5) +
    geom_point(color = col_by_cost, size = 4, alpha = 0.5) +
    ggtitle("Cost") +
    # scale_y_continuous(labels = comma) +
    geom_vline(xintercept = best_cutoff, alpha = 0.8, linetype = "dashed", color = "steelblue4")	
  
  # the main title for the two arranged plot
  sub_title <- sprintf("Cutoff at %.2f - Total Cost = %.2f, AUC = %.3f",
                       best_cutoff, best_cost, auc)
  
  # arranged into a side by side plot
  plot <- arrangeGrob(roc_plot, cost_plot, ncol = 2,
                      top = textGrob(sub_title, gp = gpar(fontsize = 16, fontface = "bold")))
  
  return(list(  plot 		    = plot, 
                cutoff 	    = best_cutoff, 
                totalcost   = best_cost, 
                auc         = auc,
                sensitivity = best_tpr, 
                specificity = 1 - best_fpr ))
}