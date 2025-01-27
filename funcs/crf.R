# -------------------------------------------------------------------
#           FUNCTION TO RUN CONDITIONAL RANDOM FOREST
# -------------------------------------------------------------------

# This function loads in specified data and runs a conditional random forest. Interactions are currently visualized through the partial dependency plot.
# My local machine does not have the memory to run it.

# for testing
# data_in <- "cleaned_fat_no_weather_no_miss"
# dv_in <- "fat_intake"
# ntree = 10
# int_vis = FALSE

crf_ana <- function(data_in = data_in, dv_in = dv_in, ntree = ntree, int_vis = int_vis) {
  
  dt_out = list()

  # check whether data_in is character  
  if (is.character(data_in) == FALSE)  {
    stop("Data_in needs to be a character string of the name of the csv file (without .csv). It needs to be placed in the data subfolder.")
  } else {
    tmp_load <- read.csv(paste("data/", data_in, ".csv", sep = ""))
    tmp_remove <- tmp_load %>% 
      select(starts_with("X"))
    cat(paste("We are removing the following variables from the dataset (make sure these are not needed!):", colnames(tmp_remove), "\n"))
    dt <- tmp_load %>% 
      select(-starts_with("X"))
    rm(tmp_load, tmp_remove)
    
  }
  
  cat("Specifying run variables for conditional random forest....\n")
  in_mtry <- round(sqrt(ncol(dt)))
  in_ntree <- ntree # change to 500 again
  in_trace <- TRUE
  replace <- FALSE
  
  formula <- as.formula(paste(dv_in, "~ ."))
  
  cat("Running conditional random forest.... \n")
  # NEEDS TO BE FIXED!!! SO THAT IT SELECTS CORRECT DV!
  cforest_model<- cforest(formula, 
                          data = dt,
                          controls = cforest_control(
                            # teststat = "quad", 
                            # testtype = "Univ",
                            # mincriterion = .95, 
                            ntree = in_ntree,
                            mtry = in_mtry, 
                            trace = in_trace,
                            replace = replace,
                          ))
  
  dt_out$cforest_model <- cforest_model
  
  cat("Creating variable importance plot....\n")
  tmp <- caret::varImp(cforest_model)
  tmp$names <- rownames(tmp)
  
  varImp_plot <- tmp %>%
    mutate(names = forcats::fct_reorder(names, Overall)) %>%
    ggplot(aes(x=names, y=Overall)) +
    geom_point() +
    geom_segment(aes(x=names,xend=names,y=0,yend=Overall)) +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    coord_flip() +
    xlab("Variable") +
    ylab("Relative Importance") +
    ggtitle(paste(data_in, dv_in, sep = "_")) +
    theme_bw()
  
  cat("Saving variable importance plot....\n")
  dt_out$varImp_plot <- varImp_plot
  ggsave(varImp_plot, filename = paste("pics/varImp", data_in, paste(dv_in, ".png", sep = ""), sep = "_"), width = 7, height = 7)
  
  cat("Creating partial dependence plot....\n")
  if (int_vis == TRUE) {
    warning(cat("Interactions are currently visualized through the partial dependency plot. Make sure you have the memory available!\n"))
  }
  
  ordering_df <- tmp[order(tmp$Overall, decreasing = T),]
  tmp_in <- ordering_df[1:8,]
  tmp_part_dep <- edarf::partial_dependence(
    fit = cforest_model,
    vars = c(tmp_in$names),
    interaction = int_vis
  )
  
  dt_out$part_depend <- edarf::plot_pd(tmp_part_dep)
  
  return(dt_out)

}
