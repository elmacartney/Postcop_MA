
### 1 ####
# contrast function
# takes: 1) data frame, 2) moderator of interest, and 3) VCV matrix
contrast_fun <- function(data, response, moderator, VCV){
  
  mod <- deparse(substitute(moderator))
  resp <- deparse(substitute(response))
  
  # get names of different levels of a categorical variable 
  names <- levels(as.factor(data[[mod]]))
  moderator <- as.factor(data[[mod]])
  
  # function for running a meta-regression
  run_rma <- function(name) {
    rma.mv(yi = data[[resp]], 
           V = VCV, 
           mods = ~ relevel(moderator, ref = name),
           random = list(~1|Study_ID, ~1|Effectsize_ID,~1|Species, ~1|Phylogeny),
           test = "t",
           data = dat,
           sparse = TRUE)
  }
  # running all teh models to get all the contrasts
  contra <- map(names[-length(names)], run_rma)
}


### 2####
# function to get relevant estimates from normal and contast models
# takes: 1) normal model output 2) contrast model outputs, and 3) moderator of interest, 
get_estimate <- function(model, contra, moderator) {
  
  # making itto the name
  mod <- deparse(substitute(moderator))
  
  # getting estimations 
  get_pred1 <- function (model, mod =  mod) {
    name <- name <- firstup(as.character(stringr::str_replace(row.names(model$beta), mod, "")))
    len <- length(name)
    newdata <- matrix(NA, ncol = len, nrow = len)
    for (i in 1:len) {
      pos <- which(model$X[, i] == 1)[[1]]
      newdata[, i] <- model$X[pos, ]
    }
    pred <- metafor::predict.rma(model, newmods = newdata)
    
    estimate <- pred$pred
    lowerCL <- pred$ci.lb
    upperCL <- pred$ci.ub 
    lowerPR <- pred$cr.lb
    upperPR <- pred$cr.ub 
    
    table <- tibble(name = factor(name, levels = name, labels = name), estimate = estimate,
                    lowerCL = lowerCL, upperCL = upperCL,
                    pval = model$pval,
                    lowerPR = lowerPR, upperPR = upperPR)
    table
  }
  # something has changed here
  # get estiamtions from normal model
  get_pred2 <- function (model) {
    name <- as.factor(str_replace(row.names(model$beta), 
                                  paste0("relevel", "\\(", "moderator, ref = name", "\\)"),
                                  ""))
    len <- length(name)
    newdata <- diag(len)
    pred1 <- data.frame(predict.rma(model, newmods = newdata[1,-1]))
    pred2 <- data.frame(predict.rma(model, intercept = FALSE, newmods = newdata[-1,-1]))
    pred <- rbind(pred1, pred2)
    
    estimate <- pred$pred
    lowerCL <- pred$ci.lb
    upperCL <- pred$ci.ub 
    lowerPR <- pred$cr.lb
    upperPR <- pred$cr.ub 
    
    table <- tibble(name = factor(name, levels = name, labels = name), 
                    estimate = estimate,
                    lowerCL = lowerCL, 
                    upperCL = upperCL,
                    pval = model$pval,
                    lowerPR = lowerPR, upperPR = upperPR)
    table
  }
  
  # getting contrasts name
  cont_gen <- function(name) {
    combination <- combn(name, 2)
    name_dat <- t(combination)
    names <- paste(name_dat[, 1], name_dat[, 2], sep = "-")
    return(names)
  }
  
  # put into a proper tale
  mr_results <- function(res1, res2) {
    restuls <-tibble(
      Levels = c(as.character(res1$name), cont_gen(res1$name)),
      Estimate = c(res1$estimate, res2$estimate),
      Lower_CI = c(res1$lowerCL, res2$lowerCL),
      Upper_CI = c(res1$upperCL, res2$upperCL),
      P_value = c(res1$pval, res2$pval),
      Lower_PI = c(res1$lowerPR, res2$lowerPR),
      Upper_PI = c(res1$upperPR, res2$upperPR),
    )
    restuls
  }
  
  res1 <- get_pred1(model, mod = mod)
  
  # applying the estimation function for each model output
  estiamtes <- map(contra, ~ get_pred2(.x))
  
  # a list of the numbers to take out unnecessary contrasts
  contra_list <- Map(seq, from=1, to =1:length(contra))
  
  # you need to flatten twice: first to make it a list and make it a vector
  # I guess we can make it a loop (or vectorise)
  res2 <- map2_dfr(estiamtes, contra_list, ~.x[-(.y), ])   
  
  # putting two results together
  
  results <-  mr_results(res1, res2)
}
