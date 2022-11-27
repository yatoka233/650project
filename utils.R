#'
#' Perform a stepwise linear regression using F tests of significance.
#'
#' @param full.model the model containing all possible terms
#' @param initial.model the first model to consider
#' @param alpha.to.enter the significance level above which a variable may enter the model
#' @param alpha.to.leave the significance level below which a variable may be deleted from the model
#' @param data the data frame to use (optional, as with lm)
#'
#' @return the final model
#'
stepwise <- 
  function(full.model, initial.model, alpha.to.enter = 0.0, alpha.to.leave = 1.0, data = NULL) {
    # Sanity check: alpha.to.enter should not be greater than alpha.to.leave.
    if (alpha.to.enter > alpha.to.leave) {
      warning("Your alpha-to-enter is greater than your alpha-to-leave, which could throw the function into an infinite loop.\n")
      return(NA)
    }
    # Warning: horrible kludge coming!
    # Acquire the full and initial models as formulas. If they are
    # entered as formulas, convert them to get their environments
    # squared away.
    # Note: "showEnv = F" is necessary to avoid having an
    # environment identifier break things if the model is
    # defined inside a function.
    if (is.character(full.model)) {
      fm <- as.formula(full.model)
    } else {
      fm <- as.formula(capture.output(print(full.model, showEnv = F)))
    }
    if (is.character(initial.model)) {
      im <- as.formula(initial.model)
    } else {
      im <- as.formula(capture.output(print(initial.model, showEnv = F)))
    }
    # Deal with a missing data argument.
    if (is.null(data)) {
      # Catch the use of "." in a formula when the data argument is null.
      if ("." %in% all.vars(fm) | "." %in% all.vars(im)) {
        warning("In order to use the shortcut '.' in a formula, you must explicitly specify the data source via the 'data' argument.\n")
        return(NA)
      } else {
        # Use the parent environment.
        data <- parent.frame()
      }
    }
    # Fit the full model.
    full <- lm(fm, data);
    # Sanity check: do not allow an overspecified full model.
    if (full$df.residual < 1) {
      warning("Your full model does not have enough observations to properly estimate it.\n")
      return(NA)
    }
    msef <- (summary(full)$sigma)^2;  # MSE of full model
    n <- length(full$residuals);  # sample size
    # Fit the initial model.
    current <- lm(im, data);
    # Process consecutive models until we break out of the loop.
    while (TRUE) {
      # Summarize the current model.
      temp <- summary(current);
      # Print the model description.
      print(temp$coefficients);
      # Get the size, MSE and Mallow's cp of the current model.
      p <- dim(temp$coefficients)[1]; # size
      mse <- (temp$sigma)^2; # MSE
      cp <- (n - p)*mse/msef - (n - 2*p);  # Mallow's cp
      # Show the fit statistics.
      fit <- sprintf("\nS = %f, R-sq = %f, R-sq(adj) = %f, C-p = %f",
                     temp$sigma, temp$r.squared, temp$adj.r.squared, cp);
      # Show the fit itself.
      write(fit, file = "");
      write("=====", file = "");
      # Try to drop a term (but only if more than one is left).
      if (p > 1) {
        # Look for terms that can be dropped based on F tests.
        d <- drop1(current, test = "F");
        # Find the term with largest p-value.
        pmax <- suppressWarnings(max(d[, 6], na.rm = TRUE));
        # If the term qualifies, drop the variable.
        if (pmax > alpha.to.leave) {
          # We have a candidate for deletion.
          # Get the name of the variable to delete.
          var <- rownames(d)[d[,6] == pmax];
          # If an intercept is present, it will be the first name in the list.
          # There also could be ties for worst p-value.
          # Taking the second entry if there is more than one is a safe solution to both issues.
          if (length(var) > 1) {
            var <- var[2];
          }
          # Print out the variable to be dropped.
          write(paste("--- Dropping", var, "\n"), file = "");
          # Modify the formulat to drop the chosen variable (by subtracting it from the current formula).
          f <- formula(current);
          f <- as.formula(paste(f[2], "~", paste(f[3], var, sep = " - ")), env = environment(f));
          # Fit the modified model and loop.
          current <- lm(f, data);
          next;
        }
      }
      # If we get here, we failed to drop a term; try adding one.
      # Note: add1 throws an error if nothing can be added (current == full), which we trap with tryCatch.
      a <- tryCatch(
        add1(current, full, test = "F"),
        error = function(e) NULL
      );
      if (is.null(a)) {
        # There are no unused variables (or something went splat), so we bail out.
        break;
      }
      # Find the minimum p-value of any term (skipping the terms with no p-value). In case none of the remaining terms have a p-value (true of the intercept and any linearly dependent predictors), suppress warnings about an empty list. The test for a suitable candidate to drop will fail since pmin will be set to infinity.
      pmin <- suppressWarnings(min(a[, 6], na.rm = TRUE));
      if (pmin < alpha.to.enter) {
        # We have a candidate for addition to the model. Get the variable's name.
        var <- rownames(a)[a[,6] == pmin];
        # We have the same issue with ties and the presence of an intercept term, and the same solution, as above.
        if (length(var) > 1) {
          var <- var[2];
        }
        # Print the variable being added.
        write(paste("+++ Adding", var, "\n"), file = "");
        # Add it to the current formula.
        f <- formula(current);
        f <- as.formula(paste(f[2], "~", paste(f[3], var, sep = " + ")), env = environment(f));
        # Fit the modified model and loop.
        current <- lm(f, data = data);
        next;
      }
      # If we get here, we failed to make any changes to the model; time to declare victory and exit.
      break;
    }
    current
  }








Imputation <- function(raw_data, contin_idx, outcome_idx, cate_na_idx, cate_full_idx){
  ## this function only work for our raw_data ##
  
  ### Categorical data imputation
  # Mode
  cate_na_data <- raw_data[,cate_na_idx]
  
  mode_list <- c()
  for( i in cate_na_idx){
    mode_list <- c(mode_list, getmode(raw_data[,i]))
  }
  mode_list
  for( i in 1:ncol(cate_na_data)){
    ind=which(is.na(cate_na_data[,i]))
    cate_na_data[ind,i]=mode_list[i]
  }
  
  # head(cate_na_data,10)
  
  ### Continuous data imputation
  ## categorical to factor
  raw_data$GRADE <- as.factor(raw_data$GRADE)
  raw_data$USBORN <- as.factor(raw_data$USBORN)
  raw_data$MARSTAT4 <- as.factor(raw_data$MARSTAT4)
  raw_data$NKIDS4 <- as.factor(raw_data$NKIDS4)
  raw_data$KHYPER41 <- as.factor(raw_data$KHYPER41)
  raw_data$MDIAB41 <- as.factor(raw_data$MDIAB41)
  raw_data$NFRAC41 <- as.factor(raw_data$NFRAC41)
  raw_data$CC43 <- as.factor(raw_data$CC43)
  raw_data$U43S <- as.factor(raw_data$U43S)
  raw_data$HHA4 <- as.factor(raw_data$HHA4)
  raw_data$OO49LANG <- as.factor(raw_data$OO49LANG)
  raw_data$MALE <- as.factor(raw_data$MALE)
  raw_data$EE46 <- as.factor(raw_data$EE46)
  raw_data$HEALTH4 <- as.factor(raw_data$HEALTH4)
  ## 1. KNN
  contin_data_knn <- knnImputation(raw_data[-c(1,21)])[,contin_idx-1]
  cate_na_data_knn <- knnImputation(raw_data[-c(1,21)])[,cate_na_idx-1]
  # summary(contin_data_knn)
  
  ## 2. Mice
  # tmp <- mice(raw_data[-c(1,21)],m=5,maxit=50,meth='pmm',seed=500)
  # contin_data_mice <- complete(tmp,1)[,contin_idx-1]
  # summary(contin_data_mice)
  
  ## 3. Mean
  contin_data_mean <- raw_data[,contin_idx]
  for(i in c(1:4)){
    contin_data_mean[,i] <- impute(contin_data_mean[,i], mean)  # 均值替代
  }
  # summary(contin_data_mean)
  
  ## 4. Median
  contin_data_median <- raw_data[,contin_idx]
  for(i in c(1:4)){
    contin_data_median[,i] <- impute(contin_data_median[,i], median)  # 中位数替代
  }
  # summary(contin_data_median)
  
  
  ### Outcome NA
  outcome_na_idx <- which(is.na(raw_data[outcome_idx]))
  
  return(list(outcome=raw_data[outcome_idx], 
              cate_full=raw_data[cate_full_idx], 
              cate_na=cate_na_data, 
              cate_na_knn=cate_na_data_knn, 
              contin_data_knn=contin_data_knn,
              # contin_data_mice=contin_data_mice,
              contin_data_mean=contin_data_mean,
              contin_data_median=contin_data_median,
              outcome_na_idx=outcome_na_idx))
}



getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

















