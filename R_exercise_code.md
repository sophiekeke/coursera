https://www.coursera.org/learn/crash-course-in-causality/home/week/3

Coursera home work input file is incorrect use this file instead https://github.com/robjellis/lalonde/blob/master/lalonde_data.csv
And use their R lab environment to complete the exercise, click checkbox in package for corresponding package you need for assignment, and it will load directly into the R lab environment.

```r
> getwd()
[1] "/home/rstudio"
> data <- read.csv(file="/home/rstudio/lalonde_data_final.csv", header=TRUE, sep=",")

> new_column_names <- c("ID","treat","age", "educ", "black", "hispan", "married", "nodegree", "re74", "re75", "re78")
> colnames(data) <- new_column_names
> vars <- c("married") # Focus on the 'married' variable

> group <- "treat" # Use the actual binary grouping variable in your dataset
> 

> tableOne <- CreateTableOne(vars = vars, data = data, strata = group, factorVars = "married")
> print(tableOne, smd = TRUE)
                 Stratified by treat
                  0           1           p      test SMD   
  n               429         185                           
  married = 1 (%) 
> mean_treated <- mean(data$re78[data$treat == 1])
> 
> # Mean of real earnings in 1978 for untreated subjects
> mean_untreated <- mean(data$re78[data$treat == 0])
> 
> # Raw mean difference
> raw_mean_difference <- mean_treated - mean_untreated
> 
> raw_mean_difference

> propensity_model <- glm(treat ~ age + educ + black + hispan + married + nodegree + re74 + re75, 
+                         data = data, family = binomial())
> 
> # Obtain the propensity scores (predicted probabilities of receiving the treatment)
> data$propensity_score <- predict(propensity_model, type = "response")
> 
> # View the first few propensity scores
> head(data$propensity_score)
[1] 0.6387699 0.2246342 0.6782439 0.7763241 0.7016387 0.6990699
> psmodel <- glm(treat ~ age + educ + black + hispan + married + nodegree + re74 + re75,
+                data = data, family = binomial(link = "logit"))
> 
> # Calculate the propensity score for each subject
> ps <- predict(psmodel, type = "response")
> 
> # You can then add the propensity scores to your original data frame
> data$propensity_score <- ps
> 
> # To view the propensity scores you can look at the head of the dataframe
> head(data$propensity_score)
[1] 0.6387699 0.2246342 0.6782439 0.7763241 0.7016387 0.6990699
> psmodel <- glm(treat ~ age + educ + black + hispan + married + nodegree + re74 + re75,
+                family  = binomial(link ="logit"))
Error in eval(predvars, data, env) : object 'treat' not found
> psmodel <- glm(treat ~ age + educ + black + hispan + married + nodegree + re74 + re75,
+                data = data ,family  = binomial(link ="logit"))
> ps <-predict(psmodel, type = "response")
> min_propensity_score <- min(data$propensity_score)
> 
> # Calculate the maximum propensity score
> max_propensity_score <- max(data$propensity_score)
> 
> # Print the minimum and maximum propensity scores
> cat("Minimum Propensity Score:", min_propensity_score, "\n")

> cat("Maximum Propensity Score:", max_propensity_score, "\n")

> match_out <- Match(Y = data$treat, Tr = data$treat, X = data$propensity_score, replace = FALSE)
> 
> matched_treated_indices <- match_out$index.treated[!is.na(match_out$index.control)]
> matched_control_indices <- match_out$index.control[!is.na(match_out$index.control)]
> 
> # Create a new data frame for matched pairs
> matched_data <- rbind(data[matched_treated_indices,], data[matched_control_indices,])
> # Calculate means for "married"
> mean_married_treated <- mean(matched_data$married[matched_data$treat == 1], na.rm = TRUE)
> mean_married_control <- mean(matched_data$married[matched_data$treat == 0], na.rm = TRUE)
> 
> # Calculate standard deviations for "married"
> sd_married_treated <- sd(matched_data$married[matched_data$treat == 1], na.rm = TRUE)
> sd_married_control <- sd(matched_data$married[matched_data$treat == 0], na.rm = TRUE)
> 
> # Calculate pooled standard deviation
> n_treated <- sum(!is.na(matched_data$married[matched_data$treat == 1]))
> n_control <- sum(!is.na(matched_data$married[matched_data$treat == 0]))
> sd_pooled_married <- sqrt(((n_treated - 1) * sd_married_treated^2 + (n_control - 1) * sd_married_control^2) / (n_treated + n_control - 2))
> 
> # Calculate standardized difference for "married"
> std_diff_married <- (mean_married_treated - mean_married_control) / sd_pooled_married
> 
> # Print the standardized difference for "married"
> cat("Standardized Difference for 'Married':", round(std_diff_married, 3), "\n")



> # Function to calculate pooled standard deviation and standardized difference
> calculate_sd <- function(data, var_name) {
+     mean_treated <- mean(data[[var_name]][data$treat == 1], na.rm = TRUE)
+     mean_control <- mean(data[[var_name]][data$treat == 0], na.rm = TRUE)
+     sd_treated <- sd(data[[var_name]][data$treat == 1], na.rm = TRUE)
+     sd_control <- sd(data[[var_name]][data$treat == 0], na.rm = TRUE)
+     
+     n_treated <- sum(!is.na(data[[var_name]][data$treat == 1]))
+     n_control <- sum(!is.na(data[[var_name]][data$treat == 0]))
+     sd_pooled <- sqrt(((n_treated - 1) * sd_treated^2 + (n_control - 1) * sd_control^2) / (n_treated + n_control - 2))
+     
+     std_diff <- (mean_treated - mean_control) / sd_pooled
+     return(std_diff)
+ }
> # Calculate standardized differences for specified variables
> std_diff_nodegree <- calculate_sd(matched_data, "nodegree")
> std_diff_re74 <- calculate_sd(matched_data, "re74")
> std_diff_age <- calculate_sd(matched_data, "age")
> std_diff_black <- calculate_sd(matched_data, "black")
> 
> # Print the standardized differences
> cat("Standardized Difference for 'Nodegree':", round(std_diff_nodegree, 3), "\n")

> cat("Standardized Difference for 'RE74':", round(std_diff_re74, 3), "\n")

> cat("Standardized Difference for 'Age':", round(std_diff_age, 3), "\n")

> cat("Standardized Difference for 'Black':", round(std_diff_black, 3), "\n")


In comparing standardized differences (SMDs) across variables to identify which has the largest difference, the magnitude of the difference is what matters, not the direction. Therefore, you should consider the absolute values of the SMDs for comparison. The sign of the SMD indicates the direction of the difference between the groups (whether the mean of the treated group is higher or lower than the control group), but the magnitude (absolute value) indicates the size of the difference regardless of direction.


> set.seed(931139)
> # Perform the matching with a caliper of 0.1
> match_out_with_caliper <- Match(Y = data$treat, Tr = data$treat, X = data$propensity_score, replace = FALSE, caliper = 0.1)
> 
> # Extract indices of matched treated and control units
> matched_treated_indices_caliper <- match_out_with_caliper$index.treated[!is.na(match_out_with_caliper$index.control)]
> matched_control_indices_caliper <- match_out_with_caliper$index.control[!is.na(match_out_with_caliper$index.control)]
> 
> # Create a new data frame for matched pairs using the caliper
> matched_data_caliper <- rbind(data[matched_treated_indices_caliper,], data[matched_control_indices_caliper,])
> 
> # Now you can proceed to analyze this matched dataset, for example, by calculating standardized differences again.
> # Calculate the number of matched pairs
> num_matched_pairs_caliper <- length(matched_treated_indices_caliper)
> 
> # Print the number of matched pairs
> cat("Number of Matched Pairs with Caliper 0.1:", num_matched_pairs_caliper, "\n")

> 

> # Calculate the mean of re78 for treated subjects
> mean_re78_treated <- mean(matched_data_caliper$re78[matched_data_caliper$treat == 1], na.rm = TRUE)
> 
> # Calculate the mean of re78 for untreated subjects
> mean_re78_untreated <- mean(matched_data_caliper$re78[matched_data_caliper$treat == 0], na.rm = TRUE)
> 
> # Calculate the difference in means
> mean_diff_re78 <- mean_re78_treated - mean_re78_untreated
> 
> # Print the result
> cat("Mean of real earnings in 1978 for treated subjects minus mean for untreated subjects:", mean_diff_re78, "\n")

> earnings_treated <- matched_data_caliper$re78[matched_data_caliper$treat == 1]
> earnings_untreated <- matched_data_caliper$re78[matched_data_caliper$treat == 0]
> 
> # Conduct a paired t-test
> paired_t_test_results <- t.test(earnings_treated, earnings_untreated, paired = TRUE)
> 
> # Extract the 95% confidence interval
> ci_95 <- paired_t_test_results$conf.int
> 
> # Print the 95% confidence interval
> cat("95% Confidence Interval for the mean difference in earnings:", ci_95, "\n")

> getwd()
[1] "/home/rstudio"
> data <- read.csv(file="/home/rstudio/lalonde_data_final.csv", header=TRUE, sep=",")
> new_column_names <- c("ID","treat","age", "educ", "black", "hispan", "married", "nodegree", "re74", "re75", "re78")
> colnames(data) <- new_column_names
> propensity_model <- glm(treat ~ age + educ + black + hispan + married + nodegree + re74 + re75, 
+                         data = data, family = binomial(link = "logit"))
> 
> # Obtain the propensity score for each subject
> data$propensity_score <- predict(propensity_model, type = "response")
> data$iptw <- ifelse(data$treat == 1, 
+                     1 / data$propensity_score, 
+                     1 / (1 - data$propensity_score))
> 
> # Find the minimum IPTW
> min_iptw <- min(data$iptw)
> 
> # Find the maximum IPTW
> max_iptw <- max(data$iptw)
> 
> # Print the minimum and maximum IPTW
> cat("Minimum IPTW:", min_iptw, "\n")
Minimum IPTW: 
> cat("Maximum IPTW:", max_iptw, "\n")
Maximum IPTW: 
> 

> # Calculate weighted means for 'nodegree'
> weighted_mean_nodegree_treat <- with(data[data$treat == 1,], sum(nodegree * iptw) / sum(iptw))
> weighted_mean_nodegree_control <- with(data[data$treat == 0,], sum(nodegree * iptw) / sum(iptw))
> 
> # Calculate weighted variances for 'nodegree'
> weighted_var_nodegree_treat <- with(data[data$treat == 1,], sum(iptw * (nodegree - weighted_mean_nodegree_treat)^2) / sum(iptw))
> weighted_var_nodegree_control <- with(data[data$treat == 0,], sum(iptw * (nodegree - weighted_mean_nodegree_control)^2) / sum(iptw))
> 
> # Calculate effective sample sizes
> eff_n_treat <- sum(data[data$treat == 1,]$iptw)^2 / sum((data[data$treat == 1,]$iptw)^2 / length(data[data$treat == 1,]$iptw))
> eff_n_control <- sum(data[data$treat == 0,]$iptw)^2 / sum((data[data$treat == 0,]$iptw)^2 / length(data[data$treat == 0,]$iptw))
> 
> # Calculate pooled weighted standard deviation
> pooled_weighted_sd_nodegree <- sqrt(((eff_n_treat - 1) * weighted_var_nodegree_treat + (eff_n_control - 1) * weighted_var_nodegree_control) / (eff_n_treat + eff_n_control - 2))
> 
> # Calculate standardized difference
> std_diff_nodegree_weighted <- (weighted_mean_nodegree_treat - weighted_mean_nodegree_control) / pooled_weighted_sd_nodegree
> 
> # Print the standardized difference for 'nodegree' and use the abosolute value
> cat("Weighted Standardized Difference for 'nodegree':", std_diff_nodegree_weighted, "\n")
Weighted Standardized Difference for 'nodegree': 
> abs_std_diff_nodegree_weighted <- abs(std_diff_nodegree_weighted)
> 
> # Print the absolute value of the standardized difference for 'nodegree'
> cat("Absolute Weighted Standardized Difference for 'nodegree':", abs_std_diff_nodegree_weighted, "\n")
Absolute Weighted Standardized Difference for 'nodegree': 

> design_iptw <- svydesign(ids = ~1, data = data, weights = ~iptw)
> # Estimate the effect of treatment on the outcome variable re78 using svyglm
> model_ace <- svyglm(re78 ~ treat, design = design_iptw)
> 
> # Extract the coefficient for the treatment variable
> estimate <- coef(model_ace)["treat"]
> 
> # Extract the 95% confidence interval for the treatment effect
> ci <- confint(model_ace, level = 0.95)["treat", ]
> 
> # Display the estimate of the average causal effect and its confidence interval
> cat("Estimate of Average Causal Effect (ACE) on re78:", estimate, "\n")
Estimate of Average Causal Effect (ACE) on re78: 224.6763 
> cat("95% Confidence Interval for ACE on re78:", ci, "\n")
95% Confidence Interval for ACE on re78: 
> 
> # Estimate from svyglm for the treatment effect
> estimate <- coef(model_ace)["treat"]
> 
> # Standard error for the treatment effect estimate
> std_error <- sqrt(vcov(model_ace)["treat", "treat"])

> # Lower and upper bounds of the 95% CI, the above result doesn't match answer 100%, so switch the critical value to 1.96 as z statistic distribution and calculate it manually, then the answer matches. calculate the confidence interval using Zα/2=1.96Zα/2=1.96 for a 95
> lower_bound <- estimate - 1.96 * std_error
> upper_bound <- estimate + 1.96 * std_error
> 
> # Display the manually calculated confidence interval
> cat("Manually Calculated 95% CI for ACE on re78: [", lower_bound, ",", upper_bound, "]\n")
Manually Calculated 95% CI for ACE on re78: 

> # check the coursera lab package for ipw, and then use the inbulit trunc function. Estimate propensity scores and create IPTW with truncation
> weight_model <- ipwpoint(exposure = treat, family = "binomial", link = "logit",
+                          denominator = ~ age + educ + black + hispan + married + nodegree + re74 + re75,
+                          data = data, trunc = 0.01)
> 
> # Extract the truncated weights
> data$iptw_truncated <- weight_model$weights
> 
> # Create a survey design object with the truncated IPTW
> design_iptw_truncated <- svydesign(ids = ~1, data = data, weights = ~iptw_truncated)
> 
> # Fit a survey generalized linear model to estimate the ACE using truncated weights
> model_ace_truncated <- svyglm(re78 ~ treat, design = design_iptw_truncated)
> 
> # Extract the coefficient (ACE estimate) and calculate its 95% confidence interval
> estimate_truncated <- coef(model_ace_truncated)["treat"]
> ci_truncated <- confint(model_ace_truncated, level = 0.95)["treat", ]
> 
> # Output the ACE estimate and its confidence interval
> cat("ACE Estimate with Truncated Weights:", estimate_truncated, "\n")
ACE Estimate with Truncated Weights: 
> cat("95% CI for ACE with Truncated Weights:", ci_truncated, "\n")
95% CI for ACE with Truncated Weights: 

'''




