variance <- function(models) {
 
 # create a matrix to store mean + 95% CI estimates
 vpc <- matrix(nrow = length(models), ncol = 3)
 # define row/column names for clarity
 colnames(vpc) <- c("Mean VPC", "Lower CI", "Upper CI")
 rownames(vpc) < c("M1", "M2")
 
 # create matrix to store SD and variance values
 var_sd <- matrix(nrow = length(models), ncol = 2)
 # define row/column names for clarity
 colnames(var_sd) <- c("Variance", "SD")
 rownames(var_sd) <- c("M1", "M2")
 
 # loop through each model fit to extract/summarize var, SD, & VPC values
 for (i in 1:length(models)) {
  # using one stanfit object at a time
  model <- models[[i]]
  # extract posterior distribution of model standard deviation (SD)
  sd <- rstan::extract(model, pars = "sd_1")
  sd <- sd$sd_1
  # convert from SD to variance (sigma2)
  sigma2 <- sd^2
  
  # store variance and SD means
  var_sd[i,1] <- mean(sigma2)
  var_sd[i,2] <- mean(sd)
  
  # calculate full posterior distribution of VPC values
  fullPosterior <- sigma2/(sigma2+((pi^2)/3))*100
  # summarize VPC as mean and 95% CI (rounded to 2 decimal places)
  vpc[i,1] <- round(mean(fullPosterior),2)
  vpc[i,2:3] <- round(quantile(fullPosterior, 
                               prob = c(0.025, 0.975)),2)
 }
 
 # calculate proportional change in variance (PCV) relative to Model 1
 pcv <- matrix(nrow = nrow(vpc), ncol = 1)
 # define row/column names for clarity
 colnames(pcv) <- c("PCV")
 rownames(pcv) <- c("M1", "M2")
 # loop through Variance values to calculate PCV from Model 1 to 2
 for (j in 2:nrow(var_sd)) {
  pcv[j,1] <- round(((var_sd[1,1] - var_sd[j,1]) / var_sd[1,1])*100,2)
 }
 
 # save output into a list
 output <- list(var_sd, vpc, pcv)
 names(output) <- c("var_sd", "vpc", "pcv")
 
 # return the output file
 return(output)
}