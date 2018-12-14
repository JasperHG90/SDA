#' Create a stratified sample of size n
#' 
#' If optim=TRUE, the function will calculate optimal allocation. Else, it calculates proportion-to-size strata.
#' 
#' @param data dataset from which to sample
#' @param var stratification variable. Must be a factor / character with H unique variables where H > 1
#' @param n sample size
#' @param seed seed to use to sample from data
#' @param ... optional arguments. Must be one of the following:
#' \itemize{
#'   \item{optim: }{boolean. If TRUE, the function will use Neyman allocation perform optimal allocation}
#'   \item{optimVar: }{character. Name of the variable used to create optimal design}
#' }
#' 
#' @return Subset of data with H strata
stratify <- function(data, var, n, seed, ...) {
  
  if(is.null(seed)) {
    
    seed <- sample(1:10000, 1)
    
  }
  
  # Checks 
  if(!var %in% names(data)) {
    
    stop(paste0("Variable ", var, " not found in data"))
    
  }
  if(n > nrow(data)) {
    
    stop("Sample size larger than population size")
    
  }
  
  # To data frame
  data <- as.data.frame(data)
  
  # If var is factor, save levels and put in character
  if("factor" %in% is(unlist(data[,var]))) {
    
    lvls <- levels(unlist(data[,var]))
    data[,var] <- as.character(data[,var])
    
  }
  
  # If not optim=TRUE in optional arguments, sample proportionally
  opts <- list(...)
  if("optim" %in% names(opts)) {
    
    optim <- opts$optim
    optimVar <- opts$optimVar
    
    # If Null, raise error
    if(is.null(optimVar)) {
      
      stop("Optimization variable not passed by user")
      
    }
    
  } else {
    
    optim <- NULL
    
  }
  
  # Get unique values for the stratifying variable
  vals <- unique(data[,var])
  
  # Sample from data ----
  
  # Two scenarios
  #  1. No additional options supplied --> proportional to size sampling
  #  2. optim passed --> optimal allocation
  
  if(is.null(optim)) {
    
    # For each group, get the number of observations
    props <- table(data[,var]) / sum(table(data[,var]))
    
    # Reorder data by reference
    data <- data[order(match(data[,var], names(props))),]
    
    # Stratify
    set.seed(seed)
    str <- sampling::strata(data, stratanames = var, size = unname(props) * n)
    
    # Remove stratification variable
    str <- str[,setdiff(names(str), var)]
    
    # Bind to data 
    data$uuid <- 1:nrow(data)
    
    # Merge
    merged <- merge(data, str, by.x="uuid", by.y="ID_unit")
    
    # Add fpc 
    fpc <- data[,var] %>%
      data_frame("strt" = .) %>%
      group_by(strt) %>%
      summarize(fpc = n()) 
    
    merged <- merge(merged, fpc, by.x=var, by.y = "strt")  
    
  } else {
    
    # Calculate optimum allocation
    
    # Get the number of observations for each group
    N_h <- table(data[,var])
    # Calculate the variances for each group
    V_h <- vapply(names(N_h), function(x) var(data[data[,var] == x,][,optimVar]), 1)
    
    # Calculate optimum strata sizes
    props <- neyman(unname(N_h), unname(V_h))
    names(props) <- names(N_h)

    # Reorder data by reference
    data <- data[order(match(data[,var], names(props))),]
    
    # Stratify
    set.seed(seed)
    str <- sampling::strata(data, stratanames = var, size = unname(props) * n)
    
    # Remove stratification variable
    str <- str[,setdiff(names(str), var)]
    
    # Bind to data 
    data$uuid <- 1:nrow(data)
    
    # Merge
    merged <- merge(data, str, by.x="uuid", by.y="ID_unit")
    
    # Add fpc 
    fpc <- data[,var] %>%
      data_frame("strt" = .) %>%
      group_by(strt) %>%
      summarize(fpc = n()) 
    
    merged <- merge(merged, fpc, by.x=var, by.y = "strt")  
    
  }
    
  # Return
  return(merged)
    
}

#' Neyman allocation given H strata
#' 
#' @param N_h population size of Hth stratum
#' @param V_h variance of Hth stratum
#' 
#' @return Proportions for each stratum
neyman <- function(N_h, V_h) {
  
  (N_h * V_h) / sum(N_h * V_h)
  
}

#' Given a threshold > 0 (threshold = Coefficient of Variation), calculate the sample size needed to have a CV of that value
#' 
#' This function uses only proportion-to-size stratified sampling
#' 
#' @param data dataset from which to draw stratified samples
#' @param strat_var stratification variable
#' @param start sample size from which to start
#' @param stop sample size at which to stop
#' @param by step size to go from start --> stop. Defaults to 10
#' @param threshold Coefficient of Variation (CV) that counts as a stopping condition
#' 
#' @return List containing:
#' \itemize{
#'    \item{final_n: }{Sample size needed to achieve desired CV}
#'    \item{data: }{data frame containing sample sizes and values for CV at each step}
#'    \item{plot: }{plot showing the progression from of the CV over the sample sizes n}
#' }
required_sample_size <- function(data, strat_var, start, stop, by = 10, threshold = 0.01) {
  
  ## Assertions
  if(by < 1) {
    stop("'by' must be larger than 0")
  } else if(by >= (stop - start)) {
    stop("'by' cannot be larger than the interval from 'start' to 'stop'")
  }
  if(threshold <= 0) {
    stop("'threshold' cannot be lower than or equal to 0")
  }
  
  ## Sequence for start to stop
  sample_sizes <- seq(start, stop, by)
  
  ## For results
  CV_out <- c()
  ## For each sample size, do ...
  for(n in sample_sizes) {
    
    ## Stratified proportion-to-size sample of size i
    sample_stratum <- stratify(data, strat_var, n=n, seed=NULL)
    
    ## Define stratified design
    stratdesign <- svydesign(ids=sample_stratum$uuid, 
                             fpc=~fpc, 
                             strata = ~Stratum,
                             data = sample_stratum)
    
    ## Save the mean 
    mean <- svymean(~DiffMeanHourlyPercent, design=stratdesign)
    
    ## Calculate the SE
    se <- SE(mean)[1]
    
    ## Coef. of variation
    CV <- se / mean[1]
    
    ## Append values
    CV_out <- c(CV_out, CV)
    
    ## Print
    if(n %% 50 == 0){
      print(paste0("CV for n=", n, ": ", CV))
    }
    
    ## If threshold
    if(CV < threshold){
      
      print(paste0("Found sample size at n=",n, " (", round(CV, digits=4), ")"))
      break
      
    }
    
    ## If not found
    if(n == sample_sizes[length(sample_sizes)]) {
      
      warning(paste0("Did not find a sample size for CV ", threshold,". Best value: ", round(CV, digits=4), " at sample size ", n))
      
    }
    
  }
  
  # Put values in data frame and plot
  data_out <- data.frame(
    "n" = sample_sizes[1:which(sample_sizes == n)],
    "CV" = CV_out
  )
  
  # Plot
  p <- ggplot2::ggplot(data_out, ggplot2::aes(x=n, y=CV)) +
    ggplot2::geom_line(size=1.1, color="blue", linetype=1) +
    ggplot2::theme_bw() +
    ggplot2::scale_x_continuous(name = "Sample size") +
    ggplot2::scale_y_continuous(name = "Coefficient of Variation")
  
  plot(p)
  
  # Return
  return(
    list(
      "final_n" = n,
      "data" = data_out,
      "plot" = p
    )
  )
  
}
