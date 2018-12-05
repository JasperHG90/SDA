#' Create a stratified sample
#' 
#' @param 
#' @param 
#' @param 
#' 
#' @return 
#' @seealso 

stratify <- function(data, var, n, seed, ...) {
  
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
#' @param 
#' @param 
#' 
#' @return 
#' @seealso 

neyman <- function(N_h, V_h) {
  
  (N_h * V_h) / sum(N_h * V_h)
  
}
