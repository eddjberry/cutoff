
#==============================================================================
# Standard version: uses signif(), defaults for p-values
#==============================================================================

cutoff = function(x, upper = 1, lower = 0.001, sig = 2, return.num = FALSE, 
                  lead.zero = FALSE, equal.sign = TRUE) {
  y = NULL
  z = NULL
  
  if (lead.zero == FALSE) {
    s = 2
  } else {
    s = 1
  }
  
  for(i in seq_along(x)) {
    if(abs(x[i]) < lower) {
      y[i] = paste("<", substring(as.character(lower), s), sep = " ") # substring of a character used to get .001 rather than 0.001
      z[i] = substring(as.character(lower), s)
      
    } else if(abs(x[i]) > upper & upper < 1) {
      y[i] = paste(">", substring(as.character(upper), s), sep = " ")
      z[i] = substring(as.character(upper), s)
      
    } else if(abs(x[i]) > upper & upper >= 1) {
      y[i] = paste(">", substring(as.character(upper), 1), sep = " ")
      z[i] = substring(as.character(upper), 1)
      
    } else {
      y[i] = paste("=", substring(as.character(signif(x[i], sig)), s), sep = " ")
      z[i] = substring(as.character(signif(x[i], sig)), s)
    }
  }
  
  if (return.num == FALSE & equal.sign == TRUE) {
    return(y)
    
  } else if(equal.sign == FALSE) {
    for(j in seq_along(y)) {
      if(substring(y[j], 1, 1) == "=") {
        y[j] <- substring(y[j],3)
      }
    }
    return(y)
    
  } else if(return.num == TRUE) {
    return(z)
  }
}

#==============================================================================
# Defaults set up for Beta values
#=============================================================================#

cutoff_beta = function(x, upper = 1, lower = 0.0001, sig = 2, return.num = FALSE, 
                       lead.zero = TRUE, equal.sign = TRUE) {
  
  y = NULL
  z = NULL
  
  if (lead.zero == FALSE) {
    s = 2
  } else {
    s = 1
  }
  
  for(i in seq_along(x)) {
    if(abs(x[i]) < lower) {
      y[i] = paste("<", substring(as.character(lower), s), sep = " ") # substring of a character used to get .001 rather than 0.001
      z[i] = substring(as.character(lower), s)
      
    } else if(abs(x[i]) > upper & upper < 1) {
      y[i] = paste(">", substring(as.character(upper), s), sep = " ")
      z[i] = substring(as.character(upper), s)
      
    } else if(abs(x[i]) > upper & upper >= 1) {
      y[i] = paste(">", substring(as.character(upper), 1), sep = " ")
      z[i] = substring(as.character(upper), 1)
      
    } else {
      y[i] = paste("=", substring(as.character(signif(x[i], sig)), s), sep = " ")
      z[i] = substring(as.character(signif(x[i], sig)), s)
    }
  }
  
  if (return.num == FALSE & equal.sign == TRUE) {
    return(y)
    
  } else if(equal.sign == FALSE) {
    for(j in seq_along(y)) {
      if(substring(y[j], 1, 1) == "=") {
        y[j] <- substring(y[j],3)
      }
    }
    return(y)
    
  } else if(return.num == TRUE) {
    return(z)
  }
}

#==============================================================================
# Defaults arguments for R-squared
#==============================================================================

cutoff_Rsq = function(x, upper = 1, lower = 0.01, sig = 2, return.num = FALSE, 
                      lead.zero = FALSE, equal.sign = TRUE) {
  y = NULL
  z = NULL
  
  if (lead.zero == FALSE) {
    s = 2
  } else {
    s = 1
  }
  
  for(i in seq_along(x)) {
    if(abs(x[i]) < lower) {
      y[i] = paste("<", substring(as.character(lower), s), sep = " ") # substring of a character used to get .001 rather than 0.001
      z[i] = substring(as.character(lower), s)
      
    } else if(abs(x[i]) > upper & upper < 1) {
      y[i] = paste(">", substring(as.character(upper), s), sep = " ")
      z[i] = substring(as.character(upper), s)
      
    } else if(abs(x[i]) > upper & upper >= 1) {
      y[i] = paste(">", substring(as.character(upper), 1), sep = " ")
      z[i] = substring(as.character(upper), 1)
      
    } else {
      y[i] = paste("=", substring(as.character(signif(x[i], sig)), s), sep = " ")
      z[i] = substring(as.character(signif(x[i], sig)), s)
    }
  }
  
  if (return.num == FALSE & equal.sign == TRUE) {
    return(y)
    
  } else if(equal.sign == FALSE) {
    for(j in seq_along(y)) {
      if(substring(y[j], 1, 1) == "=") {
        y[j] <- substring(y[j],3)
      }
    }
    return(y)
    
  } else if(return.num == TRUE) {
    return(z)
  }
}

#==============================================================================
# Cutoff2: uses round rather than signif(), defaults for percentages
#==============================================================================

cutoff2 = function(x, upper = 100, lower = 0.01, dp = 1, return.num = FALSE, 
                   lead.zero = TRUE, equal.sign = TRUE) {
  
  y = NULL
  z = NULL
  
  if (lead.zero == FALSE) {
    s = 2
  } else {
    s = 1
  }
  
  for(i in seq_along(x)) {
    if(abs(x[i]) < lower) {
      y[i] = paste("<", substring(as.character(lower), s), sep = " ") # substring of a character used to get .001 rather than 0.001
      z[i] = substring(as.character(lower), s)
      
    } else if(abs(x[i]) > upper & upper < 1) {
      y[i] = paste(">", substring(as.character(upper), s), sep = " ")
      z[i] = substring(as.character(upper), s)
      
    } else if(abs(x[i]) > upper & upper >= 1) {
      y[i] = paste(">", substring(as.character(upper), 1), sep = " ")
      z[i] = substring(as.character(upper), 1)
      
    } else {
      y[i] = paste("=", substring(as.character(round(x[i], dp)), s), sep = " ")
      z[i] = substring(as.character(round(x[i], dp)), s)
    }
  }
  
  if (return.num == FALSE & equal.sign == TRUE) {
    return(y)

  } else if(equal.sign == FALSE) {
    for(j in seq_along(y)) {
      if(substring(y[j], 1, 1) == "=") {
        y[j] <- substring(y[j],3)
      }
    }
    return(y)
    
  } else if(return.num == TRUE) {
    return(z)
  }
}

#==============================================================================
# Adaptation of cutoff2 with defaults for a Bayes Factor
#==============================================================================

cutoff_BF = function(x, upper = 1000, lower = 0.001, dp = 1, sig =2, return.num = FALSE, 
                     lead.zero = TRUE, equal.sign = TRUE) {
  
  y = NULL
  z = NULL
  
  if (lead.zero == FALSE) {
    s = 2
  } else {
    s = 1
  }
  
  for(i in seq_along(x)) {
    if(abs(x[i]) < lower) {
      y[i] = paste("<", substring(as.character(lower), s), sep = " ") # substring of a character used to get .001 rather than 0.001
      z[i] = substring(as.character(lower), s)
      
    } else if(abs(x[i]) > upper & upper < 1) {
      y[i] = paste(">", substring(as.character(upper), s), sep = " ")
      z[i] = substring(as.character(upper), s)
      
    } else if(abs(x[i]) > upper & upper >= 1) {
      y[i] = paste(">", substring(as.character(upper), 1), sep = " ")
      z[i] = substring(as.character(upper), 1)
      
    } else if(abs(x[i]) >= 1) {
      y[i] = paste("=", substring(as.character(round(x[i], dp)), s), sep = " ")
      z[i] = substring(as.character(round(x[i], dp)), s)
    
    } else {
      y[i] = paste("=", substring(as.character(signif(x[i], sig)), s), sep = " ")
      z[i] = substring(as.character(signif(x[i], sig)), s)
    }
  }
  
  if (return.num == FALSE & equal.sign == TRUE) {
    return(y)
    
  } else if(equal.sign == FALSE) {
    for(j in seq_along(y)) {
      if(substring(y[j], 1, 1) == "=") {
        y[j] <- substring(y[j],3)
      }
    }
    return(y)
    
  } else if(return.num == TRUE) {
    return(z)
  }
}