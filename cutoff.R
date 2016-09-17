
#========  P value/ Beta

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
      
    } else if(abs(x[i]) > upper) {
      y[i] = paste(">", substring(as.character(upper), s), sep = " ")
      z[i] = substring(as.character(upper), s)
      
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

#========  Version 2, round rather than significant figure

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
    if(x[i] < lower) {
      y[i] = paste("<", substring(as.character(lower), s), sep = " ") # substring of a character used to get .001 rather than 0.001
      z[i] = substring(as.character(lower), s)
      
    } else if(x[i] > upper) {
      y[i] = paste(">", substring(as.character(upper), s), sep = " ")
      z[i] = substring(as.character(upper), s)
      
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

#========  Bayes Factor

cutoff_BF = function(x, upper = 100, lower = 0.01, dp = 1, return.num = FALSE, 
                     lead.zero = TRUE, equal.sign = TRUE) {
  
  y = NULL
  z = NULL
  
  if (lead.zero == FALSE) {
    s = 2
  } else {
    s = 1
  }
  
  for(i in seq_along(x)) {
    if(x[i] < lower) {
      y[i] = paste("<", substring(as.character(lower), s), sep = " ") # substring of a character used to get .001 rather than 0.001
      z[i] = substring(as.character(lower), s)
      
    } else if(x[i] > upper) {
      y[i] = paste(">", substring(as.character(upper), s), sep = " ")
      z[i] = substring(as.character(upper), s)
      
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

#========  Rsquared 

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
    if(x[i] < lower) {
      y[i] = paste("<", substring(as.character(lower), s), sep = " ") # substring of a character used to get .001 rather than 0.001
      z[i] = substring(as.character(lower), s)
      
    } else if(x[i] > upper) {
      y[i] = paste(">", substring(as.character(upper), s), sep = " ")
      z[i] = substring(as.character(upper), s)
      
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

#======== Beta

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
      
    } else if(abs(x[i]) > upper) {
      y[i] = paste(">", substring(as.character(upper), s), sep = " ")
      z[i] = substring(as.character(upper), s)
      
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
