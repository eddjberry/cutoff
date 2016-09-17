# A function for rounding values in R Markdown

In R Markdown one is often faced with situations where a value that can vary substantially is automatically pulled out. For example, we might want to pull a p-value out of an linear model object. It's a chore to have to manually edit p-values
so that they conform with reporting conventions (e.g. p < .001).

At the end of this document is a function `cutoff` that deals with these sort of cases by taking numeric value and returning `"> [upper]"`, `"< [lower]"` or `"= [exact value to 2 sf]"` depending on the value. Thus for a p-value of, say, `6.447569e-05` the function would return `"< .001"`. Below are some examples of what the standard function would return with different inputs.

| Input | Output |
|-------|--------|
| 0.674 | "= .67" |
| 0.027 | "= .027" |
| 0.00053 | "< .001" |

By default the upper cut-off is set to 1 meaning it isn't used for a *p*-value and the lower cut-off is set to 0.001. However, these cut-offs can be adjusted with `upper` and `lower` arguments, respectively. The `sig` argument sets the number of significant figures (default = `2`) to report for exact values between the upper and lower cut-offs. The return.num argument is false by default. If it's set to true then the `"="` or `"<"` aren't included in the returned string. E.g. `".001"` rather than `"< .001"` or `.032` rather than `"= .032"`. The lead.zero argument defaults to `FALSE` and returns values with the leading zero, e.g., .004 rather than 0.004. This is the recommended formatting for numbers than cannot take values larger than 1, such a *p*-values. The arguments of the function are summarised below.

|Argument | Info|
|---------|-----|
|x | a vector |
|upper |  value above which "> [upper]" is returned |
| lower | value below which "< [lower]" is returned |
| sig  | number of significant figures to round values between upper and lower to |
| return.num | logical, whether to return just the number (i.e. not include the = or >) |
| lead.zero | logical, include leading zero (e.g. 0.01) |
| equal.sign | logical, include the equal sign for values between upper and lower |

The `equal.sign` argue might be useful when using the function in a table. This would give you ".16" instead of "= .16" for, say, a *p* value in a table but the "< .001"s would stay the same.

The file `cutoff.R` contains different version of the function with defaults for different statistics. These could, of course, be replicated by changing the upper and lower arguments. However, it might be easier and more readable to have a different versions of the function for each type of statistic. The default arguments of the different version are summarised below. `cutoff2` uses round() rather than signif() so that it's suited to large values with decimals, such as percentages. It has a `dp` rather than a `sig` arguments which defaults to 1 decimal place, e.g. 56.9. `cutoff_BF` has both `dp` and `sig` arguments for values above and below 1 respectively. The defaults allow you to round values greater than 1 to one decimal place while values less than 1 have two significant figures. 

| Function name | upper | lower | sig / dp | return.num | lead.zero | equal.sign | Notes |
|---------------|-------|-------|-----|------------|-----------|------------|-------|
| cutoff |  1 | 0.001 | 2 | FALSE | FALSE | TRUE | |
| cutoff_Rsq | 1 | 0.01 | 2 | FALSE | FALSE | TRUE | |
| cutoff_beta | 1 | 0.0001 | 2 | FALSE | TRUE | TRUE | |
| cutoff2 | 100 | 0.01 | 2 |FALSE | TRUE | TRUE | Uses round() not signif() |
| cutoff_BF | 1000 | 0.001 | 2 / 1 | FALSE | TRUE | TRUE | dp arg rather than sig |


All the function are vectorised so can be applied to a column of values.

*If this function looks a bit loop heavy for R then my only defence is I was doing a lot of Python at the time.*

```
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
```
