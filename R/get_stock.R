#' Calculate stock measures from V-Dem data. 
#' 
#' This package allows you to generate a cumulative weighted sum of past values 
#' for any V-Dem variable with user-defined weights. The resulting measure is 
#' re-scaled to represent the share of the total possible stock the country 
#' could have accumulated up until that year. 
#' The package uses the latest version of the data loaded by the 
#' vdemdata R package.
#' The function generates a data frame containing a number of stock variables 
#' equal to length(var)*length(val). Specifying multiple elements in var and val 
#' will result in higher computational time. If your machine is slower or you 
#' specify many elements in var and val, be prepared to wait for the data to process.
#' 
#' @param var The V-Dem variables you wish to calculate a stock measure from. 
#' The default is the Electoral Democracy Index (v2x_polyarchy). 
#' A stock variable is calculated for each element of var.
#' (Note: variables in var are first normalized to be between 0 and 1, if not already between 0 and 1.)\cr
#' @param val The weights to apply to the cumulative sum of past values. The 
#' default weight is 0.99. A stock variable is calculated for each element of 
#' var and val. \cr
#' (Note: the weight must be between 0 and 1 or will receive an error message.) 
#' @param fill The number of years to fill forward missing values. 
#' The default is five years. To prevent values being filled forward, use 'fill=0'\cr 
#' (Note: the fill value must be a single whole number greater than or equal to 
#' zero or will return an error message.)\cr 
#' @param add Any additional V-Dem variables you wish added to the output dataset.\cr
#' Names must match variables names in the V-Dem dataset.
#' @param name An optional name for the output. \cr
#' (Note: re-running the command without specifying a name for the new output 
#' will result in previous output being rewritten.) 
#' @return A dataframe containing the original V-Dem variable(s) listed in 'var', their stock 
#' measure(s) at each specified weight, along with country-year identifiers and 
#' any user-specified additional variables from the V-Dem dataset.
#' The calculated stock variable(s) will be labeled as "var_stock_raw" and "var_stock_weighted" 
#' (e.g. "v2x_polyarchy_stock_raw" and "v2x_polyarchy_stock_weighted").
#' @import dplyr vdemdata tidyr
#' @examples
#' get_stock()
#' get_stock(var="v2x_libdem")
#' get_stock(var="v2x_libdem", val=.975)
#' get_stock(var="v2x_libdem", fill=10)
#' get_stock(var=c("v2x_libdem", "v2csgender"), name="newdata")
#' @export
get_stock <- function(var = "v2x_polyarchy", val = 0.99, fill = 5, add = NULL, name = NULL) {
  # 1. check inputs
  check_input(var, val, fill, add, name)
  # 2. Subset variables and filter countries
  vdem_sub <- subset_data(vdemdata::vdem, var, add)
  # 3. Expand data
  vdem_sub <- expand_data(vdem_sub)
  # 4. Create stock_id
  vdem_sub <- create_stock_id(vdem_sub, stock_tt)
  # 5. Create measure based on historical antecedence
  vdem_sub <- create_antecedence(vdem_sub, var, stock_tt)
  # 6. Normalize and fill forward years
  vdem_sub <- fill_data(vdem_sub, var, fill)
  # 7. Calculate stock
  vdem_sub <- calc_stock(vdem_sub, var, val)
  # 8. check output
  stopifnot(is.data.frame(vdem_sub))
  stopifnot(!anyNA(vdem_sub$country_id))
  stopifnot(!anyNA(vdem_sub$year))
  # 9. Assign optional name in global environment
  if(is.null(name)) {
    return(vdem_sub)
  } else {
    assign_names(vdem_sub, name)
  }
}
