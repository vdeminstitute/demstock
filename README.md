## The demstock R package ##

An R package to calculate stock measures from the most recent [V-Dem (Varieties of Democracy)](https://www.v-dem.net/vdemds.html) dataset. 

Calculate stock measures from V-Dem data

Load V-Dem data and calculate stock measures for various depreciation rates

This package allows you to generate a cumulative weighted sum of past values for any [V-Dem variable](https://v-dem.net/documents/55/codebook.pdf) with user-defined weights. The resulting measure is re-scaled to represent the share of the total possible stock the country could have accumulated up until that year. 
    • The package uses the latest version of the data loaded by the vdemdata R package.
    • The function generates a data frame containing a number of stock variables equal to length(var)*length(val). Specifying multiple elements in var and val will result in higher computational time. If your machine is slower or you specify many elements in var and val, be prepared to wait for the data to process.

### Functions ###
    • `get_stock()`: calculate stock for variables in the V-Dem dataset for specific depreciation rates, with missing values imputed.  Returns a dataframe containing the original V-Dem variable(s), their stock measure(s) at each specified weight, along with country-year identifiers and any user-specified additional variables from the V-Dem dataset.

### Example: ###
``` 
getstock(var = "v2x_polyarchy", val = 0.99, fill=5, add=NULL, name = "v.dem.out") 
```

### Variables ###
    • `var`: The V-Dem variables you wish to calculate a stock measure from. 
        ◦ A stock variable is calculated for each element of var.
        ◦ Note: each variable in var is first normalized to be between 0 and 1.
    • `val`: The weights to apply to the cumulative sum of past values. 
        ◦ The default weight is 0.99. 
        ◦ A stock variable is calculated for each element of var and val.
        ◦ Note: the weight must be between 0 and 1 or will receive an error message.
    • `fill`: The number of years to fill forward missing values. 
        ◦ The default is five years.
        ◦ To prevent values being filled forward, use 'fill=0'.
        ◦ Note: the fill value must be a single whole number greater than or equal to 0 or will return an error message.
    • add: Any additional V-Dem variables you wish added to the output dataset. 
        ◦ Names must match variables names in the V-Dem dataset.
    • name: An optional name for the output. 
        ◦ The default name is 'v.dem.out'.
        ◦ Note: re-running the command without specifying a name for the new output will result in previous output being rewritten. 

### Dependencies ###
    • Requires packages vdemdata, dplyr, and tidyr. 

### Installation ###
```
# Install the development version of the getstock package 

# First, you need to have the devtools package installed
install.packages("devtools")
# now, install the getstock package directly from GitHub
devtools::install_github("vdeminstitute/getstock")

# NOTE: make sure you have an updated R version and
# - since the package is a development version - 
# an updated version of xcode (Mac), rtools (Windows), r-base-dev (Linux)
# installed. If you have trouble with the installation 
# write to contact@v-dem.net at the V-Dem Institute.
```

### Recommend Citations ###

When using this package, please include the following citations: 

Wilson, Matthew C., Amanda B. Edgell, and Vanessa Boese-Schlosser. 2025. "The demstock R package." Varieties of Democracy Institute. https://github.com/vdeminstitute/demstock. 

Edgell, Amanda B., Matthew C. Wilson, and Vanessa Boese-Schlosser. 2025. "Introducing demstock: An R package for calculating stock variables using the Varieties of Democracy (V-Dem) data." V-Dem Working Paper No. [insert #]. [insert url]

Edgell, Amanda B., Matthew C. Wilson, Vanessa A. Boese, and Sandra Grahn. 2020. "Democratic Legacies: Using Democratic Stock to Assess Norms, Growth, and Regime Trajectories." V-Dem Working Paper No.100, https://www.v-dem.net/media/publications/working_paper_100.pdf
