#' Helper function to front fill
#' @param x A vector to be filled forward.
#' @export
locf <- function(x) {
    for (i in seq_along(x)) {
        if (is.na(x[i]) && i > 1) {
        x[i] <- x[i - 1]
        }
    }
    return(x)
}

#' Function to check all inputs to get_stock
#' @param var The V-Dem variables you wish to calculate a stock measure from. 
#' @param val The weights to apply to the cumulative sum of past values.
#' @param fill The number of years to fill forward missing values.
#' @param add Any additional V-Dem variables you wish added to the output dataset.
#' @param name An optional name for the output.
#' @import vdemdata
#' @export
check_input <- function(var, val, fill, add, name) {
    if (!all(var %in% names(vdemdata::vdem))) {
        stop("var must be a variable in the V-Dem dataset.")
    }

    if (!is.null(add) & !all(add %in% names(vdemdata::vdem))) {
        stop("all elements of add must be variables in the V-Dem dataset.")
    }

    if (!is.numeric(fill) || fill != as.integer(fill) || fill < 0) {
        stop("fill must be a single whole number greater than or equal to zero.")
    }

    if (!is.character(name) && !is.null(name)) {
        stop("name must be a character string.")
    }

    if (val < 0 | val > 1) {
        stop("val must be between 0 and 1.")
    }

    if (length(fill) != 1) {
        stop("fill must be a single whole number greater than or equal to zero.")
    }

    if (length(val) != 1) {
        stop("val must be a single number between 0 and 1.")
    }

    if (!is.null(name) && exists(name)) {
        warning(sprintf("%s is already an object and will be overwritten.", name))
    }
}



#' Subset variables and remove historical states
#' @param df A dataframe to be subsetted.
#' @param var A character vector of variable names to be subsetted from the V-Dem dataset. Names must match variables names in the V-Dem dataset.
#' @param add Any additional V-Dem variables you wish added to the output dataset.
#' @import dplyr vdemdata
#' @export
subset_data <- function(df, var, add = NULL) {
    `%!in%` <- Negate(`%in%`)

    # subset data and removes German and Italian historical states
	df_out <- df %>%
        dplyr::select(country_id, country_text_id, year, all_of(var), all_of(add)) %>%
        dplyr::filter(country_id %!in% c(349,350,351,352,353,354,355,356,357,358,359,360,361,362,363,364,365,366,373))

    cat(paste0("Removed the following states from subset data: ", paste(names(table(vdemdata::vdem[which(vdemdata::vdem$country_id %in% c(349,350,351,352,353,354,355,356,357,358,359,360,361,362,363,364,365,366,373)),]$country_name)), collapse=", "),".\n"))

    return(df_out)
}

#' Expand data
#' @param df A dataframe to be expanded.
#' @import dplyr tidyr
#' @export
expand_data <- function(df) {
    df_out <- df %>%
        dplyr::mutate(original = 1)

    min_year <- min(df_out$year, na.rm = TRUE)
    max_year <- max(df_out$year, na.rm = TRUE)

    # create df with all possible country_id, year combinations
    fullset <- df_out %>%
        dplyr::distinct(country_id, country_text_id) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(year = list(seq(min_year, max_year))) %>%
        tidyr::unnest(year) %>%
        dplyr::select(country_id, country_text_id, year) %>%
        as.data.frame() 
    
    # merge fullset and df to expand all possible combinations
    df_out <- merge(df_out, fullset, all = TRUE) %>%
        dplyr::arrange(country_id, year) %>%
        dplyr::mutate(original = ifelse(is.na(original), 0, original)) 
    rownames(df_out) <- NULL

    stopifnot(nrow(df_out) == nrow(fullset))
    stopifnot(!anyNA(df_out$country_id))
    stopifnot(!anyNA(df_out$country_text_id))

    df_out <- dplyr::select(df_out, -original)

    print(sprintf("Subset data expanded. Old dimensions were %d by %d. New dimensions are %d by %d", 
        dim(df)[1], dim(df)[2]-1, dim(df_out)[1], dim(df_out)[2]))

    return(df_out)
}

#' Creates stock_id according to the stock translation table
#' @param df A dataframe to be merged with the stock translation table.
#' @param stock_tt The stock translation table, which contains the country-year combinations which will be changed to create stock_id.
#' @import dplyr
#' @export
create_stock_id <- function(df, stock_tt) {
    df_out <- dplyr::left_join(df, stock_tt, by = c("country_id", "country_text_id", "year"))

    stopifnot(nrow(df_out) == nrow(df))
    stopifnot(nrow(dplyr::filter(df_out, !is.na(stock_id))) == nrow(stock_tt))
    stopifnot(!anyNA(df_out$country_id))
    stopifnot(!anyNA(df_out$country_text_id))
    stopifnot(!anyNA(df_out$year))    

    df_out <- df_out %>%
        dplyr::mutate(stock_id = dplyr::case_when(is.na(stock_id) ~ country_id,
            TRUE ~ stock_id))

    stopifnot(!anyNA(df_out$stock_id))

    print("Stock country ID created.")
    return(df_out)
}

#' Create measure based on historical antecedence
#' @param df A dataframe to be merged with the stock translation table.
#' @param var A character vector of variable names for which the stock measure will be created.
#' @param stock_tt The stock translation table, which contains the country-year combinations which will be changed to create stock_id.
#' @import dplyr
#' @export
create_antecedence <- function(df, var, stock_tt){
    
    df_stock <- df 

    ll <- lapply(var, function(v){
        # create new stock var
        new_var <- paste(v, "_stock", sep = "")
        df_stock[[new_var]] <- df_stock[[v]]

        temp <- subset(df_stock, stock_id %in% unique(stock_tt$stock_id))
        stopifnot(length(unique(temp$stock_id)) == length(unique(stock_tt$stock_id)))

        temp <- temp %>% 
            dplyr::group_by(stock_id, year) %>% 
                dplyr::mutate("tempmean" := mean(!!as.name(new_var), na.rm=T)) %>%
            dplyr::ungroup() %>%
            dplyr::select(stock_id, country_id, year, tempmean) %>%
            # change NaN to NA
            dplyr::mutate(tempmean = ifelse(is.nan(tempmean), NA, tempmean))

        # replace normal values with tempmean where country_id != stock_id
        df_stock <- df_stock %>%    
            dplyr::left_join(temp, by = c("stock_id", "country_id", "year")) %>% 
            dplyr::mutate(!!as.name(new_var) := ifelse(country_id != stock_id, tempmean, !!as.name(new_var))) %>%
            dplyr::select(-tempmean) %>%
            dplyr::arrange(country_id, year) %>%
            dplyr::mutate(!!as.name(new_var) := dplyr::case_when(
                # Carrying forward Vietnam, which shows similar values before and after a gap from 1887-1902. This was all French colonial rule period 
                country_id == 34 & year >= 1888 & year <= 1901 ~ locf(!!as.name(new_var)),
                TRUE ~ !!as.name(new_var))) 
            
        # add averaged values from Mali, Niger, and Ivory Coast to Burkina Faso 1932-1942 under French West Africa
        avg_fwa <- df_stock %>%
            dplyr::filter(year >= 1932 & year <= 1946 & country_id %in% c(64, 28, 60)) %>%
            dplyr::group_by(year) %>%
                dplyr::summarize(avg_value = mean(!!as.name(new_var), na.rm = TRUE)) %>% 
            dplyr::mutate(country_id = 54)

        df_stock <- df_stock %>%
            dplyr::left_join(avg_fwa, by = c("country_id", "year")) %>%
            dplyr::mutate(!!as.name(new_var) := dplyr::case_when(
                country_id == 54 & year >= 1932 & year <= 1946 ~ avg_value,
                TRUE ~ !!as.name(new_var))) %>%
            dplyr::select(country_id, year, stock_id, all_of(new_var))

        return(df_stock)
    })  
    
    df_ll <- Reduce(function(x, y) dplyr::left_join(x, y, by = c("country_id", "year", "stock_id")), ll)

    stopifnot(nrow(df_ll) == nrow(df_stock))

    df_out <- df_stock %>%
        dplyr::left_join(df_ll, by = c("country_id", "year", "stock_id"))

    stopifnot(nrow(df_out) == nrow(df))
    stopifnot(!anyNA(df_out$country_id))
    stopifnot(!anyNA(df_out$country_text_id))
    stopifnot(!anyNA(df_out$year))

    print("Antecedent values imputed.")
    return(df_out)
}

#' Normalize and fill forward years
#' @param df A dataframe containing the stock variable(s) to be normalized and filled forward.
#' @param var A character vector of variable names for which the stock measure will be created.
#' @param fill The number of years to fill forward missing values. 
#' @import dplyr
#' @export
fill_data <- function(df, var, fill) {
    print(paste0("Filling forward ", fill, " years."))
    # Normalize variable(s)
    ll <- lapply(var, function(v) {
        stock_v <- paste0(v, "_stock")

        stopifnot(any(grepl(stock_v, names(df))))
        stopifnot(is.numeric(df[[stock_v]]))

        maxval <- max(df[[stock_v]], na.rm = TRUE)
        minval <- min(df[[stock_v]], na.rm = TRUE)

        if (ceiling(maxval) == 1 & floor(minval) == 0) {
            df_norm <- df %>%
                dplyr::mutate(norm_value = !!as.name(stock_v))
        } else {
            df_norm <- df %>%
                dplyr::mutate(norm_value = !!as.name(stock_v)) %>%
                dplyr::mutate(norm_value = (!!as.name(stock_v) - minval) / (maxval - minval)) 
        }

        stopifnot(max(df_norm$norm_value, na.rm = TRUE) <= 1)
        stopifnot(min(df_norm$norm_value, na.rm = TRUE) >= 0)

        df_norm <- dplyr::select(df_norm, country_id, year, stock_id, norm_value) %>%
            dplyr::rename(!!as.name(paste0(v, "_norm")) := norm_value)

        return(df_norm)
    })

    df_out <- Reduce(function(x, y) dplyr::left_join(x, y, by = c("country_id", "year", "stock_id")), ll)
    stopifnot(nrow(df_out) == nrow(df))

    # Fill forward
    if (fill > 0) {
        ll <- lapply(var, function(v) {
            df_fill <- df_out %>%
                dplyr::mutate(filled = !!as.name(paste0(v, "_norm")))

            for (lagval in c(1:fill)) {
                df_fill <- df_fill %>% 
                    dplyr::group_by(country_id) %>% 
                        dplyr::mutate(!!as.name(paste0("L",lagval)) := dplyr::lag(!!as.name(paste0(v, "_norm")), n=lagval)) %>%
                    dplyr::ungroup() %>%
                    dplyr::mutate(filled = dplyr::case_when(is.na(filled) ~ !!as.name(paste0("L",lagval)),
                        TRUE ~ filled)) %>%
                    dplyr::select(-!!as.name(paste0("L",lagval)))
            }
        
        stopifnot(nrow(dplyr::filter(df_fill, is.na(!!as.name(paste0(v, "_norm"))) & !is.na(filled))) == 
            nrow(dplyr::filter(df_fill, is.na(!!as.name(paste0(v, "_norm"))))) - nrow(dplyr::filter(df_fill, is.na(filled))))
        print(sprintf("%s values filled for %s", 
            nrow(dplyr::filter(df_fill, is.na(!!as.name(paste0(v, "_norm"))) & !is.na(filled))), v))

        df_fill <- dplyr::select(df_fill, country_id, year, stock_id, filled) %>%
            dplyr::rename(!!as.name(paste0(v, "_filled")) := filled)

        return(df_fill)
        })

        final_fill <- Reduce(function(x, y) dplyr::left_join(x, y, by = c("country_id", "year", "stock_id")), ll)
        stopifnot(nrow(final_fill) == nrow(df))

        df_out <- dplyr::left_join(df_out, final_fill, by = c("country_id", "year", "stock_id")) 
    } else {
        df_out <- df_out %>%
            dplyr::rename_with(~ gsub("_norm", "_filled", .x), dplyr::ends_with("_norm"))
    }

    final_df <- df %>% 
        dplyr::select(-c(dplyr::ends_with("_stock"))) %>% 
        dplyr::left_join(df_out, by = c("country_id", "year", "stock_id")) %>%
        dplyr::select(-c(dplyr::ends_with("_norm"))) %>%
        dplyr::rename_with(~ gsub("filled", "stock", .x), dplyr::ends_with("_filled"))

    stopifnot(nrow(final_df) == nrow(df))
    return(final_df)
}

#' Calculate stock
#' @param df A dataframe containing the stock variable(s) to be calculated.
#' @param var A character vector of variable names for which the stock measure will be created.
#' @param val The weights to apply to the cumulative sum of past values.
#' @import dplyr tidyr
#' @export
calc_stock <- function(df, var, val) {
    stopifnot(is.numeric(val))

    ll <- lapply(var, function(v) {
        print(sprintf("Calculating stock for %s with depreciation rate of %s%%", v, 100*(1-val)))
        stock_v <- paste0(v, "_stock")

        df_out <- df %>%
            dplyr::mutate(startyear = dplyr::case_when(!is.na(!!as.name(stock_v)) ~ year, TRUE ~ NA)) %>%
            dplyr::group_by(country_id) %>%
                dplyr::mutate(startyear = min(startyear, na.rm = TRUE)) %>%
            dplyr::ungroup() %>%
            #sets values to zero for first observed year
            dplyr::mutate(val_version = dplyr::case_when(year == startyear ~ 0, TRUE ~ NA)) %>%
            as.data.frame()

        # calculate stock
        pb <- txtProgressBar(min = 0, max = max(as.numeric(df_out$year) - as.numeric(unlist(df_out[,which(colnames(df_out) == "val_version")])), na.rm = TRUE), style = 3)

        for (y in 1:max(as.numeric(df_out$year)-as.numeric(unlist(df_out[,which(colnames(df_out) == "startyear")])), na.rm = TRUE)) {
            setTxtProgressBar(pb, value=y)

            df_out <- df_out %>% 
                dplyr::group_by(country_id) %>% 
                dplyr::mutate(Lval = dplyr::lag(val_version)) %>%
                dplyr::ungroup()

            df_out <- df_out %>% 
                dplyr::group_by(country_id) %>% 
                dplyr::mutate(Lfilled = dplyr::lag(!!as.name(stock_v))) %>%
                dplyr::ungroup()

            # sets all other years to previous year's stock times depreciation, plus previous year's democracy level
            df_out[which(is.na(df_out[,which(colnames(df_out) == "val_version")]) == TRUE & 
                (as.numeric(df_out$year) - as.numeric(unlist(df_out[,which(colnames(df_out) == "startyear")]))) == y), 
                which(colnames(df_out) == "val_version")] <- (val*df_out[which(is.na(df_out[,which(colnames(df_out) == "val_version")]) == TRUE & 
                (as.numeric(df_out$year) - as.numeric(unlist(df_out[,which(colnames(df_out) == "startyear")]))) == y),]$Lval)+
                df_out[which(is.na(df_out[,which(colnames(df_out) == "val_version")]) == TRUE & 
                (as.numeric(df_out$year)-as.numeric(unlist(df_out[,which(colnames(df_out) == "startyear")]))) == y),]$Lfilled
        }
    
        close(pb)
        
        df_out <- df_out %>%
            # change first year back to NA
            dplyr::mutate(val_version = dplyr::case_when(year == startyear ~ NA, TRUE ~ val_version)) %>%     
            # drop stock values for antecedent years
            dplyr::mutate(orig_year = dplyr::case_when(!is.na(!!as.name(v)) ~ year, TRUE ~ NA)) %>%
            dplyr::group_by(country_id) %>%
                dplyr::mutate(keepyear = min(orig_year, na.rm = TRUE)) %>%
            dplyr::ungroup() %>%
            dplyr::select(-orig_year) %>% 
            dplyr::mutate(val_version = dplyr::case_when(year < keepyear ~ NA, TRUE ~ val_version)) %>%
            dplyr::select(-keepyear) %>%
            # apply PT transformation to the variable
            dplyr::mutate(!!paste0(stock_v, "_weighted") := val_version*(1 - val)) %>%
            dplyr::rename(!!paste0(stock_v, "_raw") := val_version) %>%
            dplyr::select(country_id, year, stock_id, !!paste0(stock_v, "_raw"), !!paste0(stock_v, "_weighted")) %>%
            as.data.frame()

        print(sprintf("%s calculated.", stock_v)) 

        return(df_out)
    })

    df_out <- Reduce(function(x, y) dplyr::left_join(x, y, by = c("country_id", "year", "stock_id")), ll)

    stopifnot(nrow(df_out) == nrow(df))

    final_df <- df %>%
        dplyr::select(-c(dplyr::ends_with("_stock"))) %>%
        dplyr::left_join(df_out, by = c("country_id", "year", "stock_id"))
    
    return(final_df)
}

#' Assign optional name in global environment
#' @param df A dataframe containing the stock variable(s) to be assigned a name in the global environment.
#' @param name The name for the output object in the global environment.
#' @export
assign_names <- function(df, name){
    if (exists(name, envir = .GlobalEnv)) {
        warning(sprintf("An object named %s already exists in the global environment and will be overwritten.", name))
    }

    if(is.character(name) == FALSE) {
        warning("Output name must be a string.")
    } 

    assign(name, df, envir = .GlobalEnv)
        cat(paste0("Final data stored as object '", name, "'","\n"))
}
