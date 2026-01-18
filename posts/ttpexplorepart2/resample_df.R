


get_key <- function(df, 
                    key_cols = names(df)) {
  # add check to see if all key_cols available
  unique_df <- df[, key_cols, drop=F] %>%
    data.table::as.data.table() %>%
    unique(by=key_cols)
  return(dplyr::tbl_df(unique_df))
}
stratify_df <- function(df, 
                        strat_cols,
                        n,
                        replace = TRUE
) {
  frac <- n/nrow(df)
  sample <- df %>% dplyr::group_by_(.dots=strat_cols) %>% 
    dplyr::sample_frac(frac, replace=replace)
  nsample <- nrow(sample) 
  nleft <- n- nsample
  if(nleft != 0) {
    ifelse(nleft > 0, {
      extras <- dplyr::sample_n(df, size = nleft)
      sample <- rbind(sample, extras)
    }, {
      removals <- sample.int(nsample, abs(nleft))
      sample <- sample[-removals,]
    }
    )
    
  }
  return(sample)
}


resample_df <- function(df, 
                        key_cols,
                        strat_cols = NULL, 
                        n = NULL,
                        key_col_name = "KEY",
                        replace = TRUE) {
  names <- c(key_col_name,names(df))
  key <- get_key(df, key_cols)
  if(!is.character(strat_cols) & !is.null(strat_cols)) {
    stop(paste("strat_cols should be character vector of column names!\n", 
               "This could also be because you have put a numerical value for n
               without properly defining it. Please explicitly state n = <number of desired keys>"))
  }
  if(is.null(n)) n <- nrow(key)
  
  if(is.null(strat_cols)) {
    
    sample <- dplyr::sample_n(key, size = n, replace=replace)
    sample[[key_col_name]] <- 1:n
  } else {
    strat_key <- get_key(df, c(key_cols, strat_cols))
    # because getting unique key based on stratification columns as well 
    # (to easily carry columns) if there are multiple stratification values
    # per unique key may introduce a bug
    # eg if stratifying by disease status if an individual has at some time
    # both positive and negative statuses, both will be picked up as two separate
    # instances so will duplicate that individual. For now issue warning but let
    # it proceed
    if(nrow(strat_key) != nrow(key)) {
      warning("Non-unique keys introduced from stratification,
              check that all keys only have one stratification variable associated
              ")}
    sample <- stratify_df(strat_key, strat_cols, n, replace = replace)
    #drop strat cols so won't possibly mangle later left join
    sample <- dplyr::ungroup(sample)
    sample <- sample[, key_cols, drop=F] 
    sample[[key_col_name]] <- 1:nrow(sample)
  }
  resampled_df <- dplyr::left_join(sample, df, by = key_cols)
  
  
  #reorder columns to match original df with key column appended
  return(resampled_df[,names, drop=F])
}