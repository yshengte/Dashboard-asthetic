clean_data <- function(df, unique_id) {
  df <- df |> filter(
    df[[unique_id]] != '',
    !is.na(df[[unique_id]])
  )
  
  return(df)
}

check_data <- function(df) {
  errors <- c()
  
  # coupon
  # if(class())
  
  # date
  if("MAINDATE" %in% names(df)) {
    if(anyNA(as.Date(df$MAINDATE, optional = TRUE))) {
      errors <- c(errors, "Provided dates are not in the proper format.") 
    }
  }
  
  # lattitude and longtitude
  if(all(c("LAT", "LNG") %in% names(df))) {
    if(!is.numeric(df$LAT) | !is.numeric(df$LNG)) {
      errors <- c(errors, "Provided lattide and longtitude coordinates are not numeric.")
    } else if(any(df$LAT < -90) | any(df$LAT > 90)) {
      errors <- c(errors, "Lattitude must be between -90 and 90.")    
    } else if(any(df$LNG < -180) | any(df$LNG > 180)) {
      errors <- c(errors, "Longtitude must be between -180 and 180.")
    } 
  }
  
  return(errors)
}

get_week <- function(df) {
  min_year <- min(year(df$MAIN_DATE))
  start_date <- as.Date(paste0(min_year, "-05-03"))

  week <- difftime(df$MAIN_DATE, start_date) |>
    as.numeric(units = "weeks") |>
    floor()
  
  return(week + 1)
}


get_recruiter_ID <- function(df) {
  id <- df$ID
  rc <- df$R_CP %>% replace(is.na(.), '')
  tcs <- df |> select(-c(ID, R_CP))
  
  rid <- tcs |>
    apply(2, function(tc) id[match(rc, tc)]) %>%
    replace(is.na(.), '') |>
    apply(1, paste, collapse = '') |>
    na_if('')
  
  return(rid)
}


recruitment_info <- function(
    df,
    unique_id,
    redeemed_coupon,
    issued_coupons
) {
  
  if(unique_id == redeemed_coupon) {
    unique_id <- "ID_NEW"
    df[[unique_id]] <- df[[redeemed_coupon]]
  }
  
  # select relevant columns
  df <- df |> select(all_of(c(unique_id, redeemed_coupon, issued_coupons)))
  
  # convert all values to character type
  # and blank character to NA
  df <- df |> mutate_all(as.character)
  df[df == ''] <- NA
  
  # rename columns
  issued_coupons <- paste0("T_CP", 1:length(issued_coupons))
  names(df) <- c("ID", "R_CP", issued_coupons)
  
  # construct recruitment chains
  holder <- df
  df_recruiter <- data.frame(R_ID0 = df$ID) 
  i <- 0

  while(!all(is.na(df_recruiter[[paste0("R_ID", i)]]))) {
    rid <- get_recruiter_ID(holder)
    holder <- data.frame(ID = rid) |> left_join(df, by = "ID")
    i <- i + 1
    df_recruiter[[paste0("R_ID", i)]] <- rid
  }
  
  # get wave number
  df$WAVE <- df_recruiter |>
    apply(2, function(c) as.numeric(!is.na(c))) |>
    rowSums()
  df$WAVE <- df$WAVE - 1
  
 # get seed ID
  df$S_ID <- mapply(function(i, j) df_recruiter[i, j+1],
                    1:nrow(df_recruiter),
                    df$WAVE)
  
  # get recruiter ID
  df$R_ID <- df_recruiter[["R_ID1"]]
  
  
  # classify seed
  df$SEED <- ifelse(is.na(df$R_ID), 1, 0)

  # find the number of coupons issued
  df$CT_T_CP <- df |>
    select(all_of(issued_coupons)) |>
    apply(2, function(c) as.numeric(!is.na(c))) |>
    rowSums()

  # find the number of coupons used
  cu <- df |>
    group_by(R_ID) |>
    summarize(CT_T_CP_USED = n()) |>
    rename("ID" = "R_ID") |>
    na.omit()

  df <- df |>
    left_join(cu, by = "ID")

  return(df)
}

create_lat_lng <- function(df) {
  
  df <- df |> mutate(
    LAT = sample(seq(25, 48, 0.001), nrow(df)),
    LNG = sample(seq(-65, -125, -0.001), nrow(df))
  )
  
  return(df)
}

get_rchains <- function(df) {
  geo <- df |>
    select(ID, LAT, LNG) |>
    distinct(ID, .keep_all = TRUE)
  
  df <- df |>
    select(ID, S_ID, R_ID, WAVE, LAT, LNG) |>
    rename(LAT1 = LAT, LNG1 = LNG) |>
    left_join(geo, by = c("R_ID" = "ID")) |>
    rename(LAT0 = LAT, LNG0 = LNG) |>
    na.omit()
  
  return(df)
}