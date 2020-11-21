library(tidyverse)
library(lubridate)


odds_manipulate_fn <- function(df) {
  
  #date class
  df$date <- ymd(df$date)
  
  
  
  #score
  df <- df %>% 
    mutate(away_1Q_cum = away_1Q,
           away_2Q_cum = away_2Q + away_1Q_cum,
           away_3Q_cum = away_3Q + away_2Q_cum,
           away_4Q_cum = away_4Q + away_3Q_cum,
           home_1Q_cum = home_1Q,
           home_2Q_cum = home_2Q + home_1Q_cum,
           home_3Q_cum = home_3Q + home_2Q_cum,
           home_4Q_cum = home_4Q + home_3Q_cum)
  
  
  #lead team - binary coding. 1 represents lead achieved. ties represented by 0
  df <- df %>% 
    mutate(away_1Q_lead = ifelse(away_1Q_cum > home_1Q_cum, 1, 0),
           away_2Q_lead = ifelse(away_2Q_cum > home_2Q_cum, 1, 0),
           away_3Q_lead = ifelse(away_3Q_cum > home_3Q_cum, 1, 0),
           away_4Q_lead = ifelse(away_4Q_cum > home_4Q_cum, 1, 0),
           # the inverse of away doesn't capture lead changes due to binary coding of ties to 0
           home_1Q_lead = ifelse(away_1Q_cum < home_1Q_cum, 1, 0),
           home_2Q_lead = ifelse(away_2Q_cum < home_2Q_cum, 1, 0),
           home_3Q_lead = ifelse(away_3Q_cum < home_3Q_cum, 1, 0),
           home_4Q_lead = ifelse(away_4Q_cum < home_4Q_cum, 1, 0))
  
  return(df)
  
}

odds_book_remove_fn <- function(df, sports_book) {
  
  #error handle
  sports_book_list <- c("pinnacle",
                 "five_dimes",
                 "bookmaker",
                 "bOL",
                 "bovada",
                 "heritage",
                 "intertops",
                 "youwager",
                 "justbet",
                 "sportsbet")
  
  if (!(sports_book %in% sports_book_list)) {
    
    stop(glue("sports_book must be in c({book_list})"))
    
    }

  # remove other books
  book_remove <- sports_book_list[sports_book_list != sports_book]
  
  book_remove <- unlist(lapply(book_remove, function(book_remove) c(paste0(book_remove, 1),
                                             paste0(book_remove, 2))))
  
  
  df <- df[, !colnames(df) %in% book_remove]
  
  return(df)
}

fav_und_odd_score_fn <- function(df, sports_book) {
  
  #error handle
  sports_book_list <- c("pinnacle",
                        "five_dimes",
                        "bookmaker",
                        "bOL",
                        "bovada",
                        "heritage",
                        "intertops",
                        "youwager",
                        "justbet",
                        "sportsbet")
  
  if (!(sports_book %in% sports_book_list)) {
    
    stop(glue("sports_book must be in c({book_list})"))
    
  }
  
  away_book <- paste0(sports_book, 1)
  home_book <- paste0(sports_book, 2)
  
  
  df <- df %>% 
    #if ml odds are the same, assuming away team is the favorite since the home team is supposed to be given points on a spread
    mutate(away_fav = ifelse(!!rlang::sym(away_book) <= !!rlang::sym(home_book), 1, 0),
           #fav score
           fav_1Q_cum = ifelse(away_fav == 1, away_1Q_cum, home_1Q_cum),
           fav_2Q_cum = ifelse(away_fav == 1, away_2Q_cum, home_2Q_cum),
           fav_3Q_cum = ifelse(away_fav == 1, away_3Q_cum, home_3Q_cum),
           fav_4Q_cum = ifelse(away_fav == 1, away_4Q_cum, home_4Q_cum),
           #underdog score
           und_1Q_cum = ifelse(away_fav == 0, away_1Q_cum, home_1Q_cum),
           und_2Q_cum = ifelse(away_fav == 0, away_2Q_cum, home_2Q_cum),
           und_3Q_cum = ifelse(away_fav == 0, away_3Q_cum, home_3Q_cum),
           und_4Q_cum = ifelse(away_fav == 0, away_4Q_cum, home_4Q_cum),
           #lead indicator - ties are shown by a 0
           fav_1Q_lead = ifelse(fav_1Q_cum > und_1Q_cum, 1, 0),
           fav_2Q_lead = ifelse(fav_2Q_cum > und_2Q_cum, 1, 0),
           fav_3Q_lead = ifelse(fav_3Q_cum > und_3Q_cum, 1, 0),
           fav_4Q_lead = ifelse(fav_4Q_cum > und_4Q_cum, 1, 0),
           # the inverse of fav doesn't capture lead changes due to binary coding of ties to 0
           und_1Q_lead = ifelse(fav_1Q_cum < und_1Q_cum, 1, 0),
           und_2Q_lead = ifelse(fav_2Q_cum < und_2Q_cum, 1, 0),
           und_3Q_lead = ifelse(fav_3Q_cum < und_3Q_cum, 1, 0),
           und_4Q_lead = ifelse(fav_4Q_cum < und_4Q_cum, 1, 0),
           #odds
           fav_odd = as.integer(ifelse(away_fav == 1, !!rlang::sym(away_book), !!rlang::sym(home_book))),
           und_odd = as.integer(ifelse(away_fav == 0, !!rlang::sym(away_book), !!rlang::sym(home_book))))
  
  return(df)
  
}

interval_lead_fn <- function(df, game_interval) {
  
  if (game_interval == 4) {
    
    df <- df %>% 
      mutate(fav_quarters_cum_lead = (fav_1Q_lead + fav_2Q_lead + fav_3Q_lead + fav_4Q_lead),
             und_quarters_cum_lead = (und_1Q_lead + und_2Q_lead + und_3Q_lead + und_4Q_lead))
  } else {
    
    stop(glue("function not built out for selected intervals")) }
  
  
  return(df)
}

lead_chg_qtr_summary_fn <- function(df, fav_und){
  
  if (!(fav_und %in% c("favorite", "underdog"))) {
    stop(glue("fav_und must be in c(favorite, underdog)"))
  }
  
  
  df <- if (fav_und == "favorite") {
    df %>% 
      group_by(fav_quarters_cum_lead) %>%
      summarize(count = n()) %>% 
      ungroup() %>% 
      rename("quarters_led" = fav_quarters_cum_lead) %>% 
      mutate(freq = count / sum(count),
             type = "Favorite")
    
  } else {
    
    df %>% 
      group_by(und_quarters_cum_lead) %>% 
      summarize(count = n()) %>% 
      ungroup() %>% 
      rename("quarters_led" = und_quarters_cum_lead) %>% 
      mutate(freq = count / sum(count),
             type = "Underdog")
  }
  return(df)
}

