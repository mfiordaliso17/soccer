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
           home_1Q_lead = ifelse(home_1Q_cum > away_1Q_cum, 1, 0),
           home_2Q_lead = ifelse(home_2Q_cum > away_2Q_cum, 1, 0),
           home_3Q_lead = ifelse(home_3Q_cum > away_3Q_cum, 1, 0),
           home_4Q_lead = ifelse(home_4Q_cum > away_4Q_cum, 1, 0))
  
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
  library(rlang)
  
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
  
  away_book <- rlang::syms(paste0(sports_book, 1))
  home_book <- rlang::syms(paste0(sports_book, 2))

  
  odds_df <- odds_df %>% 
    mutate(away_fav = ifelse(paste(!!! away_book) > paste(!!! home_book), 1, 0)) %>% 
    glimpse()
           


}



