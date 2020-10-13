library(rvest)
library(glue)
library(tidyverse)
library(xml2)

View(get_lines(sport = "NFL",
               bet_type = "moneyline",
               period = "full",
               start_date = "20200927"))

get_lines <- function(sport,
                      bet_type,
                      period,
                      start_date){
  
  ## Error handling
  if (is.na(as.Date(as.character(start_date), "%Y%m%d"))) {
    stop("Start Date format is wrong")
  }
  
  # initialize results
  final_lines <- data.frame()
  
  # Sport for URL
  SPORT <- case_when(
    sport == "NFL" ~ "nfl-football",
    sport == "NBA" ~ "nba-basketball",
    sport == "NCAAF" ~ "college-football",
    TRUE ~ NA_character_
  )
  if (is.na(SPORT)) {
    stop("Sport must be in c('NFL', 'NBA', 'MCAAF')")
  }
  
  # Type for URL
  TYPE <- case_when(
    bet_type == "spread" ~ "spread",
    bet_type == "total" ~ "totals",
    bet_type == "moneyline" ~ "money-line",
    TRUE ~ NA_character_
  )
  if (is.na(TYPE)) {
    stop("Bet Type must be in c('spread', 'total', 'moneyline')")
  }
  
  # Period for URL
  PERIOD <- case_when(
    period == "full" ~ "full",
    period == "1H" ~ "1st-half",
    period == "2H" ~ "2nd-half",
    period == "1Q" ~ "1st-quarter",
    period == "2Q" ~ "2nd-quarter",
    period == "3Q" ~ "3rd-quarter",
    period == "4Q" ~ "4th-quarter",
    TRUE ~ NA_character_
  )
  if (is.na(PERIOD)) {
    stop("Period must be in c('full', '1H', '2H', '1Q', '2Q', '3Q', '4Q')")
  }
  
  DATE <- start_date
  
  
  ## need to loop eventually...
  # url to scrape
  oddsURL <- glue("https://classic.sportsbookreview.com/betting-odds/{SPORT}/{TYPE}/{PERIOD}/?date={DATE}")
  
  if (PERIOD == "full") {
    oddsURL <- glue("https://classic.sportsbookreview.com/betting-odds/{SPORT}/{TYPE}/?date={DATE}")
  }
  
  ## Spread bets don't need a Type in the url
  if (TYPE == "spread") {
    oddsURL <- glue("https://classic.sportsbookreview.com/betting-odds/{SPORT}/{PERIOD}/?date={DATE}")
    ## Full game & Spread bets don't need Period or Type in the url
    if (PERIOD == "full") {
      oddsURL <- glue("https://classic.sportsbookreview.com/betting-odds/{SPORT}/?date={DATE}")
    }
  }
  
  ## need to loop eventually...
  oddspage <- read_html(oddsURL)
  node <- html_nodes(oddspage, "div.event-holder.holder-complete")
  games <- length(node)
  
  for (game in 1:games) {
    # main child
    child1 <- html_children(node[game])
    
    ## Teams
    teams <- html_children(child1)[6] %>%
      html_children() %>%
      html_text()
    awayTeam <- teams[[1]]
    homeTeam <- teams[[2]]
    
    # get Scoring
    period_scores <- node[game] %>%
      html_nodes(".period") %>%
      html_text()
    
    # away period scoring
    away_1 <- as.integer(period_scores[1])
    away_2 <- as.integer(period_scores[2])
    away_3 <- as.integer(period_scores[3])
    away_4 <- as.integer(period_scores[4])
    
    # home period scoring accounting for OT
    if (length(period_scores) == 8) {
      
      home_1 <- as.integer(period_scores[5])
      home_2 <- as.integer(period_scores[6])
      home_3 <- as.integer(period_scores[7])
      home_4 <- as.integer(period_scores[8])
      
    } else {
      home_1 <- as.integer(period_scores[6])
      home_2 <- as.integer(period_scores[7])
      home_3 <- as.integer(period_scores[8])
      home_4 <- as.integer(period_scores[9])
      
    }
    
    ## Final Score
    kids <- html_children(child1)[2]
    kids <- html_children(kids)[3]
    kids <- html_children(kids)[4]
    
    # away score
    away_score <- html_node(html_children(kids)[1],"span.first.total") %>%
      html_text() %>%
      as.integer()
    
    # home score
    home_score <- html_node(html_children(kids)[2],"span.total") %>%
      html_text() %>%
      as.integer()
    
    ## Opening Line
    open <- html_children(child1)[8]
    away_open <- html_children(open)[1] %>%
      html_text()
    home_open <- html_children(open)[2] %>%
      html_text()
    
    ### SportsBooks
    # Pinnacle == 10
    pinnacle <- html_children(child1)[10] %>%
      html_children() %>%
      html_text()
    pinnacle1 <- pinnacle[[1]]
    pinnacle2 <- pinnacle[[2]]
    
    # 5 dimes == 11
    fiveDimes <- html_children(child1)[11] %>%
      html_children() %>%
      html_text()
    fiveDimes1 <- fiveDimes[[1]]
    fiveDimes2 <- fiveDimes[[2]]
    
    # Bookmaker == 12
    bookmaker <- html_children(child1)[12] %>%
      html_children() %>%
      html_text()
    bookmaker1 <- bookmaker[[1]]
    bookmaker2 <- bookmaker[[2]]
    
    # BetOnline == 13
    BOL <- html_children(child1)[13] %>%
      html_children() %>%
      html_text()
    BOL1 <- BOL[[1]]
    BOL2 <- BOL[[2]]
    
    # Bovada == 14
    Bovada <- html_children(child1)[14] %>%
      html_children() %>%
      html_text()
    Bovada1 <- Bovada[[1]]
    Bovada2 <- Bovada[[2]]
    
    # Heritage == 15
    Heritage <- html_children(child1)[15] %>%
      html_children() %>%
      html_text()
    Heritage1 <- Heritage[[1]]
    Heritage2 <- Heritage[[2]]
    
    # Intertops == 16
    Intertops <- html_children(child1)[16] %>%
      html_children() %>%
      html_text()
    Intertops1 <- Intertops[[1]]
    Intertops2 <- Intertops[[2]]
    
    # YouWager == 17
    youwager <- html_children(child1)[17] %>%
      html_children() %>%
      html_text()
    youwager1 <- youwager[[1]]
    youwager2 <- youwager[[2]]
    
    # JustBet == 18
    justbet <- html_children(child1)[18] %>%
      html_children() %>%
      html_text()
    justbet1 <- justbet[[1]]
    justbet2 <- justbet[[2]]
    
    # SportsBetting == 19
    sportsbet <- html_children(child1)[19] %>%
      html_children() %>%
      html_text()
    sportsbet1 <- sportsbet[[1]]
    sportsbet2 <- sportsbet[[2]]
    
    
    ## dataframe results
    game_lines <- as.data.frame(t(c(DATE,
                                    sport,
                                    bet_type,
                                    period,
                                    awayTeam,
                                    homeTeam,
                                    away_1,
                                    away_2,
                                    away_3,
                                    away_4,
                                    home_1,
                                    home_2,
                                    home_3,
                                    home_4,
                                    away_score,
                                    home_score,
                                    away_open,
                                    home_open,
                                    pinnacle1,
                                    pinnacle2,
                                    fiveDimes1,
                                    fiveDimes2,
                                    bookmaker1,
                                    bookmaker2,
                                    BOL1,
                                    BOL2,
                                    Bovada1,
                                    Bovada2,
                                    Heritage1,
                                    Heritage2,
                                    Intertops1,
                                    Intertops2,
                                    youwager1,
                                    youwager2,
                                    justbet1,
                                    justbet2,
                                    sportsbet1,
                                    sportsbet2,
                                    oddsURL
    )))
    
    ## save game results
    final_lines <- rbind(final_lines, game_lines)
  }
  
  # name columns
  colnames(final_lines) <- c("Date",
                             "Sport",
                             "bet_type",
                             "period",
                             "away_Team",
                             "home_Team",
                             "away_1Q",
                             "away_2Q",
                             "away_3Q",
                             "away_4Q",
                             "home_1Q",
                             "home_2Q",
                             "home_3Q",
                             "home_4Q",
                             "away_score",
                             "home_score",
                             "away_open",
                             "home_open",
                             "pinnacle1",
                             "pinnacle2",
                             "fiveDimes1",
                             "fiveDimes2",
                             "bookmaker1",
                             "bookmaker2",
                             "BOL1",
                             "BOL2",
                             "Bovada1",
                             "Bovada2",
                             "Heritage1",
                             "Heritage2",
                             "Intertops1",
                             "Intertops2",
                             "youwager1",
                             "youwager2",
                             "justbet1",
                             "justbet2",
                             "sportsbet1",
                             "sportsbet2",
                             "oddsURL")
  
  
  # Done and done
  message(glue("Scraped Day: {DATE}"))
  return(final_lines)
}
