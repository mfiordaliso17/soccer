library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggthemes)
library(scales)




header <- dashboardHeader(title = "Live Hedge Strategy")

sidebar <- dashboardSidebar("User Input",
                 sliderInput("profit_target", "Profit Target:", 0, 2, 0.5, 0.01),
                 numericInput("wager", "Wager Amount:", 10),
                 numericInput("bet", "Initial Odd Bet:", 150))


body <- dashboardBody(fluidRow(box(plotOutput("hedge_need_plot")),
                               box(plotOutput("success_plot"))),
                               box(valueBoxOutput("hedge_amt", "3Hedge Amount")),
                               box(valueBoxOutput("hedge_odd", "Hedge Odd Need"))
                      )






server <- shinyServer(function(input, output) {
    
    
    
    
    output$hedge_need_plot <- renderPlot({
        
        # create df
        wager <- 1
        
        profit_target <- reactive({
            input$profit_target
        })
        
        
        max_favorite <- ifelse(profit_target() <= 1, -100 / profit_target(), 100 * profit_target())
        
        interval <- 20
        
        odds <- if (profit_target() <= 1) {
            round(c(seq(max_favorite, -100, by = interval), seq(100, -max_favorite + 300, by = interval)), 0)
        } else {
            round(seq(max_favorite, max_favorite + 400, by = interval), 0)
        }
        
        
                       
        odds_fctr <-  ifelse(odds < 0, -100 / odds, odds / 100)
        initial_profit <- round(wager * odds_fctr, 2)
        hedge_amt <- (initial_profit - (wager * profit_target()))
        hedge_odd_need <- round(100 * (wager * profit_target() + wager) / (hedge_amt), 2)
        hedge_odd_need <- ifelse(hedge_odd_need < 100, -100 / hedge_odd_need * 100, hedge_odd_need)
        
        
        odds_df <- data.frame(odds,
                              odds_fctr,
                              initial_profit,
                              hedge_amt,
                              hedge_odd_need)
        
        
        # df consolidate
        
        n <- round(nrow(odds_df) / 2, 0)
        
        min_val <- max(n-12, 1)
        max_val <- min(n+12, nrow(odds_df))
        
        odds_df <- odds_df[min_val:max_val, ]
        
        odds_df <- odds_df %>% 
            filter(is.finite(hedge_odd_need), odds != max(odds), hedge_amt > 0, hedge_odd_need < 2000)
        
        
        # scale for graph
        scale_min <- min(odds_df$hedge_odd_need[is.finite(odds_df$hedge_odd_need)])
        scale_min <- ifelse(scale_min < 0, scale_min * 5, scale_min * 0.8)
        
        scale_max <- max(odds_df$hedge_odd_need[is.finite(odds_df$hedge_odd_need)]) * 1.4
        
        
        # graph
        
        odds_df %>%
            mutate(color = ifelse(hedge_odd_need < 0, "green", "red")) %>% 
            ggplot(aes(x = factor(odds), y = hedge_odd_need, fill = color)) +
            geom_col(color = "black", width = 0.8) +
            geom_text(aes(label = comma(round(hedge_odd_need, 0)),
                          hjust = ifelse(hedge_odd_need > 0, -0.2, 1.2),
                          angle = 90),
                      size = 5) +
            geom_vline(xintercept = factor(100), linetype = "dashed", color = "grey") +
            coord_cartesian(ylim = c(scale_min, scale_max)) +
            scale_fill_identity() +
            theme_tufte() +
            scale_y_continuous(labels = comma) +
            theme(axis.text.x = element_text(size = 15, angle = 90, vjust = 0.5),
                  axis.title = element_text(size = 17),
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  title = element_text(size = 20)) +
            labs(x = "Initial Odds",
                 y = "Live Odds",
                 title = paste0("Hedging to gurantee a profit of ", percent(profit_target())))
        
        
    })
    
    output$success_plot <- renderPlot({
        
        # create df
        wager <- 1
        profit_target <- reactive({
            input$profit_target})
        
        success <- seq(.1, 1, by = 0.01)
        return <- ((success * profit_target() * wager) - ((1 - success) * wager) / wager)
        
        success_df <- data.frame(success,
                                 return)
        
        
        
        # graph
        
        break_even <- min(success_df %>% 
                              filter(return >= 0) %>% 
                              select(success))
        
        success_df %>%
            mutate(color = ifelse(return > 0, "green", "red")) %>% 
            ggplot(aes(x = success, y = return, fill = color)) +
            geom_area(color = "black") +
            scale_fill_identity() +
            theme_few() +
            geom_vline(xintercept = break_even) +
            geom_text(aes(x = break_even, y = .1, label = break_even),
                      angle = 90,
                      vjust = -0.5,
                      size = 5,
                      family = "Helvetica") +
            scale_y_continuous(labels = percent,
                               breaks = seq(-1, profit_target(), by = .2)) +
            scale_x_continuous(labels = percent) +
            theme(axis.text.x = element_text(size = 15),
                  axis.text.y = element_text(size = 15),
                  axis.title = element_text(size = 17),
                  title = element_text(size = 20)) +
            labs(x = "Success",
                 y = "Expected Return",
                 title = paste0("Success rate given a ", percent(profit_target()), " profit target"))
        
        
    })
    
    
   
    
        
        
        output$hedge_amt <- renderValueBox({
            
            wager <- reactive({
                input$wager})
            
            profit_target <- reactive({
                input$profit_target})
            
            odds <- reactive({
                input$bet})
            
            max_profit <- if (odds() > 0) {
                odds() / 100
            } else {
                -100 / odds()
            }
            
            #odds factor
            odds_fctr <-  ifelse(odds() < 0, -100 / odds(), odds() / 100)
            initial_profit <- round(wager() * odds_fctr, 2)
            hedge_amt <- (initial_profit - (wager() * profit_target()))
            hedge_amt <- ifelse(profit_target() > max_profit, "NA", round(hedge_amt, 2))
            
            valueBox(
                paste0("$", hedge_amt),
                "Hedge Amount",
                icon = icon("list"),
                color = "purple"
            )
        
        
        
        
    })
        
        
        
        output$hedge_odd <- renderValueBox({
            
            wager <- reactive({
                input$wager})
            
            profit_target <- reactive({
                input$profit_target})
            
            odds <- reactive({
                input$bet})
            
            
            max_profit <- if (odds() > 0) {
                odds() / 100
            } else {
                -100 / odds()
            }
            
            #odds factor
            odds_fctr <-  ifelse(odds() < 0, -100 / odds(), odds() / 100)
            initial_profit <- round(wager() * odds_fctr, 2)
            hedge_amt <- (initial_profit - (wager() * profit_target()))
            hedge_odd_need <- round(100 * (wager() * profit_target() + wager()) / (hedge_amt), 2)
            hedge_odd_need <- ifelse(hedge_odd_need < 100, -100 / hedge_odd_need * 100, hedge_odd_need)
            hedge_odd_need <- ifelse(profit_target() > max_profit, "NA", round(hedge_odd_need, 1))
            
            valueBox(
                hedge_odd_need,
                "Hedge Odd Need",
                icon = icon("list"),
                color = "purple"
            )
            
            
            
            
        })
})
    
    


shinyApp(
    ui = dashboardPage(header, sidebar, body),
    server = server)



