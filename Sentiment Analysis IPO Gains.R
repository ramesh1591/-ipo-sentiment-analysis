library(shiny)
library(quantmod)
library(tidytext)
library(dplyr)
library(rvest)
library(httr)
library(ggplot2)
library(DT)
library(wordcloud)
library(RColorBrewer)
library(tidyr)
library(jsonlite)
library(scales)
library(tm)
library(TTR)

# Function to style positive, negative, and neutral values
format_value <- function(value) {
  if (is.na(value)) {
    return("<b style='color:black;'>N/A</b>")
  }
  if (value > 0) {
    paste0("<b style='color:green;'>", sprintf("%.4f", value), "</b>")
  } else if (value < 0) {
    paste0("<b style='color:red;'>", sprintf("%.4f", value), "</b>")
  } else {
    paste0("<b style='color:black;'>", sprintf("%.4f", value), "</b>")
  }
}

# Improved get_google_news using httr::GET with user-agent
get_google_news <- function(query, pre_ipo = FALSE) {
  query <- gsub(" ", "+", query)
  if (pre_ipo) {
    query <- paste0(query, "+pre-IPO")
  } else {
    query <- paste0(query, "+IPO")
  }
  url <- paste0("https://news.google.com/rss/search?q=", query)
  
  response <- tryCatch({
    GET(url, user_agent("Mozilla/5.0"))
  }, error = function(e) NULL)
  
  if (!is.null(response) && response$status_code == 200) {
    # Force usage of httr::content explicitly
    news_page <- read_html(httr::content(response, as = "text", encoding = "UTF-8"))
    headlines <- news_page %>%
      html_nodes("item title") %>%
      html_text()
    return(headlines)
  } else {
    return(NULL)
  }
}

# Function to analyze sentiment using tidytext
analyze_sentiment <- function(news) {
  if (is.null(news) || length(news) == 0) {
    return(0)  # Default sentiment score when no news is available
  }
  
  news_tibble <- tibble(text = news) %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words, by = "word")
  
  sentiment_scores <- news_tibble %>%
    inner_join(get_sentiments("bing"), by = "word") %>%
    group_by(sentiment) %>%
    summarize(score = n(), .groups = "drop") %>%
    pivot_wider(names_from = sentiment, values_from = score, values_fill = 0) %>%
    mutate(sentiment_score = positive - negative)
  
  avg_sentiment <- mean(sentiment_scores$sentiment_score, na.rm = TRUE)
  return(avg_sentiment)
}

# UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Arial', sans-serif; 
        background: linear-gradient(to bottom, #f0f8ff, #e6f7ff);
      }
      .title {
        text-align: center;
        color: #2a3d66;
        font-size: 30px;
        margin-top: 20px;
        font-weight: bold;
      }
      .content-panel {
        background-color: #ffffff;
        border: 1px solid #ddd;
        border-radius: 10px;
        padding: 20px;
        box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.1);
        margin-bottom: 20px;
      }
      .btn-primary {
        background-color: #0078d7;
        border-color: #005fa3;
      }
      .btn-primary:hover {
        background-color: #005fa3;
      }
    "))
  ),
  titlePanel(div(class = "title", "Comprehensive IPO Sentiment and Historical Analysis")),
  sidebarLayout(
    sidebarPanel(
      selectInput("ipo1", "Select IPO 1:", choices = NULL),
      selectInput("ipo2", "Select IPO 2:", choices = NULL),
      numericInput("news_weight", "News Sentiment Weight:", value = 0.7, min = 0, max = 1),
      numericInput("gains_weight", "Gains Sentiment Weight:", value = 0.3, min = 0, max = 1),
      actionButton("analyze", "Analyze", class = " btn-primary"),
      br()
    ),
    mainPanel(
      div(class = "content-panel", h4("Sentiment Analysis Result"), htmlOutput("sentiment_result")),
      div(class = "content-panel", h4("Sentiment Score Plot"), plotOutput("sentiment_plot")),
      div(class = "content-panel", h4("IPO Details Table"), DTOutput("ipo_table")),
      div(class = "content-panel", h4("Sentiment Calculation Table"), DTOutput("sentiment_table")),
      div(class = "content-panel", h4("Historical Price Chart for IPO 1"), plotOutput("historical_chart_ipo1")),
      div(class = "content-panel", h4("Historical Price Chart for IPO 2"), plotOutput("historical_chart_ipo2")),
      div(class = "content-panel", h4("Word Cloud of Top News Headlines"), plotOutput("wordcloud_plot")),
      div(class = "content-panel", h4("Top 5 News Headlines"), uiOutput("top_news"))
    )
  )
)

# Server
server <- function(input, output, session) {
  # Use global 'IPO' data frame (already in environment)
  ipo_data <- IPO
  
  # Convert gains from % string to numeric fraction
  ipo_data$gains_percentage <- gsub("%", "", ipo_data$gains_percentage)
  ipo_data$gains_percentage <- as.numeric(ipo_data$gains_percentage) / 100
  ipo_data$normalized_gains <- (ipo_data$gains_percentage - min(ipo_data$gains_percentage, na.rm = TRUE)) /
    (max(ipo_data$gains_percentage, na.rm = TRUE) - min(ipo_data$gains_percentage, na.rm = TRUE))
  
  # Populate select inputs
  updateSelectInput(session, "ipo1", choices = ipo_data$company_name)
  updateSelectInput(session, "ipo2", choices = ipo_data$company_name)
  
  # Function to fetch current stock price (LTP)
  get_current_price <- function(symbol) {
    tryCatch({
      stock_data <- getQuote(symbol)
      return(stock_data$Last)
    }, error = function(e) {
      return(NA)
    })
  }
  
  observeEvent(input$analyze, {
    # Validation: Check if the sum of weights is exactly 1
    if ((input$news_weight + input$gains_weight) != 1) {
      showNotification("Error: The sum of News Sentiment Weight and Gains Sentiment Weight should be 1.", type = "error")
      return()  # Exit the function if validation fails
    }
    
    ipo1 <- input$ipo1
    ipo2 <- input$ipo2
    
    news1_pre <- get_google_news(ipo1, pre_ipo = TRUE)
    news2_pre <- get_google_news(ipo2, pre_ipo = TRUE)
    news1 <- get_google_news(ipo1, pre_ipo = FALSE)
    news2 <- get_google_news(ipo2, pre_ipo = FALSE)
    
    # Debug logs
    print("Top 20 Pre-IPO News for IPO1:")
    print(head(news1_pre, 20))
    print("Top 20 Pre-IPO News for IPO2:")
    print(head(news2_pre, 20))
    print("Top 20 Post-IPO News for IPO1:")
    print(head(news1, 20))
    print("Top 20 Post-IPO News for IPO2:")
    print(head(news2, 20))
    
    # Sentiment calculations
    pre_sentiment1 <- analyze_sentiment(news1_pre)
    pre_sentiment2 <- analyze_sentiment(news2_pre)
    sentiment1 <- analyze_sentiment(news1)
    sentiment2 <- analyze_sentiment(news2)
    
    ipo1_gain <- ipo_data$normalized_gains[ipo_data$company_name == ipo1]
    ipo2_gain <- ipo_data$normalized_gains[ipo_data$company_name == ipo2]
    ipo1_gain <- ifelse(is.na(ipo1_gain), 0, ipo1_gain)
    ipo2_gain <- ifelse(is.na(ipo2_gain), 0, ipo2_gain)
    
    # Pre-sentiment analysis using only news
    pre_sentiment1 <- 1 * pre_sentiment1
    pre_sentiment2 <- 1 * pre_sentiment2
    
    # Post-sentiment analysis
    sentiment1_final <- input$news_weight * sentiment1 + input$gains_weight * ipo1_gain
    sentiment2_final <- input$news_weight * sentiment2 + input$gains_weight * ipo2_gain
    
    ipo1_symbol <- ipo_data$stock_symbol[ipo_data$company_name == ipo1]
    ipo2_symbol <- ipo_data$stock_symbol[ipo_data$company_name == ipo2]
    
    # Get current LTP
    ipo1_price <- get_current_price(ipo1_symbol)
    ipo2_price <- get_current_price(ipo2_symbol)
    
    # Helper to create sentiment block
    create_sentiment_section <- function(ipo_name, pre_sentiment, post_sentiment, sentiment_change) {
      sentiment_color_pre <- ifelse(pre_sentiment > 0, "green", ifelse(pre_sentiment < 0, "red", "orange"))
      sentiment_color_post <- ifelse(post_sentiment > 0, "green", ifelse(post_sentiment < 0, "red", "orange"))
      sentiment_color_change <- ifelse(sentiment_change > 0, "green", "red")
      
      tags$div(
        style = "text-align: center; width: 250px; border: 1px solid #ddd; padding: 20px; border-radius: 12px; background-color: #f9f9f9;",
        tags$h4(ipo_name),
        tags$p(style = paste("color:", sentiment_color_pre), 
               tags$b("Pre_Sentiment:", sprintf("%.4f", pre_sentiment))),
        tags$p(style = paste("color:", sentiment_color_post), 
               tags$b("Post_Sentiment:", sprintf("%.4f", post_sentiment))),
        tags$p(style = "margin-top: 10px; font-size: 14px; color: gray;", 
               tags$b(style = paste("color:", sentiment_color_change), 
                      paste("Sentiment Change: ", sprintf("%.4f", sentiment_change))))
      )
    }
    
    output$sentiment_result <- renderUI({
      sentiment_change_1 <- abs(sentiment1_final - pre_sentiment1) * sign(sentiment1_final - pre_sentiment1)
      sentiment_change_2 <- abs(sentiment2_final - pre_sentiment2) * sign(sentiment2_final - pre_sentiment2)
      
      tagList(
        tags$div(
          style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 20px;",
          create_sentiment_section(ipo1, pre_sentiment1, sentiment1_final, sentiment_change_1),
          create_sentiment_section(ipo2, pre_sentiment2, sentiment2_final, sentiment_change_2)
        )
      )
    })
    
    output$ipo_table <- renderDT({
      ipo_table <- ipo_data %>%
        filter(company_name %in% c(ipo1, ipo2)) %>%
        select(
          Company = company_name,
          `Cutoff Price` = cutoff_price,
          `Listing Price` = listing_price,
          `Listing Date` = listing_date,
          `Gains (%)` = gains_percentage
        ) %>%
        mutate(
          `Current LTP` = if_else(Company == ipo1, ipo1_price, ipo2_price)
        ) %>%
        mutate(
          across(where(is.numeric), ~ round(.x, 4))
        ) %>%
        mutate(
          `Gains (%)` = scales::percent(`Gains (%)`)
        )
      
      datatable(
        ipo_table,
        options = list(
          pageLength = 5,
          autoWidth = TRUE,
          columnDefs = list(
            list(className = "dt-center", targets = "_all")
          )
        ),
        rownames = FALSE
      ) %>%
        formatStyle(
          columns = c("Gains (%)", "Listing Date", "Current LTP"),
          color = styleInterval(
            cuts = c(0, 0),
            values = c("red", "black", "green")
          )
        ) %>%
        formatStyle(
          columns = c("Cutoff Price", "Listing Price"),
          textAlign = "center"
        ) %>%
        formatStyle(
          columns = "Company",
          fontWeight = "bold",
          textAlign = "left"
        )
    })
    
    output$sentiment_table <- renderDT({
      sentiment_df <- data.frame(
        IPO = c(ipo1, ipo2),
        Pre_Sentiment = c(format_value(pre_sentiment1), format_value(pre_sentiment2)),
        News_Sentiment = c(format_value(sentiment1), format_value(sentiment2)),
        Gains_Sentiment = c(format_value(ipo1_gain), format_value(ipo2_gain)),
        Post_Sentiment = c(format_value(sentiment1_final), format_value(sentiment2_final))
      )
      datatable(sentiment_df, escape = FALSE, rownames = FALSE)
    })
    
    output$sentiment_plot <- renderPlot({
      sentiment_data <- data.frame(
        IPO = c(ipo1, ipo2),
        Pre_Sentiment = c(pre_sentiment1, pre_sentiment2),
        Post_Sentiment = c(sentiment1_final, sentiment2_final)
      )
      sentiment_data_long <- pivot_longer(
        sentiment_data,
        cols = c("Pre_Sentiment", "Post_Sentiment"),
        names_to = "Type",
        values_to = "Sentiment"
      )
      ggplot(sentiment_data_long, aes(x = IPO, y = Sentiment, fill = Type)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        geom_text(aes(label = sprintf("%.4f", Sentiment)), vjust = -0.5, position = position_dodge(0.9)) +
        scale_fill_manual(values = c("green", "red")) +
        theme_minimal() +
        labs(title = "IPO Sentiment Analysis", y = "Sentiment Score")
    })
    
    # Force RSI on a separate panel (no row checks)
    output$historical_chart_ipo1 <- renderPlot({
      chart_data1 <- tryCatch({
        getSymbols(ipo1_symbol, src = "yahoo", auto.assign = FALSE)
      }, error = function(e) {
        message("Error fetching data for IPO1: ", e)
        return(NULL)
      })
      if (!is.null(chart_data1)) {
        # main chart
        chartSeries(chart_data1, name = ipo1, theme = chartTheme("white"))
        
        # ALWAYS add RSI to a separate panel
        tryCatch({
          addTA(
            RSI(Cl(chart_data1), n = 14),
            col = "red", type = "line", on = NA, 
            lwd = 2, legend = "RSI(14)"
          )
        }, error = function(e) {
          message("Error adding RSI for IPO1: ", e)
        })
        
        # add MACD
        tryCatch({
          addMACD(fast = 12, slow = 26, signal = 9, col = c("red", "green", "blue"))
        }, error = function(e) {
          message("Error adding MACD for IPO1: ", e)
        })
      }
    })
    
    output$historical_chart_ipo2 <- renderPlot({
      chart_data2 <- tryCatch({
        getSymbols(ipo2_symbol, src = "yahoo", auto.assign = FALSE)
      }, error = function(e) {
        message("Error fetching data for IPO2: ", e)
        return(NULL)
      })
      if (!is.null(chart_data2)) {
        chartSeries(chart_data2, name = ipo2, theme = chartTheme("white"))
        
        # ALWAYS add RSI to a separate panel
        tryCatch({
          addTA(
            RSI(Cl(chart_data2), n = 14),
            col = "red", type = "line", on = NA,
            lwd = 2, legend = "RSI(14)"
          )
        }, error = function(e) {
          message("Error adding RSI for IPO2: ", e)
        })
        
        # add MACD
        tryCatch({
          addMACD(fast = 12, slow = 26, signal = 9, col = c("red", "green", "blue"))
        }, error = function(e) {
          message("Error adding MACD for IPO2: ", e)
        })
      }
    })
    
    output$wordcloud_plot <- renderPlot({
      all_news <- c(news1, news2)
      wordcloud(words = all_news, min.freq = 2, scale = c(3, 0.5), colors = brewer.pal(8, "Dark2"))
    })
    
    output$top_news <- renderUI({
      top_news <- c(head(news1, 5), head(news2, 5))
      tagList(lapply(top_news, function(news_item) {
        tags$p(news_item)
      }))
    })
  })
}

shinyApp(ui = ui, server = server)
