# Creates plot of top 15 most frequent terms

graph.token <- function(x) {
  custom.stop.words <- tribble(
    ~word, ~lexicon,
    "http", "CUSTOM", # Common URL term
    "twd", "CUSTOM", # Within top 15, I don't know what twd means, Better to leave it out I suppose.
    "t.co", "CUSTOM", # IDK what this term is but it's a common one regarding twitter data that is not useful.
    "de", "CUSTOM", # Also in top 15, I don't think it's useful.
    "13", "CUSTOM",
    "16", "CUSTOM",
    "im", "CUSTOM" # Not needed
  )

  # Bind custom to original
  new.stop.words <- stop_words %>% bind_rows(custom.stop.words)
  # Apply new stop words
  y <- x %>% unnest_tokens(word, Comments) %>% anti_join(new.stop.words)

  # ggplot graph 15 most common
  z <- y %>% count(word) %>% arrange(desc(n))%>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) +
    geom_col(aes(fill = word)) + labs( y = "Frequency", x = "Terms", title = "Top 15 Most Frequent Terms") + coord_flip() +
    theme(legend.position = "none")

  return(z)
}








# Creates plot of top 10 frequent terms grouped by sentiment


graph.sentiment <- function(x) {
  custom.stop.words <- tribble(
    ~word, ~lexicon,
    "http", "CUSTOM", # Common URL term
    "twd", "CUSTOM", # Within top 15, I don't know what twd means, Better to leave it out I suppose.
    "t.co", "CUSTOM", # IDK what this term is but it's a common one regarding twitter data that is not useful.
    "de", "CUSTOM", # Also in top 15, I don't think it's useful.
    "13", "CUSTOM",
    "16", "CUSTOM" # Not needed
  )

  # Bind custom to original
  new.stop.words <- stop_words %>% bind_rows(custom.stop.words)
  # Apply new stop words
  y <- x %>% unnest_tokens(word, Comments) %>% anti_join(new.stop.words)


  z <- y %>% inner_join(get_sentiments("nrc")) %>%
    filter(sentiment %in% c("positive", "negative")) %>% count(word, sentiment) %>%
    group_by(sentiment) %>% top_n(10, n) %>% ungroup() %>% mutate(
      word2 = fct_reorder(word, n))

  graph <- ggplot(z, aes(x = word2, y = n, fill = sentiment)) +
    geom_col(show.legend = FALSE) +facet_wrap(~sentiment, scale = "free") +
    coord_flip() + labs(y = "Frequency", x = "Terms")

  return(graph)
}






# Creates one Bar, Bubble, and Line graph

graph.trio <- function(x) {

  ##### BUBBLE #####
  # remove the major outliers from each of three categories
  # Tidy up Min and Max for graph : Engagement
  data <- x[-160,]
  rownames(data) <- NULL


  # Tidy up Min and Max for graph : Retweets
  data <- data[-704,]
  rownames(data) <- NULL


  # Tidy up Min and Max for graph : Impressions
  data <- data[-644,]
  rownames(data) <- NULL

  Bubble <- ggplot(data, aes(x = Twitter_Activity, y = Engagement)) +
    geom_point(aes(size = Impressions), alpha = 0.3, color = "blue") +
    xlim(0, 1000) + ylim(100, 1000)  + scale_size(range = c(2, 10), limits = c(0,25000)) +
    labs(title = "Squid Game through Peirce's sign theory")



  focus <- x[, c(3:5)]

  retweetSum <- colSums(focus[,1])
  retweetSum

  ImpresSums <- colSums(focus[,2])
  ImpresSums

  EngagSum <- colSums(focus[,3])
  EngagSum

  allSums <- c(retweetSum, ImpresSums, EngagSum)
  names <- c("Twitter_Activity", "Impressions", "Engagement")

  barDF <- data.frame(allSums, names)

  Bar <- ggplot(barDF, aes(x = names, y = allSums, fill = names)) + geom_col() +
    labs(x = "Variable Types", y = "Total Sums", title = "Squid Game through Peirce's sign theory") +
    scale_y_continuous(limits=c(0,3000000), oob = rescale_none, labels = comma) + theme(legend.position = "none")





  line <- ggplot(x, aes(x = Impressions, y = Engagement, color = Twitter_Activity)) + geom_line(size = 1) +
    labs(title = "Squid Game through Peirce's sign theory") + scale_color_continuous(high = "black", low = "orange") +
    scale_x_continuous(limits = c(0, 3000)) + scale_color_continuous(high = "black", low = "orange", limits = c(0, 1000))

  z <- list(Bubble, Bar, line)

  return(z)

}










# Creates shiny scatter plot

shinySG <- function(x) {
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        # Use pop size for x and y axis
        selectInput("variableX", "X-Axis:",
                    c("Likes" = "Impressions",
                      "Retweets" = "Twitter_Activity",
                      "Engagement" = "Engagement"
                    )),


        selectInput("variableY", "Y-Axis:",
                    c("Engagement" = "Engagement",
                      "Likes" = "Impressions",
                      "Retweets" = "Twitter_Activity"

                    )),


        selectInput("variableC", "Color:",
                    c( "Retweets" = "Twitter_Activity",
                       "Likes" = "Impressions",
                       "Engagement" = "Engagement"
                    )),



      ),

      mainPanel(plotOutput(outputId = "plot"))
    )
  )




  server <- function(input, output) {

    data <- data.frame(x)




    output$plot <- renderPlot({ggplot(data, aes(x = data[, input$variableX], y = data[, input$variableY], color = data[, input$variableC])) +
        geom_point(size = 3, alpha = 0.7) + scale_x_continuous(limits = c(0, 10000)) + scale_y_continuous(limits = c(0, 10000)) + scale_color_continuous(limits = c(0, 5000), high = "orange", low = "white") +
        labs(Title = "Squid Game through Peirce's sign theory", x = input$variableX, y = input$variableY, size = input$variableS, color = input$variableC)
    }, height = 600, width = 750)


  }

  shine <- shinyApp(ui = ui, server = server)
  return(shine)

}






clean <- function(x) {
  twitter.data <- x

  name <- "Twitter_Activity"
  colnames(twitter.data)[3] <- name
  head(twitter.data)
  twitter.data <- twitter.data[,-8:-11]
  twitter.data <- na.omit(twitter.data)

  twitter.data[which.max(twitter.data$Engagement),] # major outlier skewed graph
  match("bigbertha12", twitter.data$Username)
  twitter.data <- twitter.data[-160,]
  rownames(twitter.data) <- NULL


  # Tidy up Min and Max for graph : Retweets
  twitter.data[which.max(twitter.data$Twitter_Activity),] # another major outlier
  match("danaiguriratwd", twitter.data$Username)
  twitter.data <- twitter.data[-704,]
  rownames(twitter.data) <- NULL


  # Tidy up Min and Max for graph : Impressions
  twitter.data[which.max(twitter.data$Impressions),]
  match("naaaaaaateb", twitter.data$Username)
  twitter.data <- twitter.data[-644,]
  rownames(twitter.data) <- NULL

  return(twitter.data)

}
x <- clean(twitter.data)
