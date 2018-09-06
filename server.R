library(dplyr)

function(input, output, session) {
  
  output$wineImage <- renderImage({

  })

  ##Summary Page 
  output$bubbleChart <- renderPlotly({
    summary_df = cleantable %>% na.omit() %>% group_by(Country) %>%
      summarise(avg_price = mean(Price),avg_points = mean(Points),total_reviews = n())
    summary_df = summary_df %>% mutate(ratio = avg_price/avg_points)
    
    summary_df = summary_df %>% mutate(Co = cor(avg_price, avg_points))
    
    
    ggplot(summary_df, aes(x=avg_points, y=avg_price), main = "Overview: Points vs Price by Country") + geom_point(aes(size = (total_reviews), colour = summary_df$Country, alpha= Co)) + xlim(c(85,92)) + ylim(c(20,50)) + geom_text(aes(label = summary_df$Country), size = 3, nudge_x = 0.0, nudge_y = 0) + scale_size_continuous(range=c(1, 20))
})
  
  ### word cloud page
  observe({
    cloudProvince <- if (is.null(input$cloudCountry)) character(0) else {
      filter(cleantable, Country %in% input$cloudCountry) %>%
        `$`('Province') %>%
        unique() %>%
        sort()
    }
    
    stillSelected <- isolate(input$cloudProvince[input$cloudProvince %in% cloudProvince])
    updateSelectInput(session, "cloudProvince", choices = cloudProvince,
                      selected = stillSelected)
  })
  
  observe({
    cloudRegion1 <- if (is.null(input$cloudCountry)) character(0) else {
      cleantable %>%
        filter(Province %in% input$cloudProvince,
               is.null(input$cloudProvince) | Province %in% input$cloudProvince) %>%
        `$`('Region1') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$cloudRegion1[input$cloudRegion1 %in% cloudRegion1])
    updateSelectInput(session, "cloudRegion1", choices = cloudRegion1,
                      selected = stillSelected)
  })
  observe({
    cloudRegion2 <- if (is.null(input$cloudCountry)) character(0) else {
      cleantable %>%
        filter(Region1 %in% input$cloudRegion1,
               is.null(input$cloudProvince) | Province %in% input$cloudProvince) %>%
        `$`('Region2') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$cloudRegion2[input$cloudRegion2 %in% cloudRegion2])
    updateSelectInput(session, "cloudRegion2", choices = cloudRegion2,
                      selected = stillSelected)
  })
  observe({
    cloudWinery <- if (is.null(input$cloudCountry)) character(0) else {
      cleantable %>%
        filter(Province %in% input$cloudProvince,
               is.null(input$cloudProvince) | Province %in% input$cloudProvince) %>%
        `$`('Winery') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$cloudWinery[input$cloudWinery %in% cloudWinery])
    updateSelectInput(session, "cloudWinery", choices = cloudWinery,
                      selected = stillSelected)
  })
  
  output$wordCloud <- renderPlot({

     df5 = vtable %>%
       filter(
         is.null(input$cloudCountry) | Country %in% input$cloudCountry,
         is.null(input$cloudProvince) | Province %in% input$cloudProvince,
         is.null(input$cloudRegion1) | Region1 %in% input$cloudRegion1,
         is.null(input$cloudRegion2) | Region2 %in% input$cloudRegion2,
         is.null(input$cloudWinery) | Winery %in% input$cloudWinery,
         is.null(input$cloudVarietry) | Variety %in% input$cloudVariety
       )
     wordcloud_rep <- repeatable(wordcloud)
     v <- getTermMatrix(df5$Description)
     wordcloud_rep(names(v), v, scale=c(4,0.5),
                   min.freq = 10, max.words=50,
                   colors=brewer.pal(8, "Dark2"))
     #wordcloud(d$word, d$freq,random.order = FALSE, rot.per = 0.1, scale = c(4,.2), max.words = 30, colors = brewer.pal(8, 'Dark2'))
     title(main = 'world cloud = unigram', font.main = 3, cex.main = 1.5)
  })
  
  # Data Explorer 
  observe({
    province <- if (is.null(input$country)) character(0) else {
      filter(cleantable, Country %in% input$country) %>%
        `$`('Province') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$province[input$province %in% province])
    updateSelectInput(session, "province", choices = province,
                      selected = stillSelected)
  })
  
  observe({
    region_1 <- if (is.null(input$country)) character(0) else {
      cleantable %>%
        filter(Country %in% input$country,
               is.null(input$province) | Province %in% input$province) %>%
        `$`('Region1') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$region_1[input$region_1 %in% region_1])
    updateSelectInput(session, "region_1", choices = region_1,
                      selected = stillSelected)
  })
  
  observe({
    region_2 <- if (is.null(input$country)) character(0) else {
      cleantable %>%
        filter(Country %in% input$country,
               is.null(input$province) | Province %in% input$province) %>%
        `$`('Region2') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$region_2[input$region_2 %in% region_2])
    updateSelectInput(session, "region_2", choices = region_2,
                      selected = stillSelected)
  })
  
  observe({
    winery <- if (is.null(input$country)) character(0) else {
      cleantable %>%
        filter(Country %in% input$country,
               is.null(input$province) | Province %in% input$province) %>%
        `$`('Winery') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$winery[input$winery %in% winery])
    updateSelectInput(session, "winery", choices = winery,
                      selected = stillSelected)
  })
  
  output$winetable <- DT::renderDataTable({
    df <- cleantable %>%
      filter(
        Points >= input$rangePoints[1],
        Points <= input$rangePoints[2],
        Price >= input$rangePrice[1],
        Price <= input$rangePrice[2],
        is.null(input$country) | Country %in% input$country,
        is.null(input$province) | Province %in% input$province,
        is.null(input$region_1) | Region1 %in% input$region_1,
        is.null(input$region_2) | Region2 %in% input$region_2,
        is.null(input$winery) | Winery %in% input$winery,
        is.null(input$variety) | Variety %in% input$variety
      )
    
    DT::datatable(df, options = list(lengthMenu = c(5, 30, 50), escape = FALSE))
  })
  
  #### visuals page
  observe({
    histProvince <- if (is.null(input$histCountry)) character(0) else {
      filter(cleantable, Country %in% input$histCountry) %>%
        `$`('Province') %>%
        unique() %>%
        sort()
    }
    
    stillSelected <- isolate(input$histProvince[input$histProvince %in% histProvince])
    updateSelectInput(session, "histProvince", choices = histProvince,
                      selected = stillSelected)
  })
  
  observe({
    histRegion1 <- if (is.null(input$histCountry)) character(0) else {
      cleantable %>%
        filter(Country %in% input$histCountry,
               is.null(input$histProvince) | Province %in% input$histProvince) %>%
        `$`('Region1') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$histRegion1[input$histRegion1 %in% histRegion1])
    updateSelectInput(session, "histRegion1", choices = histRegion1,
                      selected = stillSelected)
  })
  
  observe({
    histRegion2 <- if (is.null(input$histCountry)) character(0) else {
      cleantable %>%
        filter(Country %in% input$histCountry,
               is.null(input$histProvince) | Province %in% input$histProvince) %>%
        `$`('Region2') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$histRegion2[input$histRegion2 %in% histRegion2])
    updateSelectInput(session, "histRegion2", choices = histRegion2,
                      selected = stillSelected)
  })
  
  output$hist <- renderPlot({
    df2 = cleantable %>%
      filter(
        is.null(input$histCountry) | Country %in% input$histCountry,
        is.null(input$histProvince) | Province %in% input$histProvince,
        is.null(input$histRegion1) | Region1 %in% input$histRegion1,
        is.null(input$histRegion2) | Region2 %in% input$histRegion2,
        is.null(input$histWinery) | Winery %in% input$histWinery,
        is.null(input$histVariety) | Variety %in% input$histVariety
      )
    x = df2$Points
    bins <- seq(min(x), max(x), length.out = 10)
    
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Points",
         main = "Histogram of Wine Points")
  })
  
  output$priceGraph <- renderPlot({
    df3 = cleantable %>%
      filter(
        is.null(input$histCountry) | Country %in% input$histCountry,
        is.null(input$histProvince) | Province %in% input$histProvince,
        is.null(input$histRegion1) | Region1 %in% input$histRegion1,
        is.null(input$histRegion2) | Region2 %in% input$histRegion2,
        is.null(input$histWinery) | Winery %in% input$histWinery,
        is.null(input$histVariety) | Variety %in% input$histVariety
      )
    wine.by.year = df3 %>% na.omit() %>% group_by(VintageYear) %>% summarise(avg.price = mean(Price))
    wine.by.year = wine.by.year[order(wine.by.year$VintageYear),]
    ggplot(data = wine.by.year, aes(x = VintageYear, y = avg.price)) + geom_line()
  })
}