server <- function(input, output) {
  

  # VALUE BOX SPARK ---------------------------------------------
  # Reference: https://jkunst.com/blog/posts/2020-06-26-valuebox-and-sparklines/
  
  valueBoxSpark <- function(value, title, sparkobj = NULL, subtitle, info = NULL, 
                            icon = NULL, color = "teal", width = 12, href = NULL){
    shinydashboard:::validateColor(color)
    if (!is.null(icon))
      shinydashboard:::tagAssert(icon, type = "i")
    
    info_icon <- tags$small(
      tags$i(
        class = "fa fa-info-circle fa-lg",
        title = info,
        `data-toggle` = "tooltip",
        style = "color: rgba(255, 255, 255, 0.75);"
      ),
      # bs3 pull-right 
      # bs4 float-right
      class = "pull-right float-right"
    )
    
    boxContent <- div(
      class = paste0("info-box small-box bg-", color),
      div(
        class = "inner",
        tags$small(title),
        if (!is.null(sparkobj)) info_icon,
        h3(value),
        if (!is.null(sparkobj)) sparkobj,
        p(subtitle)
      ),
      # bs3 icon-large
      # bs4 icon
      if (!is.null(icon)) div(class = "icon-large icon", icon, style = "z-index; 0")
    )
    
    if (!is.null(href)) 
      boxContent <- a(href = href, boxContent)
    
    div(
      class = if (!is.null(width)) paste0("col-sm-", width), 
      boxContent
    )
  }
  
  # VALUE BOX SPARK - END --------------------------------------------
  
  # PEOPLE TAB - START -----------------------------------------------
  output$customersIncome <- renderValueBox({
    
    hcCustomerIncome <- hchart(
      density(marketing$Income),
      type = "area", name = "Income",
      color = "#db902e"
    ) %>% 
      hc_xAxis(min = 0) %>% 
      hc_size(height = 200) %>% 
      hc_title(text = "Customers Income",
               style = list(color = "#f0f0f0", fontSize =16)) %>% 
      hc_tooltip(crosshairs = TRUE,
                 borderWidth = 2,
                 pointFormat = '<span style="color:{series.color}">{series.name}</span>:
                       <b>${point.x:.4f}</b><br/>')
    
    vbCustomerIncome <- valueBoxSpark(
      value = dollar(mean(marketing$Income), 
                     prefix = "$", big.mark = ",",
                     decimal.mark = ".", 
                     accuracy = 0.01),
      title = toupper("AVERAGE CUSTOMER INCOME"),
      sparkobj = hcCustomerIncome,
      info = "Average customer income based on marketing data",
      subtitle = NULL,
      icon = icon("money-bill-wave"),
      href = NULL
    )
    
    vbCustomerIncome
  })
  
  output$customerSpentMoney <- renderValueBox({
    customerSpent <- marketing %>% 
      select(MntWines, MntFruits, MntMeatProducts, MntFishProducts, MntSweetProducts, MntGoldProds) %>% 
      mutate(TotalSpent = MntWines + MntFruits + MntMeatProducts + 
               MntFishProducts + MntSweetProducts + MntGoldProds)
    
    hcCustomerSpent <- hchart(
      density(customerSpent$TotalSpent),
      type = "area", name = "Total Spent",
      color = "#db902e"
    ) %>% 
      hc_xAxis(min = 0,
               color = "#f0f0f0") %>% 
      hc_size(height = 200) %>% 
      hc_title(text = "Customers Total Spent",
               style = list(color = "#f0f0f0", fontSize =16)) %>% 
      hc_tooltip(crosshairs = TRUE,
                 borderWidth = 2,
                 pointFormat = '<span style="color:{series.color}">{series.name}</span>:
                       <b>${point.x:.4f}</b><br/>')
    
    vbCustomerSpent <- valueBoxSpark(
      value = dollar(mean(customerSpent$TotalSpent), 
                     prefix = "$", big.mark = ",",
                     decimal.mark = ".",
                     accuracy = 0.01),
      title = toupper("AVERAGE CUSTOMER SPENT"),
      sparkobj = hcCustomerSpent,
      info = "Average customers total spent based on marketing data",
      subtitle = NULL,
      icon = icon("money-bill-wave"),
      href = NULL
    )
    
    vbCustomerSpent
    
  })
  
  output$customerPurchases <- renderValueBox({
    customerPurchases <- marketing %>% 
      select(NumWebPurchases, NumCatalogPurchases, NumStorePurchases) %>% 
      mutate(TotalPurchases = NumWebPurchases + NumCatalogPurchases + NumStorePurchases)
    
    hcCustomerPurchases <- hchart(
      density(customerPurchases$TotalPurchases),
      type = "area", name = "Total Purchases",
      color = "#db902e"
    ) %>% 
      hc_xAxis(min = 0,
               color = "#f0f0f0") %>% 
      hc_size(height = 200) %>% 
      hc_title(text = "Customers Total Purchases",
               style = list(color = "#f0f0f0", fontSize =16)) %>% 
      hc_tooltip(crosshairs = TRUE,
                 borderWidth = 2,
                 pointFormat = '<span style="color:{series.color}">{series.name}</span>:
                       <b>{point.x:.1f}</b><br/>')
    
    vbCustomerPurchases <- valueBoxSpark(
      value = dollar(mean(customerPurchases$TotalPurchases), 
                     prefix = NULL, big.mark = ",",
                     decimal.mark = ".",
                     accuracy = 0.1),
      title = toupper("AVERAGE CUSTOMER PURCHASES"),
      sparkobj = hcCustomerPurchases,
      info = "Total customers purchases based on marketing data",
      subtitle = NULL,
      icon = icon("money-bill-wave"),
      href = NULL
    )
    
    vbCustomerPurchases
  })
  
  # ADDITIONAL FUNCTION - getCustomDataCustomers ------------------------------
  getCustomDataCustomers <- function(input, feature) {
    param <- input$groupBySelector
    data <- "" 
    if (feature == "Income") {
      if (param == "Marital Status") {
        data <- marketing %>% 
          group_by(Marital_Status)
      } else if (param == "Education") {
        data <- marketing %>% 
          group_by(Education) 
      } else if (param == "Generation") {
        data <- marketing %>% 
          group_by(Era)
      } else {
        data <- marketing %>% 
          group_by(CountryName)
      }
      data <- data %>% 
        summarise(MeanIncome = mean(Income)) %>% 
        arrange(MeanIncome)
      return(data)
    } else {
      data <- marketing %>% 
        mutate(TotalSpent = MntWines + MntFruits + MntMeatProducts + 
                 MntFishProducts + MntSweetProducts + MntGoldProds)
      if (param == "Marital Status") {
        data <- data %>% 
          group_by(Marital_Status)
      } else if (param == "Education") {
        data <- data %>% 
          group_by(Education)
      } else if (param == "Generation") {
        data <- data %>% 
          group_by(Era)
      } else {
        data <- data %>% 
          group_by(CountryName)
      }
      data <- data %>% 
        summarise(MeanTotalSpent = mean(TotalSpent)) %>% 
        arrange(desc(MeanTotalSpent))
      return(data)
    }
  }
  # ADDITIONAL FUNCTION - getCustomDataCustomers - END -----------------------
  
  output$incomeBy <- renderEcharts4r({
    param <- input$groupBySelector
    data <- getCustomDataCustomers(input = input, feature = "Income")
    
    plot <- ""
    if (param == "Marital Status") {
      plot <- data %>% 
        e_chart(Marital_Status)
    } else if (param == "Education") {
      plot <- data %>% 
        e_chart(Education)
    } else if (param == "Generation") {
      plot <- data %>% 
        e_chart(Era)
    } else {
      plot <- data %>% 
        e_chart(CountryName)
    }
    plot <- plot %>% 
      e_pie(MeanIncome, radius = c("50%", "75%")) %>% 
      e_theme_custom("www/chart_theme.json") %>% 
      e_title(
        text = glue("Mean Income by {param}"),
        left = "center",
        top = "0"
      ) %>% 
      e_legend(F) %>% 
      e_tooltip(
        trigger = "item",
        formatter = JS("
                    function(params){return(
                        params.name + '<br><b>Mean Income</b>'
                           + ' : $'
                           + (params.value).toLocaleString('en-US', 
                           {maximumFractionDigits : 2, minimumFractionDigits: 2})
                           )}
                           ")
      )
    plot
  })
  
  
  
  output$spentBy <- renderEcharts4r({
    
    param <- input$groupBySelector
    data <- getCustomDataCustomers(input = input, feature = "Spent")
    
    plot <- ""
    
    if (param == "Marital Status") {
      plot <- data %>% 
        e_chart(Marital_Status)
    } else if (param == "Education") {
      plot <- data %>% 
        e_chart(Education)
    } else if (param == "Generation") {
      plot <- data %>% 
        e_chart(Era)
    } else {
      plot <- data %>% 
        e_chart(CountryName)
    }
    plot <- plot %>% 
      e_bar(MeanTotalSpent) %>% 
      e_flip_coords() %>% 
      e_y_axis(inverse = T) %>% 
      e_theme_custom("www/chart_theme.json") %>% 
      e_title(
        text = glue("Mean Total Spent by {param}"),
        left = "center",
        top = "0"
      ) %>% 
      e_legend(show = F) %>% 
      e_axis_labels(x = "Mean Total Spent") %>% 
      e_x_axis(
        name = "Mean Total Spent",
        nameLocation = "center",
        nameGap = "25",
        formatter = e_axis_formatter(style = c("currency"), currency = "USD")) %>%
      e_tooltip(
        trigger = "item",
        formatter = JS(
          "
       function(params){return(
       params.name + '<br><b>Mean Total Spent</b>'
       + ' : $' 
       + params.value[0]
       )}
       "
        )
      )
    plot
  })
  
  # PEOPLE / CUSTOMER TAB - END -----------------------------------------------
  
  # PRODUCTS TAB - START -----------------------------------------------
  
  output$spentEachProduct <- renderEcharts4r({
    
    param <- input$groupByInputSelector
    orderBy <- input$orderByButton
    
    productSpent <- marketing %>% 
      select(Marital_Status,Education, Era,CountryName, MntWines, MntFruits, MntMeatProducts,
             MntFishProducts, MntSweetProducts, MntGoldProds)
    
    colnames(productSpent) <- c("Marital_Status", "Education", "Era","Country",
                                "Wines", "Fruits", "Meats", 
                                "Fishs", "Sweets", "Gold")
    
    if (param == "Marital Status") {
      productSpent <- productSpent %>% 
        group_by(Marital_Status)
    } else if (param == "Education") {
      productSpent <- productSpent %>% 
        group_by(Education)
    } else if (param == "Generation") {
      productSpent <- productSpent %>% 
        group_by(Era)
    } else {
      productSpent <- productSpent %>% 
        group_by(Country)
    }
    
    productSpent <- productSpent %>% 
      summarise(Wines = mean(Wines), 
                Fruits = mean(Fruits),
                Meats = mean(Meats),
                Fishs = mean(Fishs),
                Sweets = mean(Sweets),
                Gold = mean(Gold)) %>% 
      mutate(MeanTotalSpent = Wines + Fruits + Meats + Fishs + Sweets + Gold)
    
    if (orderBy == "Wines") {
      productSpent <- productSpent %>% 
        arrange(desc(Wines))
    } else if (orderBy == "Fruits") {
      productSpent <- productSpent %>% 
        arrange(desc(Fruits))
    } else if (orderBy == "Meats") {
      productSpent <- productSpent %>% 
        arrange(desc(Meats))
    } else if (orderBy == "Fishs") {
      productSpent <- productSpent %>% 
        arrange(desc(Fishs))
    } else if (orderBy == "Sweets") {
      productSpent <- productSpent %>% 
        arrange(desc(Sweets))
    } else if (orderBy == "Gold") {
      productSpent <- productSpent %>% 
        arrange(desc(Gold))
    } else {
      productSpent <- productSpent %>% 
        arrange(desc(MeanTotalSpent))
    }
    
    plot <- ""
    
    if (param == "Marital Status") {
      plot <- productSpent %>% 
        e_chart(Marital_Status)
    } else if (param == "Education") {
      plot <- productSpent %>% 
        e_chart(Education)
    } else if (param == "Generation"){
      plot <- productSpent %>% 
        e_chart(Era)
    } else {
      plot <- productSpent %>% 
        e_chart(Country)
    }
    
    plot <- plot %>% 
      e_bar(Wines, stack = "grp") %>% 
      e_bar(Fruits, stack = "grp") %>% 
      e_bar(Meats, stack = "grp") %>% 
      e_bar(Fishs, stack = "grp") %>% 
      e_bar(Sweets, stack = "grp") %>% 
      e_bar(Gold, stack = "grp") %>% 
      e_flip_coords() %>% 
      e_y_axis(inverse = T) %>% 
      e_theme_custom("www/chart_theme.json") %>% 
      e_title(
        text = glue("Mean Total Spent by {param}"),
        left = "center",
        top = "0"
      ) %>% 
      e_legend(top = "30") %>% 
      e_axis_labels(x = "Mean Total Spent") %>% 
      e_x_axis(
        name = "Mean Total Spent",
        nameLocation = "center",
        nameGap = "25",
        formatter = e_axis_formatter(style = c("currency"), currency = "USD")) %>%
      e_tooltip(
        trigger = "item",
        formatter = JS(
          "
       function(params){return(
       '<b>Mean Total Spent</b>'
       + ' : $' 
       + params.value[0]
       )}
       "
        )
      )
    plot
  })
  
  output$histProducts <- renderEcharts4r({
    productType = input$productTypeSelector
    productSpent <- marketing %>% 
      select(Marital_Status,Education, Era, MntWines, MntFruits, MntMeatProducts,
             MntFishProducts, MntSweetProducts, MntGoldProds)
    
    colnames(productSpent) <- c("Marital_Status", "Education", "Era",
                                "Wines", "Fruits", "Meats", 
                                "Fishs", "Sweets", "Gold")
    plot <- productSpent %>% 
      e_charts()
    
    if (productType == "Wines") {
      plot <- plot %>% 
        e_histogram(Wines)
    } else if (productType == "Fruits") {
      plot <- plot %>% 
        e_histogram(Fruits)
    } else if (productType == "Meats") {
      plot <- plot %>% 
        e_histogram(Meats)
    } else if (productType == "Fishs") {
      plot <- plot %>% 
        e_histogram(Fishs)
    } else if (productType == "Sweets") {
      plot <- plot %>% 
        e_histogram(Sweets)
    } else {
      plot <- plot %>% 
        e_histogram(Gold)
    }
    plot <- plot %>% 
      e_theme_custom("www/chart_theme.json") %>% 
      e_title(
        text = glue("Customers Amount Spent - {productType}"),
        left = "center",
        top = "0"
      ) %>% 
      e_axis_labels(x = "Amount Spent") %>% 
      e_x_axis(
        name = "Amount Spent",
        nameLocation = "center",
        nameGap = "25",
        formatter = e_axis_formatter(style = c("currency"), currency = "USD")) %>%
      e_tooltip(
        trigger = "item",
        formatter = JS(
          "
       function(params){return(
       '<b>Freq</b>'
       + ' : ' 
       + params.value[1]
       )}
       "
        )
      )
  })
  
  output$boxplotProducts <- renderEcharts4r({
    param <- input$boxplotGroupSelector
    productType = input$productTypeSelector
    
    productSpent <- marketing %>% 
      select(Marital_Status,Education, Era,CountryName, MntWines, MntFruits, MntMeatProducts,
             MntFishProducts, MntSweetProducts, MntGoldProds)
    
    colnames(productSpent) <- c("Marital_Status", "Education", "Era", "Country",
                                "Wines", "Fruits", "Meats", 
                                "Fishs", "Sweets", "Gold")
    
    plot <- ""
    
    if (param == "Marital Status") {
     plot <- productSpent %>% 
       group_by(Marital_Status)
    } else if (param == "Education") {
      plot <- productSpent %>% 
        group_by(Education)
    } else if (param == "Generation"){
      plot <- productSpent %>% 
        group_by(Era)
    } else {
      plot <- productSpent %>% 
        group_by(Country)
    }
    
    plot <- plot %>% 
      e_charts()
    
    if (productType == "Wines") {
      plot <- plot %>% 
        e_boxplot(Wines, itemStyle = list(color = "#db902e"))
    } else if (productType == "Fruits") {
      plot <- plot %>% 
        e_boxplot(Fruits, itemStyle = list(color = "#db902e"))
    } else if (productType == "Meats") {
      plot <- plot %>% 
        e_boxplot(Meats, itemStyle = list(color = "#db902e"))
    } else if (productType == "Fishs") {
      plot <- plot %>% 
        e_boxplot(Fishs, itemStyle = list(color = "#db902e"))
    } else if (productType == "Sweets") {
      plot <- plot %>% 
        e_boxplot(Sweets, itemStyle = list(color = "#db902e"))
    } else {
      plot <- plot %>% 
        e_boxplot(Gold, itemStyle = list(color = "#db902e"))
    }
    
    plot <- plot %>% 
      e_theme_custom("www/chart_theme.json") %>% 
      e_title(
        text = glue("Customers Amount Spent by {param}"),
        left = "center",
        top = "0"
      ) %>% 
      e_axis_labels(x = glue("{param}"),
                    y = "Amount Spent") %>% 
      e_x_axis(
        name = glue("{param}"),
        nameLocation = "center",
        nameGap = "25") %>%
      e_tooltip(trigger = c("item", "axis"))
    plot
  })
  
  # PRODUCTS TAB - END -----------------------------------------------
  
  # PLATFORM TAB - START -----------------------------------------------
  
  output$webPurchases <- renderValueBox({
    hcWebPurchases <- hchart(
      density(marketing$NumWebPurchases),
      type = "area",
      name = "Web Purchases",
      color = "#db902e"
    ) %>% 
      hc_xAxis(min = 0) %>% 
      hc_size(height = 200) %>% 
      hc_title(text = "Web Purchases",
               style = list(color = "#f0f0f0", fontSize =16)) %>% 
      hc_tooltip(borderWidth = 2,
                 crosshairs = T,
                 pointFormat = '<span style="color:{series.color}">{series.name}</span>:
                       <b>{point.x:.1f}</b><br/>')
    
    vbWebPurchases <- valueBoxSpark(
      value = sum(marketing$NumWebPurchases),
      title = toupper("TOTAL WEB PURCHASES"),
      sparkobj = hcWebPurchases,
      info = "Number of purchases made through the company's website",
      subtitle = NULL,
      icon = icon("store"),
      href = NULL
    )
    vbWebPurchases
  })
  
  output$catalogPurchases <- renderValueBox({
    hcCatalogPurchases <- hchart(
      density(marketing$NumCatalogPurchases),
      type = "area",
      name = "Catalog Purchases",
      color = "#db902e"
    ) %>% 
      hc_xAxis(min = 0) %>% 
      hc_size(height = 200) %>% 
      hc_title(text = "Catalog Purchases",
               style = list(color = "#f0f0f0", fontSize =16)) %>% 
      hc_tooltip(borderWidth = 2,
                 crosshairs = T,
                 pointFormat = '<span style="color:{series.color}">{series.name}</span>:
                       <b>{point.x:.1f}</b><br/>')
    
    vbCatalogPurchases <- valueBoxSpark(
      value = sum(marketing$NumCatalogPurchases),
      title = toupper("TOTAL CATALOG PURCHASES"),
      sparkobj = hcCatalogPurchases,
      info = "Number of purchases made using catalog",
      subtitle = NULL,
      icon = icon("store"),
      href = NULL
    )
    vbCatalogPurchases
  })
  
  output$storePurchases <- renderValueBox({
    hcStorePurchases <- hchart(
      density(marketing$NumStorePurchases),
      type = "area",
      name = "Store Purchases",
      color = "#db902e"
    ) %>% 
      hc_xAxis(min = 0) %>% 
      hc_size(height = 200) %>% 
      hc_title(text = "Store Purchases",
               style = list(color = "#f0f0f0", fontSize =16)) %>% 
      hc_tooltip(borderWidth = 2,
                 crosshairs = T,
                 pointFormat = '<span style="color:{series.color}">{series.name}</span>:
                       <b>{point.x:.1f}</b><br/>')
    
    vbStorePurchases <- valueBoxSpark(
      value = sum(marketing$NumStorePurchases),
      title = toupper("TOTAL STORE PURCHASES"),
      sparkobj = hcStorePurchases,
      info = "Number of purchases made directly in store",
      subtitle = NULL,
      icon = icon("store"),
      href = NULL
    )
    vbStorePurchases
  })
  
  output$platformChart <- renderEcharts4r({
    
    group <- input$groupPlatformSelector
    platform <- input$platformTypeSelector
    
    platformPurchases <- marketing %>% 
      select(Marital_Status, Education, Era,CountryName,
             NumWebPurchases, NumCatalogPurchases, NumStorePurchases)
    
    colnames(platformPurchases) <- c("Marital_Status", "Education", "Era","Country",
                                     "Web", "Catalog", "Store")
    
    if (group == "Marital Status") {
      platformPurchases <- platformPurchases %>% 
        group_by(Marital_Status)
    } else if (group == "Education") {
      platformPurchases <- platformPurchases %>% 
        group_by(Education)
    } else if (group == "Generation") {
      platformPurchases <- platformPurchases %>% 
        group_by(Era)
    } else {
      platformPurchases <- platformPurchases %>% 
        group_by(Country)
    }
    
    platformPurchases <- platformPurchases %>% 
      summarise(Web = sum(Web),
                Catalog = sum(Catalog),
                Store = sum(Store)) %>% 
      mutate(TotalPurchases = Web + Store + Catalog)
    
    if (platform == "Web") {
      platformPurchases <- platformPurchases %>% 
        arrange(desc(Web))
    } else if (platform == "Catalog") {
      platformPurchases <- platformPurchases %>% 
        arrange(desc(Catalog))
    } else if (platform == "Store") {
      platformPurchases <- platformPurchases %>% 
        arrange(desc(Store))
    } else {
      platformPurchases <- platformPurchases %>% 
        arrange(desc(TotalPurchases))
    }
    
    plot <- ""
    
    if (group == "Marital Status") {
      plot <- platformPurchases %>% 
        e_chart(Marital_Status)
    } else if (group == "Education") {
      plot <- platformPurchases %>% 
        e_chart(Education)
    } else if (group == "Generation") {
      plot <- platformPurchases %>% 
        e_chart(Era)
    } else {
      plot <- platformPurchases %>% 
        e_chart(Country)
    }
    
    if (platform == "Web") {
      plot <- plot %>% 
        e_pie(Web, roseType = "radius")
    } else if (platform == "Catalog") {
      plot <- plot %>% 
        e_pie(Catalog, roseType = "radius")
    } else if (platform == "Store") {
      plot <- plot %>% 
        e_pie(Store, roseType = "radius")
    } else {
      plot <- plot %>% 
        e_pie(TotalPurchases, roseType = "radius")
    }
    
    plot <- plot %>% 
      e_theme_custom("www/chart_theme.json") %>% 
      e_title(
        text = glue("Total Purchases on {platform} by {group}"),
        left = "center",
        top = "0"
      ) %>% 
      e_legend(F) %>% 
      e_tooltip(
        trigger = "item",
        formatter = JS("
                function(params){return(
                 params.name + '<br><b>Total Purchases</b>'
                 + ' : ' 
                 + params.value
                 )}
                       ")
      )
    plot
  })
  
  # PLATFORM TAB - END -----------------------------------------------
  
  # PROMOTION TAB - START -----------------------------------------------
  
  output$campaignSuccessRate <- renderEcharts4r({
    campaignData <- marketing %>% 
      select(AcceptedCmp1, AcceptedCmp2, AcceptedCmp3,
             AcceptedCmp4, AcceptedCmp5)
    
    campaignData <- as.data.frame(lapply(campaignData, FUN = mean))
    colnames(campaignData) <- c("Campaign 1", "Campaign 2", "Campaign 3",
                                "Campaign 4", "Campaign 5")
    
    campaignData <- campaignData %>% 
      pivot_longer(cols = c("Campaign 1", "Campaign 2", "Campaign 3",
                            "Campaign 4", "Campaign 5"),
                   names_to = "CampaignType",
                   values_to = "SuccessRate") %>% 
      arrange(desc(SuccessRate)) %>% 
      mutate(SuccessRate = round(SuccessRate * 100, 2))
    
    campaignData %>% 
      e_chart(CampaignType) %>% 
      e_bar(SuccessRate) %>% 
      e_theme_custom("www/chart_theme.json") %>% 
      e_axis_labels(x = "Campaign Type", y ="Success Rate") %>% 
      e_title(
        text = "Marketing Capaign Success Rate",
        left = "center",
        top = "0"
      ) %>% 
      e_legend(F) %>% 
      e_tooltip(
        trigger = "item",
        formatter = JS("
                function(params){return(
                 params.name + '<br><b>Success Rate</b>'
                 + ' : ' 
                 + params.value[1] + '%'
                 )}
                       ")
      )
  })
  
  output$totalAccceptedEachCampaign <- renderEcharts4r({
    group <- input$successRateSelector
    
    campaignData <- marketing %>% 
      select(AcceptedCmp1, AcceptedCmp2, AcceptedCmp3,
             AcceptedCmp4, AcceptedCmp5, DayEnroll,
             MonthEnroll, YearEnroll)
    
    colnames(campaignData) <-  c("Campaign 1", "Campaign 2", "Campaign 3",
                                 "Campaign 4", "Campaign 5", "Day", "Month", "Year")
    
    if (group == "Day") {
      campaignData <- campaignData %>% 
        group_by(Day)
    } else if (group == "Month") {
      campaignData <- campaignData %>% 
        group_by(Month)
    } else {
      campaignData <- campaignData %>% 
        group_by(Year)
    }
    
    campaignData <- campaignData %>% 
      summarise(Campaign1 = sum(`Campaign 1`),
                Campaign2 = sum(`Campaign 2`),
                Campaign3 = sum(`Campaign 3`),
                Campaign4 = sum(`Campaign 4`),
                Campaign5 = sum(`Campaign 5`)) %>% 
      mutate(TotalAccepted = Campaign1 + Campaign2 + Campaign3 +
               Campaign4 + Campaign5) %>% 
      arrange(desc(TotalAccepted))
    
    plot <- ""
    
    if (group == "Day") {
      plot <- campaignData %>% 
        e_chart(Day)
    } else if (group == "Month") {
      plot <- campaignData %>% 
        e_chart(Month)
    } else {
      plot <- campaignData %>% 
        e_chart(Year)
    }
    
    plot <- plot %>% 
      e_bar(Campaign1, stack = "grp") %>% 
      e_bar(Campaign2, stack = "grp") %>%
      e_bar(Campaign3, stack = "grp") %>%
      e_bar(Campaign4, stack = "grp") %>%
      e_bar(Campaign5, stack = "grp") %>%
      e_theme_custom("www/chart_theme.json") %>% 
      e_axis_labels(x = glue("{group}"), y ="Total Accepted") %>% 
      e_title(
        text = glue("Total Accepted Campaign by {group}"),
        left = "center",
        top = "0"
      ) %>% 
      e_legend(top = "30") %>% 
      e_tooltip(
        trigger = "item",
        formatter = JS("
                function(params){return(
                 '<br><b>Total Accepted</b>'
                 + ' : ' 
                 + params.value[1]
                 )}
                       ")
      )
    plot
  })
  
  # PROMOTION TAB - END -----------------------------------------------
  
  # RFM Analysis TAB - START -----------------------------------------------
  
  output$boxplotRFM <- renderEcharts4r({
    
    RFM <- marketing %>%
      mutate(Monetary = MntWines + MntFruits + MntMeatProducts +
               MntFishProducts + MntSweetProducts + MntGoldProds,
             Frequency = NumWebPurchases + NumCatalogPurchases + NumStorePurchases) %>% 
      select(Recency, Frequency, Monetary)
    
    RFM_table <- RFM %>% 
      pivot_longer(cols = c("Recency", "Frequency", "Monetary"),
                   names_to = "Category",
                   values_to = "Value")
    
    RFM_table %>% 
      group_by(Category) %>% 
      e_charts() %>% 
      e_boxplot(Value, itemStyle = list(color = "#db902e")) %>% 
      e_theme_custom("www/chart_theme.json") %>% 
      e_title(
        text = glue("Boxplot of RFM (Recency, Frequency, and Monetary)"),
        left = "center",
        top = "0"
      ) %>% 
      e_axis_labels(x = "Category",
                    y = "Value") %>% 
      e_x_axis(
        name = "Category",
        nameLocation = "center",
        nameGap = "25") %>%
      e_tooltip(trigger = c("item", "axis"))
    
  })
  
  output$segmentationPlot <- renderEcharts4r({
    
    # RFM Analysis
    
    RFM <- marketing %>%
      mutate(Monetary = MntWines + MntFruits + MntMeatProducts +
               MntFishProducts + MntSweetProducts + MntGoldProds,
             Frequency = NumWebPurchases + NumCatalogPurchases + NumStorePurchases) %>% 
      select(Recency, Frequency, Monetary)
    
    #Scoring
    #R_score
    RFM$R_Score[RFM$Recency > 74.0] <- 1
    RFM$R_Score[RFM$Recency > 49 & RFM$Recency<=74 ] <- 2
    RFM$R_Score[RFM$Recency > 24 & RFM$Recency<=49 ] <- 3
    RFM$R_Score[RFM$Recency <= 24] <- 4
    #F_score
    RFM$F_Score[RFM$Frequency < 6]<-1
    RFM$F_Score[RFM$Frequency >= 6 & RFM$Frequency < 12] <- 2
    RFM$F_Score[RFM$Frequency >= 12 & RFM$Frequency < 18] <- 3
    RFM$F_Score[RFM$Frequency >= 18] <- 4
    #M_score
    RFM$M_Score[RFM$Monetary < 68.75] <-1
    RFM$M_Score[RFM$Monetary >= 68.75 & RFM$Monetary < 396.0] <- 2
    RFM$M_Score[RFM$Monetary >= 396.0 & RFM$Monetary < 1045.50 ] <- 3
    RFM$M_Score[RFM$Monetary >= 1045.50] <- 4
    
    #RFM_score
    RFM<- RFM %>% mutate(RFM_Score = 100 * R_Score + 10 * F_Score + M_Score)
    
    #Customer Segmentation
    champions <- c(444)
    loyal_customers <- c(334, 342, 343, 344, 433, 434, 443)
    potential_loyalist <- c(332,333,341,412,413,414,431,432,441,442,421,422,423,424)
    recent_customers <- c(411)
    promising <- c(311, 312, 313, 331)
    needing_attention <- c(212,213,214,231,232,233,241,314,321,322,323,324)
    about_to_sleep <- c(211)
    at_risk <- c(112,113,114,131,132,133,142,124,123,122,121,224,223,222,221)
    cant_lose <- c(134,143,144,234,242,243,244)
    hibernating <- c(141)
    lost <- c(111)
    
    convert_score <- function(score) {
      if (score %in% champions) {
        score <- "Champions"
      } else if (score %in% loyal_customers) {
        score <- "Loyal Customers"
      } else if (score %in% potential_loyalist) {
        score <- "Potential Loyalist"
      } else if (score %in% recent_customers) {
        score <- "Recent Customers"
      } else if (score %in% promising) {
        score <- "Promising"
      } else if (score %in% needing_attention) {
        score <- "Customer Needing Attention"
      } else if (score %in% about_to_sleep) {
        score <- "About to Sleep"
      } else if (score %in% at_risk) {
        score <- "At risk"
      } else if (score %in% cant_lose) {
        score <- "Can't Lose Them"
      } else if (score %in% hibernating) {
        score <- "Hibernating"
      } else if (score %in% lost) {
        score <- "Lost"
      } else {
        score <- "Unknown"
      }
    }
    
    RFM$Segment <- sapply(X = RFM$RFM_Score, FUN = convert_score)
    
    customerSegmentation <- RFM %>%
      group_by(Segment) %>% 
      summarise(Freq = n()) %>% 
      arrange(desc(Freq))
    
    # Create chart of customer segmentation
    customerSegmentation %>% 
      e_chart(Segment) %>% 
      e_bar(Freq) %>% 
      e_flip_coords() %>% 
      e_y_axis(inverse = T) %>% 
      e_theme_custom("www/chart_theme.json") %>% 
      e_title(
        text = glue("Customer Segmentation"),
        left = "center",
        top = "0"
      ) %>% 
      e_legend(show = F) %>% 
      e_axis_labels(x = "Count") %>% 
      e_x_axis(
        name = "Count",
        nameLocation = "center",
        nameGap = "25") %>%
      e_tooltip(
        trigger = "item",
        formatter = JS(
          "
       function(params){return(
       params.value[1] + '<br><b>Count</b>'
       + ' : ' 
       + params.value[0]
       )}
       "
        )
      )
  })
  
  # RFM Analysis - END -----------------------------------------------
}
