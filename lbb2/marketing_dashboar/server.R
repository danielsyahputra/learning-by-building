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
      info = "This is the customer income based on marketing data",
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
      info = "This is the total customer spent based on marketing data",
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
      info = "This is the total customer purchases based on marketing data",
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
                        '<b>' + params.name + '</b>'
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
       '<b>' + params.name + '</b>'
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
       '<b>' + params.name + '</b>'
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
                 '<b>' + params.name + '</b>'
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
                 '<b>Success Rate</b>'
                 + ' : ' 
                 + params.value[1] + '%'
                 )}
                       ")
      )
  })
  
  # PROMOTION TAB - END -----------------------------------------------
}



# function(input, output) {
#   output$infoCustomer <- renderInfoBox({
#     infoBox(
#       title = "Customer",
#       subtitle = "Jumlah Pelanggan",
#       value = nrow(marketing),
#       icon = icon("intercom"),
#       fill = T,
#       color = "purple"
#     )
#   })
#   
#   output$infoIncome <-renderInfoBox({
#     mean_income <- mean(marketing$Income)
#     infoBox(
#       title = "Income",
#       subtitle = "Rataan Penghasilan Pelanggan",
#       value = glue("${round(mean_income, 2)}"),
#       icon = icon("dollar-sign"),
#       fill = T,
#       color = "teal"
#     )
#   })
#   
#   output$infoCountry <- renderInfoBox({
#     numOfCountry <- length(unique(marketing$Country))
#     infoBox(
#       title = "Country",
#       subtitle = "Jumlah Negara Pelanggan",
#       value = numOfCountry,
#       icon = icon("globe-asia"),
#       fill = T,
#       color = "red"
#     )
#   })
#   
#   getProductInfo <- function(input) {
#     type <- input$productType
#     mean_product <- 0
#     if (type == "Wines") {
#       mean_product <- mean(marketing$MntWines)
#     } else if (type == "Fruits") {
#       mean_product <- mean(marketing$MntFruits)
#     } else if (type == "Meats") {
#       mean_product <- mean(marketing$MntMeatProducts)
#     } else if (type == "Sweets") {
#       mean_product <- mean(marketing$MntSweetProducts)
#     } else if (type == "Fishs") {
#       mean_product <- mean(marketing$MntFishProducts)
#     } else {
#       mean_product <- mean(marketing$MntGoldProds)
#     }
#     return(mean_product)
#   }
#   
#   output$productInfo <- renderInfoBox({
#     mean_product <- getProductInfo(input)
#     infoBox(
#           title = "Rataan Pengeluaran Pelanggan",
#           subtitle = input$productType,
#           value = glue("${round(mean_product, 2)}"),
#           icon = icon("dollar-sign"),
#           fill = T,
#           color = "purple"
#         )
#   })
#   
#   output$productTable <- renderDataTable({
#     type <- c("MntWines", 
#               "MntFruits", 
#               "MntMeatProducts", 
#               "MntFishProducts", 
#               "MntSweetProducts",
#               "MntGoldProds")
#     productDt <- marketing[, type]
#     colnames(productDt) <- c("AmountWines", 
#                              "AmountFruits",
#                              "AmountMeats",
#                              "AmountFishs", 
#                              "AmountSweets", 
#                              "AmountGold")
#     
#     datatable(productDt, options = list(scrollX=T, scrollY = 80, 
#                                         searching=F, 
#                                         language = list(lengthMenu = "_MENU_"),
#                                         info = F))
#   })
#   
#   # Plot density of each product type
#   output$histProduct <- renderPlotly({
#     productData <- marketing %>% 
#       pivot_longer(cols = c("MntWines", 
#                             "MntFruits", 
#                             "MntMeatProducts", 
#                             "MntFishProducts", 
#                             "MntSweetProducts",
#                             "MntGoldProds"),
#                    names_to = "ProductType",
#                    values_to = "AmountSpent",
#                    names_prefix = "Mnt") %>% 
#       select(ProductType, AmountSpent) %>% 
#       filter(AmountSpent < 1000)
#     
#     options(scipen = 99)
#     histProduct <- productData %>% 
#       ggplot(aes(x = AmountSpent, group=ProductType, fill=ProductType)) + 
#       geom_density(adjust=1.5, alpha=.4) + 
#       theme_clean() +
#       labs(
#         title = "Product Type Density Plot", x = NULL, y = NULL
#       ) + 
#       scale_fill_brewer(palette = "Set2")
#     
#     ggplotly(histProduct, tooltip = "x")
#   })
#   
#   # Plot bar plot of purchases for each platform
#   output$plotPurchases <- renderPlotly({
#     platformData <- marketing %>% 
#       select(NumWebPurchases, NumCatalogPurchases, NumStorePurchases)
#     colnames(platformData) <- c("Web", "Catalog", "Store")
#     platformData <- platformData %>% 
#       pivot_longer(cols = c("Web", 
#                             "Catalog", 
#                             "Store"),
#                    names_to = "PlatformType",
#                    values_to = "NumPurchases",
#                    names_prefix = "Num") %>% 
#       select(PlatformType, NumPurchases)
#     options(scipen = 99)
#     histPlatform <- platformData %>% 
#       ggplot(aes(x = NumPurchases, group=PlatformType, fill=PlatformType)) + 
#       geom_density(adjust=1.5, alpha=.4) + 
#       theme_clean() +
#       labs(
#         title = "Platform Type Density Plot", x = NULL, y = NULL
#       ) + 
#       scale_fill_brewer(palette = "Set2")
#     
#     ggplotly(histPlatform, tooltip = "x")
#   })
#   
#   getPlatformInfo <- function(input) {
#     type <- input$platformType
#     mean_platform <- 0
#     if (type == "Web") {
#       mean_platform <- mean(marketing$NumWebPurchases)
#     } else if (type == "Catalog") {
#       mean_platform <- mean(marketing$NumCatalogPurchases)
#     } else {
#       mean_platform <- mean(marketing$NumStorePurchases)
#     }
#     return(mean_platform)
#   }
#   
#   output$platformInfo <- renderInfoBox({
#     mean_platform <- getPlatformInfo(input)
#     infoBox(
#       title = "Rataan Jumlah Pembelian Platform",
#       subtitle = input$platformType,
#       value = glue("{round(mean_platform, 0)}"),
#       icon = icon("store"),
#       fill = T,
#       color = "purple"
#     )
#   })
#   
#   output$platformTable <- renderDataTable({
#     platformDt <- marketing %>% 
#       select(NumWebPurchases, NumCatalogPurchases, NumStorePurchases)
#     colnames(platformDt) <- c("Web", "Catalog", "Store")
#     datatable(platformDt, options = list(scrollX=T, scrollY = 80, 
#                                         searching=F, 
#                                         language = list(lengthMenu = "_MENU_"),
#                                         info = F))
#   })
#   
#   output$barCampaingn <- renderPlotly({
#     data3 <- marketing %>% 
#       select(AcceptedCmp1, AcceptedCmp2, AcceptedCmp3, AcceptedCmp4, AcceptedCmp5) 
#     campaingn_cols <- c("AcceptedCmp1", "AcceptedCmp2", "AcceptedCmp3", "AcceptedCmp4", "AcceptedCmp5")
#     data3[, campaingn_cols] <- lapply(data3[, campaingn_cols], FUN = sum)
#     data3 <- head(data3,1)
#     colnames(data3) <- c("Campaign 1", "Campaign 2", "Campaign 3",
#                          "Campaign 4", "Campaign 5")
#     data3 <- data3 %>%
#       pivot_longer(cols = c("Campaign 1", "Campaign 2", "Campaign 3",
#                             "Campaign 4", "Campaign 5"),
#                    names_to = "Campaign",
#                    values_to = "Total")
#     
#     barCampaign <- data3 %>% 
#       ggplot(aes(x = Total, 
#                  y = reorder(Campaign, Total),
#                  text = glue("Total Accepted: {Total}")))+
#       geom_col(aes(fill = Campaign), show.legend = F) + 
#       labs(title = "Campaign Total Accepted", 
#            x = "Total",
#            y = "Campaign") +
#       theme_minimal() +
#       scale_fill_brewer(palette = "Set3")
#     
#     ggplotly(barCampaign, tooltip = "text")
#     
#   })
#   
#   getCampaignInfo <- function(input) {
#     type <- input$campaignType
#     sum_accepted <- 0
#     if (type == "Campaign 1") {
#       sum_accepted <- sum(marketing$AcceptedCmp1)
#     } else if (type == "Campaign 2") {
#       sum_accepted <- sum(marketing$AcceptedCmp2)
#     } else if (type == "Campaign 3") {
#       sum_accepted <- sum(marketing$AcceptedCmp3)
#     } else if (type == "Campaign 4") {
#       sum_accepted <- sum(marketing$AcceptedCmp4)
#     } else {
#       sum_accepted <- sum(marketing$AcceptedCmp5)
#     }
#     return(sum_accepted)
#   }
#   
#   output$campaignInfo <- renderInfoBox({
#     sum_accepted <- getCampaignInfo(input)
#     infoBox(
#       title = "Total Penerima Campaign",
#       subtitle = input$campaignType,
#       value = glue("{sum_accepted}"),
#       icon = icon("bullhorn"),
#       fill = T,
#       color = "purple"
#     )
#   })
#   
#   # Output for dataItem
#   output$marketingData <- renderDataTable({
#     datatable(marketing, 
#               options = list(scrollX=T, scrollY = 400))
#   })
# }
