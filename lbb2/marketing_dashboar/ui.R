header <- dashboardHeader(
  title = "Marketing Dashboard"
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(text = "Customer", 
             tabName = "peopleItem",
             icon = icon("users")),
    menuItem(text = "Product",
             tabName = "productItem",
             icon = icon("box-open"))
  )
)

peopleItem <- tabItem(
  tabName = "peopleItem",
  fluidPage(
    fluidRow(
      valueBoxOutput(
        width = 4,
        outputId = "customersIncome"
      ),
      valueBoxOutput(
        width = 4,
        outputId = "customerSpentMoney"
      ),
      valueBoxOutput(
        width = 4,
        outputId = "customerPurchases"
      )
    ),
    
    # Chart
    fluidRow(
      box(
        selectInput(
          inputId = "groupBySelector",
          label = shiny::HTML("<span style='color: #f0f0f0'>Group by:</span>"),
          choices = c("Marital Status", "Education", "Generation"),
          selected = "Marital Status",
        )
      ),
      box(
        selectInput(
          inputId = "chartTypeSelector",
          label = shiny::HTML("<span style='color: #f0f0f0'>Chart type:</span>"),
          choices = c("Bar", "Donuth"),
          selected = "Donuth",
        )
      )
    ),
    fluidRow(
      box(
        width=12,
        echarts4rOutput(outputId = "incomeBy")
      )
    ),
    fluidRow(
      box(
        width = 12,
        echarts4rOutput(outputId = "spentBy")
      )
    )
  )
)

body <- dashboardBody(
  # CSS
  tags$link(rel = "stylesheet", 
            type = "text/css",
            href = "style.css"),
  tabItems(
    peopleItem
  )
)

ui <- dashboardPage(header, sidebar, body)

# Select Input
# fluidRow(
#   box(
#     width = 4,
#     height = 80,
#     selectInput(
#       inputId = "maritalStatusSelector",
#       label = shiny::HTML("<span style='color: #f0f0f0'>Select marital status:</span>"),
#       choices = levels(marketing$Marital_Status),
#       selected = "Married",
#       multiple = T
#     )
#   ),
#   box(
#     width = 4,
#     height = 80,
#     selectInput(
#       inputId = "educationSelector",
#       label = shiny::HTML("<span style='color: #f0f0f0'>Select education:</span>"),
#       choices = levels(marketing$Education),
#       selected = "Basic",
#       multiple = T
#     )
#   ),
#   box(
#     width = 4,
#     height = 80,
#     selectInput(
#       inputId = "eraSelector",
#       label = shiny::HTML("<span style='color: #f0f0f0'>Select generation: </span>"),
#       choices = levels(marketing$Era),
#       selected = "Gen X",
#       multiple = T
#     )
#   )
# ),


# style <- "
#     .box.box-solid.box-danger>.box-header {
#       color:#9966ff;
#       background:#9966ff
#                         }
#     
#     .box.box-solid.box-danger{
#     border-bottom-color:#9966ff;
#     border-left-color:#9966ff;
#     border-right-color:#9966ff;
#     border-top-color:#9966ff;
#     }
#     
#     .box.box-danger>.box-header {
#       color:#fff; 
#       background:#9966ff
#                         }
#     
#     .box.box-danger{
#     border-bottom-color:#9966ff;
#     border-left-color:#9966ff;
#     border-right-color:#9966ff;
#     border-top-color:#9966ff;
#     }
#     
#     .box.box-solid.box-info>.box-header {
#       color:#000000;
#       background:#FFAE66
#                         }
#     
#     .box.box-solid.box-info{
#     border-bottom-color:#FFAE66;
#     border-left-color:#FFAE66;
#     border-right-color:#FFAE66;
#     border-top-color:#FFAE66;
#     }
#     
#     .box.box-info>.box-header {
#       color:#fff; 
#       background:#FFAE66
#                         }
#     
#     .box.box-info{
#     border-bottom-color:#FFAE66;
#     border-left-color:#FFAE66;
#     border-right-color:#FFAE66;
#     border-top-color:#FFAE66;
#     }
#     
#     .box.box-solid.box-primary>.box-header {
#       color:#000000;
#       background:#82bf50
#                         }
#     
#     .box.box-solid.box-primary{
#     border-bottom-color:#82bf50;
#     border-left-color:#82bf50;
#     border-right-color:#82bf50;
#     border-top-color:#82bf50;
#     }
#     
#     .box.box-primary>.box-header {
#       color:#fff; 
#       background:#82bf50
#                         }
#     
#     .box.box-primary{
#     border-bottom-color:#82bf50;
#     border-left-color:#82bf50;
#     border-right-color:#82bf50;
#     border-top-color:#82bf50;
#     }
# 
#     .box.box-solid.box-warning>.box-header {
#       color:#000000;
#       background:#eb46e5
#                         }
#     
#     .box.box-solid.box-warning{
#     border-bottom-color:#eb46e5;
#     border-left-color:#eb46e5;
#     border-right-color:#eb46e5;
#     border-top-color:#eb46e5;
#     }
#     
#     .box.box-warning>.box-header {
#       color:#fff; 
#       background:#eb46e5
#                         }
#     
#     .box.box-warning{
#     border-bottom-color:#eb46e5;
#     border-left-color:#eb46e5;
#     border-right-color:#eb46e5;
#     border-top-color:#eb46e5;
#     }
# "
# 
# header <- dashboardHeader(
#     title = shinyDashboardLogo(
#       theme = "onenote",
#       boldText = "Marketing Dashboard",
#       mainText = "App",
#     )
#   )
# 
# sidebar <- dashboardSidebar(
#   sidebarMenu(
#     menuItem("Overview", 
#              tabName = "overview", 
#              icon = icon("cog")),
#     menuItem("People", 
#              tabName = "peopleItem",
#              icon = icon("users"),
#              badgeLabel = "in progress",
#              badgeColor = "green"),
#     menuItem("Product",
#              tabName = "productItem",
#              icon = icon("box-open"),
#              badgeLabel = "in progress",
#              badgeColor = "green"),
#     menuItem("Platform",
#              tabName = "platformItem",
#              icon = icon("store"),
#              badgeLabel = "in progress",
#              badgeColor = "green"),
#     menuItem("Promotion",
#              tabName = "promotionItem",
#              icon = icon("bullhorn"),
#              badgeLabel = "in progress",
#              badgeColor = "green"),
#     menuItem("Data", 
#              tabName = "dataItem",
#              icon = icon("table"))
#   )
# )
# 
# overviewItem <- tabItem(
#   tabName = "overview",
#   
#   # People Overview
#   fluidRow(
#     box(
#       title = "People",
#       status = "danger",
#       solidHeader = T,
#       width = 12,
#       infoBoxOutput(
#         outputId = "infoCustomer",
#         width = 4
#       ),
#       infoBoxOutput(
#         outputId = "infoIncome",
#         width = 4
#       ),
#       infoBoxOutput(
#         outputId = "infoCountry",
#         width = 4
#       )
#     )
#   ),
#   
#   # Product (Overview)
#   fluidRow(
#     box(
#       title = "Product",
#       enable_label = T,
#       status = "info",
#       solidHeader = T,
#       width = 5,
#       selectInput(
#         inputId = "productType",
#         label = "Choose Product Type:",
#         choices = c("Wines", "Fruits", 
#                     "Meats", "Sweets", 
#                     "Fishs", "Golds"),
#         selected = "Wines"
#       ),
#       infoBoxOutput(
#         outputId = "productInfo",
#         width = 12
#       ),
#       dataTableOutput("productTable")
#     ),
#     box(
#       title = "Customers Amount Spent",
#       enable_label = T,
#       status = "info",
#       solidHeader = T,
#       width = 7,
#       plotlyOutput(
#         outputId = "histProduct"
#       )
#     )
#   ),
#   
#   # Platform
#   fluidRow(
#     box(
#       title = "Platform",
#       enable_label = T,
#       status = "primary",
#       solidHeader = T,
#       width = 5,
#       radioButtons(
#         inputId = "platformType",
#         label = "Choose Platform Type:" ,
#         choices = c("Web", "Catalog", "Store")
#       ),
#       infoBoxOutput(
#         outputId = "platformInfo",
#         width = 12
#       ),
#       dataTableOutput("platformTable")
#     ),
#     box(
#       title = "Number of Purchases for Each Platform",
#       enable_label = T,
#       status = "primary",
#       solidHeader = T,
#       width = 7,
#       plotlyOutput(
#         outputId = "plotPurchases"
#       )
#     )
#   ),
#   
#   # Promotion
#   fluidRow(
#     box(
#       title = "Promotion",
#       enable_label = T,
#       status = "warning",
#       solidHeader = T,
#       width = 5,
#       selectInput(
#         inputId = "campaignType",
#         label = "Choose Campaign Type:",
#         choices = c("Campaign 1", "Campaign 2", 
#                     "Campaign 3", "Campaign 4", 
#                     "Campaign 5"),
#         selected = "Campaign1"
#       ),
#       infoBoxOutput(
#         outputId = "campaignInfo",
#         width = 12
#       )
#     ),
#     box(
#       title = "Number of Campaign Accepted",
#       enable_label = T,
#       status = "warning",
#       solidHeader = T,
#       width = 7,
#       plotlyOutput(
#         outputId = "barCampaingn"
#       )
#     )
#   )
# )
# 
# peopleItem <- tabItem(
#   tabName = "peopleItem",
#   h2("People: In Progress")
# )
# 
# productItem <- tabItem(
#   tabName = "productItem",
#   h2("Product: In Progress")
# )
# 
# platformItem <- tabItem(
#   tabName = "platformItem",
#   h2("Platform: In Progress")
# )
# 
# promotionItem <- tabItem(
#   tabName = "promotionItem",
#   h2("Promotion: In Progress")
# )
# 
# dataItem <- tabItem(
#   tabName = "dataItem",
#   fluidRow(
#     column(width = 12,
#            h2("Marketing Data"),
#            dataTableOutput("marketingData"))
#   )
# )
# 
# body <- dashboardBody(
#     shinyDashboardThemes(
#       theme = "grey_dark"
#     ),
#     tags$style(HTML(style)),
#     tabItems(
#       overviewItem,
#       peopleItem,
#       productItem,
#       platformItem,
#       promotionItem,
#       dataItem
#     )
#   )
# 
# dashboardPage(
#   title = "Marketing Dashboard App",
#   header,
#   sidebar,
#   body
# )