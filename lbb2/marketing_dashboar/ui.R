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
             icon = icon("box-open")),
    menuItem(text = "Platform",
             tabName = "platformItem",
             icon = icon("store")),
    menuItem(text = "Promotion",
             tabName = "promotionItem",
             icon = icon("bullhorn")),
    menuItem(text = "About",
             tabName = "aboutItem",
             icon = icon("user"))
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
        width = 12,
        radioButtons(
          inputId = "groupBySelector",
          label = shiny::HTML("<span style='color: #f0f0f0'>Group by:</span>"),
          choices = c("Marital Status", "Education", "Generation", "Country"),
          selected = "Marital Status",
          inline = T
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

productItem <- tabItem(
  tabName = "productItem",
  fluidPage(
    fluidRow(
      box(
        width = 12,
        selectInput(
          inputId = "groupByInputSelector",
          label = shiny::HTML("<span style='color: #f0f0f0'>Group mean total spent by:</span>"),
          choices = c("Marital Status", "Education", "Generation", "Country"),
          selected = "Marital Status"
        ),
        selectInput(
          inputId = "orderByButton",
          label = shiny::HTML("<span style='color: #f0f0f0'>Order mean total spent by:</span>"),
          choices = c("Mean Total Spent", "Wines", 
                      "Fruits", "Meats", "Fishs",
                      "Sweets", "Gold"),
          selected = "Mean Total Spent"
        )
      )
    ),
    fluidRow(
      box(
        width = 12,
        echarts4rOutput(outputId = "spentEachProduct")
      )
    ),
    fluidRow(
      box(
        width = 6,
        radioButtons(
          inputId = "productTypeSelector",
          label = shiny::HTML("<span style='color: #f0f0f0'>Choose product type:</span>"),
          choices = c("Wines", "Fruits", "Meats", "Fishs", "Sweets", "Gold"),
          selected = "Wines",
          inline = T
        )
      ),
      box(
        width = 6,
        radioButtons(
          inputId = "boxplotGroupSelector",
          label = shiny::HTML("<span style='color: #f0f0f0'>Choose product type:</span>"),
          choices = c("Marital Status", "Education", "Generation", "Country"),
          selected = "Marital Status",
          inline = T
        )
      )
    ),
    fluidRow(
      box(
        width = 6,
        echarts4rOutput(outputId = "histProducts")
      ),
      box(
        width = 6,
        echarts4rOutput(outputId = "boxplotProducts")
      )
    )
  )
)

platformItem <- tabItem(
  tabName = "platformItem",
  fluidPage(
    fluidRow(
      valueBoxOutput(
        width = 4,
        outputId = "webPurchases"
      ),
      valueBoxOutput(
        width = 4,
        outputId = "catalogPurchases"
      ),
      valueBoxOutput(
        width = 4,
        outputId = "storePurchases"
      )
    ),
    fluidRow(
      box(
        width = 6,
        radioButtons(
          inputId = "platformTypeSelector",
          label = shiny::HTML("<span style='color: #f0f0f0'>Choose platform type:</span>"),
          choices = c("Web", "Catalog", "Store", "All Platform"),
          selected = "All Platform",
          inline = T
        )
      ),
      box(
        width = 6,
        radioButtons(
          inputId = "groupPlatformSelector",
          label = shiny::HTML("<span style='color: #f0f0f0'>Group total purchases by:</span>"),
          choices = c("Marital Status", "Education", "Generation", "Country"),
          selected = "Marital Status",
          inline = T
        )
      )
    ),
    fluidRow(
      box(
        width = 12,
        echarts4rOutput(outputId = "platformChart")
      )
    )
  )
)

promotionItem <- tabItem(
  tabName = "promotionItem",
  fluidPage(
    fluidRow(
      box(
        width = 12,
        echarts4rOutput(outputId = "campaignSuccessRate")
      )
    ),
    fluidRow(
      box(
        width = 12,
        selectInput(
          inputId = "successRateSelector",
          label = shiny::HTML("<span style='color: #f0f0f0'>Group success rate by:</span>"),
          choices = c("Day", "Month", "Year"),
          selected = "Day"
        )
      )
    ),
    fluidRow(
      box(
        width = 12,
        echarts4rOutput(outputId = "totalAccceptedEachCampaign")
      )
    )
  )
)

aboutItem <- tabItem(
  tabName = "aboutItem",
  fluidPage(
    fluidRow(
      box(
        width = 6,
        h4("About me", class = "about-title"),
        hr(),
        img(src = "https://avatars.githubusercontent.com/u/57356926?s=400&u=93582113238aa2d7f0887f372595b4d27f08c5f9&v=4", 
            width = "150px",
            id = "about-image"),
        p("Hi, I'm Daniel! I'm a 3rd year CS/IT Student at University of Indonesia with big interest in Data Science and Machine Learning.
          I enjoy learning about data science and machine learning and also try to not just using ML libraries such as
          Scikit-learn, Tensorflow, but also understanding how Algorithm/Math behind it."),
        p("I'm experienced in using Python on a daily basis and currently learning R as my ML tools. 
          I have previously worked on tabular data and next time I'll try to explore image or text data"),
        p("Get in touch with me on Linkedin: ", 
          a("Daniel Syahputra", href = "https://www.linkedin.com/in/daniel-syahputra-purba-940999175/")
          )
      ),
      box(
        width = 6,
        h4("About this dashboard", class = "about-title"),
        hr(),
        p("Dashboard for marketing purposes with interactive charts. 
          This project is built based on the 4 P's principle in marketing"),
        p("This dashboard was made using marketing analytics data which can be accessed on ",
          a("Kaggle.", href="https://www.kaggle.com/jackdaoud/marketing-data")),
        p("What I've done in this project"),
        tags$ul(
          tags$li("Performed data wrangling to transform data into a desired format for better decision making in less time."), 
          tags$li("Performed feature engineering to get new features based on raw data."), 
          tags$li("Performed data visualization with interactive charts using highcharter and echarts4r"),
          tags$li("Built a dashboard using Shiny R to get a standalone web app consisting of charts that I've done before")
        )
      )
    )
  )
)


body <- dashboardBody(
  # CSS
  tags$link(rel = "stylesheet", 
            type = "text/css",
            href = "style.css"),
  tags$script(HTML("$('body').addClass('fixed');")),
  tabItems(
    peopleItem,
    productItem,
    platformItem,
    promotionItem,
    aboutItem
  )
)

ui <- dashboardPage(header, sidebar, body)
