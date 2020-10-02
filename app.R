library(shiny)
library(readr)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(ggpubr)


## read data
df = read_csv("bkCurve_all.csv", col_types = cols())
dfMD = read_csv("bkCurve_siteMetadata.csv", col_types = cols())
siteList = unique(df$siteName)

# Define UI
ui <- fluidPage(
  
  # Application title
  titlePanel("Heat Accumulation Plot"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("site", "SITE:",
                  siteList)
    ),
    
    
    # Show a plot 
    mainPanel(
      plotOutput("distPlot")
    )
  )
)


# Define server
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    dfSite = df[df$siteName==input$site,]
    dfMDSite = dfMD[dfMD$site_name==input$site,]
    
    #get point por labels
    dfLabels = dfSite %>% group_by(year, SLFL) %>% summarise(temp = min(temp), nDays = nDays[which.min(temp)])
    
    pp = ggplot(dfSite, aes(temp, nDays, group=factor(year)))
    pp + geom_line(aes(colour=factor(year)), alpha=0.5, size=0.3) + 
      geom_line(data=dfSite %>% filter(year==2017), aes(temp, nDays), colour="red", size=1) + 
      geom_line(data=dfSite %>% filter(year==2016), aes(temp, nDays), colour="orange", size=1) + 
      geom_text_repel(data=dfLabels, aes(label=year), size=3) + 
      labs(title=input$site, x="Temperature (°C)", y ="Number of days above temperature") + 
      facet_grid(SLFL~.) + 
      theme_pubclean() + 
      theme(legend.position="none")
  },
  height = 800)
}



shinyApp(ui = ui, server = server)

