library(shiny)
library(readr)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(ggpubr)
library(MonoPoly)
options(dplyr.summarise.inform = FALSE)


## read data
df = read_csv("bkCurve_all.csv", col_types = cols())
dfMD = read_csv("bkCurve_siteMetadata.csv", col_types = cols())
dfMD = dfMD %>% group_by(siteName, SLFL) %>% mutate(yearRank = rank(-Tq50))
df = left_join(df, dfMD[,c("siteName", "SLFL", "year", "yearRank")])
dfTop5 = df %>% filter(yearRank<=5) %>% 
  group_by(siteName, SLFL, temp) %>% 
  summarise(nDaysMean = mean(nDays))
  
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
    dfMDSite = dfMD[dfMD$siteName==input$site,]
    dfTop5 = dfTop5[dfTop5$siteName==input$site,]
    topSLFLList = unique(dfTop5$SLFL)
    
    ## fit polynomial for each location
    polyPredAll = data.frame()
    for (i in topSLFLList){
      model = monpol(nDaysMean~temp, degree=5, data=dfTop5, subset = SLFL==i, monotone = "decreasing")
      tempList = sort(unique(dfTop5$temp[dfTop5$SLFL==i]))
      polyPred = data.frame(SLFL = rep(i, length(tempList)), 
                            temp=tempList, 
                            nDays = predict(model, data.frame(temp=tempList)))
      names(polyPred)[3] = "nDays"
      polyPredAll = bind_rows(polyPredAll, polyPred)
    }
    
    #get point por labels
    dfLabels = dfSite %>% group_by(year, SLFL) %>% 
      summarise(temp = min(temp), nDays = nDays[which.min(temp)])
    
    pp = ggplot(data=dfSite, aes(temp, nDays, group=factor(year)))
    pp = pp + geom_line(aes(colour=factor(year)), alpha=0.5, size=0.3) + 
      geom_line(data=dfSite %>% dplyr::filter(year==2017), aes(temp, nDays), colour="#feedde", size=1.5) + 
      geom_line(data=dfSite %>% dplyr::filter(year==2016), aes(temp, nDays), colour="#fdbe85", size=1.5) + 
      geom_line(data=dfSite %>% dplyr::filter(year==2002), aes(temp, nDays), colour="#fd8d3c", size=1.5) +
      geom_line(data=dfSite %>% dplyr::filter(year==1998), aes(temp, nDays), colour="#d94701", size=1.5) +
      geom_line(data=polyPredAll, aes(temp, nDays, group=NA), colour="black", size=1.5) + ## plot the TOP5 average
      geom_text_repel(data=dfLabels, aes(label=year), size=3) + 
      labs(title=input$site, x="Temperature (°C)", y ="Number of days above temperature") + 
      facet_grid(SLFL~.) + 
      theme_pubclean() + 
      theme(legend.position="none")
    print((pp))
    }, height = 800)
  }



shinyApp(ui = ui, server = server)

