library(shiny)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(ggpubr)


# Define server
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    dfSite = df[df$siteName==input$site,]
    dfMDSite = dfMD[dfMD$site_name==input$site,]
    
    #get point por labels
    dfLabels = dfSite %>% group_by(year, SLFL) %>% summarise(temp = min(temp), nDays = nDays[which.min(temp)])
    
    pp = ggplot(dfSite, aes(temp, nDays, group=factor(year)))
    pp + geom_line(colour="grey") + 
      geom_line(data=dfSite %>% filter(year==2017), aes(temp, nDays), colour="red") + 
      geom_line(data=dfSite %>% filter(year==2016), aes(temp, nDays), colour="orange") + 
      geom_text_repel(data=dfLabels, aes(label=year), size=3) + 
      facet_grid(.~SLFL) + 
      theme_pubclean()
  })
}
