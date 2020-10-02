library(shiny)
library(readr)



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

