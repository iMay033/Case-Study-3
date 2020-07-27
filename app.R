StarData <- read.csv("StarData.csv")

library(shiny)
library(ggplot2)
library(plyr)
library(party)
library(shinyWidgets)

StarData.f = StarData
StarData.f$Star.color <- NULL
StarData.f$Spectral.Class <- NULL

ui <- fluidPage(
  titlePanel('StarData Analysis'),
  titlePanel(tags$a(href="https://github.com/iMay033/Case-Study-3", "GitHub")),
  setBackgroundImage(
    src="https://cdn.discordapp.com/attachments/218195835094040589/737124944575725568/1219598.png"
  ),
  fluidRow(
  headerPanel('StarData Clustering With k-mean'),
  inputPanel(
    selectInput('data1', 'Variable 1 (X Axis)', names(StarData.f)),
    selectInput('data2', 'Variable 2 (Y Axis)', names(StarData.f),
                selected = names(StarData.f)[[2]]),
    numericInput('clustercount', 'Number of Clusters', 5,
                 min = 1, max = 10)),
  plotOutput('ClusterPlot')),
  fluidRow(
  headerPanel('Star Color Plot Using StarData'),
  column(4,
  inputPanel(
    selectInput('data3', 'X Variable', names(StarData)))),
  column(5,
    plotOutput('ColorPlot')),
  fluidRow(
         inputPanel(
  tags$p("Clustering is a technique that uses basic pattern recognition. For the clustering process I decided to use k-means because it is able to identify clusters using simple and easy to implement principles. Another benefit is that k-means performs well on most platforms and is pretty efficient. However, there is some random chance when clustering is performed this way and it is less sophisticated than other clustering algorithms out there. 
Some examples of problems that can be solved with k-means are: simplifying large data sets by grouping similar data into clusters, creating clusters of consumers based on their demographics, grouping species based on their dietary habit, etc. This is just the tip of the iceberg when it comes to problems that can be solved with clustering and k-means.
If you look at the graph you can see a few X’s, these X’s represent k points. k points are placed into the space that the objects represent and are used to represent the centroid of the group. This is useful because it separates the objects into clusters and makes it possible for the metric to be minimized and calculated.
")))),
  fluidRow(
  sidebarPanel('Star Classification Chart Using StarData -- "The reason I chose to include a Classification tree in my application is because, despite the fact that I had already done clustering above, I feel it is an easy way to view the data and see how the star type is chosen. Working your way from the top of the chart down shows what values each variable must have in order to obtain the desired star type. Some other uses of classification would include determining the probability of winning a game and the probability that someone will vote for any political candidate."'),
  plotOutput('ClassificationPlot')),
  
)

server <- function(input, output) {
  
  ReactiveData <- reactive({
    StarData.f[, c(input$data1, input$data2)]
  })
  
  ReactiveCluster <- reactive({
    kmeans(ReactiveData(), input$clustercount)
  })
  
  output$ClusterPlot <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    plot(ReactiveData(),
         col = ReactiveCluster()$cluster,
         pch = 20, cex = 3)
    points(ReactiveCluster()$centers, pch = 4, cex = 4, lwd = 4)
  })
  ReactiveData2 <- reactive({
    StarData[, c(input$data3)]
 
  })
  output$ColorPlot <- renderPlot({
    starcolorPlot = qplot(ReactiveData2(), Star.color, data = StarData)
    starcolorPlot + geom_abline() + xlab('X Variable') + ylab('Star Color')
  })
  output$ClassificationPlot <- renderPlot({
    set.seed(1000)
    SampleStarData <- sample(2, nrow(StarData), replace=TRUE, prob=c(0.75,0.25))
    DataSet1 <- StarData[SampleStarData==1,]
    DataSet2 <- StarData[SampleStarData==2,]
    Equation <- Star.type ~ Temperature..K.+Luminosity.L.Lo.+Radius.R.Ro.+Absolute.magnitude.Mv.
    StarDataCtree <- ctree(Equation, data=DataSet1)
    table(predict(StarDataCtree), DataSet1$Star.type)
    plot(StarDataCtree)
  })
  
}

shinyApp(ui = ui, server = server)
