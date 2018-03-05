#R Shiny dataTableOutput don't work if using loop of reactive image

# link:    https://stackoverflow.com/questions/46610099/r-shiny-datatableoutput-dont-work-if-using-loop-of-reactive-image/46635954#46635954

# require(shiny)
# require(EBImage)
# require(DT)
# ## Only run examples in interactive R sessions
# if (interactive()) {
#   
#   shinyApp(
#     
#     ui = fluidPage(
#       fileInput('input.image',""),
#       dataTableOutput("tabledata")
#     ),
#     
#     
#     
#     server = function(input, output) {
#       # Load Image
#       img <- reactive({
#         if (is.null(input$input.image))
#           return(NULL)
#         x <- readImage(input$input.image$datapath)
#       })
#       # Modified Image
#       img_bw <- reactive({
#         req( img() )
#         x <- img()
#         x <- gblur(x, sigma = 5)
#         x <- thresh(x, w = 15, h = 15, offset = 0.05)
#         x <- bwlabel(x)
#       })
#       
#       
#       # Calculate cell features for each frame and rbind frame-dataframe to total
#       data <- reactive({
#         # Create empty datagrame
#         dt <- data.frame()
#         # Use dt dataframe
#         for (i in 1:numberOfFrames(img())){
#           dt <- rbind(dt, computeFeatures.basic(img_bw()[,,i], img()[,,i]))
#         }
#         # Print dt dataframe
#         dt
#       })
#       
#       # Otuput
#       output$tabledata <- renderDataTable(data())
#     }
#   )
# }


## How to load local images to shiny app for image analysis with EBImage

## link : https://stackoverflow.com/questions/45866486/how-to-load-local-images-to-shiny-app-for-image-analysis-with-ebimage

# library("shiny")
# library("EBImage")# >= 4.19.3
# 
# ui <- fluidPage(
#   
#   # Application title
#   titlePanel("Image display"),
#   
#   # Sidebar with a select input for the image
#   sidebarLayout(
#     sidebarPanel(
#       fileInput("image", "Select image")
#     ),
#     
#     # Show a plot of the generated distribution
#     mainPanel(
#       tabsetPanel(
#         tabPanel("Static raster", plotOutput("raster")),
#         tabPanel("Interactive browser", displayOutput("widget"))
#       )
#     )
#   )
#   
# )
# 
# server <- function(input, output) {
#   
#   img <- reactive({
#     f <- input$image
#     if (is.null(f))
#       return(NULL)        
#     readImage(f$datapath)
#   })
#   
#   output$widget <- renderDisplay({
#     req(img())
#     display(img())
#   })
#   
#   output$raster <- renderPlot({
#     req(img())
#     plot(img(), all=TRUE)
#   })
#   
# }
# 
# # Run the application
# shinyApp(ui = ui, server = server)




# library(shiny)
# library(ggplot2)
# library(gridExtra)
# 
# u <- shinyUI(fluidPage(
#   titlePanel("title panel"),
#   sidebarLayout(position = "left",
#                 sidebarPanel("sidebar panel",
#                              checkboxInput("donum1", "Make #1 plot", value = T),
#                              checkboxInput("donum2", "Make #2 plot", value = F),
#                              checkboxInput("donum3", "Make #3 plot", value = F),
#                              sliderInput("wt1","Weight 1",min=1,max=10,value=1),
#                              sliderInput("wt2","Weight 2",min=1,max=10,value=1),
#                              sliderInput("wt3","Weight 3",min=1,max=10,value=1)
#                 ),
#                 mainPanel("main panel",
#                           column(6,plotOutput(outputId="plotgraph", width="500px",height="400px"))
#                 ))))
# 
# s <- shinyServer(function(input, output) 
# {
#   set.seed(123)
#   pt1 <- reactive({
#     if (!input$donum1) return(NULL)
#     qplot(rnorm(500),fill=I("red"),binwidth=0.2,main="plotgraph1")
#   })
#   pt2 <- reactive({
#     if (!input$donum2) return(NULL)
#     qplot(rnorm(500),fill=I("blue"),binwidth=0.2,main="plotgraph2")
#   })
#   pt3 <- reactive({
#     if (!input$donum3) return(NULL)
#     qplot(rnorm(500),fill=I("green"),binwidth=0.2,main="plotgraph3")
#   })
#   output$plotgraph = renderPlot({
#     ptlist <- list(pt1(),pt2(),pt3())
#     wtlist <- c(input$wt1,input$wt2,input$wt3)
#     # remove the null plots from ptlist and wtlist
#     to_delete <- !sapply(ptlist,is.null)
#     ptlist <- ptlist[to_delete] 
#     wtlist <- wtlist[to_delete]
#     if (length(ptlist)==0) return(NULL)
#     
#     grid.arrange(grobs=ptlist,widths=wtlist,ncol=length(ptlist))
#   })
# })
# shinyApp(u,s)