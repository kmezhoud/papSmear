output$augmentationImg <- renderPlot({
  inFile <- input$inputAugID
  
  im <- imager::load.image(inFile$datapath)
  #im <- imager::resize(im, size_x = 50, size_y = 50)
  
  # Split pixset in X along x-axis
  par( mai = c(0, 0, 0.1, 0))
  imager::imsplit(im,"x",input$split_XID) %>% plot(layout="row", axes= FALSE)
  ## grid just to view split on y-axis will be done
  grid(nx = 0, ny= input$split_YID, lty= 1, lwd= 2, col = "white", equilogs = FALSE)
  

  #im <- EBImage::readImage(inFile$datapath)
  #im <- EBImage::resize(im, w= 50, h= 50)  # pixels
 # plot(im)
})



output$augmentationPreview <- renderPlot({
  inFile <- input$inputAugID
  ls <- im_split_df(path = inFile$datapath,
              X = input$split_XID,
              Y = input$split_YID)
  
  ## plot images from datafarme by col
  print(nrow(ls$df) )
  print(input$split_YID)
  print(ls$wide)
  print(ls$height)
  par(mfcol=c(input$split_YID, input$split_XID), 
              mai = c(0, 0, 0.1, 0))
  for(i in seq_len(nrow(ls$df))){
    
    vec2img(ls$df[-1], i , w = ls$wide, h =  ls$height)
  }
  
})

output$ui_augmentation <- renderUI({
  fluidPage(
  fluidRow(
    column(
      width = 4,
      box( class = "sand", width = 12, title = "Select Image file", collapsible = TRUE,
           collapsed = FALSE,   solidHeader = TRUE, 
           tabItem(tabName = "image_augmentation",
                   
                   # Input: Select a file ----
                   fileInput("inputAugID", "Choose Image File",
                             multiple = FALSE,
                             accept = c("text/jpeg",
                                        ".jpeg",
                                        ".jpg")),
                   sliderInput('split_XID', label = "Split x-axis into", step = 1,
                               min = 1, max = 20, ticks = FALSE, value = 1 
                              ),
                   sliderInput('split_YID', label = "Split y-axis into", step = 1,
                               min = 1, max = 20, ticks = FALSE, value = 1 
                   )
                  
           )
           
      )
      
    ),
    column(
      width = 8,
      box(class= "sand", title = "Image Augmentation Preview", width = 14 , collapsible = TRUE,
          collapsed = FALSE, solidHeader = TRUE,
          
          if(!is.null(input$inputAugID)){
            plotOutput("augmentationImg")
          }
      
    ),
    box(class= "sand", title = "Image Augmentation Preview", width = 14 , collapsible = TRUE,
        collapsed = FALSE, solidHeader = TRUE,
        
        if(!is.null(input$inputAugID)){
          plotOutput("augmentationPreview")
        }
    )
    
  )
  )
  )
})