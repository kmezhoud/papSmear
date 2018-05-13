## app.R ##


ui <- dashboardPage( skin = "green",
                     dashboardHeader(title = "Cervical Smear Test"),
                     
                     dashboardSidebar(
                       includeCSS(paste0(system.file(package = "papSmear"), "/app/www/costum.css", sep='')),
                       includeCSS(paste0(system.file(package = "papSmear"),"/app/www/switchButton.css", sep='')),
                       
                       sidebarMenu(
                         id = "sbMenu",
                         #menuItem(selectInput("bookmark",label="Site Layout",teamsChoice_3)),
                         menuItem("Front Page",tabName = "frontPage"),
                         menuItem(
                           "Preprocessing", tabName = "preprocessing",icon = icon("table"),
                           menuSubItem("Set training model", tabName = "set_train"),
                           menuSubItem("Convert Images",tabName = "convert_images"),
                           menuSubItem("Image Augmentation", tabName = "image_augmentation"),
                           menuSubItem("Team av ppg by Manager", tabName = "m_ppg",icon = icon("star"))
                         )
                       )
                       
                     ),
                     
                     
                     dashboardBody(
                       # Boxes need to be put in a row (or column)
                       tabItems(
                         # Front Page
                         tabItem("frontPage",
                                 uiOutput('ui_predict')     
                          
                         ),
                         tabItem("set_train",
                                 
                                 uiOutput('ui_training') 
                                 
                         ),
                      
                         tabItem("convert_images",
                                 
                          uiOutput('ui_convert') 
                                 
                                 
                                 ),
                         tabItem("image_augmentation",
                                 
                                 uiOutput('ui_augmentation')
                                 )
                         
                       )
                     )
                     
)