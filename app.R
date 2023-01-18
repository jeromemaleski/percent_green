#
# This is a Shiny web application. 
#
#This app evaluates percent green cover in an image
#the app converts the RGB image to an index 'Green Leaf Index' (GLI)
#The user sets a threshold for acceptable green color
#The app returns an image of the mask and the count of masked and unmasked pixels and % green.
#

library(shiny)
library(terra)

#GLI function
#used with terra package
GLI <- function(img,i,j,k){
  r<-img[[i]]
  g<-img[[j]]
  b<-img[[k]]
  sumras<-(r+g+b)
  GLI <- (2*g-b-r)/(2*g+b+r)
  return(GLI)
}

# Define UI for application 
ui <- fluidPage(

    # Application title
    titlePanel("Percent Green Cover"),

    # Sidebar with data upload widget  
    sidebarLayout(
        #data upload widget
        sidebarPanel(
        #select file
            p("version hosted on shineyapps.io will run out of memory if image over 1MB"),
            fileInput("image1", "Choose an RGB image file",
                      multiple = FALSE),
                      #accept = c("image/jpg","image/jpeg"
                      #           "image/tif","image/png")),
            #ask for thrshold   
            p("Enter threshold value for mask. For Grean Leaf Index range is 0 to 1. 0.07 is the default threshold."),
            numericInput("threshold", "Threshold:", value = 0.07, min = 0, max = 1, step = .01),
            #checkboxInput("invert", "invert mask", value=FALSE)
            #actionButton("RunAnl","Count pixels above threshold")
        ),

        mainPanel(
           h2("Image"),
          
           plotOutput("distPlot1"),
           
          # h2("Green Leaf Index"),
          # "lighter pixels are more green",
          # plotOutput("distPlot3"),
           
           h2("Mask"),
           p("This is the green cover mask. Pixels above the threshold are red"),

           plotOutput("distPlot2"),

           # Show table of results
           DT::dataTableOutput("contents")
           )
        )
    )

# Define server logic
server <- function(input, output, session) {
options(shiny.maxRequestSize=100*1024^2) 
#initialize table for data  

  values <- reactiveValues()
  
  values$df <- data.frame("name" = character(),
                   "threshold" = numeric(), 
                   "green pixels" = numeric(),
                   "total pixels" = numeric(),
                   "pct green"= numeric(),
                   stringsAsFactors = FALSE)

#load image and make raster
    observeEvent(input$image1, {
      inFile <- input$image1
      print(inFile$name)
      if (is.null(inFile))
        return()
      values$fname<-inFile$name
      values$myraster <- rast(inFile$datapath)
    })
    
#convert raster to GLI  
    observe({
      req(length(values$myraster)>0)
      values$GLI_ras<-GLI(values$myraster,1,2,3)
    })
    
#graph RGB image 
    output$distPlot1 <- renderPlot({
    req(length(values$myraster)>0)
    plotRGB(values$myraster)
  })
  
# #graph GLI image
#   output$distPlot3 <- renderPlot({
#     
#     req(length(values$GLI)>0)
#     
#     plotRGB(values$GLI)
#     
#   })
  
#graph mask
    output$distPlot2 <- renderPlot({
      req(length(values$GLI_ras)>0)
      values$mask <- which.lyr(values$GLI_ras > input$threshold)
      plot(values$mask,col=cbind(1,2)) #2=red
  })

#calculate GLI values 
  observe({
      req(length(values$GLI_ras)>0)
      #extract data
      values$green_pix<-ncell(values$GLI_ras[values$GLI_ras > input$threshold])
      values$total_pix<-ncell(values$GLI_ras)
      values$pctgreen<-round(values$green_pix/values$total_pix*100,1)
      })
#put all values in data frame
  observeEvent(values$pctgreen,{
               values$df[nrow(values$df)+1,]<-cbind(values$fname,input$threshold,values$green_pix,values$total_pix,values$pctgreen)
               })

#put data frame in display window
  output$contents <-DT::renderDataTable({
    req(length(values$GLI_ras)>0)
    values$df
  })
}

# Run the application 
shinyApp(ui = ui, server = server)




