library(shiny)
library(ggplot2)
library(reshape)
library(glmnet)
temp <-read.csv("C:/Users/82105/OneDrive/바탕 화면/shiny/data.csv")
ui <- fluidPage(
  
  # App title ----
  titlePanel("Hello Shiny!"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      selectInput("model_choice", h3("Select your Model"), 
                  choices = list("Ridge" = 0, "Lasso" = 1
                  ), selected = 1),
      
      sliderInput("rate", h3("Please rate me"),
                  min = 0, max = 100, value = 50)
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Histogram ----
      plotOutput(outputId = "distplot"),
      
      textOutput(outputId = "selected_var")
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  
  output$distplot <- renderPlot({
    
     a <- input$model_choice
     x=model.matrix(OPS~.,temp)[,-1]
     y=as.matrix(temp$OPS)
      
      ridge.mod =glmnet(x,y, alpha = a)
      
      beta=coef(ridge.mod)
      
      
      tmp <- as.data.frame(as.matrix(beta))
      tmp$coef <- row.names(tmp)
      tmp <- reshape::melt(tmp, id = "coef")
      tmp$variable <- as.numeric(gsub("s", "", tmp$variable))
      tmp$lambda <- ridge.mod$lambda[tmp$variable+1] # extract the lambda values
      tmp$norm <- apply(abs(beta[-1,]), 2, sum)[tmp$variable+1] # compute L1 norm
      
      
      ggplot(tmp[tmp$coef != "(Intercept)",], aes(lambda, value, color = coef, linetype = coef)) + 
        geom_line() + 
        scale_x_log10() + 
        xlab("Lambda (log scale)") + 
        guides(color = guide_legend(title = ""), 
               linetype = guide_legend(title = "")) +
        theme_bw() + 
        theme(legend.key.width = unit(3,"lines"))
      
   
  })
  
  
  output$selected_var <- renderText({ 
    if(input$model_choice == 0){
    k = "Ridge"
  }else{
    k = "Lasso"
  }
    paste("You have selected this",k,"   ", "your rate is ", input$rate)
  })
}

  
  
  
  
    

  
  
  
  
  
  
  
  
  
  


shinyApp(ui = ui, server = server)
