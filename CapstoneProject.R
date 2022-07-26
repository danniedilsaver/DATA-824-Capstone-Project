
#loading libraries
library(shiny)
library(readr)
library(ggplot2)
library(corrplot)

#loading dataset
df = read_csv("cbb.csv", show_col_types = FALSE)

#dataset with only numeric variables
df.num = df[3:22]

# Define UI ----
ui <- fluidPage(
  
  # Application title
  titlePanel(title = h4("College Basketball Statistics", align = "center")),

  #slider input
  sidebarLayout(

      sidebarPanel(
        
        selectInput(
          inputId = "Variable", 
          label = "Summary Stats I: Choose a variable", 
          choices = c("G" = 3, "W" = 4, "WP" = 5, "ADJOE" = 6, 
                      "ADJDE" = 7, "BARTHAG" = 8, "EFG_0" = 9, "EFG_D" = 10, "TOR"= 11, 
                      "TORD" = 12, "ORB" = 13, "DRB" = 14, "FTR" = 15, "FTRD" =16, "2P_O"=17, 
                      "2P_D" = 18, "3P_O" = 19, "3P_D" = 20, "ADJ_T" = 21, "WAB" = 22),
          selected = "ADJOE",
          multiple = FALSE),
        
        # Select a Team Drop Down Menu
        selectInput(inputId = "Team", 
                    label = "Summary Stats II: Select a Team",
                    choices = unique(df$TEAM),
                    selected = "Kansas",
                    multiple = FALSE),
        
        # Conference Drop Down Menu
          selectInput( inputId = "Conference", 
            label = "Frequency Plot: Select a Conference",
            choices = unique(df$CONF),
            selected = "ACC",
            multiple = FALSE), 
        
        # Offense vs. Defense button
        radioButtons(inputId = "plot_type",
                     label = "Scatter Plot: Choose Offense or Defense Stats",
                     choices = c("Offense", "Defense"),
                     selected = "Offense"),
  
         #Slider for winning percentage 
        sliderInput(inputId = "obs",
                    label = "Data Table: Filter by Winning Percentage",
                   min = 0,
                    max = 1.00,
                    value = 0.50)
 
  ),
    
    # Adding a Plot, Table, and Summary tab
    mainPanel(
      tabsetPanel(
        tabPanel("Correlation Plot", plotOutput(outputId = "corrplot"), verbatimTextOutput("correlations")),
        tabPanel("Summary Statistics by Variable", verbatimTextOutput("summary"), plotOutput(outputId ="boxp")),
        tabPanel("Summary Statistics by Team", verbatimTextOutput("teamsum")),
        tabPanel("Frequency Plot", plotOutput(outputId = "fplot")),
        tabPanel("Scatter Plots", plotOutput(outputId = "odplot")),
        tabPanel("Data Table", plotOutput(outputId = "WPplot"), tableOutput("table"))

        ))
    )
)
  

# Define server logic ----
server <- function(input, output) {
  
  #correlation plot
  output$corrplot <- renderPlot({
    cor.df = cor(df.num)
    corrplot(cor.df) })
  
  #correlations ouput
  output$correlations <- renderPrint({
    cor.df
  })

  #summary by variable output
  output$summary <- renderPrint({
    colm <- as.numeric(input$Variable)
    summary(df[colm])
  })
  
  #variable box plot
  output$boxp <- renderPlot({
    colm <- as.numeric(input$Variable)
    boxplot(df[colm], col = "lightblue" , main = "Box Plot of Variable")
  })
  
  #summary by team output
  output$teamsum <- renderPrint({
    df.team = subset(df, df$TEAM == input$Team)
    summary(df.team[3:20])
  })
  
  #frequency plot by conference
  output$fplot <- renderPlot({
    df2 = subset(df, df$CONF == input$Conference)
    hist(df2$WP, xlab = 'Winning Percentage', ylab = 'Frequency', 
         main = "Frequency of Winning Percentage by Conference",
         col = "lightblue")
    })
  
  #data table by winning percentage
  output$table <- renderTable({
    df3 = subset(df, df$WP == input$obs)
    head(df3, 25)
  })
  
  #winning percentage histogram
  output$WPplot <- renderPlot({
   hist(df$WP, main = "Frequency of Winning Percentage", col = "lightblue")
  })
  
  #scatter plots by offense/defense metrics
 output$odplot <- renderPlot({
   #offense subset
   offense = df.num[c(3,4,7,9,11,13,15,17)]
   #defense subset
   defense = df.num[c(3,5,8,10,12,14,16,18)]
   
   #If Offense is selected
  if (input$plot_type == "Offense") {
    par(mfrow=c(3,3))
    plot(x = df$WP, df$ADJOE, xlab = "Winning Percentage", ylab = "ADJOE", cex = .1, col = "lightblue")
    abline(lm(ADJOE ~ WP, data = df), col = "black")
    legend("topleft",legend=paste("R^2 is", format(summary(lm(ADJOE ~ WP, data = df))$r.squared,digits=3)))
   
    plot(x = df$WP, df$EFG_O, xlab = "Winning Percentage", ylab = "EFG_O", cex = .1, col = "lightblue")
    abline(lm(EFG_O ~ WP, data = df), col = "black")
    legend("topleft",legend=paste("R^2 is", format(summary(lm(EFG_O ~ WP, data = df))$r.squared,digits=3)))
    
    plot(x = df$WP, df$TOR, xlab = "Winning Percentage", ylab = "TOR", cex = .1, col = "lightblue")
    abline(lm(TOR ~ WP, data = df), col = "black")
    legend("topleft",legend=paste("R^2 is", format(summary(lm(TOR ~ WP, data = df))$r.squared,digits=3)))
    
    plot(x = df$WP, df$ORB, xlab = "Winning Percentage", ylab = "ORB", cex = .1, col = "lightblue")
    abline(lm(ORB ~ WP, data = df), col = "black")
    legend("topleft",legend=paste("R^2 is", format(summary(lm(ORB ~ WP, data = df))$r.squared,digits=3)))
    
    plot(x = df$WP, df$FTR, xlab = "Winning Percentage", ylab = "FTR", cex = .1, col = "lightblue")
    abline(lm(FTR ~ WP, data = df), col = "black")
    legend("topleft",legend=paste("R^2 is", format(summary(lm(FTR ~ WP, data = df))$r.squared,digits=3)))
   
    plot(x = df$WP, df$`2P_O`, xlab = "Winning Percentage", ylab = "2P_O", cex = .1, col = "lightblue")
    abline(lm(`2P_O` ~ WP, data = df), col = "black")
    legend("topleft",legend=paste("R^2 is", format(summary(lm(df$`2P_O` ~ df$WP))$r.squared,digits=3)))
    
    plot(x = df$WP, df$`3P_O`, xlab = "Winning Percentage", ylab = "3P_O", cex = .1, col = "lightblue")
    abline(lm(`3P_O` ~ WP, data = df), col = "black")
    legend("topleft",legend=paste("R^2 is", format(summary(lm(df$`3P_O` ~ df$WP))$r.squared,digits=3)))
    
    mtext("Offensive Statistics vs. Winning Percentage", side = 3, line = - 2, outer = TRUE)
  }
   
   #If Defense is selected 
    else if (input$plot_type == "Defense") {
      par(mfrow=c(3,3))
      plot(x = df$WP, df$ADJDE, xlab = "Winning Percentage", ylab = "ADJDE", cex = .1, col = "darkblue")
      abline(lm(ADJDE ~ WP, data = df), col = "black")
      legend("topleft",legend=paste("R^2 is", format(summary(lm(ADJDE ~ WP, data = df))$r.squared,digits=3)))
      
      plot(x = df$WP, df$EFG_D, xlab = "Winning Percentage", ylab = "EFG_D", cex = .1, col = "darkblue")
      abline(lm(EFG_D ~ WP, data = df), col = "black")
      legend("topleft",legend=paste("R^2 is", format(summary(lm(EFG_D ~ WP, data = df))$r.squared,digits=3)))
      
      plot(x = df$WP, df$TORD, xlab = "Winning Percentage", ylab = "TORD", cex = .1, col = "darkblue")
      abline(lm(TORD ~ WP, data = df), col = "black")
      legend("topleft",legend=paste("R^2 is", format(summary(lm(TORD ~ WP, data = df))$r.squared,digits=3)))
      
      plot(x = df$WP, df$DRB, xlab = "Winning Percentage", ylab = "DRB", cex = .1, col = "darkblue")
      abline(lm(DRB ~ WP, data = df), col = "black")
      legend("topleft",legend=paste("R^2 is", format(summary(lm(DRB ~ WP, data = df))$r.squared,digits=3)))
      
      plot(x = df$WP, df$FTRD, xlab = "Winning Percentage", ylab = "FTRD", cex = .1, col = "darkblue")
      abline(lm(FTRD ~ WP, data = df), col = "black")
      legend("topleft",legend=paste("R^2 is", format(summary(lm(FTRD ~ WP, data = df))$r.squared,digits=3)))
      
      plot(x = df$WP, df$`2P_D`, xlab = "Winning Percentage", ylab = "2P_D", cex = .1, col = "darkblue")
      abline(lm(`2P_D` ~ WP, data = df), col = "black")
      legend("topleft",legend=paste("R^2 is", format(summary(lm(df$'2P_D'~ df$WP))$r.squared,digits=3)))
      
      plot(x = df$WP, df$`3P_D`, xlab = "Winning Percentage", ylab = "3P_D", cex = .1, col = "darkblue")
      abline(lm(`3P_D` ~ WP, data = df), col = "black") 
      legend("topleft",legend=paste("R^2 is", format(summary(lm(df$`3P_D`~df$WP))$r.squared,digits=3)))
      
      mtext("Defensive Statistics vs. Winning Percentage", side = 3, line = - 2, outer = TRUE)
      
    }
  })
  
  
  
  
  

}

# Run the app ----
shinyApp(ui = ui, server = server)






