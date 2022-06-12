
library(shiny)
library(shinyWidgets)


# Define UI for application that draws normal distributions
ui <- fluidPage(

    # Application title
    titlePanel("平均値差の検定"),

    # Sidebar with a slider input for number of mean and stdev
    sidebarLayout(
        sidebarPanel(
            numericInput("av1", "平均 1:", 60, min = 0, max = 100, step = 0.01),
            numericInput("sd1", "標準偏差 1:", 10, min = 0.01, max = 50, step = 0.01),
            numericInput("nd1", "人数 1:", 20, min = 2, max = 1000, step = 1),
            numericInput("av2", "平均 2:", 50, min = 0, max = 100, step = 0.01),
            numericInput("sd2", "標準偏差 2:", 10, min = 0.01, max = 50, step = 0.01),
            numericInput("nd2", "人数 2:", 25, min = 2, max = 1000, step = 1)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    ),
    column(12, wellPanel(
      tags$h3("二つの平均値の差の統計値"),
      verbatimTextOutput("calcText1"),
      tags$head(tags$style("#calcText1{font-size:20px;}"))
    )),
    column(12, wellPanel(
      img(src = "KAKENHIlogoM.jpg", height="25px", width="60px"),
      img(src = "mue.png", height="30px", width="120px"),
      h6("平均値差検定システム（Ver:1.0.0）"),
      h6("開発：田端健人，菅原敏"),
      h6("科研費「グローバル世界を視野とする学力・非認知能力の効果的学校モデル」（20H01667）")
    ))
)

# Define server logic required to draw nomal distributions
server <- function(input, output) {
  output$distPlot <- renderPlot({
        av1 <- input$av1
        sd1 <- input$sd1
        av2 <- input$av2
        sd2 <- input$sd2
        
        n <- 1000
        x1 <- seq(0, 100, length=n)
        mx1 <- max( dnorm(x1,av1,sd1) )
        mx2 <- max( dnorm(x1,av2,sd2) )
        mx <- max(mx1,mx2) * 1.1
        
        # draw the normal distributions
        curve(dnorm(x,av1,sd1),0,100,col = "red",lwd=3,xlab="", ylab="", ylim=c(0,mx))
        curve(dnorm(x,av2,sd2),0,100,add = TRUE, col = "blue",lwd=3)
        legend("topleft",
               legend=c("1", "2"),
               lty=c(1,1),
               col=c("red", "blue")
        )
    })
    output$calcText1 <- renderText({
      av1 <- input$av1
      sd1 <- input$sd1
      av2 <- input$av2
      sd2 <- input$sd2
      nd1 <- input$nd1
      nd2 <- input$nd2
      dav <- abs(av1-av2)
      var1 <- sd1^2
      var2 <- sd2^2
      sdpool <- sqrt ( ( nd1*var1 + nd2*var2) / (nd1+nd2) )
      sdpool2 <- sqrt((nd1 * var1 + nd2 * var2) / (nd1 + nd2 -2))
      cohen_d <- dav/sdpool
      hedges_g <-  dav / sdpool2
      
      dof <- round((var1/nd1+var2/nd2)^2/(var1^2/nd1^2/(nd1-1)+var2^2/nd2^2/(nd2-1)))
      t <- (abs(av1-av2))/sqrt(var1/nd1+var2/nd2)
      pv <- pt(-t,df=dof)*2
      vpool <- ((nd1-1)*var1+(nd2-1)*var2)/(nd1+nd2-2)
      cl_l <- (av1-av2)-qt(0.975,nd1+nd2-2)*sqrt(vpool*(1/nd1+1/nd2))
      cl_u <- (av1-av2)+qt(0.975,nd1+nd2-2)*sqrt(vpool*(1/nd1+1/nd2))
      if (pv <= 0.05){
        txj <- "有意水準5%で帰無仮説は棄却される（差はある）"
      }
      else{
        txj <- "有意水準5%で帰無仮説は棄却されない（差はない）"
      }
      txt <- paste("平均の差(Deviation of mean values): ", dav,"\n",
                   # "プールした標準偏差(Pooled variance): ", sdpool,"\n",
                   "効果量Cohenのd(Effect Size Cohen's d): ", round(cohen_d,3),"\n",
                   "効果量Hedgesのg(Effect Size Hedges' g): ", round(hedges_g,3),"\n",
                   # "自由度(Degree of freedom): ", dof,"\n",
                   "検定統計量t値(t-value): ", round(t, 3),"\n",
                   "P値(P[T<=t]): ", format(pv, nsmall = 7), "\n",
                   "95%信頼区間(CI): [ ", round(cl_l,3), ", ", round(cl_u,3), "]", "\n",
                   txj
                   )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
