library(shiny)
library(irtoys)
library(ggplot2)

# UI for application
ui <- fluidPage(
  
  # app title
  titlePanel("Exam simulation"),
  sidebarLayout(
    sidebarPanel(titlePanel("Test characteristics"),
                 width=5,
                 # Input: select difficulty level
                 selectInput("Discriminativity", "Average discriminativity level:",
                             choices = c("Low", "Medium", "High"),
                             selected="Medium"),
                 selectInput("Difficulty", "Average difficulty level:",
                             choices = c("Very easy","Easy","Medium","Hard","Very hard"),
                             selected="Medium"),
                 selectInput("Guessing", "Guessing probability:",
                             choices = c("Low", "Medium", "High"),
                             selected="Medium"),
                 
                 plotOutput("hist", height = "150px"),
                 
                 sliderInput("crit","Threshold",min=0,max=1,value=.75,step=.01),
                 numericInput("M","Number of items",20),
                 numericInput("N","Number of examinees",value=800),
                 
    ),
    mainPanel(width=7,
              tabsetPanel(
                type = "tabs",
                tabPanel("Selection outcome", 
                         plotOutput("plot"),
                         tags$br(), # line break
                         tags$h4("Confusion matrix"),
                         tableOutput("matrix")),
                tabPanel("Item parameters", tableOutput("table")),
                # tabPanel("Confusion matrix", tableOutput("matrix"))
              )
    )
  )
)

server <- function(input, output, session) {
  
  simulate_ability <- function(N=500,m=0,sd=1) {
    th <- rnorm(N,m,sd)
    return(th)
  }
  
  simulate_params <- function(a=1.2,b=0,c=.2,M=20) {
    pa1 <- rlnorm(M,log(a),.2)
    pa2 <- rnorm(M,b)
    pa3 <- runif(M,0,c)
    pa <- cbind(pa1,pa2,pa3)
    return(pa)
  }
  
  simulate_res <- function(a,b,c,M,N) {
    pa <- simulate_params(a,b,c,M)
    th <- simulate_ability(N) 
    res <- sim(pa,th)
    results <- list(pa,th,res)
    return(results)
  }
  
  datalist <- reactive({
    # numeric values for parameters
    a <- c(.8,1.2,1.6)[
      which(c("Low", "Medium", "High")==input$Discriminativity)]
    b <- c(-1,-.5,0,.5,1)[
      which(c("Very easy","Easy","Medium","Hard","Very hard")==input$Difficulty)]
    c <- c(.05,.20,.35)[
      which(c("Low", "Medium", "High")==input$Guessing)]
    # number of items and examinees
    M <- input$M
    N <- input$N
    # simulation of parameters, ability and results
    res <- simulate_res(a,b,c,M,N)
    pa <- res[[1]]
    th <- res[[2]]
    sc <- res[[3]]
    # acceptance criterion
    crit <- input$crit
    # list of all objects
    list(a,b,c,M,N,pa,th,sc,crit)
  })
  
  output$table <- renderTable(
    striped=T, rownames=T, {
      df <- datalist()[[6]]
      colnames(df) <- c("a","b","c")
      df
    })
  
  output$hist <- renderPlot({
    scores <- datalist()[[8]]
    rs <- data.frame(rs=rowSums(scores))
    ggplot(rs,aes(rs)) +
      geom_histogram(binwidth=1) +
      labs(x="Total score",y="Number of examinees")
  })
  
  output$plot <- renderPlot({
    scores <- datalist()[[8]] # score sheet
    N <- datalist()[[5]] # number of examinees
    rs <- rowSums(scores) # raw score (number of points)
    cst <- cumsum(table(rs)) # cumulative points histogram
    crit <- datalist()[[9]] # criterion for selection
    ptlim <- as.numeric(names(which(cst>(crit*N))[1]))
    th <- datalist()[[7]] # latent ability
    pth <- pnorm(th) # quantile of latent ability
    df <- data.frame(pth,rs)
    df$comp <- ""
    df$comp <- ifelse(df$pth<crit,"unacceptable","acceptable")
    df$pass <- ifelse(df$rs<ptlim,"fail","pass")
    df$sel <- paste0(df$comp," - ",df$pass)
    ggplot(df,aes(x=pth,y=rs,col=sel)) +
      geom_point(alpha=.7,size=1.5) +
      geom_hline(yintercept=ptlim-.5,color="gray") +
      labs(x="Latent ability",y="Score",color="Outcome")
  })
  
  output$matrix <- renderTable(
    striped=T, rownames=T, digits=3, {
      scores <- datalist()[[8]] # score sheet
      N <- datalist()[[5]] # number of examinees
      rs <- rowSums(scores) # raw score (number of points)
      cst <- cumsum(table(rs)) # cumulative points histogram
      crit <- datalist()[[9]] # criterion for selection
      ptlim <- as.numeric(names(which(cst>(crit*N))[1]))
      th <- datalist()[[7]] # latent ability
      pth <- pnorm(th) # quantile of latent ability
      df <- data.frame(pth,rs)
      df$comp <- ""
      df$comp <- ifelse(df$pth<crit,"unacceptable","acceptable")
      df$pass <- ifelse(df$rs<ptlim,"fail","pass")
      m <- table(df$pass,df$comp)/N
      matrix <- data.frame(unacceptable=rev(m[,2]),acceptable=rev(m[,1]),
                           row.names=c("pass","fail"))
    })
}

# run the application 
shinyApp(ui = ui, server = server)
