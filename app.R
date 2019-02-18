library(shiny)
library(rvest)
library(dplyr)
library(plotly)
library(shinydashboard)

#Load Computer Rating Differentials table
crd <- read.csv("ratingTable.csv",stringsAsFactors = F)

##Scrape the womens data from tennisrecord
xpath <- "/html/body/div/div/main/div[1]/div[1]/div[5]/table"
url <- "http://www.tennisrecord.com/adult/ratings.aspx?sectionname=Florida&districtname=Region%202&areaname=Alachua&gender=F&orderby=DynamicRating"
ratings <- url %>%
  read_html() %>%
  html_nodes(xpath='/html/body/div/div/main/div[1]/div[1]/div[5]/table') %>%
  html_table()
ratings <- ratings[[1]] %>% select(`Player Name`,`2019EstimatedDynamic`, CurrentNTRP) 
##Fill in NA's
ratingsNa <- ratings %>% filter(is.na(`2019EstimatedDynamic`)) %>% rowwise %>% mutate(`2019EstimatedDynamic`=strsplit(CurrentNTRP," ")[[1]][1])
ratings <- ratings %>% filter(!is.na(`2019EstimatedDynamic`))
ratings <- rbind(ratings,ratingsNa) %>% select(`Player Name`,`2019EstimatedDynamic`)


##Scrape the mens data next
url2 <- "http://www.tennisrecord.com/adult/ratings.aspx?sectionname=Florida&districtname=Region%202&areaname=Alachua&gender=M&orderby=DynamicRating"
ratings2 <- url2 %>%
  read_html() %>%
  html_nodes(xpath='/html/body/div/div/main/div[1]/div[1]/div[5]/table') %>%
  html_table()
ratings2 <- ratings2[[1]] %>% select(`Player Name`,`2019EstimatedDynamic`, CurrentNTRP) 
##Fill in NA's
ratingsNa2 <- ratings2 %>% filter(is.na(`2019EstimatedDynamic`)) %>% rowwise %>% mutate(`2019EstimatedDynamic`=strsplit(CurrentNTRP," ")[[1]][1])
ratings2 <- ratings2 %>% filter(!is.na(`2019EstimatedDynamic`))
ratings2 <- rbind(ratings2,ratingsNa2) %>% select(`Player Name`,`2019EstimatedDynamic`)

##Combine them into one dataframe
ratings <- rbind(ratings,ratings2)
names(ratings) <- c("Player","Rating")


# Build the shiny app
# Define UI
ui <- dashboardPage(
  dashboardHeader(title= "USTA Match Predictor"),
  dashboardSidebar(disable = T),
  dashboardBody(
    column(width = 3,
           fluidRow(
             ##Team1 Selector
             selectizeInput('in1', 'Team 1:', ratings$Player,options = list(maxItems = 2), multiple=TRUE, selected = ratings[ratings$Player=="Timothy Ferguson",]$Player),
             uiOutput('out1')
           ),
           fluidRow(hr()),
           fluidRow(
             ##Team2 Selector
            selectizeInput('in2', 'Team 2:', ratings$Player,options = list(maxItems = 2), multiple=TRUE,selected = ratings[ratings$Player=="Nathan Wycoff",]$Player),
             uiOutput('out2')
           )),
    column(width = 9,
                  plotlyOutput("plot"),
                  uiOutput("compText"),
                  uiOutput("crdText"))
    #HTML('<footer style="bottom:0;"><br><br>This work is based on dynamic ratings provided by tennisranking.com.</footer>')
  )
)




# Define server logic
server <- function(input, output) {

  output$out1 <- renderText({
    iText1 <- ""
    for(i in seq_along(input$in1)){iText1 <- c(iText1, paste0(input$in1[i]," ","(",ratings[ratings$Player==input$in1[i],]$Rating,")","<br>") )}
    HTML(iText1)
    })
  output$out2 <- renderText({
    iText2 <- ""
    for(i in seq_along(input$in2)){iText2 <- c(iText2, paste0(input$in2[i]," ","(",ratings[ratings$Player==input$in2[i],]$Rating,")","<br>") )}
    HTML(iText2)
    })
  output$plot <- renderPlotly({
    t1r <-  sum(as.numeric(ratings[ratings$Player==input$in1,]$Rating))
    t2r <-  sum(as.numeric(ratings[ratings$Player==input$in2,]$Rating))
    data <- data.frame(diff="differential", value=t1r-t2r)
    if(data$value>.2){c<-I("green")}else if(data$value<.2&data$value>-.2){c<-I("blue")}else{c<-I("red")}
    plot_ly(data,x = ~diff, y = ~value,type = "bar",marker = list(color = c))%>%
      layout(yaxis = list(range = c(0-t2r,t1r),title = ""),xaxis =list(title = "",showticklabels = FALSE))
  })
  
  output$compText <- renderText({
    t1r <-  sum(as.numeric(ratings[ratings$Player==input$in1,]$Rating))
    t2r <-  sum(as.numeric(ratings[ratings$Player==input$in2,]$Rating))
    data <- data.frame(diff="differential", value=t1r-t2r)
    if(data$value>.2){outcome<-"win"}else if(data$value<.2&data$value>-.2){outcome<-"tiebreak set"}else{outcome<-"loss"}
    HTML(paste0("A ","<b>",outcome,"</b>", " is likely for Team 1 when ",paste0(input$in1,collapse = " & ")," plays ",paste0(input$in2,collapse = " & ")))
    })
  output$crdText <- renderText({
    t1r <-  sum(as.numeric(ratings[ratings$Player==input$in1,]$Rating))
    t2r <-  sum(as.numeric(ratings[ratings$Player==input$in2,]$Rating))
    data <- data.frame(diff="differential", value=t1r-t2r)
    crdScores <- if(data$value>=.33){"6-0,6-0"}else{crd[findInterval(data$value,crd$differential),]$scores}
    if(data$value>.2){outcome<-"win"}else if(data$value<.2&data$value>-.2){outcome<-"tiebreak set"}else{outcome<-"loss"}
    HTML(if(length(crdScores)==0){}else{paste0("Scores are likely to be: ", crdScores)})
  })
                                                                                                                 
  
  

}

# Run the application 
shinyApp(ui = ui, server = server)

