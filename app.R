library(shiny)
library(shinydashboard)
library(plyr)




header<-dashboardHeader(title = "Health Data Analytics with Cloud Based Solution", titleWidth = 500)

sidebar<-dashboardSidebar(width = 350,collapsed  = TRUE,
                          sidebarMenu(
                            
                            br(), br(),
                            textInput("Name","Enter your Name"," "),
                            selectInput("Age","Enter your Age",c(" ", "0-20","21-30","31-40","41-50","51-60","61-70")),
                            selectInput("Alcohol","Amount of Alcohol per month (ml)",c("  ","0-500","501-1000","1001-1500","1501-2000","2001-2500","2501-3000")),
                            selectInput("Tobacco","Amount of Tobacco per month (g)",c("  ","0-400","401-800","801-1200","1201-1600","1601-2000","2001-2500")),
                            br()
                           
                            
                          ))



body<-dashboardBody(
  tabsetPanel(type = "tab",
              tabPanel("Histogram",plotOutput("myhist"),
                       
                       mainPanel(width=500,height=1000,
                         
                         
                         br(),
                         selectInput("var","1. Select the Variable from the Health Dataset", choices = c("Age"=1,"Year"=2,"Alcohol"=3,"Tobacco"=4)),
                         sliderInput("bins","2. Select the Name of BINs for Histogram",min=5,max=100,value=15),
                         radioButtons("color","3. Select the Colour of the Histogram", choices=c("Green","Red","Yellow"),select="Green")
                         
                       )      
                       
                       
                       ),
              tabPanel("BarPlot",plotOutput("barplot1"),plotOutput("barplot2"),plotOutput("barplot3"),plotOutput("barplot4"),icon = icon("bar-chart-o",lib = "font-awesome")),
              tabPanel("BoxPlot",plotOutput("boxplot1"),plotOutput("boxplot2"),plotOutput("boxplot4"),plotOutput("boxplot3"),plotOutput("boxplot5"),plotOutput("boxplot6"),icon = icon("bar-chart-o",lib = "font-awesome")),
              tabPanel("Relation",plotOutput("relation"),icon = icon("bar-chart-o",lib = "font-awesome")),
              tabPanel("Test You are Infect of Cirrhosis",
                       br(),
                       box(width=50,h4(textOutput("myname"))),
                       box(width=50,h4(textOutput("myage"))),
                       box(width=50,h4(textOutput("myalcohol"))),
                       box(width=50,h4(textOutput("mytobacco"))),
                       
                       box(width=50,h4(textOutput("textout")))
              
              
  )))
    

  
  # Define UI for application that draws a histogram
  ui <- dashboardPage(header,sidebar,body)
  
  
  server <- function(input, output, session) {
    
    #setwd("E:/rwork/rnew")
    
    df<-read.csv("cir.csv",stringsAsFactors = FALSE)
    data1<-df[1:20,]
    data2<-df[21:47,]
    data3<-df[48:63,]
    
    
     output$myhist<- renderPlot({
      colm <-as.numeric(input$var)
      df[colm]
      hist(df[,colm],breaks = seq(0, max(df[,colm]),l=input$bins+1), col=input$color, main="Histogram of Affect Cirrhosis Dataset", xlab = names(df[colm]))
    })
     
       output$barplot1<-renderPlot({
         barplot(df$tobacco,xlab="tobacco",ylab="cases",col="pink",main="Bar Plot for Tobacco Amount")
         
  })
       
       output$barplot2<-renderPlot({
         barplot(data2$alcohol,xlab="tobacco",ylab="cases",col="purple",main="Bar Plot for Alcohol Amount")
         
       })
       
       
       output$barplot3<-renderPlot({
         barplot(df$age,xlab="age",ylab="cases",col="red",main="Bar Plot for Age")
         
       })
       
       
       
       output$barplot4<-renderPlot({
         barplot(data3$year,type="l",xlab="year",ylab="cases",col="yellow",main="Bar Plot for Year")
         
       })
       
       output$boxplot1<-renderPlot({
         boxplot(cases~age,data=df,xlab="age",ylab="Cirrhosis Infects",col=rainbow(length(unique(df))),main="Box Plot for Using Cirrhosis Cases According to the Age")
       })
       
       
       
       output$boxplot2<-renderPlot({
         boxplot(cases~alcohol,data=df,xlab="Alcohol amount",ylab="Cirrhosis Infects",col=rainbow(length(unique(df))),main="Box Plot for Using Cirrhosis Cases According to the Alcohol Amount")
       })
       
       
       output$boxplot3<-renderPlot({
         boxplot(cases~tobacco,data=df,xlab="tobacco amount",ylab="Cirrhosis Infects",col=rainbow(length(unique(df))),main="Box Plot for Using Cirrhosis Cases According to the Tobacco Amount")
       })
       
       output$boxplot4<-renderPlot({
         boxplot(cases~year,data=df,xlab="year",ylab="Cirrhosis Infects",col=rainbow(length(unique(df))),main="Box Plot for Using Cirrhosis Cases According to the Year")
       })
       
       output$boxplot5<-renderPlot({
         boxplot(alcohol~age,data=data3,xlab="age",ylab="alcohol",col="purple",main="B0x Plot for Alcohol Amount according to the age")
         
       })
       output$boxplot6<-renderPlot({
         boxplot(tobacco~age,data=data3,xlab="age",ylab="tobacco",col="red",main="Box Plot for Tobacco Amount according to the age")
         
       })
       output$relation<-renderPlot({
         pairs(data1[,5:1],main="RELATIONSHIP BETWEEN VARIABLES")}
       )
       
       
     
       output$myname<-renderText(input$Name)
       output$myage<-renderText(input$Age)
       output$myalcohol<-renderText(input$Alcohol)
       output$mytobacco<-renderText(input$Tobacco)
       
       
       output$textout<- renderText({ width="200"
         # Show a modal when the button is pressed
         if((input$Alcohol == "0-500") && (input$Tobacco == "0-400")){
           ("You haven't risk of Cirrhosis")
           
         }else if((input$Alcohol == "0-500")&& (input$Tobacco=="401-800")){
           "You haven't risk of Cirrhosis"
           
          } else if((input$Alcohol == "0-500")&& (input$Tobacco=="801-1200")){
             "You have risk of Cirrhosis"
            
          } else if((input$Alcohol == "501-1000")&& (input$Tobacco=="0-400")){
            "You have risk of Cirrhosis"
            
          } else if((input$Alcohol == "501-1000")&& (input$Tobacco=="401-800")){
            "You have risk of Cirrhosis"
            
          }else if((input$Alcohol == "501-1000")&& (input$Tobacco=="801-1200")){
            "You have risk of Cirrhosis"
            
          }else if((input$Alcohol == "1001-1500")&& (input$Tobacco=="0-400")){
            "You have risk of Cirrhosis"
            
          } else if((input$Alcohol == "1001-1500")&& (input$Tobacco=="1201-1600")){
            "You have risk of Cirrhosis"
            
          } else if((input$Alcohol == "1001-1500")&& (input$Tobacco=="1601-2000")){
            "You have risk of Cirrhosis"
            
          } else if((input$Alcohol == "1501-2000")&& (input$Tobacco=="401-800")){
            "You have risk of Cirrhosis"
            
          }else if((input$Alcohol == "1501-2000")&& (input$Tobacco=="2001-2500")){
            "You haven't risk of Cirrhosis"
            
          } else if((input$Alcohol == "1501-2000")&& (input$Tobacco=="1201-1600")){
            "You have risk of Cirrhosis"
            
          } else if((input$Alcohol == "2001-2500")&& (input$Tobacco=="801-1200")){
            "You have risk of Cirrhosis"
            
          } else if((input$Alcohol == "2001-2500")&& (input$Tobacco=="2001-2500")){
            "You have risk of Cirrhosis"
            
            
          } else if((input$Alcohol == "2001-2500")&& (input$Tobacco=="1601-2000")){
            "You have risk of Cirrhosis"
            
          } else if((input$Alcohol == "2501-3000")&& (input$Tobacco=="401-800")){
            "You have risk of Cirrhosis"
            
          } else if((input$Alcohol == "2501-3000")&& (input$Tobacco=="0-400")){
            "You have risk of Cirrhosis"
            
         }else{
           "Fill Required"
         }
       })
       

  }
  
  shinyApp(ui=ui,server=server)