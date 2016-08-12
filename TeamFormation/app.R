################################################################################
# Author: Perry Stephenson                                                     #
# Date:   11 August 2016                                                       #
################################################################################

library(shinydashboard)
library(ggplot2)
library(plotly)

header <- dashboardHeader(
  title = "Unearthed Teams"
)

sidebar <- 
  dashboardSidebar(
    sidebarMenu(
      menuItem("Find Similarities", tabName = "similar", icon = icon("users")),
      menuItem("Add Yourself", tabName = "add", icon = icon("plus-circle"))
    )
  )

body <- 
  dashboardBody(
    tabItems(
      tabItem(tabName = "add",
              box(title = "Introduce Yourself",
                  width = 12,
                  solidHeader = T,
                  status = "primary",
                  textInput(inputId = "name",
                            label = "Your Name"), 
                  hr(),
                  strong("How strongly do you agree with the following statements?"),
                  br(),
                  "Your answers here will be used to help match you with compatible team members.",
                  br(), 
                  "0 means you don't agree at all, 10 means you agree so much it hurts!",
                  br(),br(),
                  sliderInput(inputId = "win",
                              label = "I want to win the hackathon",
                              min = 0, max = 10, value = 5, step = 0.1),
                  sliderInput(inputId = "learn",
                              label = "I want to learn some new skills",
                              min = 0, max = 10, value = 5, step = 0.1),
                  sliderInput(inputId = "try",
                              label = "I want to try a new technique I've read about",
                              min = 0, max = 10, value = 5, step = 0.1),
                  sliderInput(inputId = "fun",
                              label = "I want to have fun",
                              min = 0, max = 10, value = 5, step = 0.1),
                  sliderInput(inputId = "startup",
                              label = "I want to join a start up and change the world",
                              min = 0, max = 10, value = 5, step = 0.1),
                  hr(),
                  strong("What do you bring to a team?"),
                  br(),
                  "These answers won't be analysed, but will be made available for people to view when forming teams.",
                  br(), br(),
                  checkboxGroupInput(inputId = "languages",
                                     label = "Languages",
                                     choices = c("R","Python","Tableau","SAS","SQL","KNIME")
                                     ),
                  checkboxGroupInput(inputId = "skills",
                                     label = "Skills",
                                     choices = c("Machine Learning",
                                                 "Data Manipulation",
                                                 "Visualisation",
                                                 "Storytelling",
                                                 "Business Case Development",
                                                 "Project Management")
                                     ),
                  selectInput(inputId = "times",
                              label = "How many hackathons have you been to?",
                              choices = list("None (first timer)" = 0,
                                             "1" = 1,
                                             "2" = 2,
                                             "3+" = 3)
                             ),
                  "Thanks for entering your information - please click 'submit' below to add your details to the database.",
                  actionButton(inputId = "submit",
                               label = "Submit",
                               icon = icon("check")),
                  textOutput("submitResult")
              )
      ),
      tabItem(tabName = "similar",
              box(title = "Similarity",
                  width = 12,
                  solidHeader = T,
                  status = "primary",
                  uiOutput("name_selector"),
                  plotlyOutput("plot")
              )
      )
    )
  )

ui <- 
  dashboardPage(
    header,
    sidebar,
    body
)

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  
  output$name_selector <- renderUI({
    
    files <- list.files(path = "people/", full.names = T)
    
    for (i in seq_along(files)) {
      if (i == 1) {
        people <- read.csv(files[i])
      } else {
        people <- rbind(people, read.csv(files[i]))
      }
    }
    
    selectInput(inputId = "who",
                label = "Who are you?",
                choices = as.character(people$name)
    )
    
  })
  
  output$submitResult <- eventReactive(input$submit, {
    
    if(is.null(input$languages)) {
      languages <- " "
    } else {
      languages <- paste(input$languages, collapse = " ")
    }
    
    if(is.null(input$skills)) {
      skills <- " "
    } else {
      skills <- paste(input$skills, collapse = " ")
    }
    
   save_row <- data.frame(
     name = input$name,
     win = input$win,
     learn = input$learn,
     try = input$try,
     fun = input$fun,
     startup = input$startup,
     languages = languages,
     skills = skills,
     times = input$times
   )
   
   filename <- paste0("people/",gsub("[^[:alnum:]]", "",input$name),".csv")
   
   write.csv(save_row, file = filename)
   
   return("Submission successful!")
  })
  
  output$plot <- renderPlotly({
    
    files <- list.files(path = "people/", full.names = T)

    for (i in seq_along(files)) {
      if (i == 1) {
        people <- read.csv(files[i])
      } else {
        people <- rbind(people, read.csv(files[i]))
      }
    }
    
    people <- people[,-1]
    
    people_pca <- prcomp(people[,2:6])
    
    people$PC1 <- people_pca$x[,1]
    people$PC2 <- people_pca$x[,2]
    
    anno <- people[people$name == input$who,]
    
    a <- list(
      x = anno$PC1,
      y = anno$PC2,
      text = "You are here",
      xref = "x",
      yref = "y",
      showarrow = TRUE,
      arrowhead = 7,
      ax = 20,
      ay = -40
    )

    plot_ly(people, x=PC1, y=PC2, text=name, mode="markers") %>%
      layout(annotations = a)
     
  })
   
})

# Run the application 
shinyApp(ui = ui, server = server)

