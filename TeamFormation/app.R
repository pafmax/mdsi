################################################################################
# Author: Perry Stephenson                                                     #
# Date:   11 August 2016                                                       #
################################################################################

library(shinydashboard) # This package provides a nice new set of Shiny UI
library(ggplot2)        # Not actually being used at the moment
library(plotly)         # This provides ggplot-like plotting with interactivity
library(DT)             # Provides interactive table functions

##### === UI Definition === #####
# The main UI elements of a dashboardPage (header, sidebar and body) are saved 
# separately as objects to improve readability. They are included as arguments
# to the dashboardPage function further down in the script.

##### Header #####
# The only important thing in the header is the page title
header <- 
  dashboardHeader(
    title = "Unearthed Teams"
  )

##### Sidebar #####
# The sidebar defines the navigation structure for the site
# The arguments are fairly self-explanatory, except for tabName which is used to
# refer to the body code for what will appear in the tab.
sidebar <- 
  dashboardSidebar(
    sidebarMenu(
      menuItem(text    = "Find Similarities",
               tabName = "similar",
               icon    = icon("users"))
      ,
      menuItem(text    = "Add Yourself",
               tabName = "add",
               icon    = icon("plus-circle"))
    )
  )


##### Body #####
# This defines the UI elements and how they are displayed on the page.
# The layout is based around "box" objects - they accept a few standard arguments
# and then a variable number of columns, rows and boxes depending on your layout.
# For readability, the boxes are saved as objects, and then passed as arguments.

##### Body - Components #####

data_entry_box <- # This is the big one on the data-entry page
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
      "0 means this is a crazy statement which is nothing like you at all, 10 means you agree so much it hurts!",
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
      br(),
      br(),
      actionButton(inputId = "submit",
                   label = "Submit",
                   icon = icon("check")),
      br(),
      br(),
      textOutput("submitResult"),
      br()
  )

who_box <- 
  box(title = "Who are you?",
      width = 12,
      solidHeader = T,
      status = "primary",
      uiOutput("name_selector")
  )

similarity_plot_box <- 
  box(title = "Preference Similarity",
      width = 12,
      solidHeader = T,
      status = "primary",
      "This plots shows how similar your preferences are to other MDSI students who have filled out the form. It does not take skills into account.",
      plotlyOutput("plot")
  )

list_box <- 
  box(title = "More Details",
      width = 12,
      solidHeader = T,
      status = "primary",
      "By default this list is sorted by preference similarity - lower numbers mean that the person has similar preferences to yours. Higher numbers mean that they have different preferences to you.",
      dataTableOutput('details_table')
  )


##### Body - Definition #####
body <- 
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "add",
        data_entry_box             # Defined above
      ),
      tabItem(tabName = "similar",
              who_box,             # Defined above
              similarity_plot_box, # Defined above
              list_box             # Defined above
      )
    )
  )

##### Combine UI #####

ui <- 
  dashboardPage(
    header,
    sidebar,
    body
)

##### === Server Definition === #####
server <- shinyServer(function(input, output) {
  
  # This pulls in all of the files from the "people" folder and joins them all
  # into a data frame. It is within a reactive context so that it can be 
  # re-triggered when things change (like when someone adds a new entry)
  getPeopleList <- reactive({
    dummy <- eventReactive(input$submit,{}) # Triggers recalculation
    files <- list.files(path = "people/", full.names = T)
    for (i in seq_along(files)) {
      if (i == 1) {
        people <- read.csv(files[i])
      } else {
        people <- rbind(people, read.csv(files[i]))
      }
    }
    people <- people[,-1] # Remove index column
    return(people)
  })
  
  # This generates a UI element (selectInput) to be displayed as part of the UI.
  # It is generated here (rather than defined in UI) so that it can incorporate
  # a dynamically generated drop-down menu containing all users.
  output$name_selector <- renderUI({
    people <- getPeopleList()
    selectInput(inputId = "who",
                label = "Who are you?",
                choices = as.character(people$name)
    )
  })
  
  # This saves the data from the form into a data frame, and then writes it out 
  # to an individual file. Individual files are used because they resolve issues
  # around concurrent read/write operations. There is probably a better way to 
  # do this, but it works well enough for now.
  output$submitResult <- eventReactive(input$submit, {
    
    # Concatenate the "languages" and "skills" selections into single cells
    # so that they can go in a single row of a data frame
    if(is.null(input$languages)) {
      languages <- " "
    } else {
      languages <- paste(input$languages, collapse = ", ")
    }
    if(is.null(input$skills)) {
      skills <- " "
    } else {
      skills <- paste(input$skills, collapse = ", ")
    }
    
    # Create the data frame
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
   
    # Write to file
   filename <- paste0("people/",gsub("[^[:alnum:]]", "",input$name),".csv")
   write.csv(save_row, file = filename)
   
   # Return a message if everything has worked. This is printed on screen by the 
   # UI.
   return("Submission successful!")
  })
  
  # This uses PCA to reduce the 5-dimension preference data to a 2-dimensional
  # plot for exploration.
  output$plot <- renderPlotly({
    
    people <- getPeopleList()
    people_pca <- prcomp(people[,2:6]) # This is the PCA step
    people$PC1 <- people_pca$x[,1]     # First principle component
    people$PC2 <- people_pca$x[,2]     # Second principle component
    
    # Create a 1-row data frame with the details for the person who should be 
    # labelled. This person is selected by the user.
    anno <- people[people$name == input$who,]
    
    # Use that 1-row data frame to create a named list with some annotation
    # information, which is passed as an argument later to create the "you are
    # here" call out on the plot.
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
    
    # Using plotly rather than ggplot as it allows dynamic annotations, and 
    # also provides the mouse-over functionality allowing you to see who is near
    # you in the plot.
    plot_ly(people, x=PC1, y=PC2, text=name, mode="markers") %>%
      layout(annotations = a)
  })
  
  # This adds a "Distance" column to the data frame, and cuts the table down to 
  # make it ready for presentation. This is most definitely NOT the most 
  # efficient way to do this, but it makes it a bit easier to follow. And there 
  # will only be 30 rows or so, so speed isn't really an issue.
  differenceTable <- reactive({
    people <- getPeopleList()
    people$distance <- NA
    sel <- which(people$name == input$who)
    for (i in seq_along(people$name)) {
      sel_vec <- people[sel,c(2:6)]
      i_vec   <- people[i,c(2:6)]
      distance_vec <- sel_vec - i_vec
      distance_calc <- sqrt(sum(distance_vec^2))
      people[i,"distance"] <- distance_calc
    }
    subset <- people[,-c(2:6)]
    names(subset) <- c("Name", "Languages", "Skills", "Hackathons", "Distance")
    return(subset)
  })
   
  # This presents the table returned from the function above, using the DT 
  # package. It also sorts the list using the "Distance" column (which is the 
  # 5th column)
  output$details_table <- 
    renderDataTable(
    expr = differenceTable(), # Defined above
    options = list(order = list(list(5, 'asc')))
  )
  
})

# Start the Shiny App
shinyApp(ui = ui, server = server)

