
# graph_datafile <- paste0(Sys.getenv("VACTIME_PATH"), "/tweets_for_graph.Rdata")
graph_datafile <- "tweets_for_graph.Rdata"

load(graph_datafile)

get_graph_data <- function(input_day, input_type, input_starting, input_search, mydt = tweets) {
    if(input_day != "All") {
        mydt <- mydt[Weekday == input_day]
    }
    if(input_type == "cvs") {
        mydt <- mydt[Type == "CVS"]
    } else if(input_type == "noncvs") {
        mydt <- mydt[Type == "Specific Location"]
    } # else if(input_type == "cvsplus") {
      #    mydt <- mydt[Location %chin% useful_locations]
#    }
    if(input_search != "") {
        input_regex <- gsub("\\s?OR\\s?", "|", input_search)
        mydt <- mydt[grepl(input_regex, Location)]
    }
    
    mydt <- mydt[Date >= input_starting, .("Created" = as.character(Time), "Tweet" = text, Date, Hour, Weekday, Number, Location)]
    return(mydt)
    
}


library(shiny)
library(shinythemes)
library(DT)
library(echarts4r)
library(data.table)
library(stringr)
library(dplyr)

# Define UI ----
ui <- navbarPage(
  "@vaccinetime Tweet Analysis",
  inverse = FALSE,
  # Application title
  # titlePanel("@vaccinetime tweets"),

    # Application title
  #  titlePanel("@vaccinetime tweets"),
    theme = shinytheme("cosmo"),

    # Sidebar with a slider input for number of bins 
    tabPanel("Data by Day of Week and Hour", 
    sidebarLayout(
        sidebarPanel(
            radioButtons("day", "Select Day: ",
                         choices = c("All", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), selected = "All"
                        ),
            radioButtons("type", "Select Type: ",
                         choices = c("All" = "all", "CVS" = "cvs", "Non-CVS" = "noncvs"), selected = "all"),
                        
            shiny::dateInput("starting", "Starting on: ",
                             min = "2021-02-10", max = Sys.Date(),
                             value = "2021-03-01"
                             ),
            shiny::textAreaInput("search", "Search Table Location Column: (separate multiple terms with OR): ", height = 50)
         #   textOutput("test")
        ),

        # Show plot and table
        mainPanel(
           uiOutput("myheadline"),
           echarts4rOutput("mygraph"),
           h3("All Matching Tweets", h4("(Use filters on left to affect above graph too)", align = "center"), align = "center"),
           DT::DTOutput("mytable")
            
            
        )
    )
    ),   # end tab panel 1
  tabPanel("About",
           fluidPage(
             fluidRow(
               column(width = 8, offset = 2,
           HTML("<p></p>
                <p>This application looks at date and time trends for when new vaccine appointments first become publicly available, using the incredible <a target='_new' href='https://twitter.com/vaccinetime'>@vaccinetime</a> Twitter bot by <a target='_new' href='https://twitter.com/dpcahoon'>Dan Cahoon</a> at <a target='_new' href='https://twitter.com/dpcahoon'>Ginko Bioworks</a>. <em>Data here only update hourly. This is not meant for real-time use. </em> If you want to pounce on appointments quickly, use the <a target='_new' href='https://twitter.com/vaccinetime'>@vaccinetime bot</a>, which updates much more frequently. This app aims to help you understand <em>when </em>you might want to be paying special attention to that bot.</p>
               <p>This app was created by <a target='_new' href='https://twitter.com/sharon000'>Sharon Machlis</a> with the <a target='_new' href='https://www.r-project.org/'>R programming language</a> and R packages <a target='_new' href='https://shiny.rstudio.com/'>shiny</a>, <a target='_new' href='https://rdatatable.gitlab.io/data.table/'>data.table</a>,  <a target='_new' href='https://docs.ropensci.org/rtweet/'>rtweet</a>, <a target='_new' href='https://echarts4r.john-coene.com/'>echarts4r</a>, <a target='_new' href='https://rstudio.github.io/DT/'>DT</a>, and <a target='_new' href='https://lubridate.tidyverse.org/'>lubridate</a>, among others. It wouldn't have been possible without the incredible contributions of the R core team and R package authors to make this free platform possible -- as well as, of course, Dan's bot. (Any and all errors in the data presentation are, however, mine alone.)</p>
               <p><em>Infrastructure provided by <a href='https://www.digitalocean.com/'>Digital Ocean</a>.</em></p>
               " 
          
           )
               ) # end columns
             ) # end fluid row
           ) # end fluid page
  )
  
)

# Define server logic ----

server <- function(input, output, session) {
  
    output$test <- renderText(
      input$search
    )
    thedata <- reactive({
        req(tweets, input$day, input$type, input$starting)
        get_graph_data(input$day, input$type, input$starting, input$search, tweets)
        
    })
    
    theheadlinetext <- reactive({
        req(input$day, input$type, input$starting)
        myhead <- paste("Tweets by Hour Starting", format(input$starting, "%B %e"))
        if(input$day != "All") {
            myhead <- stringr::str_glue("{myhead} on {input$day}s")
        }
        if(input$type == "cvs") {
            myhead <- paste(myhead, "for CVS")
        } else if (input$type == "noncvs") {
            myhead <- paste(myhead, "Except CVS")
        }
        return(myhead)
    })
    
    
    output$myheadline <- renderUI({
        h2(theheadlinetext(), align = 'center')
    })
    
    output$mytable <- renderDT({
        DT::datatable(thedata(), filter = 'top', rownames = FALSE, extensions = 'Buttons', 
                      options = list( search = list(
                                   regex = TRUE
                                    ),
                                 dom = 'Bt',
                                 buttons = 'csv')
                      )
    })
    
    output$mygraph <- renderEcharts4r({
           thedata() %>%
            dplyr::mutate(
                Hour = stringr::str_pad(Hour, 2, pad = 0)
            ) %>%
            dplyr::group_by(Hour) %>%
            dplyr::tally() %>%
            echarts4r::e_chart(Hour) %>%
            echarts4r::e_bar(n) %>%
            echarts4r::e_legend(show = FALSE) %>%
            echarts4r::e_tooltip()
        
    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)
