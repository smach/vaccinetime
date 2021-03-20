graph_datafile <-paste0(Sys.getenv("VACTIME_PATH"), "/VaccineTimeTweets/tweets_for_graph.Rds")

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
library(DT)
library(echarts4r)
library(data.table)
load(graph_datafile)
library(stringr)
library(dplyr)

# Define UI ----
ui <- fluidPage(

    # Application title
    titlePanel("@vaccinetime tweets"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            radioButtons("day", "Select Day: ",
                         choices = c("All", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), selected = "All"
                        ),
            radioButtons("type", "Select Type: ",
                         choices = c("All" = "all", "CVS" = "cvs", "Non-CVS" = "noncvs", selected = "all")
                         ),
            shiny::dateInput("starting", "Starting on: ",
                             min = "2021-02-10", max = Sys.Date(),
                             value = "2021-03-01"
                             ),
            shiny::textAreaInput("search", "Search Location For (separate multiple terms with OR): ", height = 50)
        ),

        # Show plot and table
        mainPanel(
           uiOutput("myheadline"),
           echarts4rOutput("mygraph"),
           h3("All Matching Tweets", align = "center"),
           DT::DTOutput("mytable")
            
            
        )
    )
)

# Define server logic ----

server <- function(input, output) {
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
        DT::datatable(thedata(), filter = 'top', rownames = FALSE, options = list( search = list(
            regex = TRUE
        ),
        dom = 't')
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
