
graph_datafile <- "tweets_for_graph.Rdata"

load(graph_datafile)

# function to filter data based on user-selected filters
get_graph_data <- function(input_day, input_type, input_starting, input_masssites, input_search, mydt = tweets) {
    mass_regex <- c("gillette|natick\\small|eastfield\\small|fenway|hynes|reggie\\slewis|DoubleTree Hotel...Danvers|Circuit City...Dartmouth ")
    if(input_masssites == "Yes") {
      mydt <- mydt[!grepl(mass_regex, text, ignore.case = TRUE)]
    }
    if(input_day != "All") {
        mydt <- mydt[Weekday == input_day]
    }
    if(input_type == "cvs") {
        mydt <- mydt[Type == "CVS"]
    } else if(input_type == "noncvs") {
        mydt <- mydt[Type == "Specific Location"]
    } 
  
    if(input_search != "") {
        input_regex <- gsub("\\s?OR\\s?", "|", input_search)
        mydt <- mydt[grepl(input_regex, text, ignore.case = TRUE)]
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
library(scales)

# Define UI ----
ui <- navbarPage(
  "@vaccinetime Tweet Analysis",
  inverse = FALSE,
    theme = shinytheme("cosmo"),

    # Sidebar with user filter options
    tabPanel("Tweets by Day of Week and Hour", 
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
            radioButtons("masssites", "Remove Sites Now in MA Central Reg System?", choices = c("Yes", "No"), selected = "Yes"),
            shiny::textAreaInput("search", "Search Table Tweet Column: (separate multiple terms with OR): ", height = 50)
        ),

        # Show plot and table
        mainPanel(
           uiOutput("myheadline"),
           echarts4rOutput("mygraph"),
           uiOutput("table_headline"),
           h4("(Use filters on left to affect above graph too)", align = "center"),
           DT::DTOutput("mytable")
            
            
        )
    )
    ),   # end tab panel 1
  
  tabPanel("Daily Appointment Totals (excl. CVS)",
           
           sidebarLayout(
             sidebarPanel(
               radioButtons("day2", "Select Day: ",
                            choices = c("All", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), selected = "All"
               ),
               shiny::dateInput("starting2", "Starting on: ",
                                min = "2021-02-10", max = Sys.Date(),
                                value = "2021-03-01"
               ),
               radioButtons("masssites2", "Remove Sites Now in MA Central Reg System?", choices = c("Yes", "No"), selected = "Yes"),
             ),
           mainPanel(
             uiOutput("myheadline2"),
             echarts4rOutput("totals_graph")
           )
           )
  ),
  
  tabPanel("About",
           fluidPage(
             fluidRow(
               column(width = 8, offset = 2,
           HTML("<p></p>
                <p>This application looks at date and time trends for when new vaccine appointments first become publicly available, using the incredible <a target='_new' href='https://twitter.com/vaccinetime'>@vaccinetime</a> Twitter bot by <a target='_new' href='https://twitter.com/dpcahoon'>Dan Cahoon</a> at <a target='_new' href='https://twitter.com/dpcahoon'>Ginko Bioworks</a>. <em>Data here only update every 30 minutes. This is not meant for real-time use. </em> If you want to pounce on appointments quickly, use the <a target='_new' href='https://twitter.com/vaccinetime'>@vaccinetime bot</a>, which updates much more frequently. This app aims to help you understand <em>when </em>you might want to be paying special attention to that bot.</p>
                <p>Note that there will be many other vaccine appointments in Massachusetts not covered by @vaccinetime and this app, including the seven mass vaccination centers now handled by the state's central registration system, health systems such as Mass General Brigham, and some locally run sites, among others. Try not to get too discouraged at the current number of available appointments here! (And, while CVS is listed, they don't post the exact appointments they have available each week so they aren't included in totals.) Most importantly, vaccine supplies are supposed to increase significantly in the next few weeks.</p>
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
  
   # Get filtered data for graph and table
    thedata <- reactive({
        req(tweets, input$day, input$type, input$starting)
        get_graph_data(input$day, input$type, input$starting, input$masssites, input$search, tweets)
        
    })
    
    
    thedata2 <- reactive({
      req(tweets, input$starting2)
      get_graph_data(input$day2, input_type = "noncvs", input$starting2, input$masssites2, input_search = "", tweets)
      
    })
    
    
    daily_totals <- reactive({
      req(thedata2())
      thedata2() %>%
        dplyr::group_by(Date) %>%
        dplyr::summarize(
          `Known Number of Appointments` = sum(Number, na.rm = TRUE)
        ) %>%
        mutate(
          Number = scales::comma(`Known Number of Appointments`, accuracy = 1)
        )
      
    })
    
    total_appointments <- reactive({
      req(thedata())
      sum(thedata()$Number, na.rm = TRUE)
      
    })
    
    # get tab 1 headline based on user selections
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
    
    
    # get tab 2 headline based on user selections
    theheadlinetext2 <- reactive({
      req(input$day, input$type, input$starting)
      myhead <- paste("Daily Known Appointment Totals (excluding CVS) Starting ", format(input$starting, "%B %e"))
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
    
tableheadlinetext <- reactive({
  paste0("All Matching Tweets: ", scales::comma(total_appointments(), accuracy = 1), " appointments (not including CVS)")
  
  
})    
    
    
output$table_headline <- renderUI({
  h3(tableheadlinetext(), align = "center")
})
    
    output$myheadline <- renderUI({
        h2(theheadlinetext(), align = 'center')
    })
    
    output$mytable <- renderDT({
        DT::datatable(thedata(), filter = 'top', extensions = 'Buttons', 
                      options = list( search = list(
                                   regex = TRUE
                                    ),
                                 dom = 'Btlp',
                                 buttons = 'csv',
                                 pageLength = 25,
                                 lengthMenu = c(25, 50, 100, 200, 500)
                                 
                                 )
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
    
    output$myheadline2 <- renderUI({
      h2(theheadlinetext2(), align = 'center')
    })
    
    output$totals_graph <- renderEcharts4r({
      daily_totals() %>%
        echarts4r::e_chart(Date) %>%
        echarts4r::e_bar(`Known Number of Appointments`, bind = Number) %>%
        echarts4r::e_legend(show = FALSE) %>%
        e_tooltip(formatter = htmlwidgets::JS("
      function(params){
        return(params.value[0] + '<br />' + params.name )
      }
    ")
        ) 
      
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
