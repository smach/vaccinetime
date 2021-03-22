
# graph_datafile <- "tweets_for_graph.Rdata"
graph_csv <- "tweets.csv"
data.table::fread(graph_csv)
# load(graph_datafile)

# function to filter data based on user-selected filters
get_graph_data <- function(input_day, input_type, input_starting, input_masssites, input_search, mydt = updated_tweets()) {
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
                <p>This application looks at date and time trends for when new Massachusetts Covid-19 vaccine appointments first become publicly known via the wonderful <a target='_new' href='https://twitter.com/vaccinetime'>@vaccinetime</a> Twitter bot by <a target='_new' href='https://twitter.com/dpcahoon'>Dan Cahoon</a> at <a target='_new' href='https://twitter.com/dpcahoon'>Ginko Bioworks</a>. <em>Data here only update every 30 minutes. This is not meant for real-time use. </em> If you want to try to pounce on appointments quickly, use the <a target='_new' href='https://twitter.com/vaccinetime'>@vaccinetime bot</a>, which updates much more frequently. This app aims to help you understand <em>when </em>you <em>might </em>want to be paying special attention to that bot. (Of course, appointment posting schedules can change from week to week.)</p>
                <p>The first tab looks at total number of tweets by hour of day. You can filter by day of week and starting date, as well as search for specific text in tweets and include sites that are no longer available for manual sign-up (because they are part of the state's central registration system). The second tab totals number of appointments tweeted out by @vaccinetime per day, excluding CVS totals (since tweets about CVS don't say how many appointments are available when they are posted).</p>
                
                
                <p>Note that there are many other vaccine appointments in Massachusetts which are not covered by @vaccinetime (and so this app). They include appointments at the seven mass vaccination centers now handled by the state's <a target='_new' href='https://vaccinesignup.mass.gov/#/'>central registration system</a>, health organizations such as Mass General Brigham, and some locally run sites, among others. In addition, as mentioned above, CVS does not post the exact number of appointments they have available each week, so those aren't included in totals.</p>
                <p>Try not to get too discouraged at what looks like a low total number of available appointments shown here! Many more vaccines are being administered in Massachusetts than are calculated in the app's tweet totals.</p>
                <p>And, most importantly, vaccine supplies are supposed to increase significantly in the coming weeks.</p>
               <p>This app was created by <a target='_new' href='https://twitter.com/sharon000'>Sharon Machlis</a> with the <a target='_new' href='https://www.r-project.org/'>R programming language</a> and R packages <a target='_new' href='https://shiny.rstudio.com/'>shiny</a>, <a target='_new' href='https://rdatatable.gitlab.io/data.table/'>data.table</a>,  <a target='_new' href='https://docs.ropensci.org/rtweet/'>rtweet</a>, <a target='_new' href='https://echarts4r.john-coene.com/'>echarts4r</a>, <a target='_new' href='https://rstudio.github.io/DT/'>DT</a>, and <a target='_new' href='https://lubridate.tidyverse.org/'>lubridate</a>, among others. It wouldn't have been possible without the incredible contributions of the R core team and R package authors to make this free platform possible -- as well as, of course, Dan's bot. (Any and all errors in the data presentation are, however, mine alone.)</p>
               <p>App code on GitHub at: <a target='_new' href='https://github.com/smach/vaccinetime'>https://github.com/smach/vaccinetime</a>.</p>
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
  
  updated_tweets <- reactivePoll((60000 * 5), NULL, checkFunc = function() {if (file.exists("tweets.csv")) {file.info("tweets.csv")$mtime[1]} else ""}, valueFunc = function() {
    data.table::fread("tweets.csv")
    }    )
  
  
   # Get filtered data for graph and table
    thedata <- reactive({
        req(updated_tweets(), input$day, input$type, input$starting)
        get_graph_data(input$day, input$type, input$starting, input$masssites, input$search, updated_tweets())
        
    })
    
    
    thedata2 <- reactive({
      req(tweets, input$starting2)
      get_graph_data(input$day2, input_type = "noncvs", input$starting2, input$masssites2, input_search = "", updated_tweets())
      
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
           if(nrow(thedata() > 0)) {
           thedata() %>%
            dplyr::mutate(
                Hour = stringr::str_pad(Hour, 2, pad = 0)
            ) %>%
            dplyr::group_by(Hour) %>%
            dplyr::tally() %>%
            echarts4r::e_chart(Hour) %>%
            echarts4r::e_bar(n) %>%
            echarts4r::e_legend(show = FALSE) %>%
               e_tooltip(formatter = htmlwidgets::JS("
      function(params){
        return('Hour starting ' + params.value[0] + ':00<br />' + params.value[1] + ' total tweets' )
      }
    ")
               )
           } else {
             NULL
           }
        
    })
    
    output$myheadline2 <- renderUI({
      h2(theheadlinetext2(), align = 'center')
    })
    
    output$totals_graph <- renderEcharts4r({
      if(nrow(daily_totals()) > 0) {
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
      } else {
        NULL
      } 
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
