library(shiny)
library(dplyr)
library(tidyverse)
library(shinyWidgets)

setwd("~/landres@bu.edu - Google Drive/Shared drives/SABER_Talk")

#drop down button function, custom made to reduce clutter in my server
dropdownButton <- function(label = "", status = c("default", "primary", "success", "info", "warning", "danger"), ..., width = NULL) {
  
  status <- match.arg(status)
  # dropdown button content
  html_ul <- list(
    class = "dropdown-menu",
    style = if (!is.null(width)) 
      paste0("width: ", validateCssUnit(width), ";"),
    lapply(X = list(...), FUN = tags$li, style = "margin-left: 10px; margin-right: 10px;")
  )
  # dropdown button apparence
  html_button <- list(
    class = paste0("btn btn-", status," dropdown-toggle"),
    type = "button", 
    `data-toggle` = "dropdown"
  )
  html_button <- c(html_button, list(label))
  html_button <- c(html_button, list(tags$span(class = "caret")))
  # final result
  tags$div(
    class = "dropdown",
    do.call(tags$button, html_button),
    do.call(tags$ul, html_ul),
    tags$script(
      "$('.dropdown-menu').click(function(e) {
      e.stopPropagation();
});")
  )
}

df <- read_csv("pbp1023TempoOnly.csv")
# view(df)
#df <- df1#[sample(x=nrow(df1), size=200, replace = F), ]

#NOTES TO DO
#TO DO TO DO TO DO TO DO TO DO
#for each pulldown menu, make the menu items all in order, 
#consider showing only tempo pitches, currently showing that
#make it comparable to sevant in terms of sevant values
ui <- fluidPage(
  
  titlePanel("Server"),
  
  #row 1
  fluidRow(
    #date, used date range input
    column(6, dateRangeInput('date', label = 'Date range input: '),
                          start = min(df$game_date), end=max(df$game_date)),
    #season, used checkbox group
    column(3,
           dropdownButton("Season:", status="default", 
                          width=80,checkboxGroupInput(
                            inputId = "season",
                            "Select a Season",
                            choices = unique(df$season),
                            selected = unique(df$season)
                          ), actionButton('all2', 'Select All/Deselect All'))),
    #season, used checkbox group
    column(3,
           dropdownButton("Pitch Event Result:", status="default", 
                          width=80,checkboxGroupInput(
                            inputId = "eventResult",
                            "Select a Pitch Event Result",
                            choices = unique(df$details.call.description),
                            selected = unique(df$details.call.description)
                          ), actionButton('all11', 'Select All/Deselect All'))),
  ),
  
  
  #row 2
  fluidRow(
    #game type
    column(3, 
           dropdownButton("Game Type:", status="default", 
                          width=80,checkboxGroupInput(
                            inputId = "YearSelect",
                            "Select Game Type",
                            choices = unique(df$seriesDescription),
                            selected = unique(df$seriesDescription)
                          ), actionButton('all3', 'Select All/Deselect All'))),
    #count
    column(3, 
           dropdownButton("Count:", status="default", 
                          width=80,checkboxGroupInput(
                            inputId = "count",
                            "Select Count",
                            choices = unique(df$countFull),
                            selected = unique(df$countFull)
                          ), actionButton('all4', 'Select All/Deselect All'))),
    
    #bats
    column(3,
           dropdownButton("Bats:", status="default", 
                          width=80, checkboxGroupInput(
                            inputId = "bats",
                            "Select Batter Handedness",
                            choices = unique(df$matchup.batSide.code),
                            selected = unique(df$matchup.batSide.code)
                          ), actionButton('all5', 'Select All/Deselect All'))),
    
    #basepath
    column(3, 
           dropdownButton("Basepath:", status="default", 
                          width=80, checkboxGroupInput(
                            inputId = "basepath",
                            "Select Basepath",
                            choices = unique(df$matchup.splits.menOnBase),
                            selected = unique(df$matchup.splits.menOnBase)
                          ), actionButton('all6', 'Select All/Deselect All')))),
  
  
  #row 3
  fluidRow(
    #ballpark
    column(3,
           dropdownButton("Ballpark:", status="default", 
                          width=80, checkboxGroupInput(
                            inputId = "park",
                            "Select Home Ballpark",
                            choices = unique(df$home_team),
                            selected = unique(df$home_team)
                          ), actionButton('all7', 'Select All/Deselect All'))),
    
    #outs
    column(3, dropdownButton("Outs:", status="default",
                             width=80,
                             checkboxGroupInput(
                               inputId = "outs",
                               "Select Count",
                               choices = unique(df$count.outs.start),
                               selected = unique(df$count.outs.start)
                             ), actionButton('all8', 'Select All/Deselect All'))),
    
    #throws
    column(3, dropdownButton("Throws:", status="default", 
                             width=80, checkboxGroupInput(
                               inputId = "throw",
                               "Select Pitcher Throws",
                               choices = unique(df$matchup.pitchHand.code),
                               selected = unique(df$matchup.pitchHand.code)
                             ), actionButton('all9', 'Select All/Deselect All'))),
    
    #inning
    column(3, dropdownButton("Inning:", status="default", 
                             width=80, checkboxGroupInput(
                               inputId = "inn",
                               "Select Inning",
                               choices = unique(df$about.inning),
                               selected = unique(df$about.inning)
                             ), actionButton('all10', 'Select All/Deselect All')))),
  
  #downloads the data
  downloadButton('download',"Download your custom play-by-play table: (warning could be a large file)"),
  #table 2
  #do stats change
  #this references the data outputted as a table in the server
  dataTableOutput("data_decision"),
  fluidRow(
    tableOutput("df"),
    h3("Median Tempo and Number of Pitches:"),
    radioButtons(
      inputId = "vetaDataView2",
      label = NULL,
      choiceNames = c("By Pitcher", "By League", "By Team"),
      choiceValues = c("matchup.pitcher.fullName", "pitcherLeague", "pitcherTeam"),
      selected = "pitcherLeague",
      inline = TRUE
    ),
    downloadButton('download1',"Download your custom tempo table:"),
    #table output for pitch by pitch data
    tableOutput("totals")
  )
)

server <- function(input, output, session) {
  #this enables filters referenced above in each input
  #reactive input: filter(dfColumn %in% input$inputID)
  TestFilter <- reactive({
    df %>% filter(season %in% input$season & details.call.description %in% input$eventResult & seriesDescription %in% input$YearSelect
                  & countFull %in% input$count & matchup.batSide.code %in% input$bats & matchup.splits.menOnBase %in% input$basepath 
                  & home_team %in% input$park & matchup.pitchHand.code %in% input$throw & about.inning %in% input$inn & count.outs.start %in% input$outs
                  & (game_date >= input$date[1] & game_date <= input$date[2]))
  })
  
  #each of these observe events represents an update to all events
  observe({if(input$all10 == 0){
    updateCheckboxGroupInput(
      session, 'inn', choices = sort(unique(df$about.inning)),
      selected = unique(df$about.inning))
  }
    else if (input$all10 %% 2 == 0){
      updateCheckboxGroupInput(
        session, 'inn', choices = sort(unique(df$about.inning)))
    }
    else{
      updateCheckboxGroupInput(
        session, 'inn', choices = sort(unique(df$about.inning)),
        selected = unique(df$about.inning))
    }
  })
  observe({if(input$all11 == 0){updateCheckboxGroupInput(session, 'eventResult', choices = sort(unique(df$details.call.description)), selected = unique(df$details.call.description))}
    else if (input$all11 %% 2 == 0){updateCheckboxGroupInput(session, 'eventResult', choices = sort(unique(df$details.call.description))) }
    else{updateCheckboxGroupInput(session, 'eventResult', choices = sort(unique(df$details.call.description)), selected = unique(df$details.call.description))}})
  
  observe({if(input$all2 == 0){updateCheckboxGroupInput(session, 'season', choices = sort(unique(df$season)), selected = unique(df$season))}
    else if (input$all2 %% 2 == 0){updateCheckboxGroupInput(session, 'season', choices = sort(unique(df$season)))}
    else{updateCheckboxGroupInput(session, 'season', choices = sort(unique(df$season)), selected = unique(df$season))}})
  
  observe({if(input$all3 == 0){updateCheckboxGroupInput(session, 'YearSelect', choices = sort(unique(df$seriesDescription)), selected = unique(df$seriesDescription))}
    else if (input$all3 %% 2 == 0){updateCheckboxGroupInput(session, 'YearSelect', choices = sort(unique(df$seriesDescription)))}
    else{updateCheckboxGroupInput(session, 'YearSelect', choices = sort(unique(df$seriesDescription)), selected = unique(df$seriesDescription))}})
  
  observe({if(input$all4 == 0){updateCheckboxGroupInput(session, 'count', choices = sort(unique(df$countFull)), selected = unique(df$countFull))}
    else if (input$all4 %% 2 == 0){updateCheckboxGroupInput(session, 'count', choices = sort(unique(df$countFull)))}
    else{updateCheckboxGroupInput(session, 'count', choices = sort(unique(df$countFull)), selected = unique(df$countFull))}})
  
  observe({if(input$all5 == 0){updateCheckboxGroupInput(session, 'bats', choices = sort(unique(df$matchup.batSide.code)), selected = unique(df$matchup.batSide.code))}
    else if (input$all5 %% 2 == 0){updateCheckboxGroupInput(session, 'bats', choices = sort(unique(df$matchup.batSide.code)))}
    else{updateCheckboxGroupInput(session, 'bats', choices = sort(unique(df$matchup.batSide.code)), selected = unique(df$matchup.batSide.code))}})
  
  observe({if(input$all6 == 0){updateCheckboxGroupInput(session, 'basepath', choices = sort(unique(df$matchup.splits.menOnBase)), selected = unique(df$matchup.splits.menOnBase))}
    else if (input$all6 %% 2 == 0){updateCheckboxGroupInput(session, 'basepath', choices = sort(unique(df$matchup.splits.menOnBase)))}
    else{updateCheckboxGroupInput(session, 'basepath', choices = sort(unique(df$matchup.splits.menOnBase)), selected = unique(df$matchup.splits.menOnBase))}})
  
  observe({if(input$all7 == 0){updateCheckboxGroupInput(session, 'park', choices = sort(unique(df$home_team)), selected = unique(df$home_team))}
    else if (input$all7 %% 2 == 0){updateCheckboxGroupInput(session, 'park', choices = sort(unique(df$home_team)))}
    else{updateCheckboxGroupInput(session, 'park', choices = sort(unique(df$home_team)), selected = unique(df$home_team))}})
  
  observe({if(input$all8 == 0){updateCheckboxGroupInput(session, 'outs', choices = sort(unique(df$count.outs.start)), selected = unique(df$count.outs.start))}
    else if (input$all8 %% 2 == 0){updateCheckboxGroupInput(session, 'outs', choices = sort(unique(df$count.outs.start)))}
    else{updateCheckboxGroupInput(session, 'outs', choices = sort(unique(df$count.outs.start)), selected = unique(df$count.outs.start))}})
  
  observe({if(input$all9 == 0){updateCheckboxGroupInput(session, 'throw', choices = sort(unique(df$matchup.pitchHand.code)), selected = unique(df$matchup.pitchHand.code))}
    else if (input$all9 %% 2 == 0){updateCheckboxGroupInput(session, 'throw', choices = sort(unique(df$matchup.pitchHand.code)))}
    else{updateCheckboxGroupInput(session, 'throw', choices = sort(unique(df$matchup.pitchHand.code)), selected = unique(df$matchup.pitchHand.code))}})
  
  observe({updateDateRangeInput(session, 'date', start=input$date[1], end=input$date[2])})
  
  #creates a function that calculates the median tempo given the filter
  medColA <- reactive({
    fmlaA <- as.formula(paste("tempo", input$vetaDataView2, sep = " ~ "))
    aggregate(fmlaA, TestFilter(), median)
  })
  #edit
  sumColB <- reactive({
    fmlaB <- as.formula(paste("tempo", input$vetaDataView2, sep = " ~ "))
    aggregate(fmlaB, TestFilter(), length)
  }) 
  
  #pulls a new summary table
  output$data <- renderTable(df)
  output$totals <- renderTable({
    totals <- as.data.frame(c(medColA(), sumColB()[2]))
    
    colnames(totals) <- c(input$vetaDataView2, "Median Tempo", "Number of Pitches")
    
    totals
  })
  
  #conditionally filter the date range
  DateFilter <- reactive({
    df %>% filter(game_date >= input$date[1] & game_date <= input$date[2])
  })
  #outputs the data given the filters
  output$data_decision = renderDataTable(
    req(TestFilter()),
    req(DateFilter())
  )
  
  #output code for download pbp data
  output$download <- downloadHandler(
    filename = function(){"dataPullPBP.csv"}, 
    content = function(fname){
      write.csv(TestFilter(), fname)
    }
  )
  #output code for download median tempo data
  output$download1 <- downloadHandler(
    filename = function(){"dataPullTempo.csv"}, 
    content = function(fname1){
      write.csv(as.data.frame(c(medColA(), sumColB()[2])), fname1)
    }
  )
}

shinyApp(ui, server)

