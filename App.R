library(shiny)
library(election)
library(ggplot2)

el <- election("Stockholm")
participation_rate_data <- el$participation_rate()
political_party_data <- el$political_party()
first_vote_data <- el$first_vote_data

parties <- c("Centre Party", "Christian Democrats", 
             "Sweden Democrats", "Green Party", 
             "Swedish Social Democratic Part", 
             "Liberals", "Left Party", 
             "Moderate Party", "Others")
for_kpi <- c()

for (i in 1:length(political_party_data$kpi)){
  if (political_party_data$kpi[i] == "N65841"){
    for_kpi <- c(for_kpi, parties[1])
  }
  else if (political_party_data$kpi[i] == "N65842"){
    for_kpi <- c(for_kpi, parties[2])
  }
  else if (political_party_data$kpi[i] == "N65843"){
    for_kpi <- c(for_kpi, parties[3])
  }
  else if (political_party_data$kpi[i] == "N65844"){
    for_kpi <- c(for_kpi, parties[4])
  }
  else if (political_party_data$kpi[i] == "N65845"){
    for_kpi <- c(for_kpi, parties[5])
  }
  else if (political_party_data$kpi[i] == "N65846"){
    for_kpi <- c(for_kpi, parties[6])
  }
  else if (political_party_data$kpi[i] == "N65847"){
    for_kpi <- c(for_kpi, parties[7])
  }
  else if (political_party_data$kpi[i] == "N65848"){
    for_kpi <- c(for_kpi, parties[8])
  }
  else if (political_party_data$kpi[i] == "N65849"){
    for_kpi <- c(for_kpi, parties[9])
  }
}
political_party_data$kpi <- for_kpi

political_party_data <- political_party_data[, -which(names(political_party_data) == "municipality")]

ui <- fluidPage(
  navbarPage("Election Shiny App",
             tabPanel("Datasets", fluidPage(
               title = "Examples of DataTables",
               sidebarLayout(
                 sidebarPanel(
                   conditionalPanel('input.dataset === "participation_rate_data"'
                   ),
                   conditionalPanel('input.dataset === "political_party_data"',
                                    helpText("Click the column header to sort a column.")
                   ),
                   conditionalPanel(
                     'input.dataset === "first_vote_data"',
                     helpText("Display 5 records by default.")
                   ),
                   virtualSelectInput(
                     inputId = "id",
                     selected = c("2022"),
                     label = "Select year:",
                     choices = list("Year" = c("1998", "2002", "2006", "2010", "2014", "2018", "2022")
                     ),
                     showValueAsTags = TRUE,
                     search = TRUE,
                     multiple = TRUE
                   )
                 ),
                 mainPanel(
                   tabsetPanel(
                     id = 'dataset',
                     tabPanel("Participation Rate", DT::dataTableOutput("table1")),
                     tabPanel("Political Party", DT::dataTableOutput("table2")),
                     tabPanel("First Vote", DT::dataTableOutput("table3"))
                   )
                 )
               )
             )),
             tabPanel("Graph", fluidPage(
               titlePanel(title = h4("Bar Chart for Political Party", align="center")),
               sidebarPanel(
                 virtualSelectInput(
                   inputId = "parties",
                   label = "Select parties:",
                   choices = list("Parties" = c(unique(political_party_data$kpi))),
                   selected = "Centre Party",
                   showValueAsTags = TRUE,
                   search = TRUE,
                   multiple = TRUE),
                 mainPanel(
                   plotOutput("bar", width = "1400px")
                 )
               )
             )
             ) 
  )
)

server <- function(input, output, session) {
  
  # choose columns to display
  output$table1 <- DT::renderDataTable({
    year_val <- reactive_data1()
    participation_rate_data <- participation_rate_data[participation_rate_data$period %in% year_val,]
    DT::datatable(participation_rate_data)
  })
  
  # sorted columns are colored now because CSS are attached to them
  output$table2 <- DT::renderDataTable({
    year_val <- reactive_data1()
    political_party_data <- political_party_data[political_party_data$period %in% year_val,]
    DT::datatable(political_party_data)
  })
  
  # customize the length drop-down menu; display 5 rows per page by default
  output$table3 <- DT::renderDataTable({
    year_val <- reactive_data1()
    first_vote_data <- first_vote_data[first_vote_data$period %in% year_val,]
    DT::datatable(first_vote_data)
  })
  
  reactive_data = reactive({
    selected_party = as.vector(input$parties)
    return(political_party_data[political_party_data$kpi %in% selected_party,])
  })
  
  reactive_data1 = reactive({
    selected_year = as.numeric(input$id)
    return(selected_year)
  })
  
  output$bar <- renderPlot({
    act_data <- reactive_data()
    act_data$period <- as.factor(unlist(act_data$period))
    act_data$values <- unlist(act_data$values)
    ggplot(act_data, aes(x = period, y = values, fill=kpi)) + 
      geom_bar(stat="identity", position = position_dodge()) + 
      scale_fill_brewer(palette="Paired") +
      labs(title="Plot of Election Results", 
           x="Years (2014-2018-2022)", y = "Results (%)")+
      theme_gray(base_size = 14) + 
      theme(text = element_text(size = 20)) +
      scale_y_continuous(limits = c(0, 30)) 
  })
}

shinyApp(ui, server)