library('shiny')
library('dplyr')
library('ggplot2')
library('scales')
library('DT')

epims <-
  read.csv("epims.csv") # time series of people employed in industry
vie <-
  read.csv("vie.csv", na.strings = "-", row.names = 1) # vacancy by industry and education
vie[is.na(vie)] <- 0
vio <-
  read.csv("vio.csv", na.strings = "-", row.names = 1) # vacancy by industry and occupation
vio[is.na(vio)] <- 0
voe <-
  read.csv("voe.csv", na.strings = "-", row.names = 1) # vacancy by occupation and education
voe[is.na(voe)] <- 0

epimsnames = c(
  'Year' = 'Year',
  'Agriculture, Foresty & Fishing' = 'Agriculture_Foresty_Fishing',
  'Mining & Quarrying' = 'Mining_Quarrying',
  'Manufacturing' = 'Manufacturing',
  'Electricity, Gas, Steam & AirCon' = 'Electricity_Gas_Steam_Aircon',
  'Water Supply, Sewerage, Waste Management & Remediation Activity' = 'Water_Supply_Sewerage_Waste_Management_Remediation_Activity',
  'Construction' = 'Construction',
  'Wholesale, Retail Trade & Repair of Vehicle' = 'Wholesale_Retail_Trade_Repair_of_Vehicle',
  'Transportation & Storage' = 'Transportation_Storage',
  'Accomodation & Food Service' = 'Accommodation_Food_Service',
  'Information & Communication' = 'Information_Communication',
  'Financial, Insurace & Takaful' = 'Financial_Insurance_Takaful',
  'Real Estate' = 'Real_Estate',
  'Professional, Scientific & Technical' = 'Professional_Scientific_Technical',
  'Administrative & Support Services' = 'Administrative_Support_Services',
  'Public Administration & Defense, Social Security' = 'Public_Administration_Defense_Social_Security',
  'Education' = 'Education',
  'Health & Social Work' = 'Health_Social_Work',
  'Arts, Entertainment & Recreaction' = 'Arts_Entertainment_Recreation',
  'Other Services' = 'Other_Services',
  'Households as Employers' = 'Households_as_Employers'
)

names(epims) <- epimsnames

education_names <- c(
  "Doctoral (PhD)",
  "Master's or equivalent",
  "Bachelor's or equivalent",
  "Diploma's or equivalent",
  "STPM (A-Level) or equivalent",
  "SPM (O-Level) or equivalent",
  "PMR/ PT3 or equivalent",
  "Primary Education or below"
)

colnames(vie) <- education_names

col_padding = "5%"

ui <- fluidPage(
  title = 'WQD7001 Group Project',
  column(
    6,
    fluidRow(headerPanel(h1(
      "Employment Trend in Industry"
    ))),
    fluidRow(selectInput("Industry1", "Industry:", epimsnames[-1])),
    fluidRow(plotOutput("plotEmployed")),
    fluidRow(headerPanel(h1(
      "Education Statistics 2021"
    ))),
    fluidRow(
      selectInput("Education", "Education Level:", colnames(vie)[-length(colnames(vie))]),
      fluidRow(textOutput("education")),
      fluidRow(div(tableOutput("tableVIE")), style = 'overflow-x: scroll')
      
      
    ),
    align = "center", style = paste("padding-left: ", col_padding, "; padding-right: ", col_padding, ";", sep="")
  ),
  
  column(
    6,
    fluidRow(headerPanel(h1(
      "Industry Statistics 2021"
    ))),
    fluidRow(selectInput("Industry2", "Industry:", rownames(vie)[-length(rownames(vie))])),
    fluidRow(selectInput(
      "Occupation", "Occupation:", rownames(voe)[-length(rownames(voe))]
    )),
    fluidRow(textOutput("vacancies")),
    fluidRow(textOutput("occupation")),
    fluidRow(div(tableOutput("tableVIO")), style = 'overflow-x: scroll'),
    fluidRow(plotOutput("plotVacancies")),
    fluidRow(textOutput("estimation")),
    align = "center", style = paste("padding-left: ", col_padding, "; padding-right: ", col_padding, ";", sep="")
  )
)

server <- function(input, output) {
  # Plotting the trend of employed people by industry, eg construction, embedded in a function with polynomial
  # regression poly(x,2)
  
  plotemployed = function(a) {
    input = a
    epimsnona = epims[!is.na(epims[[a]]), ]
    return(
      ggplot(epimsnona, (aes_string('Year', input))) + geom_point() +
        geom_smooth(
          method = 'lm',
          formula = y ~ poly(x, 2),
          se = F,
          col = "steelblue"
        ) +
        labs(
          title = sprintf('Time Series of People Employed in %s', names(epimsnames)[which(epimsnames == a)]),
          y = 'Number Employed \'000',
          subtitle = 'Polynomial Model'
        ) +
        scale_x_continuous(labels = as.integer) +
        theme_bw() +
        theme(
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)
        )
    )
    
  }
  
  plotbar = function(a) {
    category = colnames(vie)[-length(colnames(vie))]
    ind = rownames(vie)[-length(rownames(vie))]
    index = which(ind == a)
    catdata = as.numeric(vie[index,-ncol(vie)])
    bardata = data.frame(category, catdata)
    return(
      ggplot(bardata, (aes(
        x = reorder(category, catdata), y = catdata
      ))) +
        geom_bar(stat = "identity", fill = "steelblue") +
        geom_text(aes(label = catdata), nudge_y = 5, size = 3) +
        coord_flip() +
        theme_bw() +
        labs(
          title = sprintf('Education requirement of vacancies in the %s industry', a),
          x = 'Education Type',
          y = 'Number of Vacancy'
        ) +
        theme(axis.text.y = element_text(size = 7)) +
        theme(plot.title = element_text(hjust = 0.5))
    )
  }
  
  
  output$education <-
    renderText(paste(
      "Table of Vacancies Requiring",
      input$Education,
      "in All Industries"
    ))
  
  output$plotEmployed <- renderPlot(plotemployed(input$Industry1))
  
  output$tableVIE <-
    renderTable(
      t(
        data.frame(
          Industry = rownames(vie)[-nrow(vie)],
          Number = as.integer(vie[-nrow(vie), input$Education]),
          Percent = percent(sapply(vie[-nrow(vie), input$Education], function(z) {
            z / sum(as.numeric(vie[-nrow(vie), input$Education]))
          }), accuracy = 0.01)
        )
      ),
      rownames = TRUE,
      colnames = FALSE,
      bordered = TRUE,
      align = "c"
    )
  
  output$vacancies <-
    renderText(paste("The total number of vacancies in", input$Industry2, "is", vio[input$Industry2, ncol(vio)]))
  
  output$occupation <-
    renderText(paste("Table of Vacancies for", input$Industry2, "by Occupation."))
  
  output$tableVIO <-
    renderTable(
      t(
        data.frame(
          Occupation = rownames(voe)[-nrow(voe)],
          Number = as.integer(vio[input$Industry2,-ncol(vio)]),
          Percent = percent(sapply(vio[input$Industry2,-ncol(vio)], function(z) {
            z / sum(as.numeric(vio[input$Industry2,-ncol(vio)]))
          }), accuracy = 0.01)
        )
      ),
      rownames = TRUE,
      colnames = FALSE,
      bordered = TRUE,
      align = "c"
    )
  
  output$plotVacancies <- renderPlot(plotbar(input$Industry2))
  
  output$estimation <-
    renderText(
      paste(
        "In",
        input$Industry2,
        "for",
        input$Occupation,
        "it is estimated that at least",
        round(max(voe[input$Occupation,-ncol(voe)]) / voe[input$Occupation, ncol(voe)] * vio[input$Industry2, input$Occupation]),
        "vacancies require a",
        colnames(voe)[which(voe[input$Occupation,-ncol(voe)] == max(voe[input$Occupation,-ncol(voe)]))],
        "at a minimum which is",
        percent(max(voe[input$Occupation,-ncol(voe)]) / voe[input$Occupation, ncol(voe)], accuracy = 0.01),
        "of the total number of vacancies."
      )
    )
}

# Run the application
shinyApp(ui = ui, server = server)
