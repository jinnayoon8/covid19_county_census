library(shiny)
library(tidyverse)

#load in data
covid19 = read_csv(file = "data/covid19.csv")

ui = navbarPage(
  title = "COVID-19",
  tabPanel(
    title = "Input/Visualization",
    titlePanel(title = "US COVID-19 Outbreak Over 7 Days"),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "state", 
          label = "State:", 
          choices = sort(unique(covid19$state)), 
          selected = "Illinois"
        ),
        selectInput(
          inputId = "county",
          label = "County:",
          choices = sort(unique(covid19$county)),
          selected = "DuPage County"
        ),
        selectInput(
          inputId = "hs_area",
          label = "Health Service Area:",
          choices = sort(unique(covid19$health_service_area)),
        ),
        checkboxInput(inputId = "place",
                      label = "Filter Table to County",
                      value = FALSE)
      ),
      mainPanel(
        plotOutput("plot")
      )
    )
  ),
  tabPanel(title = "Table", dataTableOutput("table")),
  tabPanel(title = "About", includeMarkdown("about.Rmd"))
)

server <- function(input, output) {
  #creating reactive elements
  covid19_state = reactive({
    covid19 %>%
      filter(state == input$state)
      
  })
  
    observeEvent(eventExpr = input$state,
                 handlerExpr = {
                   updateSelectInput(inputId = "county", 
                                     choices = sort(unique(covid19_state()$county)),
                                     selected = sort(unique(covid19_state()$county))[1])
                 }
                 )
    covid19_state_county = reactive({
      covid19_state() %>%
        filter(county == input$county)
    })
    
    observeEvent(
      eventExpr = input$county,
      handlerExpr = {
        updateSelectInput(inputId = "hs_area",
                          choices = sort(unique(covid19_state_county()$health_service_area)),
                          selected = sort(unique(covid19_state_county()$health_service_area))[1])
      }
    )


    output$plot <- renderPlot({
      covid19 %>%
        filter(state == input$state) %>%
        filter(county == input$county) %>%
        filter(health_service_area == input$hs_area) %>%
        ggplot(aes(x=`Covid19 Hospital Admissions (per 100k)`, y = `Covid Cases (per 100k)`, color = `Covid19 Level`)) +
        geom_point(size = 3) +
        theme_bw() 
    })
    
    output$table = renderDataTable({
      tab = covid19_state() %>%
        calc_hosp()
      
      if(input$place){
        tab = tab %>%
          filter(county == input$county)
      }
      
      tab
    })
  
}

  



# Run the application 
shinyApp(ui = ui, server = server)
