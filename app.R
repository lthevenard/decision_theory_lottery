library(shiny)
library(scales)
library(bslib)
library(tidyverse)
library(plotly)
library(DT)
source("resources.R")


ui <- fluidPage(
    theme = bs_theme(bootswatch = "sandstone"),
    titlePanel("Expected value of a lottery: a simple simulation"),
    
    sidebarLayout(
        sidebarPanel(
            h3("Lottery 1"),
            textInput("values_1", "Payoffs", value = "20; 40; 0"),
            textInput("percentages_1", "Percentages", value = "0.25; 0.25; 0.5"),
            h3("Lottery 2"),
            textInput("values_2", "Payoffs", value = "120; 140; -100"),
            textInput("percentages_2", "Percentages", value = "0.25; 0.25; 0.5"),
            h3("Simulations"),
            sliderInput("simulation_n", "Number of simulations", min = 100, max = 1000, value = 500, step = 50),
            actionButton("go","Simulate")
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("About",
                         htmlOutput("about")),
                tabPanel("Lottery 1",
                         htmlOutput("disclaimer_1"),
                         htmlOutput("lottery_description_1"),
                         plotlyOutput("theoretical_dist_plot_1"),
                         htmlOutput("lottery_summary_1"),
                         plotlyOutput("expected_value_plot_1"),
                         htmlOutput("table_1_header"),
                         dataTableOutput("simulation_table_1"),
                         br(), br()),
                tabPanel("Lottery 2",
                         htmlOutput("disclaimer_2"),
                         htmlOutput("lottery_description_2"),
                         plotlyOutput("theoretical_dist_plot_2"),
                         htmlOutput("lottery_summary_2"),
                         plotlyOutput("expected_value_plot_2"),
                         htmlOutput("table_2_header"),
                         dataTableOutput("simulation_table_2"),
                         br(), br()),
                tabPanel("Simulation Comparisons",
                         htmlOutput("disclaimer_3"),
                         br(), 
                         sliderInput("tickets_n", "Number of tickets (N)", min = 1, max = 1000, 
                                     value = 10, step = 10, animate = TRUE), 
                         br(),
                         plotlyOutput("percentage_distribution_plot"),
                         br(),
                         plotlyOutput("mean_returns_plot_1"),
                         br(),
                         plotlyOutput("mean_returns_plot_2"),
                         br()),
                tabPanel("Instructions",
                         htmlOutput("instructions"))
            )
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    output$about <- renderUI({HTML(paste0(about, instructions))})
    output$instructions <- renderUI({HTML(instructions)})
    output$disclaimer_1 <- renderUI({HTML(disclaimer)})
    output$disclaimer_2 <- renderUI({HTML(disclaimer)})
    output$disclaimer_3 <- renderUI({HTML(disclaimer)})
    # Lottery 1
    describe_1 <- eventReactive(input$go,{describe_lottery(input$values_1, input$percentages_1)})
    output$lottery_description_1 <- renderUI({HTML(describe_1()$descriptions)})
    output$theoretical_dist_plot_1 <- renderPlotly({
        describe_1() %>% 
            plot_theoretical_distribution()
    })
    output$lottery_summary_1 <- renderUI({
        HTML(describe_1() %>% summarise_lottery())
    })
    output$expected_value_plot_1 <- renderPlotly({
        describe_1() %>% 
            plot_expected_value()
    })
    table_1_header <- eventReactive(input$go,{
        h2("Simulation Table - Lottery 1")
    })
    output$table_1_header <- renderUI({
        table_1_header()
    })
    get_sim_table_1_react <- reactive({
        description_list <- describe_1()
        get_sim_table(input$simulation_n, 
                      description_list$cum_percentages, 
                      description_list$result_names, 
                      description_list$values, 
                      description_list$expected_value)
    })
    output$simulation_table_1 <- renderDataTable({
        get_sim_table_1_react()  %>% 
            datatable(rownames= FALSE) %>% 
            formatStyle(
                columns = c("Returns", "Mean Returns", "Profit"), 
                color = styleInterval(cuts = -0.0001, values = c("red", "green"))
            )
    })
    # Lottery 2
    describe_2 <- eventReactive(input$go,{describe_lottery(input$values_2, input$percentages_2)})
    output$lottery_description_2 <- renderUI({HTML(describe_2()$descriptions)})
    output$theoretical_dist_plot_2 <- renderPlotly({
        describe_2() %>% 
            plot_theoretical_distribution()
    })
    output$lottery_summary_2 <- renderUI({
        HTML(describe_2() %>% summarise_lottery())
    })
    output$expected_value_plot_2 <- renderPlotly({
        describe_2() %>% 
            plot_expected_value()
    })
    table_2_header <- eventReactive(input$go,{
        h2("Simulation Table - Lottery 2")
    })
    output$table_2_header <- renderUI({
        table_2_header()
    })
    get_sim_table_2_react <- reactive({
        description_list <- describe_2()
        get_sim_table(input$simulation_n, 
                      description_list$cum_percentages, 
                      description_list$result_names, 
                      description_list$values, 
                      description_list$expected_value)
    })
    output$simulation_table_2 <- renderDataTable({
        get_sim_table_2_react() %>%
            datatable(rownames= FALSE) %>% 
            formatStyle(
                columns = c("Returns", "Mean Returns", "Profit"), 
                color = styleInterval(cuts = -0.0001, values = c("red", "green"))
            )
    })
    # Simulation slider
    observeEvent({input$simulation_n},{
            updateSliderInput(session,"tickets_n", max = input$simulation_n)
        })
    output$percentage_distribution_plot <- renderPlotly({
        tickets <- input$tickets_n
        table_1 <- get_sim_table_1_react() %>%
            filter(Tickets == tickets) %>% 
            select(!all_of(c("Tickets", "Returns", "Mean Returns", "Profit")))
        table_2 <- get_sim_table_2_react() %>%
            filter(Tickets == tickets) %>% 
            select(!all_of(c("Tickets", "Returns", "Mean Returns", "Profit")))
        plot_percentage_comparison(table_1, table_2, tickets)
    })
    output$mean_returns_plot_1 <- renderPlotly({
        get_sim_table_1_react() %>% 
            plot_dot_mean_returns("Lottery 1", describe_1()$expected_value)
    })
    output$mean_returns_plot_2 <- renderPlotly({
        get_sim_table_2_react() %>% 
            plot_dot_mean_returns("Lottery 2", describe_2()$expected_value)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)






