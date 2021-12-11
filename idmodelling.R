#Modeling Infectious Disease Dynamics

options(warn = -1)
#load the required packages and functions saved in separated file
source('load_packages.R')
source('simulation_functions.R')

#load the rmarkdown (.Rmd) file and knit it to a markdown file (.md)
rmdfiles <- c("README.Rmd")
sapply(rmdfiles, knit, quiet = T)

################################################################################

#THE USER INTERFACE (UI)

ui<-dashboardPage(
  dashboardHeader(title="Simulating Infectious Diseases Dynamics"),
  
  dashboardSidebar(
    hr(),
    
    #create the sidebar menu items
    sidebarMenu(id = "menu",
                menuItem("Home", tabName = "home", icon = icon("home")),
                menuItem("Simulate Disease Dynamics", tabName = "exp-model", icon = icon("chart-line"), selected = TRUE),
                menuItem("Analyze Intervention Effect", tabName = "intervention", icon = icon("capsules")),
                menuItem("About", tabName = "readme", icon = icon("book"))
                
    ),
    
    #populate the sidebars based on the tab clicked by user
    
    #create the sidebar to be displayed when the tab clicked is `exp-model`
    conditionalPanel(condition = 'input.menu == "exp-model"',
                     actionButton("go", "Run Model Simulation"),      
                     selectInput("model",
                                 "Select a model to simulate:",
                                 list(SI = "SI_model",
                                      SIR = "SIR_model",
                                      SEIR = "SEIR_model")),
                     
                    #create the side bar to be displayed under the `exp-model` tab when the selected model is an `SEIR-model`
                     conditionalPanel(condition = "input.model == 'SEIR_model'",
                                      
                                      splitLayout(cellWidths = c("50%", "50%"),
                                                  numericInput(inputId = "beta", label = "Infection Rate", value = 0.8, min = 0, max = 50),
                                                  numericInput(inputId = "gamma", label = "Recovery parameter", value = 0.3, min = 0)),
                                      
                                      splitLayout(cellWidths = c("50%", "50%"),
                                                  numericInput(inputId = "sigma", label = "Rate from exposure to infection", value = 1/3, min = 0),
                                                  numericInput(inputId = "S", label = "Susceptible population (S)", value = 1-1e-6, min = 0)),
                                      
                                      splitLayout(cellWidths = c("50%", "50%"),
                                                  numericInput(inputId = "E", label = "Exposed population (E)", value = 0, min = 0),
                                                  numericInput(inputId ="I", label = "Number infected (I)", value = 1e-5, min = 0)),
                                      
                                      sliderInput("R", 
                                                  "The number recovered (R):",
                                                  value = 0,
                                                  min = 0, 
                                                  max = 50,
                                                  step = 0.1),
                                      numericInput(inputId = "tfinal", label = "Final simulation time (in days):", value = 100, min = 0, max = 730)) #end of inputs for SEIR
                    
                    
                     ),
    
    #create the sidebar to be displayed when the tab clicked is `intervention`
    conditionalPanel(condition = 'input.menu == "intervention"',
                     actionButton("go", "Run Model Simulation")
                     
                     #all inputs to enable plotting the intervention effect go here
                     ),
    hr(),
    helpText("Developed by: ",
             br(),
             a("Amos Okutse", href = "www.brown.edu"),
             br(),
             a("Yingjie Zhou", href= "www.brown.edu"),
             br(),
             a("Kyla Finlayson", href = "www.brown.edu"),
             br(),
             "Powered by:",
             br(),
             a("Shiny from RStudio", href = "https://shiny.rstudio.com/"),
             style = "padding-left:1em; padding-right:1em;position:absolute;")
    
  ),
  
  dashboardBody(
    tabItems(
      
      #populate the body of the `exp-model` tab with the plots and table of simulated data
      tabItem(tabName = "exp-model",
              fluidPage(
                
                fluidRow(
                  tabBox(
                    width = 12,
                    height = "800px",
                    title = "Explore Model Dynamics", 
                    side = "right",
                    tabPanel(title = "Trajectories",
                             plotlyOutput("plot", width = "100%", height = "650px") %>% #650px"
                               withSpinner(),
                             switchInput("facet_model",
                                         label = "Plot animated disease dynamics", 
                                         value = TRUE, inline = TRUE, width = "auto"))
                  )),
                fluidRow(
                  tabBox(
                    width = 12,
                    title = "Simulated Model Data", 
                    side = "right",
                    tabPanel(title = "Data",
                             DT::dataTableOutput("simulation_results") %>% 
                               withSpinner()),
                    tabPanel(title = "Download Data", 
                             downloadLink("downloadData", "Download"))
                  )
                )
              )
      ),
      
      #populating the body panel of the `intervention` tab
      tabItem(tabName = "intervention",
              fluidPage(
        fluidRow(
          tabBox(
          width = 12,
          height = "800px",
          title = "Explore the effect of interventions on recorded infection cases", 
          side = "right",
          tabPanel(title = "Intervention Trajectories",
                   plotlyOutput("interventionplot", width = "100%", height = "650px") %>% 
                     withSpinner()
          ))
      ))),
      

      tabItem(tabName = "readme",
              withMathJax(includeMarkdown("README.md"))),
             
      
      #here is where details on our homepage go...we'll think about what to put here 
      #later!! any ideas??
      tabItem(tabName = "home",
              fluidRow(
                h1("We need to make this appealing and set it \n as the homepage of our app with 
                   appealing imagery, information and so forth.")
              ))
)
),
skin = "purple"
)

################################################################################
#THE SERVER AREA

server <- function(input, output) {
  
 #setting up the SEIR model if it is selected by user
  observeEvent(input$go, {
    if(input$model %in% "SEIR_model"){
      output$plot<-renderPlotly({
        
        #use the seir_model to return the estimated simulation results.return the data frame object
        df<-seir_model(S=input$S, E=input$E, I=input$I, R=input$R, beta = input$beta, 
                       gamma = input$gamma, sigma = input$sigma, tfinal = input$tfinal)
        
        #now create the plot to be rendered on the UI
        plot_ly(data.frame(df), x = ~df[,1], y = ~df[,2], name = 'S', type = 'scatter', mode = 'lines', color=I("blue")) %>%
          add_trace(y = ~df[,3], name = 'E', mode = 'lines', color=I("orange")) %>%
          add_trace(y = ~df[,4], name = 'I', mode = 'lines', color=I("red")) %>%
          add_trace(y = ~df[,5], name = 'R', mode = 'lines', color=I("purple")) %>%
          layout(xaxis = list(title = "Simulation Period in Days"), yaxis=list(title = "Total Susceptible Population"))
      
      })          #closes the renderPlotly object for the plot of SEIR
      
      #add the simulated data table for the SEIR infectious disease dynamics
     # observeEvent(input$go, {
        output$simulation_results<-DT::renderDataTable({
          results<-seir_model(S=input$S, E=input$E, I=input$I, R=input$R, beta = input$beta, 
                              gamma = input$gamma, sigma = input$sigma, tfinal = input$tfinal)
          
          #export the table produced by the `seir_model()` function as a datatable using DT
          results <- DT::datatable(results)
        })
        
        # add the simulated data to be downloaded by the user if needed
        datasetinput=reactive({seir_model(S=input$S, E=input$E, I=input$I, R=input$R, beta = input$beta, 
                                               gamma = input$gamma, sigma = input$sigma, tfinal = input$tfinal)})
        output$downloadData<-downloadHandler(filename = function(){paste("simulated_data-", Sys.Date(), ".csv", sep = "")},
                                             content = function(file){write.csv(datasetinput(), file, row.names=FALSE)})
        
        
      
        
      
    }             #closes the if statement for the seir model (an else could go here for the SI and SIR models)
  })              #closes the observeEvent() area. This is observed only once for all sets of models and recalculated based on model selected
  
  
  
}                 #closes the server function

# Run the application 
shinyApp(ui = ui, server = server)
