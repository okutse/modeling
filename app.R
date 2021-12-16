#Modeling Infectious Disease Dynamics

options(warn = -1)
#load the required packages and functions saved in separate files
source('load_packages.R')
source('simulation_functions.R')

################################################################################

#THE USER INTERFACE (UI)

ui<-dashboardPage(
  dashboardHeader(title = h4(HTML("Simulating Infectious<br/>Diseases Dynamics"))),
  dashboardSidebar(
    hr(),
    
    #create the sidebar menu items
    sidebarMenu(id = "menu",
                menuItem("Home", tabName = "home", icon = icon("home"), selected = TRUE),
                menuItem("Simulate Disease Dynamics", tabName = "exp-model", icon = icon("chart-line")),
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
                                      numericInput(inputId = "tfinal", label = "Final simulation time (in days):", value = 100, min = 0, max = 730)), #end of inputs for SEIR
                    
                    #create the side bar to be displayed when the selected model is an `SIR model'
                    conditionalPanel(condition = "input.model == 'SIR_model'",
                                     
                                     splitLayout(cellWidths = c("50%", "50%"),
                                                 numericInput(inputId = "BETA", label = "Infection Rate", value = 0.8, min = 0, max = 1000),
                                                 numericInput(inputId = "GAMMA", label = "Recovery parameter", value = 0.3, min = 0)),
                                     
                                     splitLayout(cellWidths = c("50%", "50%"),
                                                 numericInput(inputId = "s", label = "Susceptible population (S)", value = 1-1e-6, min = 0),
                                                 numericInput(inputId ="i", label = "Number infected (I)", value = 1e-5, min = 0)),
                                     
                                     sliderInput("r", 
                                                 "The number recovered (R):",
                                                 value = 0,
                                                 min = 0, 
                                                 max = 50,
                                                 step = 0.1),
                                     numericInput(inputId = "tfinalx", label = "Final simulation time (in days):", value = 100, min = 0, max = 730)), 
                    
                    #create the side bar to be displayed under the `exp-model` tab when the selected model is an `SI-model`
                    conditionalPanel(condition = "input.model == 'SI_model'",
                                     
                                     numericInput(inputId = "betaa", label = "Infection Rate", value = 0.8, min = 0, max = 50),
                                     
                                     splitLayout(cellWidths = c("50%", "50%"),
                                                 numericInput(inputId = "SUS", label = "Susceptible population (S)", value = 1-1e-6, min = 0),
                                                 numericInput(inputId ="IN", label = "Number infected (I)", value = 1e-5, min = 0)),
                                     
                                     numericInput(inputId = "tfinall", label = "Final simulation time (in days):", value = 100, min = 0, max = 730)) 
    ),
                    
    
    #create the sidebar to be displayed when the tab clicked is `intervention`
    conditionalPanel(condition = 'input.menu == "intervention"',
                     actionButton("go2", "Explore Intervention Effect"),
                     actionBttn("go3", label = "Erase Plot"),
                     
                     #all inputs to enable plotting the intervention effect go here
                     splitLayout(cellWidths = c("50%", "50%"),
                                 numericInput("SS", label = "Susceptible population:", min = 0, value = 1-1e-6, max = 1e10),
                                 numericInput("II", label = "Number infected:", min=0, value = 1, max = 1e10)),
                     splitLayout(cellWidths = c("50%", "50%"),
                                 numericInput("f", label = "Fraction vaccinated:", min=0, value = 0, max = 1e10),
                                 numericInput("e", label = "Vaccine efficacy:", min=0, value = 0, max = 1e10)),
                     splitLayout(cellWidths = c("50%", "50%"),
                                 numericInput("b", label = "Infection rate:", min=0, value = 0.01, max = 1e10),
                                 numericInput("g", label = "Recovery rate:", min=0, value = 10, max = 1e10)),
                     splitLayout(cellWidths = c("50%", "50%"),
                                 numericInput("n", label = "Birth rate:", min=0, value = 0, max = 1e10),
                                 numericInput("m", label = "Death rate:", min=0, value = 0, max = 1e10)),
                     splitLayout(cellWidths = c("50%", "50%"),
                                 numericInput("w", label = "Waning immunity:", min=0, value = 1, max = 1e10),
                                 numericInput("tmax", label = "Simulation time", value = 100))
                    ), #end of intervention sidebar panel
    
    #create the sidebar for the home tab
    conditionalPanel(condition = 'input.menu == "home"'),
    
    hr(),
    helpText("Developed by: ",
             br(),
             a("Amos Okutse", href = "https://www.brown.edu/academics/public-health/biostats/people/students/current-masters-students"),
             br(),
             a("Yingjie (Gary) Zhou", href= "https://www.brown.edu/academics/public-health/biostats/people/students/current-masters-students"),
             br(),
             a("Kyla Finlayson", href = "https://www.brown.edu/academics/public-health/biostats/people/students/current-masters-students"),
             br(),
             "Powered by:",
             br(),
             a("Shiny from RStudio", href = "https://shiny.rstudio.com/"),
             style = "padding-left:1em; padding-right:1em;position:absolute;")
    
  ),
  
  #create the dashboard main body, which holds the plots, simulated tables, and simulation explanations
  dashboardBody(
    tabItems(
      
      #populate the body of the `exp-model` tab with the plots and table of simulated data
      tabItem(tabName = "exp-model",
              fluidPage(
                
                #Plots
                fluidRow(
                  tabBox(
                    width = 12,
                    height = "800px",
                    title = "Explore Model Dynamics", 
                    side = "right",
                    tabPanel(title = "Trajectories",
                             plotlyOutput("plot", width = "100%", height = "650px") %>%
                               withSpinner())
                  )),
                
                #Simulated Data
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
                
                #Plots
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
                ), 
                
                #Simulated Data
                fluidRow(
                  tabBox(
                    width = 12,
                    title = "Simulated Model Data", 
                    side = "right",
                    tabPanel(title = "Data",
                             DT::dataTableOutput("sim_results") %>% 
                               withSpinner()),
                    tabPanel(title = "Download Data", 
                             downloadLink("downloaddata", "Download"))
                    )
                  )      
                )),
      
      #populating the body panel of the "About" tab
      tabItem(tabName = "readme",
              h1(strong("Introduction"), style = "font-size:50px;"),
              br(),
              h1("As our guiding objective, in this project, we created an interactive, visual simulation of an SEIR model, SIR model, 
                 and an SI model, which can be used in understanding infectious disease trajectories e.g for COVID-19. The interactive portion of 
                 our application will include customizable inputs of all three models, such as infection rate, 
                 susceptible populations, etc. Furthermore, our application includes an intervention tab simulates 
                 the differences in trajectory of COVID-19 when the intervention of vaccines is implemented. Below, you will 
                 see a detailed description of each of our models and how we move from one state to another(for example, infected to recovered).", 
                 style = "font-size:16px;"),
              br(),
              br(),
              
              h1(strong("The SEIR Model"), style = "font-size:30px;"),
              br(),
              div(img(src='SEIR.png', width="800px", align="center"), style="text-align: center;"),
              div(h1(strong("User-Specified Parameters"), style = "font-size:20px;"), style="text-align: center;"),
              div(img(src='SEIR_equations.png', width="500px", align="center"), style="text-align: center;"),
              br(),
              div(h1(strong("SEIR Equations"), style = "font-size:20px;"), style="text-align: center;"),
              h1("The SEIR model simulates the change between states for populations that are experiencing infection disease. The four states that it simulates are in its title S is for the susceptible population, E is for the exposed population, I is for the infected population, and R is for the recovered population. Over time, individuals in the population will move from susceptible to exposed, from exposed to infected, and from infected to recovered (we have chosen not to include deaths in our project). If time continues infinitely and there is no intervention that creates immunity like a vaccine, then eventually every individual within the population will move from the susceptible status all the way to the recovered status. The parameters for our model and the ordinary differential equations describing the rate of change for each state are outlined below:", style = "font-size:16px;"),
              br(),
              withMathJax(includeMarkdown("SEIR_equations.md")),
              br(),
              br(),
              br(),
              br(),
              
              h1(strong("The SIR Model"), style = "font-size:30px;"),
              br(),
              div(img(src='SIR.png', width="800px", align="center"), style="text-align: center;"),
              div(h1(strong("User-Specified Parameters"), style = "font-size:20px;"), style="text-align: center;"),
              div(img(src='SIR_equations.png', width="500px", align="center"), style="text-align: center;"),
              br(),
              div(h1(strong("SIR Equations"), style = "font-size:20px;"), style="text-align: center;"),
              h1("The SIR model simulates the change between states for populations that are experiencing infection disease. The three states that it simulates are in its title: S is for the susceptible population, I is for the infected population, and R is for the recovered population. Over time, individuals in the population will move from susceptible to infected, and from infected to recovered. If time continues infinitely and there is no intervention that creates immunity like a vaccine, then eventually every individual within the population will move from the susceptible status all the way to the recovered status. The parameters for our model and the ordinary differential equations describing the rate of change for each state are outlined below:", style = "font-size:16px;"),
              br(),
              withMathJax(includeMarkdown("SIR_equations.md")),
              br(),
              br(),
              br(),
              br(),
              
              h1(strong("The SI Model"), style = "font-size:30px;"),
              br(),
              div(img(src='SI.png', width="800px", align="center"), style="text-align: center;"),
              div(h1(strong("User-Specified Parameters"), style = "font-size:20px;"), style="text-align: center;"),
              div(img(src='SI_equations.png', width="500px", align="center"), style="text-align: center;"),
              br(),
              div(h1(strong("SI Equations"), style = "font-size:20px;"), style="text-align: center;"),
              h1("The SI model simulates the change between states for populations that are experiencing infection disease. The two states that it simulates are in its title: S is for the susceptible population, and I is for the infected population. Over time, individuals in the population will move from susceptible to infected. If time continues infinitely and there is no intervention that creates immunity like a vaccine, then eventually every individual within the population will move from the susceptible status to the infected status. The parameters for our model and the ordinary differential equations describing the rate of change for each state are outlined below:", style = "font-size:16px;"),
              br(),
              withMathJax(includeMarkdown("SI_equations.md")),
              br(),
              br(),
              br(),
              br(),
              
              h1(strong("The Intervention SIR Model"), style = "font-size:30px;"),
              br(),
              div(h1(strong("User-Specified Parameters"), style = "font-size:20px;"), style="text-align: center;"),
              div(img(src='SIR_intervention.png', width="500px", align="center"), style="text-align: center;"),
              br(),
              div(h1(strong("SIR with Intervention Equations"), style = "font-size:20px;"), style="text-align: center;"),
              h1("In the Analyze Intervention Effect tab, we have created an adaptation of the SIR model that also accounts for the intervention effect of a vaccine, and visualizes how the number of those susceptible, infected, or recovered will change over time as a result of a number of different parameters. The parameters for our intervention model and the ordinary differential equations describing the rate of change for each state are outlined below:", style = "font-size:16px;"),
              br(),
              withMathJax(includeMarkdown("SIR_equations_intervention.md")),
              br(),
              br(),
              br(),
              br(),
              
              #Citation Section
              h1(strong("References"), style = "font-size:30px;"),
              h1("Cooper, I., Mondal, A., & Antonopoulos, C. G. (2020). A SIR model assumption for the spread of COVID-19 in different communities. Chaos, solitons, and fractals, 139, 110057. 
                  https://doi.org/10.1016/j.chaos.2020.110057", style = "font-size:16px;"),
              br(),
              h1("Keeling, Matt & Rohani, Pejman & Pourbohloul, Babak. (2008). Modeling Infectious Diseases in Humans and Animals. Clinical infectious diseases : an official publication of the Infectious Diseases Society of America. 47. 864-865. 10.1086/591197.", style = "font-size:16px;"),
              br(),
              ), #end of readme inputs
      
      #populating the body panel of the home tab 
      tabItem(tabName = "home",
              tags$body(
                tags$div(
                  class="container-fluid jumbotron",
                  tags$h1(class="display-4 text-center", strong("Simulation Modeling of Infectious Disease Dynamics")),
                  tags$h3(class="lead text-center",
                         "This Shiny project simulates deterministic 
            infectious disease compartmental models. The models explored in the context of this project include the 
            simple Susceptible Infected (SI) model, the Susceptible Infected and Recovered (SIR) model, the Susceptible
            Exposed Infected and Recovered (SEIR) model. We then explore the effect of 
            vaccination efficacy on disease transmission dynamics.")
                ),
                tags$div(class="text-center pt-5", tags$a(href="#", actionButton("aboutbttn", "ABOUT SIMULATED MODELS", icon("paper-plane"),
                          style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))
              )
              ) #end of home section
    ) #end of tabs
  ), #end of dashboard body
  skin = "green"
) #end of dashboard page

################################################################################
#THE SERVER AREA

server <- function(input, output, session) {
  
  #setting up the model simulation outputs
  observeEvent(input$go, {
    
    #setting up the SEIR model if it is selected by user    
    if(input$model %in% "SEIR_model"){
      output$plot<-renderPlotly({
        
        #use the seir_model to return the estimated simulation results and return the data frame object
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
    
    }           #closes the if statement for the seir model
    
    #setting up the SIR model if selected by user
    else if(input$model %in% "SIR_model"){     
      output$plot<-renderPlotly({

      #use the sir_model to return the estimated simulation results.return the data frame object
      df<-sir_model(S=input$s, I=input$i, R=input$r, beta = input$BETA, 
                    gamma = input$GAMMA, tfinal = input$tfinalx)
      
      #now create the plot to be rendered on the UI
      plot_ly(data.frame(df), x = ~df[,1], y = ~df[,2], name = 'S', type = 'scatter', mode = 'lines', color=I("blue")) %>%
        add_trace(y = ~df[,3], name = 'I', mode = 'lines', color=I("red")) %>%
        add_trace(y = ~df[,4], name = 'R', mode = 'lines', color=I("purple")) %>%
        layout(xaxis = list(title = "Simulation Period in Days"), yaxis=list(title = "Total Susceptible Population"))
      
    })        #closes the renderPlotly object for the plot of SIR
    
    #add the simulated data table for the SIR infectious disease dynamics
      output$simulation_results<-DT::renderDataTable({
        results<-sir_model(S=input$s, I=input$i, R=input$r, beta = input$BETA, 
                          gamma = input$GAMMA, tfinal = input$tfinalx)
      
        #export the table produced by the `seir_model()` function as a datatable using DT
        results <- DT::datatable(results)
      })
    
    # add the simulated data to be downloaded by the user if needed
      datasetinput=reactive({sir_model(S=input$s, I=input$i, R=input$r, beta = input$BETA, 
                                      gamma = input$GAMMA, tfinal = input$tfinalx)})
      output$downloadData<-downloadHandler(filename = function(){paste("simulated_data-", Sys.Date(), ".csv", sep = "")},
                                          content = function(file){write.csv(datasetinput(), file, row.names=FALSE)})
    }           #closes the if statement for the sir model 
    
    #SI outputs begin
    else if (input$model == "SI_model"){ 
      output$plot<-renderPlotly({
      
      #use the sir_model to return the estimated simulation results.return the data frame object
        df<-si_model(S=input$SUS, I=input$IN, beta = input$betaa, tfinal = input$tfinall)
      
      #now create the plot to be rendered on the UI
        plot_ly(data.frame(df), x = ~df[,1], y = ~df[,2], name = 'S', type = 'scatter', mode = 'lines', color=I("blue")) %>%
          add_trace(y = ~df[,3], name = 'I', mode = 'lines', color=I("red")) %>%
          layout(xaxis = list(title = "Simulation Period in Days"), yaxis=list(title = "Total Susceptible Population"))
      })        #closes the renderPlotly object for the plot of SI
    
      #add the simulated data table for the SI infectious disease dynamics
      output$simulation_results<-DT::renderDataTable({
        results<-si_model(S=input$SUS, I=input$IN, beta = input$betaa, tfinal = input$tfinall)
      
        #export the table produced by the `seir_model()` function as a datatable using DT
        results <- DT::datatable(results)
      })
    
      # add the simulated data to be downloaded by the user if needed
      datasetinput=reactive({si_model(S=input$SS, I=input$IN, beta = input$betaa, 
                                      tfinal = input$tfinall)})
      output$downloadData<-downloadHandler(filename = function(){paste("simulated_data-", Sys.Date(), ".csv", sep = "")},
                                          content = function(file){write.csv(datasetinput(), file, row.names=FALSE)})
    }            #closes the if statement for the SI model 
  })             #closes the observeEvent() area. This is observed only once for all sets of models and recalculated based on model selected
  
 
  ##############################################################################
  #Populating the intervention tab main panel with the plot
  observeEvent(input$go2, {
    output$interventionplot<-renderPlotly({
      #derive the estimated simulation results and return the data frame object
      dt=sir_vaccine(S=input$SS, I=input$II, f=input$f, e=input$e,
                    b=input$b, g=input$g, n=input$n, m=input$m, w=input$w, tmax = input$tmax)
      dt=dt[[1]]
      
      #now create the plot to be rendered on the UI
      plot_ly(as.data.frame(dt), x = ~dt[,1], y = ~dt[,2], name = 'S', type = 'scatter', mode = 'lines', color=I("blue")) %>%
        add_trace(y = ~dt[,3], name = 'I', mode = 'lines', color=I("orange")) %>%
        add_trace(y = ~dt[,4], name = 'R', mode = 'lines', color=I("red")) %>%
        layout(xaxis = list(title = "log(Simulation Time)"), yaxis=list(title = "Susceptible Population"))
    })

    #add the simulated data table for the intervention simulation
    output$sim_results<-DT::renderDataTable({
    result<-sir_vaccine(S=input$SS, I=input$II, f=input$f, e=input$e,
                       b=input$b, g=input$g, n=input$n, m=input$m, w=input$w, tmax = input$tmax)
    
    #export the table produced by the `sir_vaccine()` function as a datatable using DT
    result <- DT::datatable(result$ts)
    })

    # add the simulated data to be downloaded by the user if needed
    datainput=reactive({sir_vaccine(S=input$SS, I=input$II, f=input$f, e=input$e,
                                     b=input$b, g=input$g, n=input$n, m=input$m, w=input$w, tmax = input$tmax)$ts})
    output$downloaddata<-downloadHandler(filename = function(){paste("simulated_data-", Sys.Date(), ".csv", sep = "")},
                                        content = function(file){write.csv(datainput(), file, row.names=FALSE)})
  }) #closes the observeEvent() area for the intervention
  
  ##############################################################################
  #Create erase plot feature
  observeEvent(input$go3, {
    output$interventionplot<-NULL})

  ##############################################################################
  #Implement switch tab option for the "ABOUT SIMULATED MODELS" button
  observeEvent(input$aboutbttn, {
    newtab <- switch(input$menu, "home" = "readme")
    updateTabItems(session, "menu", newtab)
  }
  )
}                 #closes the server function

# Run the application 
shinyApp(ui = ui, server = server)
