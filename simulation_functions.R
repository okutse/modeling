#Simulation Functions Script

#SEIR Model
seir_model<-function(S, E, I, R, beta, gamma, sigma, tfinal){
  #' Simulation of a compartmental SEIR infectious disease transmission model
  #' 
  #' @description Simulation of the more general SEIR with these compartments:
  #'    Susceptibles (S), Exposed (E), Infected/Infectious (I), and Recovered (R).
  #'    
  #'    The compartmental SEIR model implemented using this function on the Shiny app
  #'    assumes that the time is in days. The model can, however, work for any unit of 
  #'    time passed to it.
  #'    
  #' @param S : the initial number of susceptible individuals : numeric
  #' @param E : the initial number of exposed individuals : numeric
  #' @param I : the number of individuals infected : numeric
  #' @param R : the number recovered : numeric
  #' @param beta : the rate of infection : numeric
  #' @param gamma : the rate of recovery/transition from the infectious state : numeric
  #' @param sigma : rate of transition from exposed status to infected status : numeric
  #' @param tfinal : simulation time in days : numeric
  #' @usage seir_model(S, E, I, R, beta, gamma, sigma, tfinal)
  #' @return This function returns the simulation result as obtained from a call
  #'   to the deSolve ordinary differential equation (ODE) solver.
  #' @details A compartmental ID model with several states/compartments
  #'   is simulated as a set of ordinary differential
  #'   equations. The function returns the output from the odesolver as a matrix,
  #'   with one column per compartment/variable. The first column is time.
  #' @section Warning: 
  #'   No error-checking is carried out by the function. Nonsensical parameters
  #'   will result in an error.
  #' @seealso The ABOUT section of the app contains further details on this model.
  #' @references 
  #' @author Amos Okutse, Yingjie (Gary) Zhou, Kyla Finlayson
  #' @export  
  
  #function specifying the system of differential equations used by deSolve
  seir <- function(time, state, parameters) {
    with(as.list(c(state, parameters)), {
      dS <- - beta*S*I
      dE <- beta*S*I - sigma*E
      dI <- sigma*E - gamma*I
      dR <- gamma*I
      return(list(c(dS, dE, dI, dR)))
    })
  }
  
  #combine the user inputs as parameters
  parameters<-c(beta=beta, gamma=gamma, sigma=sigma)
  
  #create vector of the initial state values
  y=c(S=S, E=E, I=I, R=R)
  
  #create vector of simulation times where tfinal=input$tfinal
  times=seq(0, tfinal, by=1)
  
  #solve the system of differentials using the deSolve package
  results <- deSolve::ode(y=y, times=times, func=seir, parms=parameters)
  
  #create data frame of the results and return
  return(as.data.frame(results))
  
}

################################################################################

#SIR Model
sir_model<-function(S, I, R, beta, gamma, tfinal){
  #' Simulation of a compartmental SIR infectious disease transmission model
  #' 
  #' @description Simulation of the more general SEIR with these compartments:
  #'    Susceptibles (S), Infected/Infectious (I), and Recovered (R).
  #'    
  #'    The compartmental SIR model implemented using this function on the Shiny app
  #'    assumes that the time is in days. The model can, however, work for any unit of 
  #'    time passed to it.
  #'    
  #' @param S : the initial number of susceptible individuals : numeric
  #' @param I : the number of individuals infected : numeric
  #' @param R : the number recovered : numeric
  #' @param beta : the rate of infection : numeric
  #' @param gamma : the rate of recovery/transition from the infectious state : numeric
  #' @param tfinal : simulation time in days : numeric
  #' @usage sir_model(S, I, R, beta, gamma, tfinal)
  #' @return This function returns the simulation result as obtained from a call
  #'   to the deSolve ordinary differential equation (ODE) solver.
  #' @details A compartmental ID model with several states/compartments
  #'   is simulated as a set of ordinary differential
  #'   equations. The function returns the output from the odesolver as a matrix,
  #'   with one column per compartment/variable. The first column is time.
  #' @section Warning: 
  #'   No error-checking is carried out by the function. Nonsensical parameters
  #'   will result in an error.
  #' @seealso The ABOUT section of the app contains further details on this model.
  #' @references 
  #' @author Amos Okutse, Yingjie (Gary) Zhou, Kyla Finlayson
  #' @export  
  
  #function specifying the system of differential equations used by deSolve
  sir <- function(time, state, parameters) {
    with(as.list(c(state, parameters)), {
      dS <- - beta*S*I
      dI <- beta*S*I - gamma*I
      dR <- gamma*I
      return(list(c(dS, dI, dR)))
    })
  }
  
  #combine the user inputs as parameters
  parameters<-c(beta=beta, gamma=gamma)
  
  #create vector of the initial state values
  y=c(S=S, I=I, R=R)
  
  #create vector of simulation times where tfinal=input$tfinal
  times=seq(0, tfinal, by=1)
  
  #solve the system of differentials using the deSolve package
  results <- deSolve::ode(y=y, times=times, func=sir, parms=parameters)
  
  #create data frame of the results and return
  return(as.data.frame(results))
}

################################################################################

#SI Model
si_model<-function(S, I, beta, tfinal){
  #' Simulation of a compartmental SI infectious disease transmission model
  #' 
  #' @description Simulation of the more general SI with these compartments:
  #'    Susceptibles (S) and Infected/Infectious (I).
  #'    
  #'    The compartmental SI model implemented using this function on the Shiny app
  #'    assumes that the time is in days. The model can, however, work for any unit of 
  #'    time passed to it. The model assumes that once infected, an individual does 
  #'    not recover or is never removed from that compartment.
  #'    
  #' @param S : the initial number of susceptible individuals : numeric
  #' @param I : the number of individuals infected : numeric
  #' @param beta : the rate of infection : numeric
  #' @param tfinal : simulation time in days : numeric
  #' @usage si_model(S, I, beta, tfinal)
  #' @return This function returns the simulation result as obtained from a call
  #'   to the deSolve ordinary differential equation (ODE) solver.
  #' @details A compartmental ID model with several states/compartments
  #'   is simulated as a set of ordinary differential
  #'   equations. The function returns the output from the odesolver as a matrix,
  #'   with one column per compartment/variable. The first column is time.
  #' @section Warning: 
  #'   No error-checking is carried out by the function. Nonsensical parameters
  #'   will result in an error.
  #' @seealso The ABOUT section of the app contains further details on this model.
  #' @references 
  #' @author Amos Okutse, Yingjie (Gary) Zhou, Kyla Finlayson
  #' @export

  #function specifying the system of differential equations used deSolve
  si <- function(time, state, parameters) {
    with(as.list(c(state, parameters)), {
      dS <- - beta*S*I
      dI <- beta*S*I
      return(list(c(dS, dI)))
    })
  }
  
  #combine the user inputs as parameters
  parameters<-c(beta=beta)
  
  #create vector of the initial state values
  y=c(S=S, I=I)
  
  #create vector of simulation times where tfinal=input$tfinal
  times=seq(0, tfinal, 1)
  
  #solve the system of differentials using the deSolve package
  results <- deSolve::ode(y=y, times=times, func=si, parms=parameters)
  
  #create data frame of the results and return
  return(as.data.frame(results))
  
}

################################################################################

#Vaccine Intervention Model
sir_vaccine <- function(S = 1000, I = 1, f = 0.0, e = 0.0, b = 1e-2, g = 10, n = 0, m = 0, w = 0, tmax = 300){
  #' Simulation of a compartmental infectious disease transmission model 
  #' to study the effect of vaccine efficacy, waning immunity, and recovery rate
  #'
  #' @description  Simulation of an adaptation of the SIR compartmental model that accounts for the intervention effect
  #'   of the vaccine, with these compartments:
  #'   Susceptible (S), Infected/Infectious (I),
  #'   Recovered and Immune (R).
  #'
  #'   The model is assumed to be in units of months when run through the Shiny App.
  #'   However as long as all parameters are chosen in the same units,
  #'   one can directly call the simulator assuming any time unit.
  #'
  #' @param S : initial number of susceptible hosts : numeric
  #' @param I : initial number of infected hosts : numeric
  #' @param f : fraction of vaccinated individuals. Those individuals are moved from S to R at the beginning of the simulation : numeric
  #' @param e : efficacy of vaccine, given as fraction between 0 and 1 : numeric
  #' @param b : level/rate of infectiousness for hosts in the I compartment : numeric
  #' @param g : rate at which a person leaves the I compartment : numeric
  #' @param n : the rate at which new individuals enter the model (are born) : numeric
  #' @param m : the rate of natural death (the inverse it the average lifespan) : numeric
  #' @param w : rate of waning (rate at which recovered persons lose immunity and return to susceptible state) : numeric
  #' @param tmax : maximum simulation time : numeric
  #' @return This function returns the simulation result as obtained from a call
  #'   to the deSolve ode solver.
  #' @details A compartmental ID model with several states/compartments
  #'   is simulated as a set of ordinary differential
  #'   equations. The function returns the output from the odesolver as a matrix,
  #'   with one column per compartment/variable. The first column is time.
  #' @section Warning:
  #'   This function does not perform any error checking. So if you try to do
  #'   something nonsensical (e.g. negative values or fractions > 1),
  #'   the code will likely abort with an error message.
  
  #' @references See e.g. Keeling and Rohani 2008 for SIR models and the
  #'   documentation for the deSolve package for details on ODE solvers
  #' @author Amos Okutse, Yingjie (Gary) Zhou, and Kyla Finlayson
  #' @export
  
  # start function that specifies differential equations used by deSolve
  idvaccineode <- function(t, y, parms)
  {
    with(
      as.list(c(y,parms)), #lets us access variables and parameters stored in y and parms by name
      {
        #the ordinary differential equations
        dS = n - m*S - b*S*I +  w*R;        #susceptibles
        dI = b*S*I - g*I - m*I;             #infected/infectious
        dR = g*I - m*R - w*R;               #recovered
        
        list(c(dS, dI, dR))
      }
    ) #close with statement
  } #end function specifying the ODEs
  #####
  
  S0eff = (1 - f*e) * S;
  R = f*e * S; #initial number of recovered/removed (includes vaccinated)
  Y0 = c(S = S0eff, I = I, R = R);  #combine initial conditions into a vector
  dt = min(0.1, tmax / 1000); #time step for which to get results back
  timevec = seq(0, log(tmax), dt); #vector of times for which solution is returned (not that internal timestep of the integrator is different)
  
  #combining parameters into a parameter vector
  pars = c(b = b, g = g, n = n, m = m, w = w);
  
  #this line runs the simulation, i.e. integrates the differential equations describing the infection process
  #the result is saved in the odeoutput matrix, with the 1st column the time, the 2nd, 3rd, 4th column the variables S, I, R
  #This odeoutput matrix will be re-created every time you run the code, so any previous results will be overwritten
  odeoutput = deSolve::ode(y = Y0, times = timevec, func = idvaccineode, parms=pars, method = "vode", atol=1e-8, rtol=1e-8);
  result <- list()
  result$ts <- as.data.frame(odeoutput)
  return(result)
}





