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
#' @author Amos Okutse
#' @export  


seir_model<-function(S, E, I, R, beta, gamma, sigma, tfinal){
  
  #function specifying the system of differential equations used deSolve
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

#another function goes here!!






