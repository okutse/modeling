README
================

# Modeling Infectious Disease Dynamics

## General Description

In our shiny application, the user is able to simulate and visualize the
disease dynamics for a number of different models. In the “Simulate
Disease Dynamics” tab, the user has the option of creating one of three
different models: the SEIR model, the SIR model, and the SI model, which
we will describe more in the “Simulate Disease Dynamics” section below.
In the “Analyze Intervention Effect”, we have created a version of the
SIR model which also accounts for the intervention effect of a vaccine,
which we will describe more in the “Analyze Intervention Effect” section
below.

## Getting Started

In order to open our application, you must have R or R Studio downloaded
on your computer and you must download and expand the zip file labeled
“modeling-main”. Then, you will need to open three files using in R or R
Studio: “idmodelling.R”, “load_packages.R”, and
“simulation_functions.R”. Then you will need to click “Run App”, and the
application should show up on your screen. Alternatively, clicking the
link below should take you to the app hosted on the Shiny server
**\[https\]**.

You can then explore our different models in the “Simulate Disease
Dynamics” and “Analyze Intevention Effect” tabs, or you can learn more
about the equations and parameters behind them in our “About” tab.

## Simulate Disease Dynamics

In the “Simulate Disease Dynamics” tab, you can simulate and visualize
each of the “SEIR”, “SIR”, and “SI” disease dynamic models with
customizable inputs, as well as download your simulated data in a .csv
file. Below are the parameters and equations explained for each model:

### The SEIR Model

The “SEIR” model simulates the change between states for populations
that are experiencing infection disease. The four states that it
simulates are in its title “S” is for the susceptible population, “E” is
for the exposed population, “I” is for the infected population, and “R”
is for the recovered population. Over time, individuals in the
population will move from susceptible to exposed, from exposed to
infected, and from infected to recovered (we have chosen not to include
deaths in our project). If time continues infinitely and there is no
intervention that creates immunity like a vaccine, then eventually every
individual within the population will move from the “susceptible” status
all the way to the “recovered” status. The parameters for our model and
the ordinary differential equations describing the rate of change for
each state are outlined below:

#### User-Specified Parameters

S(t) = Number of susceptible individuals at time t

E(t) = Number of exposed individuals at time t

I(t) = Number of infected individuals at time t

R(t) = Number of recovered individuals at time t

*β* = Infection rate

*σ* = Rate from exposure to infection

*γ* = Recovery parameter

When you enter customizable parameters S(t), E(t), I(t), and R(t), you
will be entering their values at time point t=0.

#### Equations

$\\frac{dS(t)}{dt} = -\\beta\*S(t)\*I(t)$

$\\frac{dE(t)}{dt} = \\beta\*S(t)\*I(t) - \\sigma\*E(t)$

$\\frac{dI(t)}{dt} = \\sigma\*E(t) - \\gamma\*I(t)$

$\\frac{dR(t)}{dt} = \\gamma\*I(t)$

### The SIR Model

The “SIR” model simulates the change between states for populations that
are experiencing infection disease. The three states that it simulates
are in its title: “S” is for the susceptible population, “I” is for the
infected population, and “R” is for the recovered population. Over time,
individuals in the population will move from susceptible to infected,
and from infected to recovered. If time continues infinitely and there
is no intervention that creates immunity like a vaccine, then eventually
every individual within the population will move from the “susceptible”
status all the way to the “recovered” status. The parameters for our
model and the ordinary differential equations describing the rate of
change for each state are outlined below:

#### User-Specified Parameters

S(t) = Number of susceptible individuals at time t

I(t) = Number of infected individuals at time t

R(t) = Number of recovered individuals at time t

*β* = Infection rate

*γ* = Recovery parameter

When you enter customizable parameters S(t), I(t), and R(t), you will be
entering their values at time point t=0.

#### Equations

$\\frac{dS(t)}{dt} = -\\beta\*S(t)\*I(t)$

$\\frac{dI(t)}{dt} = \\beta\*S(t)\*I(t) - \\gamma\*I(t)$

$ = I(t) $

### The SI Model

The “SI” model simulates the change between states for populations that
are experiencing infection disease. The two states that it simulates are
in its title: “S” is for the susceptible population, and “I” is for the
infected population. Over time, individuals in the population will move
from susceptible to infected. If time continues infinitely and there is
no intervention that creates immunity like a vaccine, then eventually
every individual within the population will move from the “susceptible”
status to the “infected” status. The parameters for our model and the
ordinary differential equations describing the rate of change for each
state are outlined below:

#### User-Specified Parameters

S(t) = Number of susceptible individuals at time t

I(t) = Number of infected individuals at time t

*β* = Infection rate

When you enter customizable parameters S(t) and I(t), you will be
entering their values at time point t=0.

#### Equations

$\\frac{dS(t)}{dt} = -\\beta\*S(t)\*I(t)$

$$\\frac{dI(t)}{dt} = \\beta\*S(t)\*I(t)$$

## Analyze Intervention Effect

In the “Analyze Intervention Effect” tab, we have created an adaptation
of the SIR model that also accounts for the intervention effect of a
vaccine, and visualizes how the number of those susceptible, infected,
or recovered will change over time as a result of a number of different
parameters. The parameters for our intervention model and the ordinary
differential equations describing the rate of change for each state are
outlined below:

#### User-Specified Parameters

S(t) = Number of susceptible individuals at time t I(t) = Number of
infected individuals at time t R = Initial number of recovered/removed
(includes vaccinated)

f = Fraction of vaccinated individuals e = Efficacy of vaccine, given as
a fraction between 0 and 1 b = Level/rate of infectiousness for hosts in
the I(t) compartment n = Rate at which new individuals enter the model
(birth rate) g = Rate at which individuals leave the infected state m =
The rate of natural death (inverse of the average lifespan) w = Rate of
waning (the rate at which recovered individuals lose immunity)

#### Equations

$$\\frac{dS(t)}{dt} = n - m\*S(t) - b\*S(t)\*I(t) +  w\*R(t)$$

$$\\frac{dI(t)}{dt} = b\*S(t)\*I(t) - g\*I(t) - m\*I(t)$$

*S*0*e**f**f* = (1−*f*\**e*) \* *S*;
*R* = *f* \* *e* \* *S*;
#initial number of recovered/removed (inlcudes vaccinated)
