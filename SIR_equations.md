---
title: "SIR Equations"
output:
  html_document:
    css: style.css
    
---

\begin{equation}
\frac{dS(t)}{dt} = -\beta*S(t)*I(t)
\end{equation}

\begin{equation}
\frac{dI(t)}{dt} = \beta*S(t)*I(t) - \gamma*I(t)
\end{equation}

\begin{equation}
\frac{dR(t)}{dt} = \gamma*I(t)
\end{equation}