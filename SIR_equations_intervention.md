---
title: "SIR w/ intervention Equations"
output:
  html_document:
    css: style.css
    
---

\begin{equation}
\frac{dS(t)}{dt} = n - m*S(t) - b*S(t)*I(t) +  w*R(t)
\end{equation}

\begin{equation}
\frac{dI(t)}{dt} = b*S(t)*I(t) - g*I(t) - m*I(t)
\end{equation}

\begin{equation}
S_{0eff} = (1 - f*e)*S(t)
\end{equation}

\begin{equation}
R = f*e * S(t)
\end{equation}
