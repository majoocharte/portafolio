library(deSolve)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(dslabs)

# INICIALIZAR ECUACIONES


ed.sol = function(t, state, parms) {
  with(as.list(state),
       {
         dxdt = rep(0, length(state))
         dxdt[1] = u*N - B*I*(S/N) - u*S
                #birth  #infection  #death
         dxdt[2] = B*I*(S/N) - y*I - u*I
               #infection  #recovery  #death
         dxdt[3] = y*I - u*R
             #recovery  #death
         return(list(dxdt))
       })
}


# ESTABLECER CONDICIONES INICIALES 
#PARÁMETROS

B = 0.5 #r0/(S*L) # pues r0 = S*L*B
# constatnte para este modelo
# tasa de transmisión
# valor inventado 
# betta es  measure of transmisibility 
# contacts per infected per day 
# betta depende de que tan suceptible es la población y que tan infeccioso
# es el patógeno 

tasaNacimientos = 2.3 # hijos por mujer en el 2020
u = tasaNacimientos/N # porque u*N = tasa de nacimientos

L = 10 # the lenght of time an individual is infectous
# el tiempo que un individuo es contagioso es de aprox. 10 días
# gamma es el periodo de tiempo que estas infectado y vas a infectar 
# el periodo infectivo (L) es de 1/y, entonces L=1/y
y = 1/L # gamma (por definición 1/L)


#

#r0 = S*B*L # reproductive number (avearge or expected outcome of transmission)

r0 = 2.5 # cada persona infectada contagia entre 2 a 3 personas más

N = 70000 # tamaño de la población en miles (S + I + R)
S = N # susceptibles (asumimos que toda la población es susceptible)

I = 300 # número de infectados (infecciosos)
R = 10 # número de removidos (muertos y recuperados)



t = seq(0,111,1) # en dias ¿? 
init = c(S=S, I=I, R=R)

miOutput = ode(y=init, times = t, func = ed.sol, parms = NULL)
miOutput

# graficar las soluciones

ggplot(miOutput, aes(x=time)) + 
  geom_line(aes(y=S, color = "S"), lwd = 2) +
  geom_line(aes(y=I, color = "I"), lwd = 2) +
  geom_line(aes(y=R, color = "R"), lwd = 2) +
  ggtitle("solución de S I R") + labs(name = "variable") +
  xlab("tiempo") + ylab("población") +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
  scale_color_manual(values = c("S" = "red1", "I" = "limegreen", "R" = "dodgerblue"), name = "variable") +
  scale_x_continuous(limits = c(0,50))

