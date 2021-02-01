# library(data.table)
library(reshape2)
library(ggplot2)
library(deSolve)

pop.foxRabbits.ini <- c(rabbits = 34.9134, foxes = 3.8566)
kinetics.foxRabbits <- c(0.4807, 0.0248, 0.0.0276, 0.9271)
t.duration <- seq(0, 100, 1/12)

model.lv <- function (t, Y, v) {  
  dRabbits = v[1] * Y[1] - v[2] * Y[1] * Y[2];
  dFoxdes = v[3] * Y[1] * Y[2] - v[4] * Y[2];
  
  return(list( c(dRabbits, dFoxdes) ) )
}

solution.foxRabbits <- ode(y = pop.foxRabbits.ini, 
                           func = model.lv, 
                           times = t.duration, 
                           parms = kinetics.foxRabbits) 

# change the result from wide to long format using melt(...)
solution.foxRabbits.long <- melt(as.data.frame(solution.foxRabbits), id = c("time"))

# Draw a figure
ggplot(solution.foxRabbits.long, aes(time, value)) + 
  geom_line(aes(colour = variable, linetype = variable)) + 
  theme_classic() +
  labs(caption = "Lotka Voltera - Rabbits versus Foxes",
       x = "time [a.U.]",
       y = "number of animals",
       colour = "Species") + 
  theme(legend.position = "top")
