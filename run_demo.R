# library(data.table)
library(deSolve)

pop.foxRabbits.ini <- c(rabbits = 34.9134, foxes = 3.8566)
kinetics.foxRabbits <- c(0.4807, 0.0248, 0.0276, 0.9271)
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

# Draw a figure

plot(solution.foxRabbits[, "time"], solution.foxRabbits[, "rabbits"], 
     xlab = "time [years]", ylab = "number of animals [thousands]",
     type="l", col="#23689b", ylim = c(-0, 90), lwd = 2)

lines(solution.foxRabbits[, "time"], solution.foxRabbits[, "foxes"], col="#ec4646", lwd = 2, lty = 4)

legend("bottom", c("Rabbits","Foxes"), fill=c("#23689b","#ec4646") ) 
