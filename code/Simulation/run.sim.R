# Run simulations
for (sim.num in 1:100) { # replicate
for (s in 1:3) { # scenario
for (error in c(1, 25)) { # error amount
for (model in 1:7) { # model
  source("code/Simulation/sim.R", local = TRUE)
}
}
}
}

