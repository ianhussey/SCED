library(SCED)

sim <- simulate_data(participants = 2, timepoints = 20, cohens_d = 3)

sced_analysis(data = sim)
sced_plot(data = sim)
sced_plot(data = simulated_data)
