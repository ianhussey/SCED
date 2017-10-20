library(SCED)


sced_analysis(data = simulated_data)
sced_plot(data = simulated_data)

sim <- simulate_data(participants = 2, timepoints = 4, cohens_d = 1.5)

sced_analysis(data = sim)
sced_plot(data = sim)

