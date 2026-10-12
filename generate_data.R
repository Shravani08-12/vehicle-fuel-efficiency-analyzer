# Reproducible educational dataset simulation
set.seed(123)
n <- 240
vehicle_id <- sprintf("V%03d", seq_len(n))
fuel_type <- sample(c("Petrol", "Diesel"), n, replace = TRUE)
engine_cc <- round(runif(n, 800, 3000))
speed_kmph <- round(runif(n, 30, 110), 1)
fuel_effect <- ifelse(fuel_type == "Diesel", 3, 0)
mileage <- 25 + fuel_effect -
  0.0045 * (engine_cc - 1200) -
  0.035 * (speed_kmph - 60) +
  rnorm(n, mean = 0, sd = 3.5)
mileage <- round(pmax(mileage, 5), 2)
mileage_category <- ifelse(mileage > 18, "High", "Low")
vehicles <- data.frame(vehicle_id, mileage, engine_cc, speed_kmph, fuel_type, mileage_category)
dir.create("data", showWarnings = FALSE)
write.csv(vehicles, "data/vehicles.csv", row.names = FALSE)
cat("Saved data/vehicles.csv with", nrow(vehicles), "records.\n")
print(summary(vehicles))
