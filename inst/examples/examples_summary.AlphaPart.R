# --- Partition genetic values by loc ---

data(AlphaPart.ped)
res <- AlphaPart(
  x = AlphaPart.ped,
  colPath = "country",
  colBV = c("trait1", "trait2")
)

# Summarize whole population
ret <- summary(res)

# Summarize population by generation (=trend)
ret <- summary(res, by = "generation")

# Summarize population by generation (=trend) but only for domestic location
ret <- summary(res, by = "generation", subset = res[[1]]$country == "domestic")

# --- Partition genetic values by loc and gender ---

AlphaPart.ped$country.gender <- with(
  AlphaPart.ped,
  paste(country, gender, sep = "-")
)
res <- AlphaPart(
  x = AlphaPart.ped,
  colPath = "country.gender",
  colBV = c("trait1", "trait2")
)

# Summarize population by generation (=trend)
ret <- summary(res, by = "generation")

# Summarize population by generation (=trend) but only for domestic location
ret <- summary(res, by = "generation", subset = res[[1]]$country == "domestic")
