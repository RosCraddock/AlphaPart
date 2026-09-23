# Partition genetic values
data(AlphaPart.ped)
(res <- AlphaPart(
  x = AlphaPart.ped,
  colPath = "country",
  colBV = c("trait1", "trait2")
))

# Summarize population by generation (=trend)
(ret <- summary(res, by = "generation"))

# Plot the partitions
p <- plot(
  ret,
  ylab = c("BV for trait 1", "BV for trait 2"),
  xlab = "Generation"
)
print(p[[1]])
print(p[[2]])
#print(p)
