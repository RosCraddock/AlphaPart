# Partition genetic values
data(AlphaPart.ped)
res <- AlphaPart(
  x = AlphaPart.ped,
  colPath = "country",
  colBV = c("trait1", "trait2")
)

# Write summary on the disk and collect saved file names
fileName <- file.path(tempdir(), "AlphaPart")
ret <- write.csv(x = res, file = fileName)
print(ret)
file.show(ret[1])

# Clean up
files <- dir(path = tempdir(), pattern = "AlphaPart*")
unlink(x = files)
