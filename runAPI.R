library(plumber)
pr <- plumber::plumb("myAPI.R")
pr$run(port=8000)
#is hosted at http://localhost:8000
