library(plumber)
pr <- plumber::plumb("mani14_Marcus_Vogt_Exam_API/plumber.R")
pr$run(port=8000)
