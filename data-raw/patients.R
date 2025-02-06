## code to prepare `patients` dataset goes here
patients <- data.frame(sex = sample(c("M", "F"), 150, replace = T),
                       stage = sample(c("I", "II", "III"), 150, replace = T,
                                      prob = c(0.5, 0.3, 0.2)),
                       site = sample(1:10, 150, replace = T),
                       eligible = sample(c("ABC", "AB", "AC", "BC"), 150,
                                         replace = T))

usethis::use_data(patients, overwrite = TRUE)
