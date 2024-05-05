rm(list=ls())
setwd("D:/Parmenides/Eye-Tracker/Exp")

library(dplyr)
library(xlsx)

# Read data from CSV
filename <- "fixations_KI_vp006"
data <- read.csv(paste0(filename, ".csv"))

reasoning_begin <- 1709985871890263386
reasoning_end <- reasoning_begin + 245000000000

# Define box dimensions and positions in normalized pixel values
box_width <- 240 / 1250
box_height <- 130 / 700

box_list <- list(
  c(40 / 1250, 70 / 700),
  c(310 / 1250, 70 / 700),
  c(170 / 1250, 210 / 700),
  c(440 / 1250, 210 / 700),
  c(700 / 1250, 210 / 700),
  c(950 / 1250, 115 / 700),
  c(170 / 1250, 355 / 700),
  c(440 / 1250, 355 / 700),
  c(700 / 1250, 355 / 700),
  c(950 / 1250, 285 / 700),
  c(40 / 1250, 510 / 700),
  c(310 / 1250, 510 / 700)
)

symbol_list <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L")

# Box allocation
data$box <- NA

data <- data[data[, 4] > reasoning_begin, ]
data <- data[data[, 4] < reasoning_end, ]

for (i in 1:length(box_list)) {
  for (j in 1:nrow(data)) {
    tryCatch({
      if (box_list[[i]][1] < data[j, 8] && data[j, 8] < box_list[[i]][1] + box_width &&
          box_list[[i]][2] < data[j, 9] && data[j, 9] < box_list[[i]][2] + box_height) {
        data[j, 10] <- symbol_list[i]
      }
    }, error = function(e) {
      next
    })
  }
}


# remove NAs 
data <- na.omit(data)

# Summation of consecutive duplicate characters
for (i in 1:(nrow(data)-1)) {
  try ({
    if (data$box[i] == data$box[i+1]) {
      data$duration..ms.[i+1] <- data$duration..ms.[i] + data$duration..ms.[i+1]
    }
  })
}


data <- data %>% filter(data$box != lead(data$box))



# Saving the data
write.csv(data, file = paste0(filename, "_box.csv"), row.names = FALSE)
write.xlsx(data, file = paste0(filename, "_box.xlsx"), row.names = FALSE)

box_string <- paste(data$box, collapse = "")
write.table(box_string, file = paste0(filename, "_box.txt"), row.names = FALSE, col.names=FALSE, quote = FALSE)
