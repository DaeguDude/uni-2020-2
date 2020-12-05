raw_heart_disease <- read.csv("cleveland-data.csv", header = TRUE)

# remove unused variables: 'slope, 'ca', 'thal'
heart_disease <- raw_heart_disease[,-11:-13]

# Change '?' to NA and remove rows that has 'NA'
heart_disease[heart_disease=="?"] <- NA
heart_disease <- na.omit(heart_disease)



