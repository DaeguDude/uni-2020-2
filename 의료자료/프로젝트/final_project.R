raw_heart_disease <- read.csv("data.csv")

# --------------------------------------------------------------------
# DATA PREPROCESSING
# --------------------------------------------------------------------

# remove unused variable 'slope', 'ca', 'thal'
heart_disease <- raw_heart_disease[,-11:-13]

# change '?' to NA
heart_disease[heart_disease=='?'] <- NA

# remove rows that has 'NA'
heart_disease <- na.omit(heart_disease)

heart_disease$sex <- as.factor(heart_disease$sex)
heart_disease$cp <- as.factor(heart_disease$cp)
heart_disease$fbs <- as.factor(heart_disease$fbs)
heart_disease$restecg <- as.factor(heart_disease$restecg)
heart_disease$exang <- as.factor(heart_disease$exang)
heart_disease$num <- as.factor(heart_disease$num)
# --------------------------------------------------------------------
# trestbps <- as.numeric(trestbps)

