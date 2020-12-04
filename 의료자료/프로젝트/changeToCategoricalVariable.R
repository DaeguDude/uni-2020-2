# trestbps

table(trestbps >= 180)
table(trestbps >= 140 & trestbps < 180)
table(trestbps >= 130 & trestbps < 140)
table(trestbps >= 120 & trestbps < 130)
table(trestbps < 120)

# age
table(heart_disease$age < 30)
table(heart_disease$age < 40 && heart_disease$age >= 30)
table(heart_disease$age < 50 & heart_disease$age >= 40)
table(heart_disease$age < 60 & heart_disease$age >= 50)
table(heart_disease$age > 60)

# chol - normal, high
# normal < 200
# high >= 200
chol <- as.numeric(heart_disease$chol)
table(chol < 200)
table(chol >= 200)
