x <- c(10:1)
y <- c(-4:5)
q <- c("Hotkey", "Football", "Baseball", "Curling", "Rugby",
       "Lacrosse", "Basketball", "Tennis", "Cricket", "Soccer")

theDF <- data.frame(x, y, q);

names(theDF)[names(theDF) == "x"] <- "First"
names(theDF)[names(theDF) == "y"] <- "Second"
names(theDF)[names(theDF) == "q"] <- "Third"

theDF[3:5, ]
theDF[, 2:3]
