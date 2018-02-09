
#monthly apprehensions by year

msum  <- read.csv("PB monthly summaries.csv", stringsAsFactors = FALSE)
years <- msum[c(1)]
msum  <- msum[-c(1)]
rownames(msum) <- as.vector(t(years))
msum

#monthly apprehensions by Sector
ap10 <- read.csv("BP apprehensions 2010.csv")
cities <- ap10[,1]
ap10 <- ap10[-c(1,14)]
rownames(ap10) <- cities
ap10
SumCol10 <- colSums(Filter(is.numeric, ap10))

#monthly apprehensions by Sector
#ap17 <- read.delim("clipboard")
#write.csv(ap17, file = "edited_app17.csv")

ap17 <- read.csv("edited_app17.csv")

ap17 <- ap17[-c(1,2,15)]
ap17 <- ap17[-c(10),]
rownames(ap17) <- cities
SumCol17 <- colSums(Filter(is.numeric, ap17))
ap17


#Comparison total apprehensions by month 
CmbColS <- rbind((SumCol10),(SumCol17))
colours <- c("orange","red")
barplot(CmbColS,
        beside=TRUE, 
        las=2, 
        main = "Apprehensions by month",
        cex.lab =1.5,
        col = colours )
legend("topright",
       c("Apprehensions in 2010","Apprehensions in 2017"),
       cex=0.8,
       bty="n",
       fill=colours )

#Comparison total apprehensions by city
CmbRowS <- rbind(rowSums(ap10), rowSums(ap17))
barplot(CmbRowS,
        beside=TRUE, 
        las=2, main = "Apprehensions by city", 
        cex.lab =1.5, 
        col = colours )
legend("topleft",
          c("Apprehensions in 2010","Apprehensions in 2017"),
          cex=0.8,
          bty="n",
          fill=colours )