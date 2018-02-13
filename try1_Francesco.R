
#monthly apprehensions by year

msum  <- read.csv("PB monthly summaries.csv", stringsAsFactors = FALSE)
years <- msum[c(1)]
msum  <- msum[-c(1)]
rownames(msum) <- as.vector(t(years))
msum
##################################################################################################################
#histogram
ap10 <- read.csv("BP apprehensions 2010.csv")
cities <- ap10[,1]
ap10 <- ap10[-c(1,14)]
rownames(ap10) <- cities
SumCol10 <- colSums(Filter(is.numeric, ap10))

ap17 <- read.csv("edited_app17.csv")
ap17 <- ap17[-c(1,2,15)]
rownames(ap17) <- cities
SumCol17 <- colSums(Filter(is.numeric, ap17))

CmbColS <- rbind((SumCol10),(SumCol17))
CmbRowS <- rbind(rowSums(ap10), rowSums(ap17))
colours <- c("orange","red")


#Comparison total apprehensions by month 

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







##################################################################################################################


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

#function to get t-tests for by-sector compasison
by_sector.t <- function(y){

   t.test(as.numeric(ap10[y,]), as.numeric(ap17[y,]), paired = TRUE)
}
# sector with max yearly sum in 2010 --> test on max-apprehension sector changes
by_sector.t(6)
# sector with max yearly sum in 2017 --> test on max-apprehension sector changes
by_sector.t(8)

# t.test(as.numeric(ap10[8,]), as.numeric(ap17[8,]), paired = TRUE) # Tucson

#function to get t-test for by-month comparison
by.month.t <- function(a,b){
  sum3m_10 = rowSums(ap10[,a:b],na.rm=FALSE) #sum of 3-months peak for each sector
  sum3m_17 = rowSums(ap17[,a:b],na.rm=FALSE)
  t.test(sum3m_10,sum3m_17, paired = TRUE)

}
# peak in 2010: March April May
# test 2010 vs 2017 for March, April, May
by.month.t(6,8)
# peak in 2017: October, November, December
# test 2010 vs 2017 for October, November, December
by.month.t(1,3)

#total numbers

sum(ap17)
sum(ap10)

1 - sum(ap17)/sum(ap10)

