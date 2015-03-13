#Boxplot 1
png(filename = "boxplot1.png")
par(mfrow = c(1,1), mar = c(5,5,3,2))
boxplot(creditdata$AMOUNT ~ creditdata$INSTALL_RAT, col="red", xlab ="INSTALL_RATE", 
ylab="CREDIT AMOUNT", main = "CREDIT AMOUNT Vs INSTALLMENT RATE")
text(x = 1, y= 3000, labels = "mean")
text(x = 1, y= 700, labels = "1Q", pos=2, offset = 2.55)
text(x = 1, y= 2000, labels = "2Q", pos=2, offset = 2.55)
text(x = 1, y= 3500, labels = "3Q", pos=2, offset = 2.55)
text(x = 1, y= 5600, labels = "4Q", pos=2, offset = 2.55)
text(x = 2, y= 3000, labels = "3Q", pos=2, offset = 2.55)
text(x = 3, y= 2500, labels = "3Q", pos=2, offset = 2.55)
text(x = 4, y= 2000, labels = "3Q", pos=2, offset = 2.55)
dev.off()


#boxplot 2
png(filename = "boxplot2.png")
par(mfrow = c(1,1), mar = c(5,5,3,2))
boxplot(creditdata$DURATION ~ creditdata$INSTALL_RAT, col="red", xlab ="INSTALL_RATE", ylab="CREDIT DURATION(Months)", main = "CREDIT DURATION Vs INSTALLMENT RATE")
text(x = 1, y= 4, labels = "1Q", pos=2, offset = 2.55)
text(x = 1, y= 7, labels = "2Q", pos=2, offset = 2.55)
text(x = 1, y= 14, labels = "3Q", pos=2, offset = 2.55)
text(x = 1, y= 24, labels = "4Q", pos=2, offset = 2.55)
text(x = 2, y= 17, labels = "3Q", pos=2, offset = 2.55)
text(x = 3, y= 17,labels = "3Q", pos=2, offset = 2.55)
text(x = 4, y= 17,labels = "3Q", pos=2, offset = 2.55)
text(x = 2, y= 17, labels = "mean", pos = 3, offset = .5)
abline(h = 18)
text(x = 1, y= 16.5, labels = "18", pos = 2, offset = 3)
dev.off()

#boxplot 3
png(file = "boxplot3.png")
par(mfrow = c(1,1), mar=c(5,5,2,2))
boxplot(creditdata$AGE~creditdata$NUM_CREDITS, col="blue", main = "AGE Vs NUMBER OF CREDITS AT THE BANK", xlab="NUM_CREDITS", ylab="AGE")
text(x = 1, y= 20, labels = "1Q", pos=2, offset = 2.55)
text(x = 1, y= 25, labels = "2Q", pos=2, offset = 2.55)
text(x = 1, y= 32, labels = "3Q", pos=2, offset = 2.55)
text(x = 1, y= 40, labels = "4Q", pos=2, offset = 2.55)
text(x = 2, y= 34, labels = "3Q", pos=2, offset = 2.55)
text(x = 3, y= 40, labels = "3Q", pos=2, offset = 2.55)
text(x = 4, y= 45, labels = "3Q", pos=2, offset = 2.55)
text(x = 1, y= 32, labels = "mean", pos=3, offset = .5)
dev.off()

#calculating the mean value of numeric fields
DURATION <- mean(creditdata$DURATION)
AMOUNT <- mean(creditdata$AMOUNT)
NUM_CREDITS <- mean(creditdata$NUM_CREDITS)
INSTALL_RATE <- mean(creditdata$INSTALL_RATE)
NUM_CREDITS <- mean(creditdata$NUM_CREDITS)
AGE <- mean(creditdata$AGE)
matrix(nrow = 1, ncol = 5, data = c(DURATION, AMOUNT, INSTALL_RATE, AGE, NUM_CREDITS ))

#calculating the std deviations from the mean values of numeric fields
DURATION <- sd(creditdata$DURATION)
AMOUNT <- sd(creditdata$AMOUNT)
NUM_CREDITS <- sd(creditdata$NUM_CREDITS)
INSTALL_RATE <- sd(creditdata$INSTALL_RATE)
NUM_CREDITS <- sd(creditdata$NUM_CREDITS)
AGE <- sd(creditdata$AGE)
matrix(nrow = 1, ncol = 5, data = c(DURATION, AMOUNT, INSTALL_RATE, AGE, NUM_CREDITS ))

#Scatterplot 1
png(filename = "scatterplot1.png")
par(mar=c(5,5,2,2))
plot(creditdata$AGE,creditdata$AMOUNT, col= creditdata$NUM_CREDITS, main = "AGE Vs CREDIT AMOUNT", 
xlab="AGE(years)", ylab="AMOUNT(USD)", pch=20)
legend("topright", pch=20, col=c(1,2,3,4), legend=c("1 Credit","2 Credits", "3 Credits", "4 Credits") )
model <- lm(AMOUNT~AGE, creditdata)
abline(model, lwd=2)
text(x=60,y=3500,label="lm(x)", pos=3, offset=.5,font=4)
dev.off()

#Scatterplot 2
png(filename = "scatterplot2.png")
par(mar=c(5,5,2,2))
plot(creditdata$AMOUNT~creditdata$NUM_CREDITS, col= "blue", 
main = "NUMBER OF CREDITS Vs CREDIT AMOUNT", xlab="NUM_CREDITS", ylab="AMOUNT(USD)", pch=19)
legend("topright", pch=19, fill="blue", legend=c("Number of Credits with the Bank") )
model <- lm(AMOUNT~NUM_CREDITS, creditdata)
abline(model, lwd=2)
text(x=3,y=3500,label="lm(x)", pos=3, offset=.5,font=4)
dev.off()

#histogram 1
png(filename = "histogram1.png")
creditdata <- na.omit(creditdata)
hist(creditdata$CHK_ACCT, col="blue", main = "Histogram of Checking Account", xlab="CHK_ACCT(x)")
legend("topleft",bg = "wheat", fill="blue", legend=c("0: x<0DM", "1: 0<x<200DM","2: x>=200DM", "3:No CHK Acct"))
dev.off()

#histogram 2

png(filename = "histogram2.png")
par(mar=c(5,5,2,2))
creditdata <- na.omit(creditdata)
hist(creditdata$HISTORY, col="green", main = "Histogram of History", xlab="HISTORY")
legend("topright",bg = "wheat", fill="green", legend=c("0: 0Credits", "1: All Credits Paid back Duly","2:Existing Credits paid back Duly ", "3:Delay in paying off", "4: Critical Account"),
text.width = 1.5, cex=0.7)
dev.off()

#histogram 3
png(filename = "histogram3.png")
par(mar=c(5,5,2,2))
creditdata <- na.omit(creditdata)
hist(creditdata$EMPLOYMENT, col="Red", main = "Histogram of Employment", xlab="EMPLOYMENT")
legend("topleft",bg = "wheat", fill="red", legend=c("0: Unemployed", "1: <1yr", "2: 1<=...<4yrs","3: 4<=...<7yrs", "4: =>7yrs"))
dev.off()