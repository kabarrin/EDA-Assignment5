read.csv("https://raw.githubusercontent.com/kabarrin/EDA-Assignment5/main/Master.csv")
Master<-read.csv("https://raw.githubusercontent.com/kabarrin/EDA-Assignment5/main/Master.csv")

#Remove rows including "NA"
dat<-na.omit(Master)
#Filter rows where UOMCube is greater than 2 and any less than 0
dat<-dat[dat$UomCube>0&dat$UomCube<2,]
#Filter out rows where UOMWeight is greater than 50 and any less than 0
dat<-dat[dat$UomWeight>0&dat$UomWeight<50,]


        

#scatterplots of UnitsPerCase and UomWeight
plot(dat$UomWeight~dat$UnitsPerCase,
     pch=19,
     cex=0.75,
     xlab="Units Per Case",
     ylab="Weight",
     main="Weight v. Units Per Case",
     col="gray50")
   
plot(dat$UomWeight)

#barchart to show frequuency of Uom levels
plot(dat$Uom,
     main="Units of Measure",
     col=c("lightgray","gray","darkgray","black"),
     border="red",
     ylab="Count",
     xlab="UOM Type",
     font.lab=4,
     ylim=c(0,3000))

na.omit(dat$Commodity) 
dat$Commodity=as.factor(dat$Commodity)
droplevels(dat$Commodity)
plot(dat$Commodity,
     main="Frequency of Commodities",
     sub="Frequency per Type of Commodity",
     cex.main=1.5,
     cex.sub=1.25,
     font.sub=4,
     font.main=2,
     cex.lab=1.15,
     col=rainbow(nlevels(dat$Commodity)),
     xlab="Type of Commodity",
     ylab="Frequency/Count")


#assign Flow as a factor
dat$Flow<-as.factor(dat$Flow)
str(dat$Flow)
#side by side plot of UomCube, Uom, and Flow
boxplot(dat$UomCube~dat$Flow,
        col="turquoise",
        border="navyblue",
        main="UOM Cube/Flow Types",
        pch=15,
        cex=.4,
        ylab="UOM Cube",
        xlab="Type of Flow")


#Filter to only "direct to store items"
datDD<-dat[dat$Flow=="DD",]

#boxplot of UomWeight
boxplot(datDD$UomWeight,
        main="Weight",
        col="red",
        border="black",
        pch=21,
        cex=1.2,
        bg="red",
        ylab="Weight",
        sub="Direct to Store Items Only")
boxplot.stats(datDD$UomWeight)#outliers appear to be those greater than 14.5
boxplot.stats(datDD$UomWeight)$out #outliers appear to be 20.0,29.0, and 17.8

#event of Weight reaching 30 or plastic/disposable items having UnitsPerCase over 1000
#would remove lines of outliers associated with plastic/disposable items with UnitsPerCase at 1000
datDD<-datDD[!(datDD$UomWeight==29.0)&!(datDD$UomWeight==17.8),]

str(datDD)

#histogram of UomWeight
hist(datDD$UomWeight,
     breaks=5,
     main="Weight Frequencies",
     xlab="Weight",
     col=rainbow(4),
     ylim=c(0,20))

#create dotchart of weight and sku number
dotchart(datDD$UomWeight,datDD$ï..SkuNbr,
         pch=19,
         main="Weight",
         cex.main=2,
         col="black",
         ylab="SkuNumber",
         xlab="Weight",
         cex=.5)
#identify the SkuNbr of the item with the maximum weight
maxweight<-datDD$ï..SkuNbr[datDD$UomWeight==max(datDD$UomWeight)]
maxweight #Sku Number of item with maxweight is 06992111

#filter Uom to EA and CA
datstripchart<-dat[dat$Uom%in%c("CA","EA"),]
datstripchart<-droplevels(datstripchart)
droplevels(datstripchart$Uom)
is.factor(dat$Uom)
#Create stripchart
stripchart(datstripchart$UomWeight~datstripchart$Uom,
           pch=5,
           cex=.5,
           xlab="Weight", 
           main="Stripchart for CA and EA UOMs",
           ylab="UOM Type")

