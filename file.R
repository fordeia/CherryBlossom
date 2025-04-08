#1. Loading Packages
library(randomForest)

#2. Loading Data
CBloom4_24 <-read.table("Cherryblossom_2004-2024.txt",header =TRUE,sep="\t", fill = TRUE)

#3. Data preparation
# Create a new variable by summing two existing variables
CBloom4_24$OceTemp <- 2*CBloom4_24$EL.NINO+CBloom4_24$LA.NINA

#Dropping unwanted variables
CBloom4_24<-CBloom4_24[ , -c(1,3, 4, 5, 6,10,11,12,13,14,15,16)]

#Dropping rows with NA
CBloom4_24<-CBloom4_24[-c(22,23,24,25,26),]

#3. Fitting the random forest model with original dataset. 
set.seed(1237)
CBloom4_24.rg <- randomForest(PEAK ~ ., data=CBloom4_24, importance=TRUE,
                        proximity=TRUE)

sqrt(sum((CBloom4_24.rg$predicted - CBloom4_24$PEAK)^2) / nrow(CBloom4_24))

#4. Generating the bootstrap sample
set.seed(25)
bootdataCBloom4_24=CBloom4_24[sample(nrow(CBloom4_24), 1000, replace=TRUE), ]
head(bootdataCBloom4_24)
nrow(bootdataCBloom4_24)
str(bootdataCBloom4_24)

#5. Creating the training and testing datasets
#Splitting the data 70:30 
set.seed(25)
samp <- sample(nrow(bootdataCBloom4_24), 0.7 * nrow(bootdataCBloom4_24))
train <- bootdataCBloom4_24[samp, ]
nrow(train)
test <- bootdataCBloom4_24[-samp, ]
nrow(test)

#6. Training the random forest model with the bootstrap dataset
set.seed(25)
CBloom4_24.rgBoot <- randomForest(PEAK ~ ., data=train, importance=TRUE,
                        proximity=TRUE)

sqrt(sum((CBloom4_24.rgBoot$predicted - train$PEAK)^2) / nrow(train))

#Extracting feature importance

importance(CBloom4_24.rgBoot)
importance(CBloom4_24.rgBoot, type=1)

