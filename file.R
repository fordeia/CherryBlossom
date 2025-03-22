#Setting the directory
setwd("C:/Users/fordeia/CherryBlossom")

#CBloom <-read.table("cherryBlossomData.txt",header =TRUE,sep="\t", fill = TRUE)
CBloom4_24 <-read.table("Cherryblossom_2004-2024.txt",header =TRUE,sep="\t", fill = TRUE)

#Dropping unwanted variables
#CBloom<-CBloom[ , !(names(CBloom) %in% "X")]

# Create a new variable by summing two existing variables
    #CBloom$OceTemp <- 2*CBloom$El.Nino+CBloom$La.Nina

#head(CBloom)
head(CBloom4_24)

