CBloom4_24 <-read.table("Cherryblossom_2004-2024.txt",header =TRUE,sep="\t", fill = TRUE)

#Dropping unwanted variables
CBloom4_24<-CBloom4_24[ , -c(1,3)]

# Create a new variable by summing two existing variables
    #CBloom$OceTemp <- 2*CBloom$El.Nino+CBloom$La.Nina

#Dropping rows with NA
CBloom4_24<-CBloom4_24[-c(22,23,24,25,26),]
head(CBloom4_24)

