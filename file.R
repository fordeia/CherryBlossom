CBloom4_24 <-read.table("Cherryblossom_2004-2024.txt",header =TRUE,sep="\t", fill = TRUE)

# Create a new variable by summing two existing variables
CBloom4_24$OceTemp <- 2*CBloom4_24$EL.NINO+CBloom4_24$LA.NINA

#Dropping unwanted variables
CBloom4_24<-CBloom4_24[ , -c(1,3, 4, 5, 6,10,11,12,13,14,15,16)]

#Dropping rows with NA
CBloom4_24<-CBloom4_24[-c(22,23,24,25,26),]
head(CBloom4_24)

