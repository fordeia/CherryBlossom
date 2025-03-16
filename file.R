CBloom <-read.table("cherryBlossomData.txt",header =TRUE,sep="\t", fill = TRUE)

#Dropping unwanted variables
CBloom<-CBloom[ , !(names(CBloom) %in% "X")]

# Create a new variable by summing two existing variables
    CBloom$OceTemp <- 2*CBloom$El.Nino+CBloom$La.Nina

head(CBloom)
