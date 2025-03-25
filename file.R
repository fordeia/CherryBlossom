#Setting the directory
#setwd("C:/Users/fordeia/CherryBlossom")

#CBloom <-read.table("cherryBlossomData.txt",header =TRUE,sep="\t", fill = TRUE)
CBloom4_24 <-read.table("Cherryblossom_2004-2024.txt",header =TRUE,sep="\t", fill = TRUE)

#Dropping unwanted variables
#CBloom<-CBloom[ , !(names(CBloom) %in% "X")]

# Create a new variable by summing two existing variables
    #CBloom$OceTemp <- 2*CBloom$El.Nino+CBloom$La.Nina

#Dropping rows with NA
CBloom4_24<-CBloom4_24[-c(22,23,24,25,26),]

#Bootstrap sampling

# Import library for bootstrap methods
library(boot)
# Import library for plotting
library(ggplot2)
diff.fun <- function(CBloom4_24, idx)
{
  df <- CBloom4_24[idx, ]
 
  # Find the mean difference for the weights 
  # for men and women
  c(mean(df[, 2])- mean(df[, 3]))
}

# Setting the seed for 
# reproducability of results
set.seed(25)
 
# Calling the boot function with the dataset
# our function and no. of rounds
bootstrap <- boot(CBloom4_24, diff.fun, R = 1000)

# Display the result of boot function
bootstrap
