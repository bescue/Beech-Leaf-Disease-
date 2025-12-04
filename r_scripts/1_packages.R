
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# Load this script every time you work on the project
# Make sure all packages load correctly
# You might need to install some (install.packages())
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

# Loading in packages ----------------------------------------------------------
#
#
#---
library(readxl) 
library(writexl)
library(sp)
library(sf)
library(tidyverse)
library(raster) # pointDistance()
library(geosphere) # radii from a fixed point
library(concaveman) # concave hull polygon
library(geodata)
library(spdep)
library(terra)
library(patchwork)
library(beepr)
library(dplyr)
library(stringr)
library(tidycensus)
library(wesanderson)
library(RColorBrewer)
library(viridis)
library(writexl)
library(cowplot)
library(segmented)
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 





# Loading in useful functions --------------------------------------------------
#
#
#---
# enables copying from R to the "clipboard" for pasting into Excel
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

# function for "is not contained within" 
'%!in%' <- function(x,y)!('%in%'(x,y))

# creates a plotting window of a specific size on a windows machine
resize.win <- function(Width=6, Height=6)
{
  # works for windows
  dev.off(); # dev.new(width=6, height=6)
  windows(record=TRUE, width=Width, height=Height)
}

# plotting theme for ggplot
theme_clean <- theme_bw()+  
  theme(panel.border = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

# standard error function
stderr <- function(x, na.rm=FALSE) {
  if (na.rm) x <- na.omit(x)
  sqrt(var(x)/length(x))
}
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
