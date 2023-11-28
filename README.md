In your computer, follow these steps:
1. Open RStudio
2. Create en R script and copy the following sintax
   
3. Verify the installation of these packages
   
```
library(Metrics); library(forecast); library(forecTheta);library(tsfgrnn);library(tsintermittent); 
library(prophet);library(yager);library(dplyr);library(univariateML);library(tseries);library(nnfor); library(thief);library(tidyverse);library(Metrics); library(svDialogs)


4. Set Working Directory
   
```
e.g. :   setwd("E:\\alPCA_menu")
 

5. Upload the data on which you want alPCA predictions, previously saved as an .rds file in your working directory

```
#e.g.: x <- readRDS("ford.rds")

6. Run this code next:

```
#(url of the execution file in Github.... to be changed to Carlos repository)
url="https://raw.githubusercontent.com/asunmayoral/series/main/alPCA_menu.r"
resul <- source(url)

