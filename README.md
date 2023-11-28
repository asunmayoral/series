In your computer, follow these steps:

1. Open R or RStudio in your computer.
2. Create en R script on your working directory and set it down. Copy all the code in this page.

```
# Set your working directory
# e.g. setwd("E:\\alPCA_menu")
```
   
3. Verify the installation of these packages and execute loading
   
```
library(Metrics); library(forecast); library(forecTheta);library(tsfgrnn);library(tsintermittent); 
library(prophet);library(yager);library(dplyr);library(univariateML);library(tseries);library(nnfor); library(thief);library(tidyverse);library(Metrics); library(svDialogs)
```

4. Upload the data on which you want alPCA predictions, previously saved as an .rds file in your working directory

```
# e.g.: x <- readRDS("ford.rds")
```

5. Execute prediction, saved in 'resul':

```
#(url of the execution file in Github.... to be changed to Carlos repository)
url="https://raw.githubusercontent.com/asunmayoral/series/main/alPCA_menu.r"
resul <- source(url)
```
