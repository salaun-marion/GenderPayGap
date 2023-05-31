# GenderPayGap Shiny Application
Made by Marion, Ainhoa and Jenny :)


*Note : you can open this README into a markdown reader to get a better visualisation*

Welcome to our Shiny Application project about the Gender Pay Gap among 27 european countries from 2010 until 2021. 

## Files contained

Few information about how the directory works :

-  `app.R`, `GenderPayGap.RProj` : don't touch to these files, they are critical for the good work of the project
-  `pay_gap_Europe.csv` : this is our dataset, you don't need to edit it, to use this application
-  `Extra Code` directory : you will find our draft file for coding this application (like moddeling, linear correlation, train/trend try, etc ...) which are relevant according to us to understand our process and where you can find extra information

## How to launch the application ?

You can open this directory on R Studio and then from the `app.R` file, you have just to click on `Run App` in the right corner of the working panel. Then, enjoy !

## Which packages do I need to use ?

Thanks to the following command (line 10-12 in `app.R` file), you will not need to install manually all the packages : 

`if(!require('pacman'))install.packages('pacman')
pacman::p_load(shiny,dplyr,tidyverse,ggplot2,maps,rworldmap,shinydashboard,reshape2,RColorBrewer,hexbin,bslib,gridExtra,readr,ERSA,tidymodels)`
               
If by any chance, this command will not work, we invite you to install manually the package, thanks to the `install.package('nameofthepackage')` command. You can replace `nameofthepackage` by the following names packages :

- shiny
- dplyr
- tidyverse
- ggplot2
- maps
- rworldmap
- shinydashboard
- reshape2
- RColorBrewer
- hexbin
- bslib
- gridExtra
- readr
- ERSA
- tidymodels

## Which version of R was used to create the application ?

**R version 4.3.0 (2023-04-21)**