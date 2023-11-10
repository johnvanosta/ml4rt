
### Commence Radio Tracking EU R Shiny analysis app
install.packages('shiny')

library('shiny')

runGitHub('radiotrackingeu/logger_app')

#read raw radiotracking data 
raw_data<-readRDS("C:/Users/John/Google Drive/PhD/Thesis/Chapter 3 ML Method/Papers/Gottwald 2019 supplementary/radiotracking_data_2019_06_27.rds")
