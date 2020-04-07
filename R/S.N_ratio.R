#'Ignore raman signal using threshold
#'
#'using melt data
#'
#'@param raman,  spectrumdata and set threshold
#'@return check ramna intensity whether signal or not and noise clearing
#'@export
S.N_ratio <- function(data, Threshold=0.0010){
  library(tidyverse)
  data=data %>%
    mutate("SigNoi"=paste(ifelse(abs(data$value)<=noise,"Noise","Signal"))) %>%
    mutate("Nocan"=ifelse(abs(data$value)<=noise,0,as.numeric(data$value)))

}
