anova_stat <- function(data, grpcol=1, varcol=3){
  var=as.vector(unique(data[,varcol]))
  d=data.frame()
  for (i in 1:length(var)) {
    temp=subset(data, data$variable==var[i])
    head(temp)
    stat=aov(temp$value~temp[,grpcol])
    a=as.data.frame(matrix(unlist(summary(stat)),nrow=1))
    new_d=data.frame(Var=var[i],F.value=a[,7],P.value=a[,9])
    d=rbind.data.frame(d,new_d)
  }
  return(d)
}
