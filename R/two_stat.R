#'Perform t-test according to normality
#'
#'using use data
#'@param data:input data, grpcol:column number of group, varcol: column of number of variable, var.opt=Var.equal in t.test
#'@return compare two group with p.val according to variable
#'@export
two_stat <- function(data, grpcol=1, varcol=3, var.opt=T){
  var=as.vector(unique(data[,varcol]))
  d=data.frame()
  for (i in 1:length(var)) {
    #var[i]
    temp=subset(data, data$variable==var[i])
    grp=as.vector(unique(temp[,grpcol]))
    g1=subset(subset(temp,temp[,grpcol]==grp[1]))
    g2=subset(subset(temp,temp[,grpcol]==grp[2]))

    s1=shapiro.test(g1$value)
    s2=shapiro.test(g2$value)
    var.test(g1$value, g2$value)

    if(s1$p.value>0.05&s2$p.value>0.05){
      st=t.test(g1$value,g2$value, var.equal = var.opt)
      new_row=data.frame(var[i],round(s1$p.value,2),round(s2$p.value,2),"t",round(st$p.value,3))%>%
        `colnames<-`(c("variable","Nor_s1","Nor_s2","Method","p.value"))
    }else{
      st=wilcox.test(g1$value,g2$value, exact = F)
      new_row=data.frame(var[i],round(s1$p.value,2),round(s2$p.value,2),"wilcox", round(st$p.value,3))%>%
        `colnames<-`(c("variable","Nor_s1","Nor_s2","Method","p.value"))
    }
    d=rbind.data.frame(d,new_row)
  }
}
