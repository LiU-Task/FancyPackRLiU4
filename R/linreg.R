linreg<-setRefClass("linreg",
  fields=list(formula="formula",
              data="data.frame",
              guo="list"),
  
  
  methods=list(
    
    #Predefined methods
    initialize=function(formula="formula",data="data.frame") {
       if(!class(formula)=="formula") stop("The formula should be a formula object!")
       if(!is.data.frame(data)) stop("The data should be a data frame object!")
      
       library(ggplot2)
       ##install.packages("gridExtra")
      
       va=all.vars(formula)
       x=model.matrix(formula,data)
       y=data[[va[1]]]  
       tem=as.character(formula)
       gs=paste(tem[2],tem[1],tem[3],sep="")
       shu=as.data.frame(cbind(x,y))  ## Data prepared

       co=solve(t(x)%*%x,t(x)%*%y) #1.Estimated Coefficients
       fi=x%*%co                   #2.Fitted values
       re=y-fi                     #3.Residuals
       df=length(y)-(length(all.vars(formula))-1) #4.Freedom degree
       red=t(re)%*%re/df           #5.Variance of residuals
       cod=rep(red,length(co))*diag(solve(t(x)%*%x))   #6.Variance of coefficients
       cot=co/sqrt(cod)            #7.t-values of coefficients
       tpv=1-pt(cot,df)            #8.Possibility of testing wrong
       guo<<-list(Coefficients=co,FittedValues=fi,Residuals=re,FreedomDegree=df,VarianceResiduals=red,VarianceCoefficients=cod,Tcoefficients=cot,Possibility=tpv,X=x,Y=y,Formula=gs,Data=shu,VariableNames=va)
       return (guo)},
  
      
    #Required methods
    print=function() {
       te<-dimnames(guo$Coefficients)[[1]]
       cat(paste("Call:\nFormula=",guo$Formula,sep=""))
       cat("\n\nCoefficients:\n")
       for(i in 1:length(te)) cat(sprintf("%*s",20,te[i]))
       cat("\n")
       for(i in 1:length(te)) cat(sprintf("%*s",20,guo$Coefficients[i]))},
    
    plot=function(){
      if(length(guo$VariableNames)>2) stop("Too many independent variables!")
      
      g1<-ggplot(guo$Data)+
         geom_point(aes(x=guo$FittedValues,y=guo$Residuals),shape=1)+
         xlab(paste("Fitted values\n",guo$Formula,sep=""))+
         ylab("Residuals")+
         ggtitle("Residuals VS Fitted")+
         theme(plot.title = element_text(hjust = 0.5))+
         stat_smooth(aes(x=guo$FittedValues,y=guo$Residuals),method = "lm", col = "red")
      
      rest<-sqrt(abs(guo$Residuals/sd(guo$Residuals)))
      g2<-ggplot()+
        geom_point(aes(x=guo$FittedValues,y=rest),shape=1)+
        xlab(paste("Fitted values\n",guo$Formula,sep=""))+
        ylab(expression(sqrt(abs(StandardizedResiduals))))+
        ggtitle("Scale-Location")+
        theme(plot.title = element_text(hjust = 0.5))+
        stat_smooth(aes(x=guo$FittedValues,y=rest),method = "lm", col = "red")
      
        readline(prompt="Press any key to continue")
        g1     
        readline(prompt="Press any key to continue")
        g2},
    
    resid=function() return(as.vector(guo$Residuals)),
    pred=function() return(as.vector(guo$FittedValues)),
    coef=function() {te<-as.vector(guo$Coefficients)
                     ge<-dimnames(guo$Coefficients)[[1]]
                     names(te)<-ge
                     return(te)},
    summary=function(){}
  ))
