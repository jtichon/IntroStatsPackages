p.test<-function(phat,p,n,direction,alpha=0.05)
{

  z<-(phat-p)/sqrt(p*(1-p)/n)

  tail<-pnorm(z)

  if(direction=="greater")
  {
    pvalue<-1-pnorm(z)
    print(paste("H0: p is equal to",p))
    print(paste("HA: p is greater than",p))
  }
  if(direction=="less")
  {
    pvalue<-pnorm(z)
    print(paste("H0: p is equal to",p))
    print(paste("HA: p is less than",p))
  }
  if(direction=="two.sided")
  {
    pvalue<-2*pnorm(abs(z))
    print(paste("H0: p is equal to",p))
    print(paste("HA: p is not equal to",p))
  }

  statistics<-c("z","p-value","alpha")
  values<-c(z,pvalue, alpha)
  results<-data.frame(statistics,values)
  print(results)

  if(pvalue<=alpha)
  {
    print("Reject the null hypothesis.")
    if(direction=="less")
    {
      print(paste("Conclude there is sufficient evidence the true proportion is less than",p))
    }
    if(direction=="greater")
    {
      print(paste("Conclude there is sufficient evidence the true proportion is greater than",p))
    }
    if(direction=="two.sided")
    {
      print(paste("Conclude there is sufficient evidence the true proportion is different from",p))
    }
  }

  if(pvalue>alpha)
  {
    print("Fail to reject the null hypothesis.")
    if(direction=="less")
    {
      print(paste("Conclude there is insufficient evidence the true proportion is less than",p))
    }
    if(direction=="greater")
    {
      print(paste("Conclude there is insufficient evidence the true proportion is greater than",p))
    }
    if(direction=="two.sided")
    {
      print(paste("Conclude there is insufficient evidence the true proportion is different from",p))
    }
  }

}
