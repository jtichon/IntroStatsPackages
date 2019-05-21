z.test<-function(x,mu,sigma,n,direction,alpha=0.05)
{

  z<-(x-mu)/(sigma/sqrt(n))

  tail<-pnorm(z)

  if(direction=="greater")
  {
    pvalue<-1-pnorm(z)
    print(paste("H0:Mu is equal to",mu))
    print(paste("HA:Mu is greater than",mu))
  }
  if(direction=="less")
  {
    pvalue<-pnorm(z)
    print(paste("H0:Mu is equal to",mu))
    print(paste("HA:Mu is less than",mu))
  }
  if(direction=="two.sided")
  {
    pvalue<-2*pnorm(abs(z))
    print(paste("H0:Mu is equal to",mu))
    print(paste("HA:Mu is not equal to",mu))
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
      print(paste("Conclude there is sufficient evidence the mean is less than",mu))
    }
    if(direction=="greater")
    {
      print(paste("Conclude there is sufficient evidence the mean is greater than",mu))
    }
    if(direction=="two.sided")
    {
      print(paste("Conclude there is sufficient evidence the mean is different from",mu))
    }
  }

  if(pvalue>alpha)
  {
    print("Fail to reject the null hypothesis.")
    if(direction=="less")
    {
      print(paste("Conclude there is insufficient evidence the mean is less than",mu))
    }
    if(direction=="greater")
    {
      print(paste("Conclude there is insufficient evidence the mean is greater than",mu))
    }
    if(direction=="two.sided")
    {
      print(paste("Conclude there is insufficient evidence the mean is different from",mu))
    }
  }

}
