graph.z<-function(a=-4,b=4,c=4, d=4)
{
z=seq(-4,4,length=200)
y=dnorm(z,mean=0,sd=1)
plot(z,y,type="l",lwd=2,col="red", xaxt='n')

if(a!=-4)
{
  axis(1, at = c(0,a), las=2)
}

if(b!=4)
{
axis(1, at = c(0,b), las=2)
}

z1=seq(a,b,length=200)
y=dnorm(z1,mean=0,sd=1)
polygon(c(a,z1,b),c(0,y,0),col="peachpuff")

if(c<4)
{
z2=seq(c,d,length=200)
y=dnorm(z2,mean=0,sd=1)
polygon(c(c,z2,d),c(0,y,0),col="peachpuff")
}

if(c!=-4)
{
  axis(1, at = c(0,c), las=2)
}

if(d!=4)
{
  axis(1, at = c(0,d), las=2)
}

}
