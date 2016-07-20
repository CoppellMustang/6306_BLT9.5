library(fpp) #fpp package must be installed first

data(elecequip)
plot(elecequip)

fitd <- decompose(elecequip)
plot(fitd)

eeadj <- seasadj(fitd)
plot(eeadj)

elecequip2 <- ts(c(elecequip[1:54],elecequip[55]+200,elecequip[56:191]),start=c(1978,1),frequency=12)
plot(elecequip2)

















plot(elecequip, col="gray",
     main="Electrical equipment manufacturing",
     ylab="New orders index", xlab="")
lines(fit$time.series[,2],col="red",ylab="Trend")

install.packages("tseries")

library(tseries)

SNPdata <- get.hist.quote('^gspc',quote="Close")

SNPret <- log(lag(SNPdata)) - log(SNPdata)

SNPvol <- sd(SNPret) * sqrt(250) * 100



## volatility
get
Vol <- function(d, logrets)
{
  
  var = 0
  
  lam = 0
  
  varlist <- c()
  
  for (r in logrets) {
    
    lam = lam*(1 - 1/d) + 1
    
    var = (1 - 1/lam)*var + (1/lam)*r^2
    
    varlist <- c(varlist, var)
    
  }
  
  sqrt(varlist)
}


# Recreate Figure 6.12 in the text on page 155

volest <- Vol(10,SNPret)

volest2 <- Vol(30,SNPret)

volest3 <- Vol(100,SNPret)

plot(volest,type="l")

lines(volest2,type="l",col="red")

lines(volest3, type = "l", col="blue")
