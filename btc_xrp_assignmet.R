library(readr)

xrp=read_csv("C:/Users/alanj/Downloads/XRP Historical Data.csv")
btc=read_csv("C:/Users/alanj/Downloads/Bitcoin Historical Data.csv")


xrp$Date=as.Date(xrp$Date,format = "%d-%m-%Y")
btc$Date=as.Date(btc$Date,format="%d-%m-%Y")
xrp
btc

#calculation of returns
names(xrp)
library(dplyr)

xrp=xrp%>%
  mutate(daily_return = (Price - lag(Price))/lag(Price))

btc=btc%>%
  mutate(daily_return = (Price - lag(Price))/lag(Price))

sd_xrp=sd(xrp$daily_return,na.rm=TRUE)
sd_btc=sd(btc$daily_return,na.rm=TRUE)
sd_xrp
sd_btc
btc
xrp
#calcu;ation of cumulative returns
xrp=xrp%>%
  mutate(daily_return = ifelse(is.na(daily_return),0,daily_return),
         cumulative_return =cumprod(1+daily_return))
xrp

btc=btc%>%
  mutate(daily_return=ifelse(is.na(daily_return),0,daily_return),
         cumulative_return=cumprod(1+daily_return))
btc

library(ggplot2)
ggplot()+
  geom_line(data = xrp,aes(x=Date,y=Price,color="xrp"))+
  geom_line(data = btc,aes(x=Date,y=Price,color="btc"))+
  labs(title="price of BTC  and XRP over year",
       x="Date",
       y="Price")

#the price of BTC and XRP has high difference so it shows a straight line plot
#plot returns

ggplot()+
  geom_line(data = xrp,aes(x=Date,y=daily_return,color="xrp"))+
  geom_line(data = btc,aes(x=Date,y=daily_return,color="btc"))+
  labs(title="daily returns of BTC  and XRP",
       x="Date",
       y="daily return")
#plot cumulative returns
ggplot()+
  geom_line(data = xrp,aes(x=Date,y=cumulative_return,color="xrp"))+
  geom_line(data = btc,aes(x=Date,y=cumulative_return,color="btc"))+
  labs(title="cumulative returns of BTC  and XRP",
       x="Date",
       y="cumulative return")

#XRP is riskier than BTC due to its sharp price swings, while BTC shows a steadier
#upward trend with lower volatility.

# but also considering the current btc price fluctuation it is also risky as btc
# price is also fluctuating a lot

risk_free_rate=0.06/365
sharp_ratio_xrp=(mean(xrp$daily_return)-risk_free_rate)/sd_xrp
sharp_ratio_btc=(mean(btc$daily_return)-risk_free_rate)/sd_btc
sharp_ratio_xrp
sharp_ratio_btc

#Both XRP and BTC have negative Sharpe ratios, which means that the returns are not worth the risk.
#XRP (-0.0763) has a lower Sharpe ratio than BTC
#BTC (-0.0588) which is comparitivey better than XRP

# it is better to invest in BTC than XRP as it has a higher cumulative return

#XRP has a higher standard deviation of daily returns (0.04409649) compared to 
#BTC (0.02698727), which indicates that XRP is riskier than BTC.


