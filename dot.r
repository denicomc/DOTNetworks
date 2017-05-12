library(readr)
library(dplyr)
library(Matrix)
library(stringr)
library(igraph)
library(ggplot2)
library(tidyr)
library(lubridate)

dot<-read_csv("~/Projects/TradeNetworks/data/DOT_04-26-2017 02-39-44-26_panel.csv")
dot<-dot[,-14]
names(dot) <- c("country1","country1_code","country2","country2_code","period",
                    "exports_free","status1",
                    "imports_cost","status2",
                    "imports_free","status3",
                    "trade_bal","status4")
dot$period %>% 
dot_monthly<-dot %>% filter(str_detect(period,"M") & period >= "1999M12")

edges<-dot_monthly[,c(1:6,8,10)]
edges$exports_free[is.na(edges$exports_free)]<-0
edges$imports_cost[is.na(edges$imports_cost)]<-0
edges$imports_free[is.na(edges$imports_free)]<-0

countries <- edges %>% select(country1,country1_code) %>% unique() %>% arrange(country1_code)

excludes<-c(1,110,170,200,205,440,505,603,898,899,901,903,910,
            605,405,998,80,92) # group codes

singles<- edges %>% filter(!country1_code %in% excludes & !country2_code %in% excludes )
countries.singles <- singles %>% select(country1,country1_code) %>% unique() %>% arrange(country1_code)
singles$imports_free <- singles$imports_free %>% as.numeric() 
singles <- singles %>% mutate(log_vol = log(exports_free + imports_free + imports_cost + 1)) %>% filter(!is.na(log_vol) & log_vol>0)




us_cent<-function(country1,country2,log_vol){
  cid<-c(country1,country2) %>% unique() %>% factor()
  us_cid<-which(levels(cid)=="United States")
  c1<-sapply(country1,function(x){ which(levels(cid)==x) })
  c2<-sapply(country2,function(x){ which(levels(cid)==x) })
  D<-spMatrix(nrow = length(cid), ncol = length(cid), i = c1, j = c2,x = log_vol)
  D <- (D + t(D))/2
  g<-graph_from_adjacency_matrix(D,mode="directed",weighted=T)
  cent<-eigen_centrality(g,directed = F)
  cent$vector[us_cid]
}
s<-singles %>% select(country1,country2,period,log_vol) %>% group_by(period)
ecs<-s %>% summarize(cent=us_cent(country1,country2,log_vol))

date<-ecs$period %>% str_split_fixed("M",2)
date<-data_frame(year=date[,1],month=date[,2])
ecs<- cbind(ecs,date)
ecs<-ecs %>% mutate(year=as.numeric(year),month=as.numeric(month)) %>% arrange(year,month)
ecs<-ecs %>% mutate(date=as.Date(sprintf("%d-%02d-01",year,month)))

ggplot(ecs,aes(date,log(cent)-log(lag(cent)))) + geom_line()

library(quantmod)

getSymbols("GDP",src='FRED',from="2000-01-01")

ecs.q<-ecs %>% mutate(q=quarter(date)) %>% filter(date >= as.Date("2000-01-01")) %>% group_by(year,q) %>% summarize(cent=mean(cent))
ecs.q$cent %>% plot(type="l")
gdp<-GDP['2000-01/'] %>% as.numeric()
gdpg<-log(gdp) - log(lag(gdp))

model<-data_frame(y=gdpg, centg = log(ecs.q$cent) - log(lag(ecs.q$cent)), cent=ecs.q$cent, year=ecs.q$year, q=factor(ecs.q$q), w=.90^(69:1)  )
fit1<-lm(y ~ lag(y) + q ,model,weights = w )
summary(fit1)
fit2<-lm(y ~ lag(y) + lag(centg)  + q,model,weights = w  )
summary(fit2)
out<-data.frame(y=model$y,
                y.hat2=c(NA,NA,fitted(fit2)),
                y.hat1=c(NA,NA,fitted(fit1)))
out %>% mutate(id=row_number()) %>% 
  gather(key=k,value=v,-id) %>%  ggplot(aes(id,v,group=k,colour=k)) + geom_line()
