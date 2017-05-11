library(readr)
library(dplyr)

dot<-read_csv("C:/home/data/DOT_04-26-2017 02-39-44-26_panel.csv")
head(dot)
names(dot)
dot2016<-dot %>% filter(`Time Period`=="2016")
names(dot2016) <- c("country1","country1_code","country2","country2_code","period",
                    "exports_free","status1",
                    "imports_cost","status2",
                    "imports_free","status3",
                    "trade_bal","status4",
                    "x14")
edges<-dot2016[,c(1:4,6,8,10)]
edges$exports_free[is.na(edges$exports_free)]<-0
edges$imports_cost[is.na(edges$imports_cost)]<-0
edges$imports_free[is.na(edges$imports_free)]<-0
edges

countries <- edges %>% select(country1,country1_code) %>% unique() %>% arrange(country1_code)

excludes<-c(1,80,92,110,200,205,440,505,903,910,998) # group codes

singles<- edges %>% filter(!country1_code %in% excludes & !country2_code %in% excludes )
countries <- singles %>% select(country1,country1_code) %>% unique() %>% arrange(country1_code)


singles$imports_free <- singles$imports_free %>% as.numeric() 
singles <- singles %>% mutate(log_vol = log(exports_free + imports_free + imports_cost + 1))
cid<-c(singles$country1,singles$country2) %>% unique() %>% factor()

singles$log_vol %>% hist()

library(Matrix)
singles <- singles %>% mutate(c1 = which(levels(cid)==country1))
spMatrix()


