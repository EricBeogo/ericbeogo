library(devtools)
library(sp)
library(raster)
tnMAP<- getData(name="GADM",
                country="TUN", level=1)
library(leaflet)
library(readr)
Code <- read_delim("C:/Users/ERIC BEOGO/Desktop/projet r/dashboard/Code Delegation Tunisie.csv", 
                   ";", escape_double = FALSE, trim_ws = TRUE)
Code[24,1]="Kébili"
colnames(Code)=c("region","HASC")
a=merge(ba,Code,by.x="Gouvernorat",by.y="region")


a1=a[which(a$Date==2017),]

i=match(tnMAP$HASC_1,a$HASC)
a2=a1[i,]
d<- cbind.data.frame(a2,tnMAP$HASC_1)

m=NULL
m=leaflet(a1)
m=m%>% addProviderTiles(provider = providers$Thunderforest
                        )
labels<-sprintf("<strong>%s<br/></strong>%g<br/>",a1$Gouvernorat,a1$Production)
labels<-paste(labels)%>%lapply(htmltools::HTML)

col<-colorRampPalette(c("gray","green","blue","red","yellow"))

pal<-colorNumeric(col(268),domain = a1$Production,n=268)
m=m%>% addPolygons(data=tnMAP,
                   fillColor=~pal(a1$Production),
                   fillOpacity=10,
                   col="black",
                   weight=1.1,
                   opacity=0.7,
                   highlight=highlightOptions(weight=4.0,
                                              color="#FFFFFF",
                                              fillOpacity = 0.7,
                                              bringToFront = TRUE),
                   label=labels,
                   labelOptions=labelOptions( style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "15px",
                                              direction = "auto"))
m

a2=a[which(a$Gouvernorat=="Ariana"),]
plot(a2$Production)
a21=ts(a2$Production,start = 2006,frequency = 1)
plot(a21,plot.type="m")

hist(a2$Production)

require(PerformanceAnalytics) 
par(mfrow=c(3, 1)) 
  xts::plot.xts(as.xts(a21)) 
PerformanceAnalytics::chart.TimeSeries(as.xts(a21)) 
plot(a21)




library(tidyr)
library(dplyr)
library(ggplot2)
ggplot(a2, aes(x = a2$Date, y = a2$Production))+ 
  geom_area(aes(), 
            alpha = 0.5, position = position_dodge(0.8)) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))+ ggtitle(a2$Gouvernorat) + xlab('Year') + ylab('production')
