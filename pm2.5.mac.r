library(XML)
ibaraki=readHTMLTable("http://www.taiki.pref.ibaraki.jp/data.asp",which=6,skip.rows=1,trim=T,colClasses=c("integer","character",rep("numeric",13)))
pm.2.5.ibaraki<-ibaraki[!is.na(ibaraki[,12]),c(2,12)]
pm.2.5.ibaraki[,1]=as.factor(as.character(pm.2.5.ibaraki[,1]))
quartzFonts(HiraMaru=quartzFont(rep("HiraMaruProN-W4", 4)))
par(family="HiraMaru")
plot(pm.2.5.ibaraki,main="PM2.5 Air Polution Come From China (μg/㎥)",cex.axis=0.8, las=2, xlab='')
