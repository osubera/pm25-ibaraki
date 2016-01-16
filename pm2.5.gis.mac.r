library(maptools)

cities.japan <- readShapeSpatial('~/var/gis/gm-jpn-all_u_2_1/polbnda_jpn.shp')
cities.ibaraki <- cities.japan[cities.japan$nam == 'Ibaraki Ken',]

library(XML)

parsePlacemarks <- function(node) {
  x <- xmlToList(node)
  c(splitNameId(x[['name']]), splitLongLat(x[['Point']][['coordinates']]))
}

splitNameId <- function(x) {
  # fixed delimiter located at 4
  at <- 4
  id <- substring(x, 1, at - 1)
  name <- substring(x, at + 1)
  c(id=id, name=name)
}

splitLongLat <- function(x) {
  # three values are delimited by a comma
  coordinates <- strsplit(x, ',')[[1]]
  c(longitude=coordinates[1], latitude=coordinates[2])
}

observatories <- xmlInternalTreeParse('~/var/gis/obs.ibaraki.kml')
placemarks <- xpathSApply(observatories, '//kml:Placemark', parsePlacemarks)
placedata <- data.frame(id=placemarks['id',], obs.name=placemarks['name',], 
  longitude=as.numeric(placemarks['longitude',]), 
  latitude=as.numeric(placemarks['latitude',]))

url.ibaraki <- 'http://www.taiki.pref.ibaraki.jp/data.asp'
main <- readHTMLTable(url.ibaraki, which=4, trim=T, header=F, skip.rows=2:48)[1,1]
air.ibaraki <- readHTMLTable(url.ibaraki, which=6, skip.rows=1, trim=T, 
  colClasses=c('integer', 'character', rep('numeric', 13)))
pm.2.5.ibaraki <- air.ibaraki[!is.na(air.ibaraki[,12]), c(1,2,12)]
names(pm.2.5.ibaraki) <- c('id', 'name', 'pm2.5')
for(i in 1:2)
  pm.2.5.ibaraki[,i] <- as.factor(as.character(pm.2.5.ibaraki[,i]))


pm25 <- merge(pm.2.5.ibaraki, placedata, by='id')
pm25.all <- merge(pm.2.5.ibaraki, placedata, by='id', all.x=T)
pm25.noplace <- pm25.all[is.na(pm25.all[,'obs.name']),]

to.cex <- function(x) {
  ifelse(x > 5, x/10, 0.5)
}

to.col <- function(x) {
  x.3 <- ifelse(x < 30, 1, ifelse(x < 60, 2, 3)) 
  c('forestgreen', 'darkorange', 'deeppink')[x.3]
}

quartzFonts(HiraMaru=quartzFont(rep("HiraMaruProN-W4", 4)))
par(family="HiraMaru")

plot(cities.ibaraki, border='gray', main=main)
with(pm25, text(x=longitude, y=latitude, labels=name, col='royalblue', pos=1, cex=0.7))
points(latitude ~ longitude, pm25, cex=to.cex(pm2.5), col=to.col(pm2.5))
legend(x='topleft', legend=with(pm25, paste('    ', name)), cex=0.7, bty='n')
legend(x='topleft', legend=with(pm25, format(pm2.5)), xjust=1, cex=0.7, bty='n')

