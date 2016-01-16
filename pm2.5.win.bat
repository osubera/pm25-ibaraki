rem <- function(...) invisible(T)
rem( '
SET S=%HOMEPATH:~0,1%
"E:%S%bin%S%R%S%R-2.15.3%S%bin%S%x64%S%Rscript.exe" %~F0
"E:%S%bin%S%R%S%tmp%S%ibaraki.png"
EXIT /B
rem ')
###

toLocalEncoding <-
function(x, sep="\t", quote=FALSE, encoding="utf-8")
{
  rawtsv <- tempfile()
  write.table(x, file=rawtsv, sep="\t", quote=quote)
  result <- read.table(file(rawtsv, encoding=encoding), sep=sep)
  unlink(rawtsv)
  result
}

library(XML)
ibaraki=toLocalEncoding(readHTMLTable("http://www.taiki.pref.ibaraki.jp/data.asp",which=6,skip.rows=1,trim=T,colClasses=c("integer","character",rep("numeric",13)),encoding="shift-jis"))
pm.2.5.ibaraki<-ibaraki[!is.na(ibaraki[,12]),c(2,12)]
pm.2.5.ibaraki[,1]=as.factor(as.character(pm.2.5.ibaraki[,1]))

png("E:\\bin\\R\\tmp\\ibaraki.png")
plot(pm.2.5.ibaraki,main="PM2.5 (ƒÊg/m3)",cex.axis=0.8, las=2, xlab="")
dev.off()
