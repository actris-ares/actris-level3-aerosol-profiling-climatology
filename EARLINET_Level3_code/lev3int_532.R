#caricare pacchetto isotone per weighted median e radiant.data per weighted standard deviation

load("Lev3Int_e.Rda")
load("Lev3Int_b.Rda")

db_e=db_int_e[db_int_e$Wavelength=="0532",]
db_b=db_int_b[db_int_b$Wavelength=="0532",]

#medie mensili

db_e_m=db_e[,c(2,5:6,8:20)]
db_b_m=db_b[,c(2,5:6,8:20)]

db_e_m_mean=aggregate(db_e_m[4:16],by=list(db_e_m$Station,db_e_m$Year,db_e_m$Month),FUN=mean,na.rm=T)
db_b_m_mean=aggregate(db_b_m[4:16],by=list(db_b_m$Station,db_b_m$Year,db_b_m$Month),FUN=mean,na.rm=T)

nms=c("Station","Year","Month")
colnames(db_e_m_mean)[1:3]=nms
colnames(db_b_m_mean)[1:3]=nms

db_e_m_mean=db_e_m_mean[order(db_e_m_mean$Station,db_e_m_mean$Year,db_e_m_mean$Month),]
db_e_m_mean[is.na(db_e_m_mean)]=NA
rownames(db_e_m_mean)=1:nrow(db_e_m_mean)
db_b_m_mean=db_b_m_mean[order(db_b_m_mean$Station,db_b_m_mean$Year,db_b_m_mean$Month),]
db_b_m_mean[is.na(db_b_m_mean)]=NA
rownames(db_b_m_mean)=1:nrow(db_b_m_mean)

cnt=function(x)
{s=sum(!is.na(x))
return(s)}

db_e_m_cnt=aggregate(db_e_m[4:16],by=list(db_e_m$Station,db_e_m$Year,db_e_m$Month),FUN=cnt)
db_b_m_cnt=aggregate(db_b_m[4:16],by=list(db_b_m$Station,db_b_m$Year,db_b_m$Month),FUN=cnt)
colnames(db_e_m_cnt)[1:3]=nms
colnames(db_b_m_cnt)[1:3]=nms

db_e_m_cnt=db_e_m_cnt[order(db_e_m_cnt$Station,db_e_m_cnt$Year,db_e_m_cnt$Month),]
db_e_m_cnt[is.na(db_e_m_cnt)]=NA
rownames(db_e_m_cnt)=1:nrow(db_e_m_cnt)
db_b_m_cnt=db_b_m_cnt[order(db_b_m_cnt$Station,db_b_m_cnt$Year,db_b_m_cnt$Month),]
db_b_m_cnt[is.na(db_b_m_cnt)]=NA
rownames(db_b_m_cnt)=1:nrow(db_b_m_cnt)

#medie annuali

ws=function(x)
{x=x[x>0]
if (length(x)>0)
{l=1/(length(x)*x)
l1=NA
for (j in 1:length(x))
{l1=c(l1,rep(l[j],times=x[j]))}
l1=l1[-1]
return(l1)}
else
{return(NA)}}

db_e_y=db_e[,c(2,5,8:20)]
db_b_y=db_b[,c(2,5,8:20)]

wgs_e=aggregate(db_e_m_cnt[4:16],by=list(db_e_m_cnt$Station,db_e_m_cnt$Year),FUN=ws)
wgs_b=aggregate(db_b_m_cnt[4:16],by=list(db_b_m_cnt$Station,db_b_m_cnt$Year),FUN=ws)

nms1=c("Station","Year")
colnames(wgs_e)[1:2]=nms1
colnames(wgs_b)[1:2]=nms1

wgs_e=wgs_e[order(wgs_e$Station,wgs_e$Year),]
wgs_b=wgs_b[order(wgs_b$Station,wgs_b$Year),]

db_e_y_mean=aggregate(db_e_m_mean[4:16],by=list(db_e_m_mean$Station,db_e_m_mean$Year),FUN=mean,na.rm=T)
db_b_y_mean=aggregate(db_b_m_mean[4:16],by=list(db_b_m_mean$Station,db_b_m_mean$Year),FUN=mean,na.rm=T)

colnames(db_e_y_mean)[1:2]=nms1
colnames(db_b_y_mean)[1:2]=nms1
db_e_y_mean[is.na(db_e_y_mean)]=NA
db_b_y_mean[is.na(db_b_y_mean)]=NA
db_e_y_mean=db_e_y_mean[order(db_e_y_mean$Station,db_e_y_mean$Year),]
db_b_y_mean=db_b_y_mean[order(db_b_y_mean$Station,db_b_y_mean$Year),]

db_e_y_c=aggregate(db_e_y[3:15],by=list(db_e_y$Station,db_e_y$Year),FUN=c)
db_b_y_c=aggregate(db_b_y[3:15],by=list(db_b_y$Station,db_b_y$Year),FUN=c)
colnames(db_e_y_c)=colnames(db_e_y)
colnames(db_b_y_c)=colnames(db_b_y)
db_e_y_c=db_e_y_c[order(db_e_y_c$Station,db_e_y_c$Year),]
db_b_y_c=db_b_y_c[order(db_b_y_c$Station,db_b_y_c$Year),]

w_median=function(x,w)
{x=unlist(x)
w=unlist(w)
x=x[!is.na(x)]
if (length(x)>0)
{s=weighted.median(x,w)}
else
{s=NA}
return(s)}

w_sd=function(x,w)
{x=unlist(x)
w=unlist(w)
x=x[!is.na(x)]
if (length(x)>1)
{stdev=weighted.sd(x,w)}
else
{stdev=NA}
return(stdev)}

db_e_y_median=data.frame(matrix(NA,nrow=nrow(db_e_y_c),ncol=15))
colnames(db_e_y_median)=colnames(db_e_y_c)
db_e_y_median[,1:2]=db_e_y_c[,1:2]
db_b_y_median=data.frame(matrix(NA,nrow=nrow(db_b_y_c),ncol=15))
colnames(db_b_y_median)=colnames(db_b_y_c)
db_b_y_median[,1:2]=db_b_y_c[,1:2]

for (j in 3:15)
{db_e_y_median[,j]=mapply(w_median,db_e_y_c[,j],wgs_e[,j])}
for (j in 3:15)
{db_b_y_median[,j]=mapply(w_median,db_b_y_c[,j],wgs_b[,j])}

db_e_y_sd=data.frame(matrix(NA,nrow=nrow(db_e_y_c),ncol=15))
colnames(db_e_y_sd)=colnames(db_e_y_c)
db_e_y_sd[,1:2]=db_e_y_c[,1:2]
db_b_y_sd=data.frame(matrix(NA,nrow=nrow(db_b_y_c),ncol=15))
colnames(db_b_y_sd)=colnames(db_b_y_c)
db_b_y_sd[,1:2]=db_b_y_c[,1:2]

for (j in 3:15)
{db_e_y_sd[,j]=mapply(w_sd,db_e_y_c[,j],wgs_e[,j])}
for (j in 3:15)
{db_b_y_sd[,j]=mapply(w_sd,db_b_y_c[,j],wgs_b[,j])}

db_e_y_cnt=aggregate(db_e_m[4:16],by=list(db_e_m$Station,db_e_m$Year),FUN=cnt)
db_b_y_cnt=aggregate(db_b_m[4:16],by=list(db_b_m$Station,db_b_m$Year),FUN=cnt)

colnames(db_e_y_cnt)[1:2]=nms1
colnames(db_b_y_cnt)[1:2]=nms1
db_e_y_cnt=db_e_y_cnt[order(db_e_y_cnt$Station,db_e_y_cnt$Year),]
db_b_y_cnt=db_b_y_cnt[order(db_b_y_cnt$Station,db_b_y_cnt$Year),]

colnames(db_e_y_mean)[3:15]=paste0(colnames(db_e_y_mean)[3:15],"_Mean_532")
colnames(db_b_y_mean)[3:15]=paste0(colnames(db_b_y_mean)[3:15],"_Mean_532")

colnames(db_e_y_median)[3:15]=paste0(colnames(db_e_y_median)[3:15],"_Median_532")
colnames(db_b_y_median)[3:15]=paste0(colnames(db_b_y_median)[3:15],"_Median_532")

colnames(db_e_y_sd)[3:15]=paste0(colnames(db_e_y_sd)[3:15],"_StDev_532")
colnames(db_b_y_sd)[3:15]=paste0(colnames(db_b_y_sd)[3:15],"_StDev_532")

colnames(db_e_y_cnt)[3:15]=paste0(colnames(db_e_y_cnt)[3:15],"_NumberValues_532")
colnames(db_b_y_cnt)[3:15]=paste0(colnames(db_b_y_cnt)[3:15],"_NumberValues_532")

rownames(db_b_y_mean)=1:nrow(db_b_y_mean)
rownames(db_b_y_median)=1:nrow(db_b_y_mean)
rownames(db_b_y_sd)=1:nrow(db_b_y_mean)
rownames(db_b_y_cnt)=1:nrow(db_b_y_mean)

rownames(db_e_y_mean)=1:nrow(db_e_y_mean)
rownames(db_e_y_median)=1:nrow(db_e_y_mean)
rownames(db_e_y_sd)=1:nrow(db_e_y_mean)
rownames(db_e_y_cnt)=1:nrow(db_e_y_mean)

db_b_y_532=cbind(db_b_y_mean,db_b_y_median[3:15],db_b_y_sd[3:15],db_b_y_cnt[3:15])
db_e_y_532=cbind(db_e_y_mean,db_e_y_median[3:15],db_e_y_sd[3:15],db_e_y_cnt[3:15])

save(db_b_y_532,file="Lev3Int_b_y_532.Rda")
save(db_e_y_532,file="Lev3Int_e_y_532.Rda")

#medie stagionali

seas=function(m,y)
{m=as.numeric(m)
y=as.numeric(y)
s=NA
if (m %in% c(3,4,5))
{s=paste0("MarAprMay_",y)}
else if (m %in% c(6,7,8))
{s=paste0("JunJulAug_",y)}
else if (m %in% c(9,10,11))
{s=paste0("SepOctNov_",y)}
else if (m %in% c(1,2))
{s=paste0("DecJanFeb_",y-1,"/",y)}
else if (m==12)
{s=paste0("DecJanFeb_",y,"/",y+1)}
return(s)}

db_e_s=db_e[,c(2,5:6,8:20)]
db_b_s=db_b[,c(2,5:6,8:20)]

db_e_s$Season_Year=mapply(seas,db_e_s[,3],db_e_s[,2])
db_b_s$Season_Year=mapply(seas,db_b_s[,3],db_b_s[,2])

db_e_s=db_e_s[,c(1,17,4:16)]
db_b_s=db_b_s[,c(1,17,4:16)]

db_e_s_mean=aggregate(db_e_s[3:15],by=list(db_e_s$Station,db_e_s$Season_Year),FUN=mean,na.rm=T)
colnames(db_e_s_mean)[1:2]=c("Station","Season_Year")
db_e_s_mean=db_e_s_mean[order(db_e_s_mean$Station,substr(db_e_s_mean$Season_Year,11,14),match(substr(db_e_s_mean$Season_Year,1,9),c("MarAprMay","JunJulAug","SepOctNov","DecJanFeb"))),]
rownames(db_e_s_mean)=1:nrow(db_e_s_mean)
db_e_s_mean[is.na(db_e_s_mean)]=NA

db_b_s_mean=aggregate(db_b_s[3:15],by=list(db_b_s$Station,db_b_s$Season_Year),FUN=mean,na.rm=T)
colnames(db_b_s_mean)[1:2]=c("Station","Season_Year")
db_b_s_mean=db_b_s_mean[order(db_b_s_mean$Station,substr(db_b_s_mean$Season_Year,11,14),match(substr(db_b_s_mean$Season_Year,1,9),c("MarAprMay","JunJulAug","SepOctNov","DecJanFeb"))),]
rownames(db_b_s_mean)=1:nrow(db_b_s_mean)
db_b_s_mean[is.na(db_b_s_mean)]=NA

db_e_s_median=aggregate(db_e_s[3:15],by=list(db_e_s$Station,db_e_s$Season_Year),FUN=median,na.rm=T)
colnames(db_e_s_median)[1:2]=c("Station","Season_Year")
db_e_s_median=db_e_s_median[order(db_e_s_median$Station,substr(db_e_s_median$Season_Year,11,14),match(substr(db_e_s_median$Season_Year,1,9),c("MarAprMay","JunJulAug","SepOctNov","DecJanFeb"))),]
rownames(db_e_s_median)=1:nrow(db_e_s_median)
db_e_s_median[is.na(db_e_s_median)]=NA

db_b_s_median=aggregate(db_b_s[3:15],by=list(db_b_s$Station,db_b_s$Season_Year),FUN=median,na.rm=T)
colnames(db_b_s_median)[1:2]=c("Station","Season_Year")
db_b_s_median=db_b_s_median[order(db_b_s_median$Station,substr(db_b_s_median$Season_Year,11,14),match(substr(db_b_s_median$Season_Year,1,9),c("MarAprMay","JunJulAug","SepOctNov","DecJanFeb"))),]
rownames(db_b_s_median)=1:nrow(db_b_s_median)
db_b_s_median[is.na(db_b_s_median)]=NA

db_e_s_sd=aggregate(db_e_s[3:15],by=list(db_e_s$Station,db_e_s$Season_Year),FUN=sd,na.rm=T)
colnames(db_e_s_sd)[1:2]=c("Station","Season_Year")
db_e_s_sd=db_e_s_sd[order(db_e_s_sd$Station,substr(db_e_s_sd$Season_Year,11,14),match(substr(db_e_s_sd$Season_Year,1,9),c("MarAprMay","JunJulAug","SepOctNov","DecJanFeb"))),]
rownames(db_e_s_sd)=1:nrow(db_e_s_sd)
db_e_s_sd[is.na(db_e_s_sd)]=NA

db_b_s_sd=aggregate(db_b_s[3:15],by=list(db_b_s$Station,db_b_s$Season_Year),FUN=sd,na.rm=T)
colnames(db_b_s_sd)[1:2]=c("Station","Season_Year")
db_b_s_sd=db_b_s_sd[order(db_b_s_sd$Station,substr(db_b_s_sd$Season_Year,11,14),match(substr(db_b_s_sd$Season_Year,1,9),c("MarAprMay","JunJulAug","SepOctNov","DecJanFeb"))),]
rownames(db_b_s_sd)=1:nrow(db_b_s_sd)
db_b_s_sd[is.na(db_b_s_sd)]=NA

db_e_s_cnt=aggregate(db_e_s[3:15],by=list(db_e_s$Station,db_e_s$Season_Year),FUN=cnt)
colnames(db_e_s_cnt)[1:2]=c("Station","Season_Year")
db_e_s_cnt=db_e_s_cnt[order(db_e_s_cnt$Station,substr(db_e_s_cnt$Season_Year,11,14),match(substr(db_e_s_cnt$Season_Year,1,9),c("MarAprMay","JunJulAug","SepOctNov","DecJanFeb"))),]
rownames(db_e_s_cnt)=1:nrow(db_e_s_cnt)
db_e_s_cnt[is.na(db_e_s_cnt)]=NA

db_b_s_cnt=aggregate(db_b_s[3:15],by=list(db_b_s$Station,db_b_s$Season_Year),FUN=cnt)
colnames(db_b_s_cnt)[1:2]=c("Station","Season_Year")
db_b_s_cnt=db_b_s_cnt[order(db_b_s_cnt$Station,substr(db_b_s_cnt$Season_Year,11,14),match(substr(db_b_s_cnt$Season_Year,1,9),c("MarAprMay","JunJulAug","SepOctNov","DecJanFeb"))),]
rownames(db_b_s_cnt)=1:nrow(db_b_s_cnt)
db_b_s_cnt[is.na(db_b_s_cnt)]=NA

colnames(db_e_s_mean)[3:15]=paste0(colnames(db_e_s_mean)[3:15],"_Mean_532")
colnames(db_b_s_mean)[3:15]=paste0(colnames(db_b_s_mean)[3:15],"_Mean_532")

colnames(db_e_s_median)[3:15]=paste0(colnames(db_e_s_median)[3:15],"_Median_532")
colnames(db_b_s_median)[3:15]=paste0(colnames(db_b_s_median)[3:15],"_Median_532")

colnames(db_e_s_sd)[3:15]=paste0(colnames(db_e_s_sd)[3:15],"_StDev_532")
colnames(db_b_s_sd)[3:15]=paste0(colnames(db_b_s_sd)[3:15],"_StDev_532")

colnames(db_e_s_cnt)[3:15]=paste0(colnames(db_e_s_cnt)[3:15],"_NumberValues_532")
colnames(db_b_s_cnt)[3:15]=paste0(colnames(db_b_s_cnt)[3:15],"_NumberValues_532")

db_b_s_532=cbind(db_b_s_mean,db_b_s_median[3:15],db_b_s_sd[3:15],db_b_s_cnt[3:15])
db_e_s_532=cbind(db_e_s_mean,db_e_s_median[3:15],db_e_s_sd[3:15],db_e_s_cnt[3:15])

save(db_b_s_532,file="Lev3Int_b_s_532.Rda")
save(db_e_s_532,file="Lev3Int_e_s_532.Rda")

#medie normali mensili

ws=function(x)
{x=x[x>0]
if (length(x)>0)
{l=1/(length(x)*x)
l1=NA
for (j in 1:length(x))
{l1=c(l1,rep(l[j],times=x[j]))}
l1=l1[-1]
return(l1)}
else
{return(NA)}}

w_median=function(x,w)
{x=unlist(x)
w=unlist(w)
x=x[!is.na(x)]
if (length(x)>0)
{s=weighted.median(x,w)}
else
{s=NA}
return(s)}

w_sd=function(x,w)
{x=unlist(x)
w=unlist(w)
x=x[!is.na(x)]
if (length(x)>1)
{stdev=weighted.sd(x,w)}
else
{stdev=NA}
return(stdev)}

db_e_nm=db_e[,c(2,6,8:20)]
db_b_nm=db_b[,c(2,6,8:20)]

wgs_e=aggregate(db_e_m_cnt[4:16],by=list(db_e_m_cnt$Station,db_e_m_cnt$Month),FUN=ws)
wgs_b=aggregate(db_b_m_cnt[4:16],by=list(db_b_m_cnt$Station,db_b_m_cnt$Month),FUN=ws)

nms2=c("Station","Month")
colnames(wgs_e)[1:2]=nms2
colnames(wgs_b)[1:2]=nms2

wgs_e=wgs_e[order(wgs_e$Station,wgs_e$Month),]
wgs_b=wgs_b[order(wgs_b$Station,wgs_b$Month),]

db_e_nm_mean=aggregate(db_e_m_mean[4:16],by=list(db_e_m_mean$Station,db_e_m_mean$Month),FUN=mean,na.rm=T)
db_b_nm_mean=aggregate(db_b_m_mean[4:16],by=list(db_b_m_mean$Station,db_b_m_mean$Month),FUN=mean,na.rm=T)


colnames(db_e_nm_mean)[1:2]=nms2
colnames(db_b_nm_mean)[1:2]=nms2
db_e_nm_mean[is.na(db_e_nm_mean)]=NA
db_b_nm_mean[is.na(db_b_nm_mean)]=NA
db_e_nm_mean=db_e_nm_mean[order(db_e_nm_mean$Station,db_e_nm_mean$Month),]
db_b_nm_mean=db_b_nm_mean[order(db_b_nm_mean$Station,db_b_nm_mean$Month),]

db_e_nm_c=aggregate(db_e_nm[3:15],by=list(db_e_nm$Station,db_e_nm$Month),FUN=c)
db_b_nm_c=aggregate(db_b_nm[3:15],by=list(db_b_nm$Station,db_b_nm$Month),FUN=c)
colnames(db_e_nm_c)=colnames(db_e_nm)
colnames(db_b_nm_c)=colnames(db_b_nm)
db_e_nm_c=db_e_nm_c[order(db_e_nm_c$Station,db_e_nm_c$Month),]
db_b_nm_c=db_b_nm_c[order(db_b_nm_c$Station,db_b_nm_c$Month),]

db_e_nm_median=data.frame(matrix(NA,nrow=nrow(db_e_nm_c),ncol=15))
colnames(db_e_nm_median)=colnames(db_e_nm_c)
db_e_nm_median[,1:2]=db_e_nm_c[,1:2]
db_b_nm_median=data.frame(matrix(NA,nrow=nrow(db_b_nm_c),ncol=15))
colnames(db_b_nm_median)=colnames(db_b_nm_c)
db_b_nm_median[,1:2]=db_b_nm_c[,1:2]

for (j in 3:15)
{db_e_nm_median[,j]=mapply(w_median,db_e_nm_c[,j],wgs_e[,j])}
for (j in 3:15)
{db_b_nm_median[,j]=mapply(w_median,db_b_nm_c[,j],wgs_b[,j])}

db_e_nm_sd=data.frame(matrix(NA,nrow=nrow(db_e_nm_c),ncol=15))
colnames(db_e_nm_sd)=colnames(db_e_nm_c)
db_e_nm_sd[,1:2]=db_e_nm_c[,1:2]
db_b_nm_sd=data.frame(matrix(NA,nrow=nrow(db_b_nm_c),ncol=15))
colnames(db_b_nm_sd)=colnames(db_b_nm_c)
db_b_nm_sd[,1:2]=db_b_nm_c[,1:2]

for (j in 3:15)
{db_e_nm_sd[,j]=mapply(w_sd,db_e_nm_c[,j],wgs_e[,j])}
for (j in 3:15)
{db_b_nm_sd[,j]=mapply(w_sd,db_b_nm_c[,j],wgs_b[,j])}

db_e_nm_cnt=aggregate(db_e_m[4:16],by=list(db_e_m$Station,db_e_m$Month),FUN=cnt)
db_b_nm_cnt=aggregate(db_b_m[4:16],by=list(db_b_m$Station,db_b_m$Month),FUN=cnt)

colnames(db_e_nm_cnt)[1:2]=nms2
colnames(db_b_nm_cnt)[1:2]=nms2
db_e_nm_cnt=db_e_nm_cnt[order(db_e_nm_cnt$Station,db_e_nm_cnt$Month),]
db_b_nm_cnt=db_b_nm_cnt[order(db_b_nm_cnt$Station,db_b_nm_cnt$Month),]

colnames(db_e_nm_mean)[3:15]=paste0(colnames(db_e_nm_mean)[3:15],"_Mean_532")
colnames(db_b_nm_mean)[3:15]=paste0(colnames(db_b_nm_mean)[3:15],"_Mean_532")

colnames(db_e_nm_median)[3:15]=paste0(colnames(db_e_nm_median)[3:15],"_Median_532")
colnames(db_b_nm_median)[3:15]=paste0(colnames(db_b_nm_median)[3:15],"_Median_532")

colnames(db_e_nm_sd)[3:15]=paste0(colnames(db_e_nm_sd)[3:15],"_StDev_532")
colnames(db_b_nm_sd)[3:15]=paste0(colnames(db_b_nm_sd)[3:15],"_StDev_532")

colnames(db_e_nm_cnt)[3:15]=paste0(colnames(db_e_nm_cnt)[3:15],"_NumberValues_532")
colnames(db_b_nm_cnt)[3:15]=paste0(colnames(db_b_nm_cnt)[3:15],"_NumberValues_532")

rownames(db_b_nm_mean)=1:nrow(db_b_nm_mean)
rownames(db_b_nm_median)=1:nrow(db_b_nm_mean)
rownames(db_b_nm_sd)=1:nrow(db_b_nm_mean)
rownames(db_b_nm_cnt)=1:nrow(db_b_nm_mean)

rownames(db_e_nm_mean)=1:nrow(db_e_nm_mean)
rownames(db_e_nm_median)=1:nrow(db_e_nm_mean)
rownames(db_e_nm_sd)=1:nrow(db_e_nm_mean)
rownames(db_e_nm_cnt)=1:nrow(db_e_nm_mean)

db_b_nm_532=cbind(db_b_nm_mean,db_b_nm_median[3:15],db_b_nm_sd[3:15],db_b_nm_cnt[3:15])
db_e_nm_532=cbind(db_e_nm_mean,db_e_nm_median[3:15],db_e_nm_sd[3:15],db_e_nm_cnt[3:15])

save(db_b_nm_532,file="Lev3Int_b_nm_532.Rda")
save(db_e_nm_532,file="Lev3Int_e_nm_532.Rda")

#medie normali stagionali

ws=function(x)
{x=x[x>0]
if (length(x)>0)
{l=1/(length(x)*x)
l1=NA
for (j in 1:length(x))
{l1=c(l1,rep(l[j],times=x[j]))}
l1=l1[-1]
return(l1)}
else
{return(NA)}}

w_median=function(x,w)
{x=unlist(x)
w=unlist(w)
x=x[!is.na(x)]
if (length(x)>0)
{s=weighted.median(x,w)}
else
{s=NA}
return(s)}

w_sd=function(x,w)
{x=unlist(x)
w=unlist(w)
x=x[!is.na(x)]
if (length(x)>1)
{stdev=weighted.sd(x,w)}
else
{stdev=NA}
return(stdev)}

seas1=function(m)
{m=as.numeric(m)
s=NA
if (m %in% c(3,4,5))
{s="MarAprMay"}
else if (m %in% c(6,7,8))
{s="JunJulAug"}
else if (m %in% c(9,10,11))
{s="SepOctNov"}
else if (m %in% c(1,2,12))
{s="DecJanFeb"}
return(s)}

db_e_ns=db_e[,c(2,6,8:20)]
db_b_ns=db_b[,c(2,6,8:20)]

db_e_ns$Season=mapply(seas1,db_e_ns[,2])
db_b_ns$Season=mapply(seas1,db_b_ns[,2])

db_e_ns=db_e_ns[,c(1,16,3:15)]
db_b_ns=db_b_ns[,c(1,16,3:15)]

db_e_s_mean$Season=substr(db_e_s_mean$Season_Year,1,9)
db_b_s_mean$Season=substr(db_b_s_mean$Season_Year,1,9)
db_e_s_cnt$Season=substr(db_e_s_cnt$Season_Year,1,9)
db_b_s_cnt$Season=substr(db_b_s_cnt$Season_Year,1,9)

wgs_e=aggregate(db_e_s_cnt[3:15],by=list(db_e_s_cnt$Station,db_e_s_cnt$Season),FUN=ws)
wgs_b=aggregate(db_b_s_cnt[3:15],by=list(db_b_s_cnt$Station,db_b_s_cnt$Season),FUN=ws)

nms3=c("Station","Season")
colnames(wgs_e)[1:2]=nms3
colnames(wgs_b)[1:2]=nms3

wgs_e=wgs_e[order(wgs_e$Station,match(wgs_e$Season,c("MarAprMay","JunJulAug","SepOctNov","DecJanFeb"))),]
wgs_b=wgs_b[order(wgs_b$Station,match(wgs_b$Season,c("MarAprMay","JunJulAug","SepOctNov","DecJanFeb"))),]

rownames(wgs_e)=1:nrow(wgs_e)
rownames(wgs_b)=1:nrow(wgs_b)

db_e_ns_mean=aggregate(db_e_s_mean[3:15],by=list(db_e_s_mean$Station,db_e_s_mean$Season),FUN=mean,na.rm=T)
db_b_ns_mean=aggregate(db_b_s_mean[3:15],by=list(db_b_s_mean$Station,db_b_s_mean$Season),FUN=mean,na.rm=T)

colnames(db_e_ns_mean)[1:2]=nms3
colnames(db_b_ns_mean)[1:2]=nms3
db_e_ns_mean[is.na(db_e_ns_mean)]=NA
db_b_ns_mean[is.na(db_b_ns_mean)]=NA
db_e_ns_mean=db_e_ns_mean[order(db_e_ns_mean$Station,match(db_e_ns_mean$Season,c("MarAprMay","JunJulAug","SepOctNov","DecJanFeb"))),]
db_b_ns_mean=db_b_ns_mean[order(db_b_ns_mean$Station,match(db_b_ns_mean$Season,c("MarAprMay","JunJulAug","SepOctNov","DecJanFeb"))),]
rownames(db_e_ns_mean)=1:nrow(db_e_ns_mean)
rownames(db_b_ns_mean)=1:nrow(db_b_ns_mean)

db_e_ns_c=aggregate(db_e_ns[3:15],by=list(db_e_ns$Station,db_e_ns$Season),FUN=c)
db_b_ns_c=aggregate(db_b_ns[3:15],by=list(db_b_ns$Station,db_b_ns$Season),FUN=c)
colnames(db_e_ns_c)=colnames(db_e_ns)
colnames(db_b_ns_c)=colnames(db_b_ns)
db_e_ns_c=db_e_ns_c[order(db_e_ns_c$Station,match(db_e_ns_c$Season,c("MarAprMay","JunJulAug","SepOctNov","DecJanFeb"))),]
db_b_ns_c=db_b_ns_c[order(db_b_ns_c$Station,match(db_b_ns_c$Season,c("MarAprMay","JunJulAug","SepOctNov","DecJanFeb"))),]
rownames(db_e_ns_c)=1:nrow(db_e_ns_c)
rownames(db_b_ns_c)=1:nrow(db_b_ns_c)

db_e_ns_median=data.frame(matrix(NA,nrow=nrow(db_e_ns_c),ncol=15))
colnames(db_e_ns_median)=colnames(db_e_ns_c)
db_e_ns_median[,1:2]=db_e_ns_c[,1:2]
db_b_ns_median=data.frame(matrix(NA,nrow=nrow(db_b_ns_c),ncol=15))
colnames(db_b_ns_median)=colnames(db_b_ns_c)
db_b_ns_median[,1:2]=db_b_ns_c[,1:2]

for (j in 3:15)
{db_e_ns_median[,j]=mapply(w_median,db_e_ns_c[,j],wgs_e[,j])}
for (j in 3:15)
{db_b_ns_median[,j]=mapply(w_median,db_b_ns_c[,j],wgs_b[,j])}

db_e_ns_sd=data.frame(matrix(NA,nrow=nrow(db_e_ns_c),ncol=15))
colnames(db_e_ns_sd)=colnames(db_e_ns_c)
db_e_ns_sd[,1:2]=db_e_ns_c[,1:2]
db_b_ns_sd=data.frame(matrix(NA,nrow=nrow(db_b_ns_c),ncol=15))
colnames(db_b_ns_sd)=colnames(db_b_ns_c)
db_b_ns_sd[,1:2]=db_b_ns_c[,1:2]

for (j in 3:15)
{db_e_ns_sd[,j]=mapply(w_sd,db_e_ns_c[,j],wgs_e[,j])}
for (j in 3:15)
{db_b_ns_sd[,j]=mapply(w_sd,db_b_ns_c[,j],wgs_b[,j])}

db_e_ns_cnt=aggregate(db_e_ns[3:15],by=list(db_e_ns$Station,db_e_ns$Season),FUN=cnt)
db_b_ns_cnt=aggregate(db_b_ns[3:15],by=list(db_b_ns$Station,db_b_ns$Season),FUN=cnt)

colnames(db_e_ns_cnt)[1:2]=nms3
colnames(db_b_ns_cnt)[1:2]=nms3
db_e_ns_cnt=db_e_ns_cnt[order(db_e_ns_cnt$Station,match(db_e_ns_cnt$Season,c("MarAprMay","JunJulAug","SepOctNov","DecJanFeb"))),]
db_b_ns_cnt=db_b_ns_cnt[order(db_b_ns_cnt$Station,match(db_b_ns_cnt$Season,c("MarAprMay","JunJulAug","SepOctNov","DecJanFeb"))),]

colnames(db_e_ns_median)[3:15]=paste0(colnames(db_e_ns_median)[3:15],"_Median_532")
colnames(db_b_ns_median)[3:15]=paste0(colnames(db_b_ns_median)[3:15],"_Median_532")

colnames(db_e_ns_sd)[3:15]=paste0(colnames(db_e_ns_sd)[3:15],"_StDev_532")
colnames(db_b_ns_sd)[3:15]=paste0(colnames(db_b_ns_sd)[3:15],"_StDev_532")

colnames(db_e_ns_cnt)[3:15]=paste0(colnames(db_e_ns_cnt)[3:15],"_NumberValues_532")
colnames(db_b_ns_cnt)[3:15]=paste0(colnames(db_b_ns_cnt)[3:15],"_NumberValues_532")

db_b_ns_532=cbind(db_b_ns_mean,db_b_ns_median[3:15],db_b_ns_sd[3:15],db_b_ns_cnt[3:15])
db_e_ns_532=cbind(db_e_ns_mean,db_e_ns_median[3:15],db_e_ns_sd[3:15],db_e_ns_cnt[3:15])

save(db_b_ns_532,file="Lev3Int_b_ns_532.Rda")
save(db_e_ns_532,file="Lev3Int_e_ns_532.Rda")

#----PartDep_532----

load("Lev3PartDep.Rda")

db_pd=db_pd[db_pd$Wavelength=="0532",]

#medie mensili

db_pd_m=db_pd[,c(1,3:4,8:13)]

db_pd_m_mean=aggregate(db_pd_m[4:9],by=list(db_pd_m$Station,db_pd_m$Year,db_pd_m$Month),FUN=mean,na.rm=T)

nms=c("Station","Year","Month")
colnames(db_pd_m_mean)[1:3]=nms

db_pd_m_mean=db_pd_m_mean[order(db_pd_m_mean$Station,db_pd_m_mean$Year,db_pd_m_mean$Month),]
db_pd_m_mean[is.na(db_pd_m_mean)]=NA
rownames(db_pd_m_mean)=1:nrow(db_pd_m_mean)

cnt=function(x)
{s=sum(!is.na(x))
return(s)}

db_pd_m_cnt=aggregate(db_pd_m[4:9],by=list(db_pd_m$Station,db_pd_m$Year,db_pd_m$Month),FUN=cnt)
colnames(db_pd_m_cnt)[1:3]=nms

db_pd_m_cnt=db_pd_m_cnt[order(db_pd_m_cnt$Station,db_pd_m_cnt$Year,db_pd_m_cnt$Month),]
db_pd_m_cnt[is.na(db_pd_m_cnt)]=NA
rownames(db_pd_m_cnt)=1:nrow(db_pd_m_cnt)

#medie annuali

ws=function(x)
{x=x[x>0]
if (length(x)>0)
{l=1/(length(x)*x)
l1=NA
for (j in 1:length(x))
{l1=c(l1,rep(l[j],times=x[j]))}
l1=l1[-1]
return(l1)}
else
{return(NA)}}

db_pd_y=db_pd[,c(1,3,8:13)]

wgs_pd=aggregate(db_pd_m_cnt[4:9],by=list(db_pd_m_cnt$Station,db_pd_m_cnt$Year),FUN=ws)

nms1=c("Station","Year")
colnames(wgs_pd)[1:2]=nms1

wgs_pd=wgs_pd[order(wgs_pd$Station,wgs_pd$Year),]

db_pd_y_mean=aggregate(db_pd_m_mean[4:9],by=list(db_pd_m_mean$Station,db_pd_m_mean$Year),FUN=mean,na.rm=T)

colnames(db_pd_y_mean)[1:2]=nms1
db_pd_y_mean[is.na(db_pd_y_mean)]=NA
db_pd_y_mean=db_pd_y_mean[order(db_pd_y_mean$Station,db_pd_y_mean$Year),]

db_pd_y_c=aggregate(db_pd_y[3:8],by=list(db_pd_y$Station,db_pd_y$Year),FUN=c)
colnames(db_pd_y_c)=colnames(db_pd_y)
db_pd_y_c=db_pd_y_c[order(db_pd_y_c$Station,db_pd_y_c$Year),]

w_median=function(x,w)
{x=unlist(x)
w=unlist(w)
x=x[!is.na(x)]
if (length(x)>0)
{s=weighted.median(x,w)}
else
{s=NA}
return(s)}

w_sd=function(x,w)
{x=unlist(x)
w=unlist(w)
x=x[!is.na(x)]
if (length(x)>1)
{stdev=weighted.sd(x,w)}
else
{stdev=NA}
return(stdev)}

db_pd_y_median=data.frame(matrix(NA,nrow=nrow(db_pd_y_c),ncol=8))
colnames(db_pd_y_median)=colnames(db_pd_y_c)
db_pd_y_median[,1:2]=db_pd_y_c[,1:2]

for (j in 3:8)
{db_pd_y_median[,j]=mapply(w_median,db_pd_y_c[,j],wgs_pd[,j])}

db_pd_y_sd=data.frame(matrix(NA,nrow=nrow(db_pd_y_c),ncol=8))
colnames(db_pd_y_sd)=colnames(db_pd_y_c)
db_pd_y_sd[,1:2]=db_pd_y_c[,1:2]

for (j in 3:8)
{db_pd_y_sd[,j]=mapply(w_sd,db_pd_y_c[,j],wgs_pd[,j])}

db_pd_y_cnt=aggregate(db_pd_m[4:9],by=list(db_pd_m$Station,db_pd_m$Year),FUN=cnt)

colnames(db_pd_y_cnt)[1:2]=nms1
db_pd_y_cnt=db_pd_y_cnt[order(db_pd_y_cnt$Station,db_pd_y_cnt$Year),]

colnames(db_pd_y_mean)[3:8]=paste0(colnames(db_pd_y_mean)[3:8],"_Mean_532")

colnames(db_pd_y_median)[3:8]=paste0(colnames(db_pd_y_median)[3:8],"_Median_532")

colnames(db_pd_y_sd)[3:8]=paste0(colnames(db_pd_y_sd)[3:8],"_StDev_532")

colnames(db_pd_y_cnt)[3:8]=paste0(colnames(db_pd_y_cnt)[3:8],"_NumberValues_532")

rownames(db_pd_y_mean)=1:nrow(db_pd_y_mean)
rownames(db_pd_y_median)=1:nrow(db_pd_y_mean)
rownames(db_pd_y_sd)=1:nrow(db_pd_y_mean)
rownames(db_pd_y_cnt)=1:nrow(db_pd_y_mean)

db_pd_y_532=cbind(db_pd_y_mean,db_pd_y_median[3:8],db_pd_y_sd[3:8],db_pd_y_cnt[3:8])

save(db_pd_y_532,file="Lev3PartDep_y_532.Rda")

#medie stagionali

seas=function(m,y)
{m=as.numeric(m)
y=as.numeric(y)
s=NA
if (m %in% c(3,4,5))
{s=paste0("MarAprMay_",y)}
else if (m %in% c(6,7,8))
{s=paste0("JunJulAug_",y)}
else if (m %in% c(9,10,11))
{s=paste0("SepOctNov_",y)}
else if (m %in% c(1,2))
{s=paste0("DecJanFeb_",y-1,"/",y)}
else if (m==12)
{s=paste0("DecJanFeb_",y,"/",y+1)}
return(s)}

db_pd_s=db_pd[,c(1,3:4,8:13)]

db_pd_s$Season_Year=mapply(seas,db_pd_s[,3],db_pd_s[,2])

db_pd_s=db_pd_s[,c(1,10,4:9)]

db_pd_s_mean=aggregate(db_pd_s[3:8],by=list(db_pd_s$Station,db_pd_s$Season_Year),FUN=mean,na.rm=T)
colnames(db_pd_s_mean)[1:2]=c("Station","Season_Year")
db_pd_s_mean=db_pd_s_mean[order(db_pd_s_mean$Station,substr(db_pd_s_mean$Season_Year,11,14),match(substr(db_pd_s_mean$Season_Year,1,9),c("MarAprMay","JunJulAug","SepOctNov","DecJanFeb"))),]
rownames(db_pd_s_mean)=1:nrow(db_pd_s_mean)
db_pd_s_mean[is.na(db_pd_s_mean)]=NA

db_pd_s_median=aggregate(db_pd_s[3:8],by=list(db_pd_s$Station,db_pd_s$Season_Year),FUN=median,na.rm=T)
colnames(db_pd_s_median)[1:2]=c("Station","Season_Year")
db_pd_s_median=db_pd_s_median[order(db_pd_s_median$Station,substr(db_pd_s_median$Season_Year,11,14),match(substr(db_pd_s_median$Season_Year,1,9),c("MarAprMay","JunJulAug","SepOctNov","DecJanFeb"))),]
rownames(db_pd_s_median)=1:nrow(db_pd_s_median)
db_pd_s_median[is.na(db_pd_s_median)]=NA

db_pd_s_sd=aggregate(db_pd_s[3:8],by=list(db_pd_s$Station,db_pd_s$Season_Year),FUN=sd,na.rm=T)
colnames(db_pd_s_sd)[1:2]=c("Station","Season_Year")
db_pd_s_sd=db_pd_s_sd[order(db_pd_s_sd$Station,substr(db_pd_s_sd$Season_Year,11,14),match(substr(db_pd_s_sd$Season_Year,1,9),c("MarAprMay","JunJulAug","SepOctNov","DecJanFeb"))),]
rownames(db_pd_s_sd)=1:nrow(db_pd_s_sd)
db_pd_s_sd[is.na(db_pd_s_sd)]=NA

db_pd_s_cnt=aggregate(db_pd_s[3:8],by=list(db_pd_s$Station,db_pd_s$Season_Year),FUN=cnt)
colnames(db_pd_s_cnt)[1:2]=c("Station","Season_Year")
db_pd_s_cnt=db_pd_s_cnt[order(db_pd_s_cnt$Station,substr(db_pd_s_cnt$Season_Year,11,14),match(substr(db_pd_s_cnt$Season_Year,1,9),c("MarAprMay","JunJulAug","SepOctNov","DecJanFeb"))),]
rownames(db_pd_s_cnt)=1:nrow(db_pd_s_cnt)
db_pd_s_cnt[is.na(db_pd_s_cnt)]=NA

colnames(db_pd_s_mean)[3:8]=paste0(colnames(db_pd_s_mean)[3:8],"_Mean_532")

colnames(db_pd_s_median)[3:8]=paste0(colnames(db_pd_s_median)[3:8],"_Median_532")

colnames(db_pd_s_sd)[3:8]=paste0(colnames(db_pd_s_sd)[3:8],"_StDev_532")

colnames(db_pd_s_cnt)[3:8]=paste0(colnames(db_pd_s_cnt)[3:8],"_NumberValues_532")

db_pd_s_532=cbind(db_pd_s_mean,db_pd_s_median[3:8],db_pd_s_sd[3:8],db_pd_s_cnt[3:8])

save(db_pd_s_532,file="Lev3PartDep_s_532.Rda")

#medie normali mensili

ws=function(x)
{x=x[x>0]
if (length(x)>0)
{l=1/(length(x)*x)
l1=NA
for (j in 1:length(x))
{l1=c(l1,rep(l[j],times=x[j]))}
l1=l1[-1]
return(l1)}
else
{return(NA)}}

w_median=function(x,w)
{x=unlist(x)
w=unlist(w)
x=x[!is.na(x)]
if (length(x)>0)
{s=weighted.median(x,w)}
else
{s=NA}
return(s)}

w_sd=function(x,w)
{x=unlist(x)
w=unlist(w)
x=x[!is.na(x)]
if (length(x)>1)
{stdev=weighted.sd(x,w)}
else
{stdev=NA}
return(stdev)}

db_pd_nm=db_pd[,c(1,4,8:13)]

wgs_pd=aggregate(db_pd_m_cnt[4:9],by=list(db_pd_m_cnt$Station,db_pd_m_cnt$Month),FUN=ws)

nms2=c("Station","Month")
colnames(wgs_pd)[1:2]=nms2

wgs_pd=wgs_pd[order(wgs_pd$Station,wgs_pd$Month),]

db_pd_nm_mean=aggregate(db_pd_m_mean[4:9],by=list(db_pd_m_mean$Station,db_pd_m_mean$Month),FUN=mean,na.rm=T)

colnames(db_pd_nm_mean)[1:2]=nms2
db_pd_nm_mean[is.na(db_pd_nm_mean)]=NA
db_pd_nm_mean=db_pd_nm_mean[order(db_pd_nm_mean$Station,db_pd_nm_mean$Month),]

db_pd_nm_c=aggregate(db_pd_nm[3:8],by=list(db_pd_nm$Station,db_pd_nm$Month),FUN=c)
colnames(db_pd_nm_c)=colnames(db_pd_nm)
db_pd_nm_c=db_pd_nm_c[order(db_pd_nm_c$Station,db_pd_nm_c$Month),]

db_pd_nm_median=data.frame(matrix(NA,nrow=nrow(db_pd_nm_c),ncol=8))
colnames(db_pd_nm_median)=colnames(db_pd_nm_c)
db_pd_nm_median[,1:2]=db_pd_nm_c[,1:2]

for (j in 3:8)
{db_pd_nm_median[,j]=mapply(w_median,db_pd_nm_c[,j],wgs_pd[,j])}

db_pd_nm_sd=data.frame(matrix(NA,nrow=nrow(db_pd_nm_c),ncol=8))
colnames(db_pd_nm_sd)=colnames(db_pd_nm_c)
db_pd_nm_sd[,1:2]=db_pd_nm_c[,1:2]

for (j in 3:8)
{db_pd_nm_sd[,j]=mapply(w_sd,db_pd_nm_c[,j],wgs_pd[,j])}

db_pd_nm_cnt=aggregate(db_pd_m[4:9],by=list(db_pd_m$Station,db_pd_m$Month),FUN=cnt)

colnames(db_pd_nm_cnt)[1:2]=nms2
db_pd_nm_cnt=db_pd_nm_cnt[order(db_pd_nm_cnt$Station,db_pd_nm_cnt$Month),]

colnames(db_pd_nm_mean)[3:8]=paste0(colnames(db_pd_nm_mean)[3:8],"_Mean_532")

colnames(db_pd_nm_median)[3:8]=paste0(colnames(db_pd_nm_median)[3:8],"_Median_532")

colnames(db_pd_nm_sd)[3:8]=paste0(colnames(db_pd_nm_sd)[3:8],"_StDev_532")

colnames(db_pd_nm_cnt)[3:8]=paste0(colnames(db_pd_nm_cnt)[3:8],"_NumberValues_532")

rownames(db_pd_nm_mean)=1:nrow(db_pd_nm_mean)
rownames(db_pd_nm_median)=1:nrow(db_pd_nm_mean)
rownames(db_pd_nm_sd)=1:nrow(db_pd_nm_mean)
rownames(db_pd_nm_cnt)=1:nrow(db_pd_nm_mean)

db_pd_nm_532=cbind(db_pd_nm_mean,db_pd_nm_median[3:8],db_pd_nm_sd[3:8],db_pd_nm_cnt[3:8])

save(db_pd_nm_532,file="Lev3PartDep_nm_532.Rda")

#medie normali stagionali

ws=function(x)
{x=x[x>0]
if (length(x)>0)
{l=1/(length(x)*x)
l1=NA
for (j in 1:length(x))
{l1=c(l1,rep(l[j],times=x[j]))}
l1=l1[-1]
return(l1)}
else
{return(NA)}}

w_median=function(x,w)
{x=unlist(x)
w=unlist(w)
x=x[!is.na(x)]
if (length(x)>0)
{s=weighted.median(x,w)}
else
{s=NA}
return(s)}

w_sd=function(x,w)
{x=unlist(x)
w=unlist(w)
x=x[!is.na(x)]
if (length(x)>1)
{stdev=weighted.sd(x,w)}
else
{stdev=NA}
return(stdev)}

seas1=function(m)
{m=as.numeric(m)
s=NA
if (m %in% c(3,4,5))
{s="MarAprMay"}
else if (m %in% c(6,7,8))
{s="JunJulAug"}
else if (m %in% c(9,10,11))
{s="SepOctNov"}
else if (m %in% c(1,2,12))
{s="DecJanFeb"}
return(s)}

db_pd_ns=db_pd[,c(1,4,8:13)]

db_pd_ns$Season=mapply(seas1,db_pd_ns[,2])

db_pd_ns=db_pd_ns[,c(1,9,3:8)]

db_pd_s_mean$Season=substr(db_pd_s_mean$Season_Year,1,9)
db_pd_s_cnt$Season=substr(db_pd_s_cnt$Season_Year,1,9)

wgs_pd=aggregate(db_pd_s_cnt[3:8],by=list(db_pd_s_cnt$Station,db_pd_s_cnt$Season),FUN=ws)

nms3=c("Station","Season")
colnames(wgs_pd)[1:2]=nms3

wgs_pd=wgs_pd[order(wgs_pd$Station,match(wgs_pd$Season,c("MarAprMay","JunJulAug","SepOctNov","DecJanFeb"))),]

rownames(wgs_pd)=1:nrow(wgs_pd)

db_pd_ns_mean=aggregate(db_pd_s_mean[3:8],by=list(db_pd_s_mean$Station,db_pd_s_mean$Season),FUN=mean,na.rm=T)

colnames(db_pd_ns_mean)[1:2]=nms3
db_pd_ns_mean[is.na(db_pd_ns_mean)]=NA
db_pd_ns_mean=db_pd_ns_mean[order(db_pd_ns_mean$Station,match(db_pd_ns_mean$Season,c("MarAprMay","JunJulAug","SepOctNov","DecJanFeb"))),]
rownames(db_pd_ns_mean)=1:nrow(db_pd_ns_mean)

db_pd_ns_c=aggregate(db_pd_ns[3:8],by=list(db_pd_ns$Station,db_pd_ns$Season),FUN=c)
colnames(db_pd_ns_c)=colnames(db_pd_ns)
db_pd_ns_c=db_pd_ns_c[order(db_pd_ns_c$Station,match(db_pd_ns_c$Season,c("MarAprMay","JunJulAug","SepOctNov","DecJanFeb"))),]
rownames(db_pd_ns_c)=1:nrow(db_pd_ns_c)

db_pd_ns_median=data.frame(matrix(NA,nrow=nrow(db_pd_ns_c),ncol=8))
colnames(db_pd_ns_median)=colnames(db_pd_ns_c)
db_pd_ns_median[,1:2]=db_pd_ns_c[,1:2]

for (j in 3:8)
{db_pd_ns_median[,j]=mapply(w_median,db_pd_ns_c[,j],wgs_pd[,j])}

db_pd_ns_sd=data.frame(matrix(NA,nrow=nrow(db_pd_ns_c),ncol=8))
colnames(db_pd_ns_sd)=colnames(db_pd_ns_c)
db_pd_ns_sd[,1:2]=db_pd_ns_c[,1:2]

for (j in 3:8)
{db_pd_ns_sd[,j]=mapply(w_sd,db_pd_ns_c[,j],wgs_pd[,j])}

db_pd_ns_cnt=aggregate(db_pd_ns[3:8],by=list(db_pd_ns$Station,db_pd_ns$Season),FUN=cnt)

colnames(db_pd_ns_cnt)[1:2]=nms3
db_pd_ns_cnt=db_pd_ns_cnt[order(db_pd_ns_cnt$Station,match(db_pd_ns_cnt$Season,c("MarAprMay","JunJulAug","SepOctNov","DecJanFeb"))),]

colnames(db_pd_ns_median)[3:8]=paste0(colnames(db_pd_ns_median)[3:8],"_Median_532")

colnames(db_pd_ns_sd)[3:8]=paste0(colnames(db_pd_ns_sd)[3:8],"_StDev_532")

colnames(db_pd_ns_cnt)[3:8]=paste0(colnames(db_pd_ns_cnt)[3:8],"_NumberValues_532")

db_pd_ns_532=cbind(db_pd_ns_mean,db_pd_ns_median[3:8],db_pd_ns_sd[3:8],db_pd_ns_cnt[3:8])

save(db_pd_ns_532,file="Lev3PartDep_ns_532.Rda")
