#========================================================================================================================
# Author: Francisco Javier Arceo
# Purpose: Data Science Hackathon 
#========================================================================================================================
setwd('/Users/franciscojavierarceo/Projects/yelp/')
#========================================================================================================================
rvwdf <- read.csv('yelp_academic_dataset_review.csv')
bizdf <- read.csv('yelp_academic_dataset_business.csv')
tipdf <- read.csv('yelp_academic_dataset_tip.csv')
usrdf <- read.csv('yelp_academic_dataset_user.csv')
chkdf <- read.csv('yelp_academic_dataset_checkin.csv')
#========================================================================================================================
source('/Users/franciscojavierarceo/MyPrograms/R/My_Functions.R')
#========================================================================================================================
# Subsets the data
x <- data.frame(table(bizdf$city))
x <- x[order(x$Freq),]
# This part selects Las Vegas
UsrCntDf <- data.frame(table(rvwdf$user_id))
RowCntDf <- data.frame(table(rvwdf$business_id))
udf <- merge(rvwdf,UsrCntDf,by.x='user_id',by.y='Var1')
tst <- merge(bizdf,RowCntDf,by.x='business_id',by.y='Var1')
udf <- udf[udf$Freq>5,]
tst$Res <- word_search('Restaurant',tst$categories)
tst <- tst[,c('categories','Freq','business_id','city','Res')][tst$Res==1,]
x <- sqldf('select city, sum(Freq) as ReviewCount, count(*) as BizCount from tst group by 1')
x <- x[order(x$ReviewCount,decreasing=T),]
# Selecting the top five
geos <- as.character(x$city[1:5])
# This part selects Las Vegas
bizlist <- bizdf$business_id[bizdf$city == geos[1] & bizdf$attributes.Waiter.Service=='True']
# Selecting all of the businesses within the top 5
df2 <- udf[udf$business_id %in% bizlist,]
tmp <- cat_summary(df2,'business_id')
tst <- sqldf('select a.business_id, b.Count from df2 a left join tmp b on a.business_id=b.business_id')
df2$RevCntPerBiz <- tst$Count
df3 <- df2[df2$RevCntPerBiz>=250,]
#========================================================================================================================
dim(df3)
deciles(df2$RevCntPerBiz)
ndistinct(df3$business_id)
#========================================================================================================================
bizlistfin <- unique(df3$business_id)
fnl_biz <- bizdf[bizdf$business_id %in% bizlistfin,]
with(fnl_biz,plot(longitude,latitude,pch=16,col='blue'))
#========================================================================================================================
write.csv(bizlistfin,'FinalBusinessIDS.csv',row.names=F)
#========================================================================================================================
txtvector <- as.character(df3$text)
ProcessMyText <- function(string_vec){
  string_vec <- tolower(string_vec)
  wordvals  <- c('a','for','i','in','on','the','you','He','Her','she','him','her','so')
  for(i in 1:length(wordvals)){
    string_vec <- word_replace(string_vec,wordvals[i])    
  }
  return(string_vec)
}
var <- as.character(txtvector)
MyCorpus <- VCorpus(VectorSource(var))
t0 <- Sys.time()
CorpusMatrix <- DocumentTermMatrix(MyCorpus, control=list(removePunctuation = TRUE,
                                                            stopwords =  TRUE, 
                                                            wordLengths=c(10, 15), 
                                                          removeNumbers=TRUE,
                                                  bounds = list(global = c(10,Inf))))
tEnd <- Sys.time()
print(tEnd-t0)
xs <- as.matrix(CorpusMatrix,sparse=T)
cnts <- colSums(xs)
WordSummary <- data.frame(Word=c(colnames(CorpusMatrix)),Freq=c(cnts))
WordSummary[1:20,]
#========================================================================================================================
plot(density(log(cnts)))
#========================================================================================================================
df3$tfilt <- ifelse(runif(dim(df3)[1])<0.7,1,0)
df3$BadRating <- ifelse(df3$stars<=2,1,0)
y <- df3$BadRating
tflt <- df3$tfilt==1
vflt <- df3$tfilt==0
#========================================================================================================================
registerDoParallel(cores=detectCores(all.test=TRUE))
#========================================================================================================================
MyModel <- cv.glmnet(x=xs[tflt,],
                 y=df3$BadRating[tflt],
                 alpha=0.95,
                 nfolds=10,
                 family='binomial')
#========================================================================================================================
yhat1 <- predict(MyModel,xs,s='lambda.min',type='response')
bs <- as.matrix(coef(MyModel,s='lambda.min'))
beta1 <- data.frame(Words=rownames(bs),Coefficients=c(bs[,1])); row.names(beta1) <- NULL
beta1 <- beta1[beta1$Words!='(Intercept)',]
head(beta1)
my_roc(yhat1[vflt],y[vflt])
#========================================================================================================================
df3$probs <- c(yhat1)
bizlist <- bizdf$business_id[bizdf$city == geos[1] & bizdf$attributes.Waiter.Service=='True']
bizdf2 <- bizdf[bizdf$business_id %in% bizlist,]
#========================================================================================================================
df3$star1 <- ifelse(df3$stars==1,1,0)
df3$star2 <- ifelse(df3$stars==2,1,0)
df3$star3 <- ifelse(df3$stars==3,1,0)
df3$star4 <- ifelse(df3$stars==4,1,0)
df3$star5 <- ifelse(df3$stars==5,1,0)
bizsmry <- sqldf('select business_id,
                 count(distinct user_id) as uniq_users,
                 sum(probs) as TotalProbs,
                 sum(star1) as Star1rvws,
                 sum(star2) as Star2rvws,
                 sum(star3) as Star3rvws,
                 sum(star4) as Star4rvws,
                 sum(star5) as Star5rvws,
                 sum(1) as nreviews
                 from df3 
                 group by 1')
bizsmry[1:10,]
#========================================================================================================================
tmpdf <- merge(bizdf,bizsmry,by.x='business_id',by.y='business_id')
xvars <- c('categories','Star1rvws','Star2rvws','Star3rvws','Star4rvws','Star5rvws',
           'attributes.Noise.Level','latitude','longitude','stars','business_id','name')
fnldf <- tmpdf[,xvars]
fnldf$categories <- as.character(fnldf$categories)
unspxs <- model.matrix(~.-1,data=fnldf[,c(8,9)])
vals <- data.frame(Clusters=rep(0,dim(unspxs)[1]))
# 
for(i in 1:15){
  vals[paste('Clusters',i,sep='')] <- kmeans(unspxs,i)$cluster
}
# Cluster only by lat long
unspx0 <- fnldf[,c('latitude','longitude')]
unspxs1 <- model.matrix(~categories-1,data=fnldf)
unspsx2 <- data.frame(clstrs1,fnldf[,c('latitude','longitude')])
clstrs0 <- kmeans(unspx0,4)$cluster
clstrs1 <- kmeans(unspxs1,5)$cluster
clstrs2 <- kmeans(unspxs2,8)$cluster
par(mfrow=c(1,2))
plot(fnldf$longitude,fnldf$latitude,pch=16,col=factor(clstrs0),cex=2,main='Clusters with only lat long')
plot(fnldf$longitude,fnldf$latitude,pch=16,col=factor(clstrs1),cex=2,main='Clusters with only category')
plot(fnldf$longitude,fnldf$latitude,pch=16,col=factor(clstrs0),cex=2,main='Clusters with only lat long')
plot(fnldf$longitude,fnldf$latitude,pch=16,col=factor(clstrs2),cex=2,main='Two Stage Cluster')
fnldf$clstr <- clstrs2
par(mfrow=c(1,2))
plot(fnldf$longitude,fnldf$latitude,pch=16,col=factor(clstrs2),cex=2,main='Two Stage Cluster');grid(10,10,'gray44')
with(fnldf[fnldf$clstr==5,],plot(longitude,latitude,xlim=c(min(fnldf$longitude),max(fnldf$longitude)),
                                 ylim=c(min(fnldf$latitude),max(fnldf$latitude)),
                                 col='deepskyblue',pch=16,cex=1.5));grid(10,10,'gray44')
plot(density(fnldf$Star1rvws[fnldf$clstr==5],kernel='epanechnikov'),col=1,xlim=c(0,300))
lines(density(fnldf$Star2rvws[fnldf$clstr==5],kernel='epanechnikov'),col=2)
lines(density(fnldf$Star3rvws[fnldf$clstr==5],kernel='epanechnikov'),col=3)
lines(density(fnldf$Star4rvws[fnldf$clstr==5],kernel='epanechnikov'),col=4)
lines(density(fnldf$Star5rvws[fnldf$clstr==5],kernel='epanechnikov'),col=5)
plot(ecdf(fnldf$Star1rvws[fnldf$clstr==5]),col=1,xlim=c(0,600),main='')
lines(ecdf(fnldf$Star2rvws[fnldf$clstr==5]),col=2)
lines(ecdf(fnldf$Star3rvws[fnldf$clstr==5]),col=3)
lines(ecdf(fnldf$Star4rvws[fnldf$clstr==5]),col=4)
lines(ecdf(fnldf$Star5rvws[fnldf$clstr==5]),col=5,main='');grid(10,10,'gray44')
legend('bottomright',legend=c('Star 1','Star 2','Star 3','Star 4','Star 5'),pch=rep(16,5),col=1:5)
#========================================================================================================================
bzs <- unique(df3$business_id)
for(i in 1:length(bzs)){
  if((i%%10)==0){print(paste('Iteration',i,', mofucka',sep=''))}
  xs_ss <- xs[df3$business_id==bzs[i],]
  wordsms <-data.frame(colSums(xs_ss));
  out <- data.frame(Businessid=bzs[i],Words=rownames(wordsms),RowSums=wordsms[,1]);rownames(out) <- NULL
  if(i==1){
    BizWordDf <- out
  } else {
    BizWordDf <- rbind(BizWordDf,out)
  }
}
bzwrdttl <- sqldf('select a.*,b.Freq as TotalWordCount 
             from BizWordDf a
             left join WordSummary b
             on a.Words=b.Word')
bzwrdttl2 <- sqldf('select a.*,b.Coefficients
                  from bzwrdttl a 
                  left join beta1 b
                  on a.Words = b.Words')
bzwrdttl3 <- sqldf('select a.*,b.clstr
                  from bzwrdttl2 a 
                  left join fnldf b
                  on a.Businessid = b.business_id')
bzwrdttl3$ExpctdSuckiness <- with(bzwrdttl3,RowSums*Coefficients)
bizwrdsmry <- sqldf('select businessid, clstr, words,
                    sum(ExpctdSuckiness) as ttlBizSuckiness,
                    avg(ExpctdSuckiness) as avgBizSuckiness,
                    sum(1) as BizWrdCount
                    from bzwrdttl3
                    group by 1,2,3') 
clstrsmry <- sqldf('select clstr, words,
                   sum(ExpctdSuckiness) as ttlClstrSuckiness,
                   avg(ExpctdSuckiness) as AvgClstrSuckiness,
                   sum(1) as ClstrWrdCount
                   from bzwrdttl3
                   group by 1,2')
clstrsmry[1:10,]
bizwrdsmry[1:10,]
FinalWordSmry <- sqldf('select b.Businessid, b.words,
                       b.clstr as Cluster,
                       b.ttlBizSuckiness,
                       a.ttlClstrSuckiness,
                       b.avgBizSuckiness,
                       a.AvgClstrSuckiness,
                       b.ttlBizSuckiness-a.ttlClstrSuckiness as RelativeSuckiness
                       from clstrsmry a 
                       left join bizwrdsmry b
                       on a.clstr = b.clstr and
                       a.words = b.words
                       order by 1,2')
FinalWordSmry <-FinalWordSmry[abs(FinalWordSmry$ttlBizSuckiness)>0,]
FinalWordSmry$RelativeSuckiness[is.na(FinalWordSmry$RelativeSuckiness)==T] <- 0
FinalWordSmry[FinalWordSmry$Cluster==3,][1:100,c(1,2,3,8)]
# tst <- FinalWordSmry[FinalWordSmry$Businessid =='8buIr1zBCO7OEcAQSZko7w',]
# tst <- tst[order(tst$RelativeSuckiness),]
TtlRank <- sqldf('select Businessid, Cluster, 
              sum(RelativeSuckiness) as ttlRelSuck
              from FinalWordSmry
              group by 1,2 order by 2,3')
vals <-unique(TtlRank$Cluster)
j <- length(vals)
TtlRank$ClusterRank <- 0
for(i in 1:j){
  n <- length(TtlRank$ClusterRank[TtlRank$Cluster==vals[i]])
  TtlRank$ClusterRank[TtlRank$Cluster==vals[i]] <- 1:n
}
TtlRank[1:25,]
FinalData <- sqldf('select a.*,b.*
                   from fnldf a
                   left join TtlRank b
                   on a.business_id=b.Businessid 
                   and a.clstr=b.Cluster')
FinalData[1:25,]
with(FinalData,plot(longitude,latitude,pch=16,col=Cluster))
fuckdf <- bizdf2[,c('business_id','name')]
lastfin <- sqldf('select a.*, b.name
                 from FinalWordSmry a 
                 left join fuckdf b
                 on a.Businessid=b.business_id')
#========================================================================================================================
write.csv(beta1,'WordProbs.csv',row.names=FALSE)
write.csv(FinalWordSmry,'WordClusterSuckiness.csv',row.names=F)
write.csv(lastfin,'WordClusterSuckiness2.csv',row.names=F)
write.csv(FinalData,'ClusterSuckiness.csv',row.names=F)
#========================================================================================================================
#  	 _____________________                              ______________________
#		 `-._:  .:'   `:::  .:\           |\__/|           /::  .:'   `:::  .:.-'
#		     \      :          \          |:   |          /         :       /    
#		      \     ::    .     `-_______/ ::   \_______-'   .      ::   . /      
#		       |  :   :: ::'  :   :: ::'  :   :: ::'      :: ::'  :   :: :|      
#		       |     ;::         ;::         ;::         ;::         ;::  |      
#		       |  .:'   `:::  .:'   `:::  .:'   `:::  .:'   `:::  .:'   `:|      
#		       /     :           :           :           :           :    \      
#		      /______::_____     ::    .     ::    .     ::   _____._::____\      
#		                    `----._:: ::'  :   :: ::'  _.----'                    
#		                           `--.       ;::  .--'                          
#		                               `-. .:'  .-'                              
#		                                  \    /  
#		                                   \  /                                  
#		                                    \/        BatmanFran
#========================================================================================================================
# End
#========================================================================================================================