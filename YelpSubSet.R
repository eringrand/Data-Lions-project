rvwdf <- read.csv('yelp_academic_dataset_review.csv')
bizdf <- read.csv('yelp_academic_dataset_business.csv')
tipdf <- read.csv('yelp_academic_dataset_tip.csv')
usrdf <- read.csv('yelp_academic_dataset_user.csv')
chkdf <- read.csv('yelp_academic_dataset_checkin.csv')
# Subsets the data
rvwdf$BadRating <- ifelse(rvwdf$stars<=1,1,0)
x <- data.frame(table(bizdf$city))
x <- x[order(x$Freq),]
RowCntDf <- data.frame(table(rvwdf$business_id))
tst <- merge(bizdf,RowCntDf,by.x='business_id','Var1')
tst$Res <- word_search('Restaurant',tst$categories)
tst <- tst[,c('categories','Freq','business_id','city','Res')][tst$Res==1,]
x <- sqldf('select city, sum(Freq) as ReviewCount, count(*) as BizCount from tst group by 1')
x <- x[order(x$ReviewCount,decreasing=T),]
geos <- as.character(x$city[1:5])
# This part selects Las Vegas
bizlist <- bizdf$business_id[bizdf$city == geos[1] & bizdf$attributes.Waiter.Service=='True']
txtvector <- as.character(rvwdf$text[rvwdf$business_id %in% bizlist])
