setwd('D:\\Lab\\Rcode')
getwd()


########Load data############
library(data.table)
library(dplyr)
library(ggplot2)
hammer_detail= fread('./data/Amazon_reviews_hammer_detail_update.csv')
Amazon=fread('./data/Amazon_reviews.csv')
rank= fread('./data/rank.csv',stringsAsFactors = FALSE,header=F)




##########hammer_detail Data Clean####################
hammer_detail=hammer_detail %>% 
  sapply(.,function(v)ifelse(v=='',NA,v)) %>% 
  as.data.frame() 
hammer_detail$tool_price_nosign=hammer_detail$tool_price %>% substr(2,1000000L) %>% as.numeric()





##########Amazon revews Data Clean####################
##Date:
Amazon$artDate=Amazon$artDate %>% as.Date
Amazon$Year=year(Amazon$artDate)
Amazon$Month=format(Amazon$artDate, "%b")

##Toolname:
Amazon$tool_name=Amazon$tool_name %>% as.factor
tool_name=Amazon$tool_name %>% unique
##ToolStar:
Amazon$tool_star_sub=Amazon$tool_star %>% substr(1,3)
Amazon$tool_star_sub =Amazon$tool_star_sub %>% as.numeric()
########Data Analysis############

is.na(hammer_detail) %>% colSums() %>% sort

(hammer_detail$error==0) %>% sum

subset(hammer_detail,!is.na(tool_name)) %>% is.na() %>% colSums()
subset(hammer_detail,!is.na(tool_name))  %>% dim
hammer_detail$star=substr(hammer_detail$tool_star,1,3) 



##################Brand Discuss############################
#by年分
tool_name %>% unique


brand_discuss_df=Amazon %>% 
  group_by(.,tool_name,Year) %>%
  summarise(
    discuss_n=n()
  )


brand_discuss_df %>% 
  group_by(.,tool_name) %>%
  summarise(
    discuss_sum=sum(discuss_n)
  )->top10_df





#top10品牌討論度by年分
brand_discuss_df$tool_name %in% 
  top10_df [order(top10_df$discuss_sum,decreasing = T),]$tool_name[1:10] %>%
  
  subset(brand_discuss_df,.)->discuss_plot_df



##############資料分布##################
#top10品牌討論度

top10_brand<-top10_df [order(-top10_df$discuss_sum ),] %>% .$tool_name %>% .[1:10] 

#各品牌價錢分布(all)

top10_brand<-gsub("\\Stilletto",replacement="Stiletto",top10_brand) #字串取代

filter(hammer_detail,tool_name %in%  top10_brand ,!is.na(tool_price)) %>% 
  ggplot(data=.,aes(x=tool_name,y=tool_price_nosign,fill=tool_name))+
  geom_boxplot()+ scale_y_continuous(breaks=c(10,30,60,90,120,150,180,210))

filter(hammer_detail,tool_name== 'MAXCRAFT' ) %>% View

#各品牌價錢分布(-Stilletto)
top10_brand=top10_brand[-c(9,10)]
filter(hammer_detail,tool_name %in%  top10_brand ,!is.na(tool_price)) %>% 
  ggplot(data=.,aes(x=tool_name,y=tool_price_nosign,fill=tool_name))+geom_boxplot()+
  scale_y_continuous(breaks=c(10,20,30,40,50,60,70,80,90,100))

#材質
hammer_detail$material %>% summary %>% sort %>% View
hammer_material=hammer_detail$material %>% summary %>% sort  %>%as.data.frame()
hammer_material$material=row.names(hammer_material)
hammer_material$material[1:25]='other'
hammer_material=hammer_material[-50,]
hammer_material=hammer_material[-c(1:25),]
ggplot(data=hammer_material,aes(x=material,y=.,fill=material))+
  geom_bar(stat="identity")

hammer_material[1:49,1] %>% sum


#############ounce############

##字串切割
library(stringr)
#hammer_detail <- hammer_detail %>% 
#mutate(size1 = str_extract(hammer_detail$art_title, "[0-9]+ Ounce|[0-9]+Ounce|[0-9]+-Ounce|[0-9]+-oz|[0-9]+ oz|[0-9]+oz|[0-9]+ Oz|[0-9]+-Oz|[0-9]+Oz|[0-9]+ pounds|[0-9]+ Pounds|[0-9]+-pound|[0-9]+-Pound|[0-9]+lb|[0-9]+ lb|[0-9]+-lb|\\dLB|[0-9]+ LB|[0-9]+-LB"))
hammer_detail <- hammer_detail %>% 
  mutate(size_ounce = str_extract(hammer_detail$art_title, "[0-9]+ Ounce|[0-9]+Ounce|[0-9]+-Ounce|[0-9]+-oz|[0-9]+ oz|[0-9]+oz|[0-9]+ Oz|[0-9]+-Oz|[0-9]+Oz"))
#hammer_detail <- hammer_detail %>% 
#mutate(size_lb = str_extract(hammer_detail$art_title, "[0-9]+ pounds|[0-9]+ Pounds|[0-9]+-pound|[0-9]+-Pound|[0-9]+lb|[0-9]+ lb|[0-9]+-lb|\\dLB|[0-9]+ LB|[0-9]+-LB"))

#hammer_detail$size_lb<-substr(hammer_detail$size_lb,1,1) %>%  as.numeric()

#ounce=16*pound
#hammer_detail$size_lb=hammer_detail$size_lb*16

##取出字串
hammer_detail$size_ounce=str_extract(hammer_detail$size_ounce,"[0-9]+") %>% as.integer()




hammer_size=hammer_detail[,c("size_ounce","tool_name")]  %>% na.omit()
hammer_size=hammer_size [order(hammer_size$size_ounce),] 
hammer_size2=hammer_size %>% group_by(size_ounce) %>% summarize(count=n(),brand=n_distinct(tool_name)) %>% filter(count>=10) 

ggplot(data=hammer_size2,aes(x=size_ounce,y=brand))  + scale_x_continuous(breaks=1:40)+ scale_y_continuous(breaks=c(10,20,30,40,50,60,70,80,90,100)) + geom_line()+geom_point() +ggtitle("競爭狀況分析")+xlab('產品(OZ)')+ylab('品牌數量')

##################rank################

