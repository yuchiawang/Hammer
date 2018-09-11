############Read file##############
getwd()
setwd("D:/Lab/Rcode")
library(data.table)
library(dplyr)
Amazon=fread('./data/Amazon_reviews.csv',stringsAsFactors=FALSE)

############Data clear###############
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


################googleVis####################
#install.packages('googleVis')
library(googleVis)
load('./data/googleVis_data.Rdata')
Amazon %>% 
  group_by(.,tool_name,Year) %>%
  summarise(
    star_num=n(),
    star=mean(tool_star_sub,na.rm = T)
    
  ) %>% as.data.frame()->googleVis_df
plot(gvisMotionChart(
  googleVis_df,"tool_name", "Year",
  options=list(width=1000, height=600)))
#save(Amazon,googleVis_df,file='googleVis_data.Rdata')

##################Brand Discuss############################
#by年分
tool_name %>% unique


brand_discuss_df=Amazon %>% 
  group_by(.,tool_name,Year) %>%
  summarise(
    discuss_n=n()
  ) %>% filter(Year>=2010)


brand_discuss_df %>% 
  group_by(.,tool_name) %>%
  summarise(
    discuss_sum=sum(discuss_n)
  )->top10_df





#top10品牌討論度by年分
brand_discuss_df$tool_name %in% 
  top10_df [order(top10_df$discuss_sum,decreasing = T),]$tool_name[1:10] %>%
  
  subset(brand_discuss_df,.)->discuss_plot_df


#by 月份
brand_discuss_df_month=Amazon %>% 
  group_by(.,tool_name,Year,Month) %>%
  summarise(
    discuss_n=n()
  )




#top10品牌討論度by月分
brand_discuss_df_month$tool_name %in% 
  top10_df [order(top10_df$discuss_sum,decreasing = T),]$tool_name[1:10] %>%
  subset(brand_discuss_df_month,.)->discuss_plot_df_month

######################品牌被討論數隨時間分布圖################################################
library(ggplot2)

#by年分


ggplot(discuss_plot_df, aes(x = Year, y =discuss_n,colour=tool_name)) + 
  geom_line() + 
  geom_point(  fill = "white") 


#by月分
subset(discuss_plot_df_month,Year>=2015) %>%
  ggplot(data=.,aes(x=Month, y=discuss_n, color=tool_name,group=tool_name) ) + geom_line() +geom_point() ->p
p <- p + facet_grid(facets = Year ~ ., margins = FALSE) + theme_bw()
p + scale_y_continuous() + scale_x_discrete(labels=labels) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8))

#############商品討論次數###########
Amazon %>% 
  group_by(.,artTitle) %>%
  summarise(
    discuss_n=n(),
    url=first(artUrl),
    brand=first(tool_name)
  )  ->top10_tool_df

top10_tool_df[order(top10_tool_df$discuss_n,decreasing = T),] %>% View


##############評論字頻表#############
packages = c(
  "dplyr","ggplot2","caTools","tm","SnowballC","ROCR","rpart","rpart.plot","randomForest")
existing = as.character(installed.packages()[,1])
for(pkg in packages[!(packages %in% existing)]) install.packages(pkg)

# Sys.setlocale("LC_ALL","C")
options(digits=5, scipen=10)

library(dplyr)
library(tm)
library(SnowballC)
library(ROCR)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(wordcloud)

##建立文集
corpus = Corpus(VectorSource(Amazon$artContent)) 
corpus[[1]]$content[1:10]

##轉為小寫
corpus = tm_map(corpus, content_transformer(tolower))
corpus[[1]]$content

##移除標點
corpus = tm_map(corpus, removePunctuation)
corpus[[1]]$content

##去除贅字
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))#因為每一篇都有apple，所以apple本身就沒有意義，因此去除
corpus[[1]]$content

##字根還原
corpus = tm_map(corpus, stemDocument)
corpus[[1]]$content

### 文件字詞矩陣 (字頻表，DTM)
##建立文件字詞矩陣 (Document Term Matrix)
frequencies = DocumentTermMatrix(corpus)
frequencies
#document:文件數量;term:裡面總共有用到11625個字;sparsity:稀疏性

# Look at matrix 
inspect(frequencies[1000:1005,505:515])
findFreqTerms(frequencies, lowfreq=20) #出現次數超過20以上的字顯現出來

##### 移除頻率太低的字詞
sparse = removeSparseTerms(frequencies, 0.995) #此字在一千篇裡出現少過5次就會被移除
sparse

### 模型與預測

##### 轉成資料框
# Convert to a data frame把矩陣轉成資料框
tweetsSparse = as.data.frame(as.matrix(sparse)) 
# Make all variable names R-friendly 當矩陣轉成資料框後，把矩陣裡的欄位名稱用合法的方式取出來，所以用make.name的方式
colnames(tweetsSparse) = make.names(colnames(tweetsSparse))

text=tweetsSparse %>% colSums %>% sort %>% as.data.frame()
text$word=row.names(text)
#text=text %>% filter(.>100)  

set.seed(1233)
wordcloud(words = text$word, freq = text$., min.freq = 200,
          max.words=5000, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
'good' %in% text$word


##讀取字典
appearance=read.table('./詞庫/appearance.txt')
handle=read.table('./詞庫/use.txt')
convenient=read.table('./詞庫/convenient.txt')
quality=read.table('./詞庫/quality.txt')
price=read.table('./詞庫/price.txt')

dic_list=list(appearance,handle,convenient,quality,price)




qq=data.frame()#宣告空的dataframe


for(i in 1:5){
  
  V1=dic_list[[i]]$V1[1] %>% as.character()
  V2=text$word  %in% dic_list[[i]]$V1 %>% sum 
  
  qq=rbind(qq,data.frame(V1=V1,V2=V2))
}

ggplot(qq, aes(x=V1,y=V2))+geom_bar(stat="identity")+xlab("話題討論")+ylab("次數") 

text$word  %in% dic_list[[1]]$V1 
