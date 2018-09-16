############Read file##############
getwd()
setwd("D:/Lab/Rcode/Hammer")
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
library(stringr)

##建立文集
Amazon$artContent<-toupper(str_trim(Amazon$artContent)) #把評論都先變大寫(不然轉小寫都會出問題)
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


#存csv
writeLines(as.character(corpus), con="corpus.txt")

##詞性
#install.packages("NLP")
#install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at/", type = "source")
library("NLP")
library("openNLP")
library("openNLPmodels.en")

corpus1=scan('./corpus.txt',"")

corpus1 <- as.String(corpus1)
## Need sentence and word token annotations.
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
a2 <- annotate(corpus1, list(sent_token_annotator, word_token_annotator))
## Entity recognition for persons.
entity_annotator <- Maxent_Entity_Annotator()
entity_annotator
a3<-annotate(corpus1, entity_annotator, a2) #跑很久
## Directly:
entity_annotator(corpus1, a2)
## And slice ...
corpus1[entity_annotator(corpus1, a2)] #跑很久
## Variant with sentence probabilities as features.
annotate(corpus1, Maxent_Entity_Annotator(probs = TRUE), a2) #跑很久
head(annotate(corpus1, Maxent_POS_Tag_Annotator(probs = TRUE), a2))

a3 = a3; a2 = a2; 
save(a3,a2, file="辭庫/POS tagging.rdata")


q <- strsplit(unlist(corpus1),'/NN')
q <- tail(strsplit(unlist(q[1])," ")[[1]],1)




### 文件字詞矩陣 (字頻表，DTM)
##建立文件字詞矩陣 (Document Term Matrix)
frequencies = DocumentTermMatrix(corpus)
frequencies
#document:文件數量;term:裡面總共有用到11625個字;sparsity:稀疏性

# Look at matrix 
findFreqTerms(frequencies, lowfreq=20) #出現次數超過20以上的字顯現出來


##### 移除頻率太低的字詞
sparse = removeSparseTerms(frequencies, 0.995) #此字在一千篇裡出現少過5次就會被移除
sparse


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
design=read.table('./詞庫/design.txt')
use=read.table('./詞庫/use.txt')
use1=read.table('./詞庫/use1.txt')
service=read.table('./詞庫/service.txt')
quality=read.table('./詞庫/quality.txt')
price=read.table('./詞庫/price.txt')

dic_list=list(design,use,service,quality,price)




qq=data.frame()#宣告空的dataframe


for(i in 1:5){
  
  V1=dic_list[[i]]$V1[1] %>% as.character()
  V2=text$word  %in% dic_list[[i]]$V1 %>% sum 
  
  qq=rbind(qq,data.frame(V1=V1,V2=V2))
}

ggplot(qq, aes(x=V1,y=V2))+geom_bar(stat="identity")+xlab("話題討論")+ylab("次數") +scale_y_continuous(breaks =c(0:15)) 




writeLines(as.character(text$word), con="corpus.txt") #寫入記事本



###########詞性標註、無字根還原
txt <- paste(readLines("corpus.txt"), collapse="\n")

extractPOS <- function(x, thisPOSregex) {
  x <- as.String(x)
  wordAnnotation <- annotate(x, list(Maxent_Sent_Token_Annotator(), Maxent_Word_Token_Annotator()))
  POSAnnotation <- annotate(x, Maxent_POS_Tag_Annotator(), wordAnnotation)
  POSwords <- subset(POSAnnotation, type == "word")
  tags <- sapply(POSwords$features, '[[', "POS")
  thisPOSindex <- grep(thisPOSregex, tags)
  tokenizedAndTagged <- sprintf("%s", x[POSwords][thisPOSindex])
  untokenizedAndTagged <- paste(tokenizedAndTagged, collapse = " ")
  untokenizedAndTagged
}


txt=as.data.frame(lapply(txt, extractPOS, "NN"))
txt

word<-"arm building furniture tasks space guy company star hope addition trim type return surface pins complaints garage hits standard lifetime mallets delivery length elbow strike tape favorite everyone pay force material pull control shape issues compact wall cost fall china version problems color ground power kids friend idea products man others belt damage months shock wear reason replacement blow project pictures household repairs breaking stiletto purpose shop wife items titanium carpenter life fiberglass daughter people point ball pieces stakes help duty bag beat youre reviews apartment days part hammering couple car ones anyone order store nothing pound someone stars thanks picture starter drive screwdriver brand shipping mine father stuff fathers construction leather usa problem value screw design times wont plastic sledge handles finish rocks anything piece thought break son basic hit hands year projects swing pick hold claw christmas purchase place end side money doesnt jobs husband face steel lot makes bit something item things metal dad box thing home feel everything wood rubber rock mallet size nails grip house light hand day nail years job weight time gift work love need product hammers price head will tools quality tool use hammer"
txt=read.table(text=word,col.names=c('word'))


#與字典比對

##讀取字典
design=read.table('./詞庫/design.txt')
use=read.table('./詞庫/use.txt')
use1=read.table('./詞庫/use1.txt')
service=read.table('./詞庫/service.txt')
quality=read.table('./詞庫/quality.txt')
price=read.table('./詞庫/price.txt')

dic_list=list(design,use,service,quality,price)
qq=data.frame()#宣告空的dataframe


for(i in 1:5){
  
  V1=dic_list[[i]]$V1[1] %>% as.character()
  V2=txt$word  %in% dic_list[[i]]$V1 %>% sum 
  
  qq=rbind(qq,data.frame(V1=V1,V2=V2))
}

ggplot(qq, aes(x=V1,y=V2))+geom_bar(stat="identity")+xlab("話題討論")+ylab("次數")

