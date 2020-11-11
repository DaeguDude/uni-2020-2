# 텍스트마이닝
# install.packages("tm")

# 형태소 분석
# install.packages("SnowballC")

# 단어 구름
# install.packages("wordcloud")

# 나이브 베이즈 구현
# install.packages("e1071")

library(tm)
library(SnowballC)
library(wordcloud)
library(e1071)

# 데이터 입력
sms_raw <- read.csv("sms_spam.csv", stringsAsFactors = FALSE)


# 데이터 구조 확인
str(sms_raw)

# 변수 팩터화(명목화)
sms_raw$type <- factor(sms_raw$type)
 
# 팩터화 된 변수 구조 확인
str(sms_raw)
table(sms_raw$type)

# corpus
# VCorpus 코퍼스(말뭉치)생성
sms_corpus <- VCorpus(VectorSource(sms_raw$text))
print(sms_corpus)

inspect(sms_corpus[1:2])
as.character(sms_corpus[1])
as.character(sms_corpus[2])
as.character(sms_corpus[[1]])
as.character(sms_corpus[[2]])
sms_corpus[[1]]$meta
sms_corpus[[1]]$content
lapply(sms_corpus, as.character)

# 대문자를 소문자로 치환
sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))
# 숫자 제거
sms_corpus_clean <- tm_map(sms_corpus, removeNumbers)
getTransformations()

# 불용어 제거


# 구두점 제거
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)
removePunctuation("hello...world")

# 어근 추출
wordStem(c("learns", "learned", "learning"))
sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)

# 공백 제거
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)

# DTM(Document Term Matrix) 생성



# DTM2
sms_dtm2 <- DocumentTermMatrix(sms_corpus, control = list(
  tolower = TRUE,
  removeNumbers = TRUE,
  stopwords = TRUE,
  removePunctuation = TRUE,
  stemming = TRUE
))

# DTM3
sms_dtm3 <- DocumentTermMatrix(sms_corpus, control = list(
  tolower = TRUE,
  removeNumbers = TRUE,
  stopwords = function(x){
    removeWords(x, stopwords())
  },
  removePunctuation = TRUE,
  stemming = TRUE 
))



