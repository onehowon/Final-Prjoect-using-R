## 목차
# 수집과정
# 전처리과정

# 수집 과정
```{r setup, include=FALSE}
library(tidyverse)
library(httr)
library(rvest)
library(jsonlite)
library(xml2)
library(XML)
library(data.table)
library(dplyr)
library(stringr)
library(plyr)

# 공공 데이터 포털에서 오픈 api로 데이터를 가져오기로 함
api_url <- ("http://openapi.molit.go.kr/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcAptTradeDev")

# 발급받은 api key
serviceKey <- "your api key" # 이 곳에 api key 할당

# 전국 중에서 서울시 지역코드만 추출
locCode_nm <-c("11110","11140","11170","11200","11215","11230","11260","11290","11305","11320",
               "11350","11380","11410","11440","11470","11500","11530","11545","11560","11590",
               "11620","11650","11680","11710","11740")

# 서울시 구 추출
locCode <-c("종로구","중구","용산구","성동구","광진구","동대문구","중랑구","성북구","강북구","도봉구",
            "노원구","은평구","서대문구","마포구","양천구","강서구","구로구","금천구","영등포구","동작구",
            "관악구","서초구","강남구","송파구","강동구")

# 분석 데이터는 2020년 5월부터 2021년 5월까지로 설정
datelist <-c("202005","202006","202007","202008","202009","202010","202011","202012","202101","202102","202103","202104","202105")

# URL을 긁어와 전체 데이터를 추출하기 위해 빈 리스트를 하나 만들고 카운트 변수 할당
urllist <- list()
cnt <-0

# 데이터 리스트를 추출하기 위해 반복문을 만듬
for(i in 1:length(locCode)){
  for(j in 1:length(datelist)){
    cnt = cnt + 1
    urllist[cnt] <- paste0(api_url,serviceKey, "&pageNo=1", "&numOfRows=1000", "&LAWD_CD=",locCode[i],"&DEAL_YMD=", datelist[j]) 
  }
}

# 전체 데이터를 담기 위해 url 리스트를 긁어와야함
total<-list()

# xml 형식으로 전체 url을 긁어옴
for(i in 1:length(urllist)){
  
  apt <- list()
  apt_data <-data.table()
  
  raw.data <- xmlTreeParse(urllist[i], useInternalNodes = TRUE,encoding = "utf-8")
  rootNode <- xmlRoot(raw.data)
  items <- rootNode[[2]][['items']]
  
  size <- xmlSize(items)
  
  for(j in 1:size){ # 할당해야하는 컬럼들만 추출
    apt <- xmlSApply(items[[j]],xmlValue)
    apt_data <- data.table( price = item_temp[1],
                            con_year = item_temp[2],
                            year = item_temp[3],
                            street = item_temp[4],
                            dong = item_temp[11],
                            aptnm = item_temp[17],
                            month = item_temp[18],
                            dat = item_temp[19],
                            area = item_temp[21],
                            bungi = item_temp[22],
                            floor = item_temp[24],
                            gu_code = locCode[((j-1)%/%12)+1],
                            gu = locCode_nm[((j-1)%/%12)+1]
    )
    item[[j]]<- apt_data
  }
  total[[i]] <- rbindlist(item) # 데이터 프레임 형태로 정리
}

APT_2021 <- rbindlist(total) # 변수에 데이터프레임 할당

head(APT_2021) # 결과 확인
# write.csv(APT_2021, APT2021.csv)
```
* 수집 과정
* (1) 필요한 라이브러리 할당 후 공공데이터포털 홈페이지에 API Key 요청(24시간 소요)
* (2) URL과 사용할 개인 API Key 불러오기
* (3) 추출할 항목(연도, 지역, 지역코드) 리스트 할당
* (4) 빈 리스트 할당 후 반복문으로 필요한 데이터 추출
* (5) XML 형식으로 전체 데이터를 긁어올 수 있는 반복문 입력
* (6) 추출된 데이터의 데이터프레임화

## 1. 카카오맵 API 불러오기
## 2. 필요한 위도, 경도만 추출
```{r}
for (i in 1:nrow(x = df)){
  cat('현재', i, '번째 주소의 위경도 좌표를 얻는 중입니다!\n')
  res <- GET(url = 'https://dapi.kakao.com/v2/local/search/address.json',
             query = list(query = df$지번주소[i]),
             add_headers(Authorization = Sys.getenv('KAKAO_API' ))) # 이곳에 api키 할당
  
  tryCatch({
    coord <- res %>% content(as = 'text') %>% fromJSON()
    
    df$위도[i] <- coord$documents$y
    df$경도[i] <- coord$documents$x
    
    Sys.sleep(time = 1)
  }, error = function(e) cat('--> 에러가 발생하여 건너 뜁니다.\n'))
}

is.na(x = df$위도) %>% sum()

df <- dplyr::filter(.data= df, is.na(x= 위도) == FALSE)
head(df)

#write.csv(df, "카카오 맵 위도,경도.csv")
```
* 수집과정
* (1) 카카오 개발자 사이트에서 개인 API Key 요청
* (2) df = 구별(25개 구) 40개의 아파트가 담긴 데이터(Total = 1,000)
* (3) 반복문을 통해 1,000개의 아파트 위도, 경도를 받아옴(JSON 형식 사용)
* (4) 카카오 맵에서 검색이 안되고 위도, 경도를 구하지 못하는 부분은 NA값이므로 제거
* (5) 추출된 데이터의 데이터프레임화




# 전처리 과정
## 1. 서울의 지역구 별 그룹화하여 데이터 합치기
## 2. 고등교육기관, 

## 1. 서울시 행정구별 교육기관 데이터
### 1.1 서울시 행정구별 초중등 및 고등교육기관 병합
### 1.2 할당해야 하는 컬럼만 추출해 변수 할당 후 구별로 합산
```{r}
library(dplyr)
library(plyr)
library(stringr)
high_edu <- read.csv("고등교육기관 주소록.csv")
high <- subset(high_edu, 시도=="서울")
high_2 <- high[, -c(1,2,3,6,7,8,9,10,11,12,13,14)]

element_edu <- read.csv("유초중등교육기관 주소록.csv")
element <- subset(element_edu, 시도=="서울")
element_2 <- element[,-c(1,2,3,4,5,7,9,10,11,12,13,14,15,16,17)]

education <- full_join(high_2, element_2)
education_final <- rename(education, "gu" = "행정구")
education_final <- education_final %>%
  group_by(gu) %>%
  summarise(
    gu_num = n()
  )
head(education_final)
```
* (1) 고등 교육기관과 유초중등교육기관의 컬럼이 같으므로 서울시에 존재하는 교육기관만 추출 후 불필요한 컬럼 제거
* (2) 합집합으로 병합 후 데이터의 행정구 컬럼 이름을 gu로 수정(향후 데이터 병합을 위해)
* (3) 구 단위로 교육기관 합계 산출 

## 2. 서울시 행정구별 전철역 데이터
### 2.1. 전국 도시철도 역사 정보 표준 데이터에서 필요한 열 추출
### 2.2. 도로명 주소에서 행정구 추출해 새로운 열에 할당
### 2.3. 행정구별로 그룹화하여 합산
```{r}
library(plyr)
rail <- read.csv("전국도시철도역사정보표준데이터.csv")
rail_seoul <- subset(rail, (노선명 == '경의중앙선'| 노선명 == '경원선'| 노선명 == '분당선'| 노선명 == '경춘선'| 노선명 == '수인선'| 노선명 == '서울 도시철도 1호선'| 노선명 == '서울 도시철도 2호선'| 노선명 == '서울 도시철도 3호선'| 노선명 == '서울 도시철도 4호선'| 노선명 == '서울 도시철도 5호선'| 노선명 == '서울 도시철도 6호선'| 노선명 == '서울 도시철도 7호선'| 노선명 == '서울 도시철도 8호선'| 노선명 == '서울 도시철도 9호선'))
rail_total <- rail_seoul[,-c(1,3,4,5,6,7,8,9,10,11,12,14,15,16,17)]
head(rail_total)

rail_loc <- function(x){
  seoul <- filter(rail_total, grepl(x, 역사도로명주소))
  return(seoul)
}

rail_1 <- rail_loc("강서구") ;rail_2 <- rail_loc("양천구")
rail_3 <- rail_loc("구로구"); rail_4 <- rail_loc("영등포구")
rail_5 <- rail_loc("금천구"); rail_6 <- rail_loc("동작구")
rail_7 <- rail_loc("관악구"); rail_8 <- rail_loc("서초구")
rail_9 <- rail_loc("강남구") ;rail_10 <- rail_loc("송파구")
rail_11 <- rail_loc("강동구"); rail_12 <- rail_loc("광진구")
rail_13 <- rail_loc("중랑구"); rail_14 <- rail_loc("노원구")
rail_15 <- rail_loc("도봉구"); rail_16 <- rail_loc("강북구")
rail_17 <- rail_loc("성북구") ;rail_18 <- rail_loc("동대문구")
rail_19 <- rail_loc("성동구") ;rail_20 <- rail_loc("중구")
rail_21 <- rail_loc("용산구") ;rail_22 <- rail_loc("종로구")
rail_23 <- rail_loc("서대문구") ;rail_24 <- rail_loc("마포구")
rail_25 <- rail_loc("은평구")

rail_1$역사도로명주소= c("강서구") ; rail_2$역사도로명주소 = c("양천구")
rail_3$역사도로명주소 = c("구로구") ;rail_4$역사도로명주소 = c("영등포구")
rail_5$역사도로명주소 = c("금천구") ;rail_6$역사도로명주소 = c("동작구")
rail_7$역사도로명주소 = c("관악구") ;rail_8$역사도로명주소 = c("서초구")
rail_9$역사도로명주소 = c("강남구") ;rail_10$역사도로명주소 = c("송파구")
rail_11$역사도로명주소 = c("강동구") ;rail_12$역사도로명주소 = c("광진구")
rail_13$역사도로명주소 = c("중랑구") ;rail_14$역사도로명주소 = c("노원구")
rail_15$역사도로명주소 = c("도봉구") ;rail_16$역사도로명주소 = c("강북구")
rail_17$역사도로명주소 = c("성북구") ;rail_18$역사도로명주소 = c("동대문구")
rail_19$역사도로명주소 = c("성동구") ;rail_20$역사도로명주소 = c("중구")
rail_21$역사도로명주소 = c("용산구") ;rail_22$역사도로명주소 = c("종로구") 
rail_23$역사도로명주소 = c("서대문구") ;rail_24$역사도로명주소 = c("마포구")
rail_25$역사도로명주소 = c("은평구")

rail_seoul_total <- join_all(list(rail_1,rail_2,rail_3,rail_4,rail_5,rail_6,rail_7,rail_8,rail_9,rail_10,rail_11,rail_12,rail_13,rail_14,rail_15,rail_16,rail_17,rail_18,rail_19,rail_20,rail_21,rail_22,rail_23,rail_24,rail_25), by="역사도로명주소", type="full")
head(rail_seoul_total)
names(rail_seoul_total)[2] <- "gu"

rail_final <- rail_seoul_total %>%
  group_by(gu) %>%
  dplyr::summarise(
    gu_num = n()
  )
```
* (1) 서울시에 존재하는 전철 노선만 할당
* (2) 불필요한 컬럼 제거 후 역사 도로명 주소를 정식명칭(OO구) 형태로 바꿔줌(향후 데이터 병합 위해)
* (3) 데이터 병합 후 합계 산출

##3. 서울시 행정구별 공원 데이터
### 3.1. 서울시 주요 공원현황에서 필요한 열만 추출
### 3.2. 행정구별로 그룹화 하여 합산
```{r}
park <- read.csv("서울시 주요 공원현황.csv")
park_seoul <- park[-c(3),-c(1,3,4,5,6,7,8,9,10,11,13,14,15,16,17,18,19,20)]
head(park_seoul) #구
names(park_seoul)[2] <- "gu"

park_final <- park_seoul %>%
  group_by(gu) %>%
  dplyr::summarise(
    gu_num = n()
  )
park_final <- park_final[-c(1),]
```
* (1) 불필요한 컬럼 제거
* (2) 합계 산출 후 서울시 내 존재 하지 않는 데이터(1개) 제거

## 4. 서울시 행정구별 병원 데이터
### 4.1. 전국 병원 주소록 데이터에서 서울시에 해당하는 행 추출
```{r}
hospital <- read.csv("병원 주소록.csv")
hospital_seoul <- subset(hospital, 시도=="서울")
```

## 5. 서울시 행정구별 보건소 보건분소 데이터
### 5.1. 전국 보건소 및 보건분소 데이터에서 서울시에 해당하는 행 추출
### 5.1. 행정구별로 그룹화 하여 합산
```{r}
health <- read.csv("보건소 주소록.csv")
health_seoul <- subset(health, 시도=="서울")

heal <- full_join(health_seoul, hospital_seoul)

heal_total <- heal[,-c(1,2,3,4,5,8,9,10,11)]
head(heal_total);tail(heal_total)
names(heal_total)[1] <- "gu"

heal_final <- heal_total %>%
  group_by(gu) %>%
  dplyr::summarise(
    gu_num = n()
  )
```
* (1) 서울시에 존재하는 병원과 보건소만 추출해 합집합 병합
* (2) 불필요한 컬럼 제거 후 합계 산출

## 6. 서울시 행정구별 범죄 데이터
### 6.1. 서울시 범죄 현황 데이터에서 필요한 열만 추출
### 6.2. 행정구별로 그룹화하여 합산
```{r}
crime <- read.csv("서울시 범죄 현황.csv")
head(crime) #구
crime_final <- crime[-c(1,2),-c(1,4,5,6,7,8,9,10,11,12,13,14)]
names(crime_final)[1] <- "gu"
```

## 7. 서울시 행정구별 유흥주점 데이터
### 7.1. 서울특별시 유흥주점영업 인허가 정보 데이터에서 필요한 열만 추출
### 7.2. 도로명 주소에서 행정구 정보 추출해 새로운 열에 할당
### 7.3.행정구별로 그룹화하여 합산
```{r}
entertain <- read.csv("서울특별시 유흥주점영업 인허가 정보.csv")
head(entertain)
entertain_2 <- entertain[,-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,18,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44)]
head(entertain_2)

enter_seoul <- function(x){
  seoul <- filter(entertain_2, grepl(x, 도로명주소))
  return(seoul)
}

loc_1 <- enter_seoul("강서구") ;loc_2 <- enter_seoul("양천구")
loc_3 <- enter_seoul("구로구"); loc_4 <- enter_seoul("영등포구")
loc_5 <- enter_seoul("금천구"); loc_6 <- enter_seoul("동작구")
loc_7 <- enter_seoul("관악구"); loc_8 <- enter_seoul("서초구")
loc_9 <- enter_seoul("강남구") ;loc_10 <- enter_seoul("송파구")
loc_11 <- enter_seoul("강동구"); loc_12 <- enter_seoul("광진구")
loc_13 <- enter_seoul("중랑구"); loc_14 <- enter_seoul("노원구")
loc_15 <- enter_seoul("도봉구"); loc_16 <- enter_seoul("강북구")
loc_17 <- enter_seoul("성북구") ;loc_18 <- enter_seoul("동대문구")
loc_19 <- enter_seoul("성동구") ;loc_20 <- enter_seoul("중구")
loc_21 <- enter_seoul("용산구") ;loc_22 <- enter_seoul("종로구")
loc_23 <- enter_seoul("서대문구") ;loc_24 <- enter_seoul("마포구")
loc_25 <- enter_seoul("은평구")

loc_1$도로명주소 = c("강서구") ; loc_2$도로명주소 = c("양천구")
loc_3$도로명주소 = c("구로구") ;loc_4$도로명주소 = c("영등포구")
loc_5$도로명주소 = c("금천구") ;loc_6$도로명주소 = c("동작구")
loc_7$도로명주소 = c("관악구") ;loc_8$도로명주소 = c("서초구")
loc_9$도로명주소 = c("강남구") ;loc_10$도로명주소 = c("송파구")
loc_11$도로명주소 = c("강동구") ;loc_12$도로명주소 = c("광진구")
loc_13$도로명주소 = c("중랑구") ;loc_14$도로명주소 = c("노원구")
loc_15$도로명주소 = c("도봉구") ;loc_16$도로명주소 = c("강북구")
loc_17$도로명주소 = c("성북구") ;loc_18$도로명주소 = c("동대문구")
loc_19$도로명주소 = c("성동구") ;loc_20$도로명주소 = c("중구")
loc_21$도로명주소 = c("용산구") ;loc_22$도로명주소 = c("종로구") 
loc_23$도로명주소 = c("서대문구") ;loc_24$도로명주소 = c("마포구")
loc_25$도로명주소 = c("은평구")

enter_final <- plyr::join_all(list(loc_1, loc_2, loc_3, loc_5,loc_6,loc_7,loc_8,loc_9,loc_10,loc_11,loc_12,loc_13,loc_14,loc_15,loc_16,loc_17,loc_18,loc_19,loc_20,loc_21,loc_22,loc_23,loc_24,loc_25), by="도로명주소", type="full")
head(enter_final)
names(enter_final)[1] <- "gu"

enter_final <- enter_final %>%
  group_by(gu) %>%
  dplyr::summarise(
    gu_num = n()
  )
```
* (1) 불필요한 컬럼 제거 후 전철 데이터와 마찬가지로 정식 명칭으로 구 통일
* (2) 데이터 병합 후 합계 산출

## 8. 아파트 매매정보와 독립 변수들 행정구를 기준으로 합치기
```{r}
apart <- read.csv("APT2021.csv")
head(apart)

names(crime_final)[2] <- "범죄발생수"
names(heal_final)[2] <- "보건기관수"
names(education_final)[2] <- "교육기관수"
names(enter_final)[2] <- "유흥업소수"
names(park_final)[2] <- "국립공원수"
names(rail_final)[2] <- "해당 지역 지하철 지점수"

apt_final <- plyr::join_all(list(apart, crime_final, heal_final, education_final, enter_final, park_final, rail_final), by="gu", type="full")

apt_final <- apt_final[,-c(1,5,6,9)]
names(apt_final)[1] <- "매매가" ; names(apt_final)[2] <- "건축 연도"
names(apt_final)[3] <- "최근 매매 연도" ; names(apt_final)[4] <- "아파트 이름"
names(apt_final)[5] <- "최근 매매 월" ; names(apt_final)[6] <- "전용면적"
names(apt_final)[7] <- "지번" ; names(apt_final)[8] <- "층수"
names(apt_final)[9] <- "지역코드" ; names(apt_final)[10] <- "지역"

```
* (1) APT2021 = 공공데이터포탈 아파트 실거래가 매매 데이터
* (2) 컬럼 내용을 더 잘 알아볼 수 있도록 컬럼명 변환 
* (3) 병합 후 불필요한 컬럼 제거, 이후 영어로 된 컬럼을 한글로 변환

```{r}
apt_final_test <- apt_final %>% na.omit(apt_final)

str(apt_final_test)
noquote(apt_final$전용면적)
apt_final_test$전용면적 <- apt_final_test$전용면적 <- as.numeric(apt_final_test$전용면적)

apt_final_test$매매가 <- noquote(gsub("\\s", "", apt_final_test$매매가))
apt_final_test$매매가 <- gsub(",", "", apt_final_test$매매가)
apt_final_test$매매가 <- as.numeric(apt_final_test$매매가)

apt_final_test$`최근 매매 월` <- as.factor(apt_final_test$`최근 매매 월`)
str(apt_final_test$`최근 매매 월`)

apt_final_test$지역코드 <- as.factor(apt_final_test$지역코드)

apt_final_test$범죄발생수 <- noquote(gsub(",","",apt_final_test$범죄발생수))
apt_final_test$범죄발생수 <- as.numeric(apt_final_test$범죄발생수)
str(apt_final_test$범죄발생수)

apt_final_test$층수 <-noquote(apt_final_test$층수)
apt_final_test$층수 <- as.numeric(apt_final_test$층수)

apt_final_test$전용면적 <- round(apt_final_test$전용면적)

apt_final_test$평 <- transform(apt_final_test, "평" = 전용면적 * 0.3025)

apt_final_test$평당가격 <- apt_final_test$매매가/apt_final_test$평

apt_final_test$평당가격 <- round(apt_final_test$평당가격)

apt_final_test <- na.omit(apt_final_test)

head(apt_final_test)

#write.csv(apt_final, "apt_2021.csv")
```
* (1) 데이터의 결측값 제거
* (2) 평당 가격 연산에 필요한 아파트 평수를 구하기 위해 불필요한 기호 제거 및 데이터 형태(numeric) 변환
* (3) 평수 계산(평 = 전용면적 * 0.3025) 이후 평 컬럼 새로 생성하여 할당
* (4) 평당가격(매매가 / 평) 계산 후 평당가격 할당
* (5) 데이터 결측값 제거 후 데이터 저장

## 9. 행정구를 기준으로 병합
```{r}
apt<- read.csv("apt_final_1.csv")

apt_1<- aggregate(평당가격~지역+ 범죄발생수+보건기관수+교육기관수+유흥업소수+국립공원수+해당.지역.지하철.지점수, apt, mean)

#write.csv(apt_1, "gu_data.csv")
```
* (1) 아파트의 구별 평당가 평균을 구하기 위해 평균 값이 들어간 데이터 생성

## 10. 아파트 매매정보, 범죄, 보건기관, 교육기관, 유흥업소, 국립공원, 전철역 데이터에 정보도서관, 여권 발급, 자가용, 은행, 미세먼지에 대한 데이터를 행정구를 기준으로 병합
```{r}
gu<- read.csv("gu_data.csv")
library<- read.csv("서울시 공공도서관.csv")
passport<- read.csv("서울시 여권 발급.csv")
car<- read.csv("서울시 자동차 등록.csv")

car<- car[-1,c(2,5)]
library<- library[,c(2,5,8)]
passport<- passport[-1,2:3]

car<- rename(car, 지역 = 자치구)
library<- rename(library, 지역 = 자치구)
passport<- rename(passport, 지역 = 자치구)

a<- merge(gu, car, by = "지역")
b<- merge(a, library, by="지역")
c<- merge(b, passport, by="지역")

str(c)
c<- rename(c, 여권발급합계= 합계)
c<- rename(c, 보유도서합계 = 도서)
c<- c[,-2]
c[,12]<-gsub(",", "", c[,12])
c[,12]<-as.numeric(c[,12])

write.csv("gu_data_2.csv")
```
* (1) 불필요한 컬럼 제거 후 자치구를 자치구 이름을 지역으로 변환
* (2) 지역을 기준으로 데이터 통합
* (3) 편의를 위해 이외의 필요한 컬럼 이름 수정
* (4) 숫자로 된 데이터의 수치화

## 11. 카카오맵 api를 이용해 위도, 경도 데이터 가져오기
---
  title: "Kakao_API"
author: "성원호"
date: "6/22/2021"
output: html_document
---
  
  ```{r setup, include=FALSE}
# usethis::edit_r_environ()
library(httr)
library(tidyverse)
library(leaflet)
library(jsonlite)
library(glue)
library(dplyr)
apt <- read.csv("APT2021.csv", fileEncoding = "EUC-KR", encoding = "UTF-8")
head(apt)

d_1 <- subset(apt, gu == '강남구') ; d1 <- d_1[sample(nrow(d_1), 40), ]
d_2 <- subset(apt, gu == '강서구') ; d2 <- d_2[sample(nrow(d_2), 40), ]
d_3 <- subset(apt, gu == '양천구') ; d3 <- d_3[sample(nrow(d_3), 40), ]
d_4 <- subset(apt, gu == '구로구') ; d4 <- d_4[sample(nrow(d_4), 40), ]
d_5 <- subset(apt, gu == '금천구') ; d5 <- d_5[sample(nrow(d_5), 40), ]
d_6 <- subset(apt, gu == '영등포구') ; d6 <- d_6[sample(nrow(d_6), 40), ]
d_7 <- subset(apt, gu == '동작구') ; d7 <- d_7[sample(nrow(d_7), 40), ]
d_8 <- subset(apt, gu == '서초구') ; d8 <- d_8[sample(nrow(d_8), 40), ]
d_9 <- subset(apt, gu == '송파구') ; d9 <- d_9[sample(nrow(d_9), 40), ]
d_10 <- subset(apt, gu == '강동구') ; d10 <- d_10[sample(nrow(d_10), 40), ]
d_11 <- subset(apt, gu == '광진구') ; d11 <- d_11[sample(nrow(d_11), 40), ]
d_12 <- subset(apt, gu == '성동구') ; d12 <- d_12[sample(nrow(d_12), 40), ]
d_13 <- subset(apt, gu == '용산구') ; d13 <- d_13[sample(nrow(d_13), 40), ]
d_14 <- subset(apt, gu == '마포구') ; d14 <- d_14[sample(nrow(d_14), 40), ]
d_15 <- subset(apt, gu == '서대문구') ; d15 <- d_15[sample(nrow(d_15), 40), ]
d_16 <- subset(apt, gu == '은평구') ; d16 <- d_16[sample(nrow(d_16), 40), ]
d_17 <- subset(apt, gu == '종로구') ; d17 <- d_17[sample(nrow(d_17), 40), ]
d_18 <- subset(apt, gu == '중구') ; d18 <- d_18[sample(nrow(d_18), 40), ]
d_19 <- subset(apt, gu == '동대문구') ; d19 <- d_19[sample(nrow(d_19), 40), ]
d_20 <- subset(apt, gu == '성북구') ; d20 <- d_20[sample(nrow(d_20), 40), ]
d_21 <- subset(apt, gu == '중랑구') ; d21 <- d_21[sample(nrow(d_21), 40), ]
d_22 <- subset(apt, gu == '강북구') ; d22 <- d_22[sample(nrow(d_22), 40), ]
d_23 <- subset(apt, gu == '도봉구') ; d23 <- d_23[sample(nrow(d_23), 40), ]
d_24 <- subset(apt, gu == '노원구') ; d24 <- d_24[sample(nrow(d_24), 40), ] 
d_25 <- subset(apt, gu == '관악구') ; d25 <- d_25[sample(nrow(d_25), 40), ]

dt <- plyr::join_all(list(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,
                          d11,d12,d13,d14,d15,d16,d17,d18,d19,
                          d20,d21,d22,d23,d24,d25),
                     type = 'full')

df <- dt %>%
  mutate(지번주소 =str_c(dong, bungi, sep=' '))%>%
  select(aptnm, 지번주소) %>%
  group_by(aptnm, 지번주소)

df$위도 <- NA
df$경도 <- NA

df


```
