---
  title: "수집"
author: "성원호"
date: "6/22/2021"
output: html_document
---
  
  # 데이터 수집 과정
  ## 1. 공공데이터포탈 API 불러오기
  ## 2. 아파트 매매 데이터에 필요한 데이터 추출
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

## 이외 다른 소스에서 가져온 데이터
* 고등교육기관 주소록.csv
* 버스정류소현황.csv
* 보건소 주소록.csv
* 서울시 공공도서관.csv
* 서울시 동별 보건소 및 보건분소.csv
* 서울시 동별 세대원수별 세대수.csv
* 서울시 동별 아파트 매매거래 현황.csv
* 서울시 범죄 현황.csv
* 서울시 여권 발급.csv
* 서울시 외국인 유학생 현황.csv
* 서울시 자동차 등록.csv
* 서울시 주요 공원현황.csv
* 서울특별시 버스정류소 위치정보.csv
* 서울특별시 유흥주점영업 인허가 정보.csv
* 유초중등교육기관 주소록.csv
* 전국도시철도역사정보표준데이터.csv
* 한국사회보장정보원_보건기관 기본정보_20200901.csv

## 데이터의 출처
* 1. 공공데이터포탈
* 2. 카카오맵 계발자 계정 사이트
* 3. 서울 열린데이터 광장
* 4. 교육통계서비스 KESS