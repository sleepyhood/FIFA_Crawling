############################
## Q1. 피파 랭킹 연도별 테이블_RSelenium
############################

########공통 작업###########
# 1. 모든 패키지 다운로드
setRepositories(ind = 1:7)

# 2-1. 작업경로 설정(*단, 역슬래시 2번씩 표기)
WORK_DIR <- "C:\\Users\\osw\\Desktop\\#Workspace\\GIT_BackUp\\R\\FIFA_Crawling"

setwd(WORK_DIR) # 작업디렉토리 설정
getwd() # 작업디렉토리 확인


# 2-2. 데이터 폴더 가져오기
#DATA_DIR<-"C:\\Users\\osw\\Desktop\\#Workspace\\[언어] R\\[학교] 2023-1 데이터 마이닝\\MidTerm_2017270961_Seungwon Oh\\data"
#setwd(DATA_DIR)
#getwd()


# 3. 라이브러리 불러오기
## 그래프 함수
library(ggplot2)
library(gganimate)

##tibble() 자료형
library(tidyverse)

##날짜 처리 
library('lubridate')

## clean_names(), tabyl()
library('janitor')

## 문자열 내부에 변수 값 {x} 삽입
library(glue)

# 데이터프레임 기반 라이브러리
library(data.table)#fread를 사용하기 위한 라이브러리

library(caret)
library(robotstxt)
library(rvest) # 웹 스크래퍼
library(RSelenium)
library(dplyr)
#install.packages("netstat")
library(netstat)

# 영어로 설정해야 월을 Date 객체로 바꿀 때 NA를 반환하지 않는다
Sys.setlocale("LC_TIME", "en_US.UTF-8")

##################################
# Step 1
# 연도를 불러오고, 연도에 해당하는 데이터 불러오기
##################################

# url
url <- "https://www.fifa.com/fifa-world-ranking/men?dateId=id13974"

# 주의사항: 반드시 실행 전, 셀레니움 및 CMD 실행
# C:\Rselenium
# java -jar selenium-server-4.9.0.jar --ext .jar;dir standalone --port 4445
# chrome는 호환성 문제로, 파이어폭스가 안정적으로 연결된다.
rD <- rsDriver(browser="firefox", port=netstat::free_port(), chromever = NULL)
remDr <- rD[["client"]]
remDr$navigate(url)

#로드까지 1초 대기
Sys.sleep(1)

########사이트 연결완료##########

# 개인정보 수집 동의 버튼 클릭
# 쿠키 설정을 해야만, 사이트 진입 가능
privacy_button <- remDr$findElement(using = 'id', value = 'onetrust-accept-btn-handler')
privacy_button$clickElement() # clickElement는 원격으로 누르게 만들 수 있는 매우 유용한 함수

# 누를 수 있는 연도
options <- remDr$findElements(using = 'css selector', value = '.ff-dropdown_dropupContentButton__WC4zi')
# 선택 가능한 날짜들
length(options)


#새로고침
remDr$navigate(url)
Sys.sleep(2)

Q1_step1 <- data.frame()
page_length <- 2

# 연도의 개수만큼 불러오기
# (* 페이지 배율이 100%보다 크면 clickElement가 인식을 못할 수도 있다.)
# 단, 각 페이지마다 50위까지의 데이터만 존재하므로, 연도마다 2페이지까지 참조가 필요하다.
for (i in 1:length(options)) {
  options <- remDr$findElements(using = 'css selector', value = '.ff-dropdown_dropupContentButton__WC4zi')
  dropdown <- remDr$findElement(using = 'xpath', value = '/html/body/div[1]/div/div[2]/main/section[1]/div/div/div[1]/div[2]/div')
  dropdown$clickElement()# 선택창 클릭
  options[[i]]$clickElement()# 연도 선택
  Sys.sleep(1)
  
  # 날짜 가져오기
  date_str<-remDr$findElement(using = 'xpath', value ='//*[@id="content"]/main/section[1]/div/div/div[1]/div[2]/div/button/div/div[1]/p[2]')$getElementText()
  date_obj<-as.Date(date_str[[1]], "%d %b %Y")
  # Date 객체로 가져오는게 직관성이 좋음
  
  next_page_button<-remDr$findElement(using = 'xpath', value ='//*[@id="content"]/main/section[2]/div/div/div[2]/div/div/div/div/div[3]/div/button')

  for(j in 1:page_length){
    # 웹 페이지에서 XPath를 사용하여 테이블 가져오기
    table <- remDr$findElement(using = "class", value = "table_rankingTable__7gmVl")
    # 테이블 HTML 가져오기
    table_html <- table$getElementAttribute("outerHTML")[[1]]
    
    # html_node()와 html_table()을 이용하여 테이블 가져오기
    table_data <- read_html(table_html) %>% 
      html_node("table") %>%
      html_table(header = TRUE)%>% 
      select(c("RK", "Team", "Total PointsPTS")) %>% 
      mutate(Date = date_obj) %>% 
      filter(RK <= 90)
    Q1_step1 <- bind_rows(Q1_step1, table_data)
    
    # 다음 페이지로 이동
    next_page_button$clickElement()
    Sys.sleep(0.5)
    next_page_button$clickElement()
    Sys.sleep(0.5)
    dropdown$clickElement()# 선택창 클릭
  }
  Sys.sleep(1)
  print(paste0(date_str, " is done."))
}
Q1_step1<-as_tibble(Q1_step1)
View(Q1_step1)

# 데이터가 사라질 경우를 대비하여 rData로 저장
Q1_step1_backup<-Q1_step1
saveRDS(Q1_step1_backup, "Q1_df_backup.RData")


##################################
# Step 2
# 상위 30위에 들었던 나라들의 횟수
##################################
#df<-readRDS("Q1_df_backup.RData")

Q1_step2<-Q1_step1 %>% 
  filter(RK <= 30) %>% 
  group_by(Team) %>% 
  summarize(Count=n())%>% 
  arrange(desc(Count))
View(Q1_step2)


##################################
# Step 3
# 상위 20개국을 Total point 순서로 정렬 및 애니메이션 시각화
##################################
#df<-readRDS("Q1_df_backup.RData")

Q1_step3<-Q1_step1 %>% 
  group_by(Date) %>% 
  filter(RK <= 20) %>% 
  ungroup()

Q1_step3 <- Q1_step3 %>% rename(TotalPoint = `Total PointsPTS`)

## 20개씩 집계되었으나, RK가 올바르지 않은 경우가 존재
## RK를 다시 정의 해주면 해결
Q1_step3<-Q1_step3 %>% 
  mutate(RK = rep(1:20, length.out = n()))


# 과거 데이터가 나오게끔 순위를 유지하며 재정렬
Q1_step3<- Q1_step3 %>% 
  group_by(Date) %>% 
  arrange(Date, RK) 

View(Q1_step3)

# value를 내림차순으로, rk를 오름차순으로 정렬
# 애니메이션 출력
# Y축은 랭크로 작은 숫자가 오게끔 정렬
maxPoint<-max(Q1_step3$TotalPoint)

staticplot<-ggplot(Q1_step3, aes(-RK, group=Team ,fill = as.factor(Team), color = as.factor(Team))) +
  geom_tile(aes(y = TotalPoint/2,
                height = TotalPoint,
                width = 0.9), alpha = 0.8, color = NA) +
  # 왼쪽에는 팀 이름이, 오른쪽에는 점수
  ylim(0, maxPoint*1.1)+
  geom_text(aes(y = 0, label = paste(Team, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=TotalPoint,label = format(round(TotalPoint, 2),nsmall = 2), hjust=0)) +
  #colour = "white", fontface = "bold", vjust = 0.5, hjust = 0)+
  ggtitle("FIFA Top 20 Team Rankings") +
  # 그래프가 연도에 따라 바뀌도록 글자도 변하게 설정
  # 범례(랭크)를 오른쪽에 둔다.+
  coord_flip(clip = "off", expand = FALSE) +
  xlab("")+
  ylab("TotalPoint")+
  guides(fill = FALSE, color=FALSE)+
  theme(plot.title = element_text(hjust = 0, size = 22),
        axis.ticks.y = element_blank(),  # These relate to the axes post-flip
        axis.text.y  = element_blank(),  # These relate to the axes post-flip
        plot.margin = margin(0.5,3,0.2,3, "cm")) 

  
staticplot

anim <- staticplot + transition_states(Date, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'FIFA Top 20 Team Rankings : {closest_state}',  
       subtitle  =  "Top 20 Countries",
       caption  = "Reference: https://www.fifa.com/fifa-world-ranking")+
  ease_aes('cubic-in-out')

anim

animate(anim,fps = 10, duration = 500, width = 800, height = 600, renderer = gifski_renderer("Q1_step3.gif"))


# 종료
remDr$close()

