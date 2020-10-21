#연관 규칙 분석 실습을 위한 패키지를 설치하고 로드하기
#install.packages("arules")
#install.packages("arulesViz")
library(arules)
library(arulesViz)
#예제 데이터 불러오기
library(datasets)
#예제 데이터 로드하기
data(Groceries)
Groceries
#많이 발생하는 아이템 상위 20개 출력하기
itemFrequencyPlot(Groceries, topN = 20, type = "absolute")

#연관 규칙 분석을 위한 Apriori 알고리즘 적용하고 시각화하기
#연관  발견하기 : Aprioori 적용, 최소 지지도 0.1% 최소 신뢰도 80%
rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8))
summary(rules)

plot(rules)
#Apriori 알고리즘 결과 필터링하기
#결과 중 처음 5개만 출력
inspect(rules[1:5])
#신뢰도 기준으로 내림차순의 패턴들을 rules에 할당
rules <- sort(rules, by = "confidence", decreasing = TRUE)
inspect(rules[1:5])

#이항 분포 만들고 계산하기
set.seed(0)
x <- rbinom(1000, 10, 0.3) #n=10, p=0.3인 이항 분포에서 난수 1,000개 생성
hist(x) #히스토그램
mean(x) #평균
var(x) #분산

#포아송 분포 만들고 히스토그램 그리기
rx = rpois(3000, 2) #포아송 분포에서 lambda는 2, 난수 3,000개 생성
mean(rx) #지정된 lambda와 거의 같은 값이 평균, 포아송 분포의 특징
var(rx)
hist(rx, probability=TRUE) # y축에 확률 값을 지정한 히스토그램
rnorm(1, 100, 16) #평균 = 100, 편차=16인 정규 분포에서 난수 1개 생성
x=rnorm(100)
hist(x, probability=TRUE) # 표준 정규 분포에서 난수 100개 생성
curve(dnorm(x), add=T) #정규 분포 PDF 표현, 곡선 추가
#t 분포 만들고 히스토그램 그리기
x=rt(100, df=3) # t 분포에서 자유도 3으로 난수 100개 생성
hist(x, probability=TRUE)
curve(dt(x, 3), add=T) # t 분포 PDF 표현, 곡선 추가

#카이제곱 분포 만들고 히스토그램 그리기
x=rchisq(100, 1) #자유도 1에서 난수 100개 생성
hist(x, probability=TRUE)
curve(dchisq(x, 1), add=T)

#정규성 검정하기
x <- rnorm(100, mean=0) #정규 분포에서 난수 100개 생성
shapiro.test(x) # 정규성 검정, 정규 분포를 따름

y <- c02[, 5] #예제 데이터 중 5번째 열 선택
shapiro.test(y) #정규 분포를 따르지 않음
# 예제 데이터 확인
# 쌍체 t 검정하기
x1 <- c(51.4, 52.0, 45.5, 54.5, 52.3, 50.9, 52.7, 50.3, 53.8, 53.1)
x2 <- c(50.1, 51.5, 45.9, 53.1, 51.8, 50.3, 52.0, 49.9, 52.5, 53.0)
t.test(x1, x2, paired=TRUE, conf.level=0.95) # x1과 x2평균 비교, 귀무 가설은 두 집단 모평균은 같음

freq = c(22, 21, 22, 27 ,22, 36)   # 주사위를 150번 던졌을 때 눈별로 나온 빈도
probs = c(1, 1, 1, 1, 1, 1)/6      # 이론적으로 각 눈은 동일한 확률로 나오는 것을 표현

chisq.test(freq, p=probs)

#install.packages("corrplot")
library(corrplot)
data(mtcars)
mtcars.cor = cor(mtcars)

corrplot(mtcars.cor, method="circle")
corrplot.mixed(mtcars.cor)

x1 = 1:10
x2 = 10:1

cor.test(x1, x2)

data(InsectSprays)
attach(InsectSprays)
str(InsectSprays)

mean(InsectSprays$count)
var(InsectSprays$count)
table(InsectSprays$spray)
mean(InsectSprays[InsectSprays$spray=="A", 1])

attach(InsectSprays)

oneway.test(count~spray, var.equal=TRUE)
aov.out = aov(count~spray, data=InsectSprays)
summary(aov.out)

pairwise.t.test(count, spray, p.adjust="bonferroni")
TukeyHSD(aov.out)

# 객체 할당
# ;을 사용하면 한줄에 여러 명령어를 사용할 수 있음
x = 3.14159; y = 'hello world'; z = TRUE
is() #현재 작업 공간의 모든 객체를 반환
print(y) #객체의 값을 출력력
rm(y) #객체를 삭제
rm(list = ls()) #모든 객체를 삭제

getwd() #현재 작업 경로를 출력
setwd("경로") #설정하기

save(y, file = "y.RData") # 지정한 객체를 파일로 저장하기
save.images("total.RData") # 전체 객체를 파일로 저장하기
load("y.RData") # 저장한 파일에서 객체 호출하기

#벡터 만들기
#vec라는 객체에 벡터 형태로 1부터 20까지 연속된 정수를 할당
vec <- 1:20 # <- 는 =와 동일한 할당 연산자
vec [3] #vec 벡터에서 3번째 값 출력
vec[3:6] # vec 벡터에서 3번째부터 6번째 값 출력
vec[c(1,3,8)] # vec 벡터에서 1, 3, 8번 값 출력
vec[vec > 15] # vec 벡터에서 15보다 큰 값을 출력

5 %in% vec # vec 벡터에 5가 포함되었는지 확인
12 %in% vec

#행렬, 배열 데이터프레임 연습하기
# 1~16까지 연속된 정수를 사용해 행이 4개인 행렬 m을 생성
m <- matrix(1:16, nrow = 4)
dim(m) # 행렬 m의 행과 열을 발견
t(m)
# df 라는 이름의 데이터프레임(표)을 만듦
df <- data.frame(times = c(4,3,5), brand = c("버거킹","맥도날드","롯데리아"))
str(df) #df의 구조를 확인, 3행 2열

df[1, ] #df의 첫 번째 행을 출력
df[2:3, ] #df의 2부터 3번째 행을 출력
df[2, 1] #df의 2행, 1열을 출력
names(df)
row.names(df)
df$brand #df에서 brand 열만 출력

#cbind 함수를 사용해 데이터를 열로 추가하기
h = 1:4 # 벡터 h에 1~4까지 값을 할당
I <- cbind(h,c(1,2,3,4))
I

#rbind 함수를 사용해 데이터를 다음 행으로 추가하기
j <- rbind(h,c(1,2,3,4)) # 1~4 값을 갖는 벡터를 h 벡터의 행에 결합한다.
j
#인덱싱
data(InsectSprays) # InsectSprays 라는 내장 데이터를 사용용
head(InsectSprays) # 대상 데이터의 첫 6개의 행의 레코드 확인

summary(InsectSprays) # 요약 값 확인

#리스트 사용해보기
lst = list() #lst라는 리스트 생성
lst[1] = "one" #첫 번째 값 넣기
lst2 <- "two" #두 번째 값 넣기
lst[length(lst)+1] <- "three" #세번째 값 넣기
print(lst)
lst[1]
lst[2:3]
lst[c(1, 3)]

#반복문 연습하기
for (i in 1:10) {
  print(paste('number', i))  
}

#함수 만들기
#function 명령어로 함수 만들고, para1과 para2는 함수 입력 값
funcName <- function(para1, para2){
print(para1)
print(para2)
return(para1 + para2)
}
funcName(3, 4)  

#1부터 16까지 연속된 정수로 행이 4개인 m1 행렬 만들기
m1 <- matrix(1:16, nrow=4)
dim(m1) # m1행렬의 행과 열을 발견
m1
t(m1) # m1 행렬의 행과 열을 바꿈

# m2 행렬을 생성한 후 m1, m2를 이용해서 다양한 연산하기
# 행을 기준으로 값이 차례대로 들어가는 m2 행렬 생성
m2 <- matrix(1:16, nrow=4, byrow=T)
m2
m1 + 1
m1 * 2
m1 + m2
m1 * m2 # m1과 m2에 같은 위치의 값끼리 단순 곱셈
m1 %*% m2 #m1과 m2의 행렬 곱

#m3 행렬을 생성하고 대각 원소 찾기
m3 = matrix(1:25, nrow=5)
m3

diag(m3) #대각 원소만 추출
diag(m2)

#역행렬 계산하기
A = matrix( c(2, -5, 4, 1,-2, 1, 1, -4, 6), byrow=T, nrow=3)
B = solve(A) # a의 역행렬
print(B)
c = c(-3, 5, 10)
# Ab = c에서 b는 B(A의 역행렬)와 벡터 b를 곱하여 구함
b = B %*% c
b

#역행렬이 존재하지 않는 행렬 D를 만들고 행렬식을 계산하기
#역행렬이 존재하지 않는 행렬
D = matrix(c(2, -5, 4, 1, -2, 1, 1, -4, 5), byrow=T, nrow=3)
#solve로 역행렬을 구하며 역행렬이 존재하지 않는 특이행렬(singular)로 결고ㅏ출력
solve(D)

#D의 1행을 2배, 2행을 -3배의 합과 D의 3행을 비교
D[1,] * 2 + D[2,] * -3 # D의 1행을 2배, 2행을 -3배의 합
D[3,] #D의 3행

det(A) #0이 아니면 행들이 선형적 독립, 비특이(정칙)행렬
det(D)

#입력 값을 제곱해서 반환하는 함수f만들기(1,2 번의 실습은 이후에도 연결됨)
f <- function(x) {
  print(x)
  return(x^2)
}

#x3(세제곱) + 3x2(제곱) - 6x - 8을 미분하고 그래프 그리기
# 수식을 함수로 표현
f <- function(x) (x^3 + 3 * x^2 -6*x - 8)
curve(f, -5, 4, ylab = "y= f(x)")

# 미분하고 그래프 그리기
g <- function(x) {}
body(g) <- D(body(f), 'x')
curve(g, -5, 4, ylab = "g(x)")

# x제곱+1 적분하기
integrate(g, lower = 0, upper = 10)
f(10) - f(0)

#미적분 응용하기 - 뉴턴랩슨 메서드
# f(x) = 0인 x를 찾기
newton = function(f, tol = 1e-7, x0 = 1, n = 100) {
  h = 1e-7;
  i = 1
  x1 = x0;
  p = numeric(N)
  while(i <= N) {
    df.dx = (f(x0 + h)-f(x0)) / h
    x1 = (x0 - (f(x0) / df.dx))
    p[i] = x1
    i = i + 1
    if(abs(x1 - x0) < tol) break
    x0 = x1
  }
  return(p[1 : (i - 1)])
}

#함수 적용
f <- function(x) (x^2 - 2) #x2-2 함수에 적용
newton(f)

f <- function(x) (x^3 + 3 * x^2 - 6 * x - 8) # x3+3x2-6x-8 함수에 적용
newton(f)
