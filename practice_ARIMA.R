library(forecast)
library(tidyverse)
library(tseries)


plot(AirPassengers) #그림

#ARIMA(p,d,q)(P,D,Q) - 계산상 가장 적합한 모수 찾아주는 작업
airline.fit = auto.arima(AirPassengers)
airline.fit

#시계열 자료에서 어떤 반복된 계절패턴을 보고자 한다면 계절주기가 얼마인지를 알아야 함
#계절주기는 보통 1년을 기준으로 함
#연도별 자료 - 계절 주기:1
#분기별 자료 - 계절 주기:4
#월별 자료 - 계절 주기:12 - 3년치보다 작은 자료면 계절패턴을 보기 어려움(18-23까지여서 주기를 보기 힘듬)

#데이터프레임을 사용
#ts()함수 사용 - 시간 순으로 정돈해줌
is.ts(AirPassengers)
class(AirPassengers)
frequency(AirPassengers)


airline1.ts = ts(AirPassengers, start = c(1949,1), frequency = 12)
airline1.ts


#실제 데이터 적용
anti <- read_csv("MEDICINE/data/total/anti.csv")

anti.pt <- anti$category_pt
anti.pt.ts <- ts(anti.pt,start =c(2018,1),frequency = 12)
anti.pt.ts

plot(anti.pt.ts)

plot(forecast(airline.fit, h=12))#예측을 위해 사용하는 함수

#시계열 그리메서 추세를 갖는지 또는 계절성을 보이는 지를 살펴봐야 함
#1.계절성과 추세성을 중심으로 자료의 진동성(폭)이 일정한지를 본다
#진동폭이 일정하지 않으면 데이터 전처리를 해야 ARIMA에 돌릴 수 있다
# -> 진동폭이 변화한다면, 데이터 자체를 로그변환으로 진동폭을 안정화시킬 수 있다.
#대부분의 데이터는 등분산이지만 이분산성(진동폭이 다르다)이 있으면 모델에서 오류가 잘 나타남

#2.추세가 있다면 차분을 통하거나 선형변수를 설명변수로 넣어 추세를 제거할 수 있다
#ARIMA는 백색잡음 상태(등분산성+추세X+계절성X)여야 피티잉 가능한 수식임
#차분: 현제값을 이전값으로 뺸 값으로 계산

#3. 계절성 - 계절차분(12개월마다 뺌)을 하거나 계절 임시 변수를 넣어서 계절성(계절추세)을 제거할 수 있다




#연습 - 원래는 함수를 사용하여 통계적으로 아래 3가지가 필요한지를 확인하는 작업이 있음(그림으로 확인하고 난 후에)
##이분산성 제거(로그변환)
airline_log.ts = log(airline1.ts)
plot(airline_log.ts)

plot(airline1.ts) #이전과 비교

##추세 제거(차분)
#시계열 자료 y(t) => 1차 차분(y(t) - y(t-1))
#1차 차분 필요성 확인: ndiffs()
ndiffs(airline1.ts) #1이 나오면 차분이 필요하다고 나옴
airline_diff.ts = diff(airline1.ts)
plot(airline_diff.ts)

#단위근 검정(논문에 사용할 때 써야하는 검정)(차분 여부 결정) - p value > 0.05면 차분 필요
PP.test(airline1.ts) #좀 이상함
adf.test(airline1.ts) #ADF 검정, k값 찾기
airline1.ts


##계절성 제거(계절차분)
#계절 차분 필요성 확인: nsdiffs()
nsdiffs(airline1.ts) #1이 나오면 계절 차분이 필요하다고 나옴
airline_diff12.ts = diff(airline1.ts, lag=12)
plot(airline_diff12.ts)


##3가지 전부 적용
airline2.ts = log(airline1.ts)
airline2.ts = diff(airline2.ts)
airline2.ts = diff(airline2.ts, lag=12)
plot(airline2.ts)

#decompose() => 추세, 계절성, 불규칙한 항들을 분해
decompose(airline1.ts)
airline_decompose <- decompose(airline1.ts)
plot(airline_decompose) #이분산은 그래프 자체에서 보고 판단하는 경우가 많음(random은 관련 없음)



###예측변수 하나만 가지고 시계열 모형을 적합할 경우
#지수평활모형
#이동평균모형
#자기회귀-이동평균(ARMA, auto-regressive moving-average)
#ARIMA
##자기회귀 차수와 이동평균 차수를 정해야 함
##자기상관함수: MA 차수(q)를 선택하는데 사용
##부분자기상관함수: AR 차수(p)를 선택하는데 사용
#auto.arima를 쓰면 p,d,q 추정 -> 모형을 자동으로 찾음




#24.07.31
#ARMA(자기회귀-이동평균 모형)
#시계열 정상성(stationarity)
##정상성 시계열; 수준을 중심으로 어떤 패턴이 없고 진동폭이 변하지 않는 시계열(변화가 없다)(백색소음)
##비정상성 시계열 -> 정상 시계열로 바꿔서 분석

#비정상성, 정상성 구분
##시계열 그림 - 눈으로 보기
##자기상관함수(ACF),부분자기상관함수(PACF)의 값의 변화로 판단
##비정상성 시계열; ACF 천천히 줄어듬 / 정상성 시계열;ACF가 상대적으로 빠르게 감소

economic.df <- read.csv("Data/BOK_macro_economic_rate.csv")
economic.ts = ts(economic.df[-c(1)],start = c(2010,1),frequency = 12)
economic.ts


economic.ts[,2]
plot(economic.ts[,2], col = "red", lwd = 2) #lwd는 선 굵기

#비정상성 확인(ACF)
acf(economic.ts[,2]) #천천히 떨어짐
#차분 여부 확인
ndiffs(economic.ts[,2])
#차분
bond_dif.ts = diff(economic.ts[,2])
plot(bond_dif.ts)
#차분 후 비정상성 확인(ACF)
acf(bond_dif.ts) #대부분이 파란 선 안에 있으면 좋다고 봄(하나 두개 밖에 있는 건 문제 아님)

Acf(economic.ts[,2])
Acf(bond_dif.ts) #0은 지움(무조건 1이여서, lag 문제 해결)


##PACF - AR 모형 차수 결정
##ARIMA 모형 결정 (p,d,q)
#d = 차분, d=1; 차분을 한번 했다 - 가장 관심사(차분이 필요한가 아닌가)
#p는 자기회귀(AR), q는 이동평균(MA) 차수 - 그래프

#ARIMA 모형 적합
bond_auto_fit = auto.arima(economic.ts[,2])
bond_auto_fit


#잔차 분석 - 만든 ARIMA모형의 적합성 확인; 잔차(실제값과 예측값의 차이)가 자유로워야 함(특정한 패턴을 보이면 안됨)
tsdiag(bond_auto_fit)
##표준화된 잔차의 시계열
##잔차의 ACF
##잔차의 Ljung-Box 통계량 - 대부분이 파란 선 위에 있으면 잘 했다고 볼 수 있음

#계절성이 있는 데이터에서의 ARIMA(SARIMA)
#SARIMA
employment.ts = ts(economic.df$employment_rate,start = c(2010,1), frequency = 12)
#Acf 확인
plot(employment.ts)
Acf(employment.ts) #12마다 반복(연관성 높음) -> 계절성이 있음

#계절차분
employment_dif1.ts <- diff(employment.ts, lag=12)
plot(employment_dif1.ts)

Acf(employment_dif1.ts) #계절차분을 해도 추세가 존재함

adf.test(employment_dif1.ts)

#일차차분
employment_dif2.ts = diff(employment_dif1.ts)
Acf(employment_dif2.ts)


#auto.arima
##d=차분, D= 계절차분
employment_auto.fit1 = auto.arima(employment.ts, d=1,D=1) #계절차분에서는 d를 0을 주는 경우가 많아 강제로 줘야 하는 경우가 많음
employment_auto.fit1 #즉, 위의 과정을 해ㅇ야 함
##잔차 분석
tsdiag(employment_auto.fit1)






