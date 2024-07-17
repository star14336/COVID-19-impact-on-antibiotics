library(tidyverse)
library(nlme)
library(AICcmodavg)


#test 1 ----










#분석 ----


# 여기서부터 수정
df3 <- readxl::read_xlsx("total.xlsx")


# P와 Q 탐지
mod.1 = quantity.x ~ Time + Intervention + Post.intervention.time + Intervention.2 + Post.intervention.2.time

fx = function(pval,qval){summary(gls(mod.1, data = df3, correlation= corARMA(p=pval,q=qval, form = ~ Time),method="ML"))$AIC}

p = summary(gls(mod.1, data = df3,method="ML"))$AIC
message(str_c ("AIC Uncorrelated model = ",p))

autocorrel = expand.grid(pval = 0:5, qval = 0:5)

for(i in 2:nrow(autocorrel)){p[i] = try(summary(gls(mod.1, data = df3, correlation= corARMA(p=autocorrel$pval[i],q=autocorrel$qval[i], form = ~ Time),method="ML"))$AIC)}

autocorrel<- autocorrel %>%
  mutate(AIC = as.numeric(p)) %>%
  arrange(AIC)

autocorrel

model.d = gls(quantity.x ~ Time + Intervention + Post.intervention.time + Intervention.2 + Post.intervention.2.time, data = df3,method="ML", correlation= corARMA(p=1,q=0, form = ~ Time))

# Show a summary of the model
summary(model.d)

df3<-df3 %>% mutate(
  model.d.predictions = predictSE.gls (model.d, df3, se.fit=T)$fit,
  model.d.se = predictSE.gls (model.d, df3, se.fit=T)$se
)

ggplot(df3,aes(Time,quantity.x))+
  geom_ribbon(aes(ymin = model.d.predictions - (1.96*model.d.se), ymax = model.d.predictions + (1.96*model.d.se)), fill = "lightgreen")+
  geom_line(aes(Time,model.d.predictions),color="black",lty=1)+
  geom_point(alpha=0.3)

# 반사실 그래프화
df4<-filter(df3,Time<25)

model.e = gls(quantity.x ~ Time, data = df4, correlation= corARMA(p=1, q=0, form = ~ Time),method="ML")

df3<-df3 %>% mutate(
  model.e.predictions = predictSE.gls (model.e, newdata = df3, se.fit=T)$fit,
  model.e.se = predictSE.gls (model.e, df3, se.fit=T)$se
)

df5<-filter(df3,Time<49)
model.f = gls(quantity.x ~ Time + Intervention + Post.intervention.time, data = df5, correlation= corARMA(p=1, q=0, form = ~ Time),method="ML")

df3<-df3 %>% mutate(
  model.f.predictions = predictSE.gls (model.f, newdata = df3, se.fit=T)$fit,
  model.f.se = predictSE.gls (model.f, df3, se.fit=T)$se
)

# log -> exponential
df3 <- df3 %>% 
  mutate(quantity.x = exp(quantity.x),
         model.d.predictions = exp(model.d.predictions),
         model.d.se = exp(model.d.se),
         model.e.predictions = exp(model.e.predictions),
         model.e.se = exp(model.e.se),
         model.f.predictions = exp(model.f.predictions),
         model.f.se = exp(model.f.se))

ggplot(df3,aes(Time,quantity.x))+
  geom_ribbon(aes(ymin = model.f.predictions - (1.96*model.d.se), ymax = model.f.predictions + (1.96*model.e.se)), fill = "lightblue")+
  geom_line(aes(Time,model.f.predictions),color="blue",lty=2)+
  geom_ribbon(aes(ymin = model.e.predictions - (1.96*model.d.se), ymax = model.e.predictions + (1.96*model.e.se)), fill = "pink")+
  geom_line(aes(Time,model.e.predictions),color="red",lty=2)+
  geom_ribbon(aes(ymin = model.d.predictions - (1.96*model.d.se), ymax = model.d.predictions + (1.96*model.d.se)), fill = "lightgreen")+
  geom_line(aes(Time,model.d.predictions),color="black",lty=1)+
  geom_point(alpha=0.3)





##total pt ----
total_pt <- readxl::read_xlsx("total_pt.xlsx")


# P와 Q 탐지
mod.1 = quantity.x ~ Time + Intervention + Post.intervention.time + Intervention.2 + Post.intervention.2.time

fx = function(pval,qval){summary(gls(mod.1, data = total_pt, correlation= corARMA(p=pval,q=qval, form = ~ Time),method="ML"))$AIC}

p = summary(gls(mod.1, data = total_pt,method="ML"))$AIC
message(str_c ("AIC Uncorrelated model = ",p))

autocorrel = expand.grid(pval = 0:5, qval = 0:5)

for(i in 2:nrow(autocorrel)){p[i] = try(summary(gls(mod.1, data = total_pt, correlation= corARMA(p=autocorrel$pval[i],q=autocorrel$qval[i], form = ~ Time),method="ML"))$AIC)}

autocorrel<- autocorrel %>%
  mutate(AIC = as.numeric(p)) %>%
  arrange(AIC)

autocorrel


model.pt = gls(quantity.x ~ Time + Intervention + Post.intervention.time + Intervention.2 + Post.intervention.2.time, data = total_pt,method="ML", correlation= corARMA(p=1,q=0, form = ~ Time))

# Show a summary of the model
summary(model.pt)

total_pt<-total_pt %>% mutate(
  model.pt.predictions = predictSE.gls (model.pt, total_pt, se.fit=T)$fit,
  model.pt.se = predictSE.gls (model.pt, total_pt, se.fit=T)$se
)

ggplot(total_pt,aes(Time,quantity.x))+
  geom_ribbon(aes(ymin = model.pt.predictions - (1.96*model.pt.se), ymax = model.pt.predictions + (1.96*model.pt.se)), fill = "lightgreen")+
  geom_line(aes(Time,model.pt.predictions),color="black",lty=1)+
  geom_point(alpha=0.3)

# 반사실 그래프화
total_pt2<-filter(total_pt,Time<25)

model.pt2 = gls(quantity.x ~ Time, data = total_pt2, correlation= corARMA(p=1, q=0, form = ~ Time),method="ML")

total_pt<-total_pt %>% mutate(
  model.pt2.predictions = predictSE.gls (model.pt2, newdata = total_pt, se.fit=T)$fit,
  model.pt2.se = predictSE.gls (model.pt2, total_pt, se.fit=T)$se
)

total_pt3<-filter(total_pt,Time<49)
model.pt3 = gls(quantity.x ~ Time + Intervention + Post.intervention.time, data = total_pt3, correlation= corARMA(p=1, q=0, form = ~ Time),method="ML")

total_pt<-total_pt %>% mutate(
  model.pt3.predictions = predictSE.gls (model.f, newdata = total_pt, se.fit=T)$fit,
  model.pt3.se = predictSE.gls (model.f, total_pt, se.fit=T)$se
)


ggplot(total_pt,aes(Time,quantity.x))+
  geom_ribbon(aes(ymin = model.pt3.predictions - (1.96*model.pt.se), ymax = model.pt3.predictions + (1.96*model.pt2.se)), fill = "lightblue")+
  geom_line(aes(Time,model.pt3.predictions),color="blue",lty=2)+
  geom_ribbon(aes(ymin = model.pt2.predictions - (1.96*model.pt.se), ymax = model.pt2.predictions + (1.96*model.pt2.se)), fill = "pink")+
  geom_line(aes(Time,model.pt2.predictions),color="red",lty=2)+
  geom_ribbon(aes(ymin = model.pt.predictions - (1.96*model.pt.se), ymax = model.pt.predictions + (1.96*model.pt.se)), fill = "lightgreen")+
  geom_line(aes(Time,model.pt.predictions),color="black",lty=1)+
  geom_point(alpha=0.3)


###log 그래프 ----
# P와 Q 탐지
mod.1 = quantity.x2 ~ Time + Intervention + Post.intervention.time + Intervention.2 + Post.intervention.2.time

fx = function(pval,qval){summary(gls(mod.1, data = total_pt, correlation= corARMA(p=pval,q=qval, form = ~ Time),method="ML"))$AIC}

p = summary(gls(mod.1, data = total_pt,method="ML"))$AIC
message(str_c ("AIC Uncorrelated model = ",p))

autocorrel = expand.grid(pval = 0:5, qval = 0:5)

for(i in 2:nrow(autocorrel)){p[i] = try(summary(gls(mod.1, data = total_pt, correlation= corARMA(p=autocorrel$pval[i],q=autocorrel$qval[i], form = ~ Time),method="ML"))$AIC)}

autocorrel<- autocorrel %>%
  mutate(AIC = as.numeric(p)) %>%
  arrange(AIC)

autocorrel


model.pt = gls(quantity.x2 ~ Time + Intervention + Post.intervention.time + Intervention.2 + Post.intervention.2.time, data = total_pt,method="ML", correlation= corARMA(p=1,q=0, form = ~ Time))

# Show a summary of the model
summary(model.pt)

total_pt<-total_pt %>% mutate(
  model.pt.predictions = predictSE.gls (model.pt, total_pt, se.fit=T)$fit,
  model.pt.se = predictSE.gls (model.pt, total_pt, se.fit=T)$se
)

ggplot(total_pt,aes(Time,quantity.x2))+
  geom_ribbon(aes(ymin = model.pt.predictions - (1.96*model.pt.se), ymax = model.pt.predictions + (1.96*model.pt.se)), fill = "lightgreen")+
  geom_line(aes(Time,model.pt.predictions),color="black",lty=1)+
  geom_point(alpha=0.3)

# 반사실 그래프화
total_pt2<-filter(total_pt,Time<25)

model.pt2 = gls(quantity.x2 ~ Time, data = total_pt2, correlation= corARMA(p=1, q=0, form = ~ Time),method="ML")

total_pt<-total_pt %>% mutate(
  model.pt2.predictions = predictSE.gls (model.pt2, newdata = total_pt, se.fit=T)$fit,
  model.pt2.se = predictSE.gls (model.pt2, total_pt, se.fit=T)$se
)

total_pt3<-filter(total_pt,Time<49)
model.pt3 = gls(quantity.x2 ~ Time + Intervention + Post.intervention.time, data = total_pt3, correlation= corARMA(p=1, q=0, form = ~ Time),method="ML")

total_pt<-total_pt %>% mutate(
  model.pt3.predictions = predictSE.gls (model.f, newdata = total_pt, se.fit=T)$fit,
  model.pt3.se = predictSE.gls (model.f, total_pt, se.fit=T)$se
)


# log -> exponential - 로그값 변환은 quantity.x2를 사용할때 사용
total_pt <- total_pt %>% 
  mutate(quantity.x2 = exp(quantity.x2),
         model.pt.predictions = exp(model.pt.predictions),
         model.pt.se = exp(model.pt.se),
         model.pt2.predictions = exp(model.pt2.predictions),
         model.pt2.se = exp(model.pt2.se),
         model.pt3.predictions = exp(model.pt3.predictions),
         model.pt3.se = exp(model.pt3.se))


#log값
ggplot(total_pt,aes(Time,quantity.x2))+
  geom_ribbon(aes(ymin = model.pt3.predictions - (1.96*model.pt.se), ymax = model.pt3.predictions + (1.96*model.pt2.se)), fill = "lightblue")+
  geom_line(aes(Time,model.pt3.predictions),color="blue",lty=2)+
  geom_ribbon(aes(ymin = model.pt2.predictions - (1.96*model.pt.se), ymax = model.pt2.predictions + (1.96*model.pt2.se)), fill = "pink")+
  geom_line(aes(Time,model.pt2.predictions),color="red",lty=2)+
  geom_ribbon(aes(ymin = model.pt.predictions - (1.96*model.pt.se), ymax = model.pt.predictions + (1.96*model.pt.se)), fill = "lightgreen")+
  geom_line(aes(Time,model.pt.predictions),color="black",lty=1)+
  geom_point(alpha=0.3)



## total pres ----
total_pres <- readxl::read_xlsx("total_pres.xlsx")


# P와 Q 탐지
mod.1 = quantity.x ~ Time + Intervention + Post.intervention.time + Intervention.2 + Post.intervention.2.time

fx = function(pval,qval){summary(gls(mod.1, data = total_pres, correlation= corARMA(p=pval,q=qval, form = ~ Time),method="ML"))$AIC}

p = summary(gls(mod.1, data = total_pres,method="ML"))$AIC
message(str_c ("AIC Uncorrelated model = ",p))

autocorrel = expand.grid(pval = 0:5, qval = 0:5)

for(i in 2:nrow(autocorrel)){p[i] = try(summary(gls(mod.1, data = total_pres, correlation= corARMA(p=autocorrel$pval[i],q=autocorrel$qval[i], form = ~ Time),method="ML"))$AIC)}

autocorrel<- autocorrel %>%
  mutate(AIC = as.numeric(p)) %>%
  arrange(AIC)

autocorrel


model.pres = gls(quantity.x ~ Time + Intervention + Post.intervention.time + Intervention.2 + Post.intervention.2.time, data = total_pres,method="ML", correlation= corARMA(p=1,q=0, form = ~ Time))

# Show a summary of the model
summary(model.pres)

total_pres<-total_pres %>% mutate(
  model.pres.predictions = predictSE.gls (model.pres, total_pres, se.fit=T)$fit,
  model.pres.se = predictSE.gls (model.pres, total_pres, se.fit=T)$se
)

ggplot(total_pres,aes(Time,quantity.x))+
  geom_ribbon(aes(ymin = model.pres.predictions - (1.96*model.pres.se), ymax = model.pres.predictions + (1.96*model.pres.se)), fill = "lightgreen")+
  geom_line(aes(Time,model.pres.predictions),color="black",lty=1)+
  geom_point(alpha=0.3)

# 반사실 그래프화
total_pres2 <- filter(total_pres, Time < 25)

model.pres2 = gls(quantity.x ~ Time, data = total_pres2, correlation = corARMA(p = 1, q = 0, form = ~ Time), method = "ML")

total_pres <- total_pres %>% mutate(
  model.pres2.predictions = predictSE.gls(model.pres2, newdata = total_pres, se.fit = TRUE)$fit,
  model.pres2.se = predictSE.gls(model.pres2, total_pres, se.fit = TRUE)$se.fit
)

total_pres3 <- filter(total_pres, Time < 49)
model.pres3 = gls(quantity.x ~ Time + Intervention + Post.intervention.time, data = total_pres3, correlation = corARMA(p = 1, q = 0, form = ~ Time), method = "ML")

total_pres <- total_pres %>% mutate(
  model.pres3.predictions = predictSE.gls(model.pres3, newdata = total_pres, se.fit = TRUE)$fit,
  model.pres3.se = predictSE.gls(model.pres3, total_pres, se.fit = TRUE)$se.fit
)

# 그래프 생성
ggplot(total_pres, aes(Time, quantity.x)) +
  geom_ribbon(aes(ymin = model.pres3.predictions - (1.96 * model.pres3.se), ymax = model.pres3.predictions + (1.96 * model.pres3.se)), fill = "lightblue") +
  geom_line(aes(Time, model.pres3.predictions), color = "blue", lty = 2) +
  geom_ribbon(aes(ymin = model.pres2.predictions - (1.96 * model.pres2.se), ymax = model.pres2.predictions + (1.96 * model.pres2.se)), fill = "pink") +
  geom_line(aes(Time, model.pres2.predictions), color = "red", lty = 2) +
  geom_ribbon(aes(ymin = model.pres.predictions - (1.96 * model.pres.se), ymax = model.pres.predictions + (1.96 * model.pres.se)), fill = "lightgreen") +
  geom_line(aes(Time, model.pres.predictions), color = "black", lty = 1) +
  geom_point(alpha = 0.3)



### 로그 그래프 생성 ----
mod.1 = quantity.x2 ~ Time + Intervention + Post.intervention.time + Intervention.2 + Post.intervention.2.time

fx = function(pval, qval) {
  summary(gls(mod.1, data = total_pres, correlation = corARMA(p = pval, q = qval, form = ~ Time), method = "ML"))$AIC
}

p = summary(gls(mod.1, data = total_pres, method = "ML"))$AIC
message(str_c("AIC Uncorrelated model = ", p))

autocorrel = expand.grid(pval = 0:5, qval = 0:5)

for (i in 2:nrow(autocorrel)) {
  p[i] = try(summary(gls(mod.1, data = total_pres, correlation = corARMA(p = autocorrel$pval[i], q = autocorrel$qval[i], form = ~ Time), method = "ML"))$AIC)
}

autocorrel <- autocorrel %>%
  mutate(AIC = as.numeric(p)) %>%
  arrange(AIC)

autocorrel

model.pres = gls(quantity.x2 ~ Time + Intervention + Post.intervention.time + Intervention.2 + Post.intervention.2.time, data = total_pres, method = "ML", correlation = corARMA(p = 1, q = 0, form = ~ Time))

# 모델 요약
summary(model.pres)

total_pres <- total_pres %>% mutate(
  model.pres.predictions = predictSE.gls(model.pres, total_pres, se.fit = TRUE)$fit,
  model.pres.se = predictSE.gls(model.pres, total_pres, se.fit = TRUE)$se.fit
)

ggplot(total_pres, aes(Time, quantity.x2)) +
  geom_ribbon(aes(ymin = model.pres.predictions - (1.96 * model.pres.se), ymax = model.pres.predictions + (1.96 * model.pres.se)), fill = "lightgreen") +
  geom_line(aes(Time, model.pres.predictions), color = "black", lty = 1) +
  geom_point(alpha = 0.3)

# 반사실 그래프화
total_pres2 <- filter(total_pres, Time < 25)

model.pres2 = gls(quantity.x2 ~ Time, data = total_pres2, correlation = corARMA(p = 1, q = 0, form = ~ Time), method = "ML")

total_pres <- total_pres %>% mutate(
  model.pres2.predictions = predictSE.gls(model.pres2, newdata = total_pres, se.fit = TRUE)$fit,
  model.pres2.se = predictSE.gls(model.pres2, total_pres, se.fit = TRUE)$se.fit
)

total_pres3 <- filter(total_pres, Time < 49)
model.pres3 = gls(quantity.x2 ~ Time + Intervention + Post.intervention.time, data = total_pres3, correlation = corARMA(p = 1, q = 0, form = ~ Time), method = "ML")

total_pres <- total_pres %>% mutate(
  model.pres3.predictions = predictSE.gls(model.pres3, newdata = total_pres, se.fit = TRUE)$fit,
  model.pres3.se = predictSE.gls(model.pres3, total_pres, se.fit = TRUE)$se.fit
)

# 로그 -> 지수 변환 (quantity.x2를 사용할 때)
total_pres <- total_pres %>% 
  mutate(quantity.x2 = exp(quantity.x2),
         model.pres.predictions = exp(model.pres.predictions),
         model.pres.se = exp(model.pres.se),
         model.pres2.predictions = exp(model.pres2.predictions),
         model.pres2.se = exp(model.pres2.se),
         model.pres3.predictions = exp(model.pres3.predictions),
         model.pres3.se = exp(model.pres3.se))

# 로그 값 그래프화
ggplot(total_pres, aes(Time, quantity.x2)) +
  geom_ribbon(aes(ymin = model.pres3.predictions - (1.96 * model.pres3.se), ymax = model.pres3.predictions + (1.96 * model.pres3.se)), fill = "lightblue") +
  geom_line(aes(Time, model.pres3.predictions), color = "blue", lty = 2) +
  geom_ribbon(aes(ymin = model.pres2.predictions - (1.96 * model.pres2.se), ymax = model.pres2.predictions + (1.96 * model.pres2.se)), fill = "pink") +
  geom_line(aes(Time, model.pres2.predictions), color = "red", lty = 2) +
  geom_ribbon(aes(ymin = model.pres.predictions - (1.96 * model.pres.se), ymax = model.pres.predictions + (1.96 * model.pres.se)), fill = "lightgreen") +
  geom_line(aes(Time, model.pres.predictions), color = "black", lty = 1) +
  geom_point(alpha = 0.3)



##total_amt ----
total_amt <- readxl::read_xlsx("total_amt.xlsx")

# P와 Q 탐지
mod.1 = quantity.x ~ Time + Intervention + Post.intervention.time + Intervention.2 + Post.intervention.2.time

fx = function(pval, qval) {
  summary(gls(mod.1, data = total_amt, correlation = corARMA(p = pval, q = qval, form = ~ Time), method = "ML"))$AIC
}

p = summary(gls(mod.1, data = total_amt, method = "ML"))$AIC
message(str_c("AIC Uncorrelated model = ", p))

autocorrel = expand.grid(pval = 0:5, qval = 0:5)

for (i in 2:nrow(autocorrel)) {
  p[i] = try(summary(gls(mod.1, data = total_amt, correlation = corARMA(p = autocorrel$pval[i], q = autocorrel$qval[i], form = ~ Time), method = "ML"))$AIC)
}

autocorrel <- autocorrel %>%
  mutate(AIC = as.numeric(p)) %>%
  arrange(AIC)

autocorrel

model.amt = gls(quantity.x ~ Time + Intervention + Post.intervention.time + Intervention.2 + Post.intervention.2.time, data = total_amt, method = "ML", correlation = corARMA(p = 4, q = 0, form = ~ Time))

# 모델 요약
summary(model.amt)

total_amt <- total_amt %>% mutate(
  model.amt.predictions = predictSE.gls(model.amt, total_amt, se.fit = TRUE)$fit,
  model.amt.se = predictSE.gls(model.amt, total_amt, se.fit = TRUE)$se.fit
)

ggplot(total_amt, aes(Time, quantity.x)) +
  geom_ribbon(aes(ymin = model.amt.predictions - (1.96 * model.amt.se), ymax = model.amt.predictions + (1.96 * model.amt.se)), fill = "lightgreen") +
  geom_line(aes(Time, model.amt.predictions), color = "black", lty = 1) +
  geom_point(alpha = 0.3)

# 반사실 그래프화
total_amt2 <- filter(total_amt, Time < 25)

model.amt2 = gls(quantity.x ~ Time, data = total_amt2, correlation = corARMA(p = 4, q = 0, form = ~ Time), method = "ML")

total_amt <- total_amt %>% mutate(
  model.amt2.predictions = predictSE.gls(model.amt2, newdata = total_amt, se.fit = TRUE)$fit,
  model.amt2.se = predictSE.gls(model.amt2, total_amt, se.fit = TRUE)$se.fit
)

total_amt3 <- filter(total_amt, Time < 49)
model.amt3 = gls(quantity.x ~ Time + Intervention + Post.intervention.time, data = total_amt3, correlation = corARMA(p = 4, q = 0, form = ~ Time), method = "ML")

total_amt <- total_amt %>% mutate(
  model.amt3.predictions = predictSE.gls(model.amt3, newdata = total_amt, se.fit = TRUE)$fit,
  model.amt3.se = predictSE.gls(model.amt3, total_amt, se.fit = TRUE)$se.fit
)

# 그래프 생성
ggplot(total_amt, aes(Time, quantity.x)) +
  geom_ribbon(aes(ymin = model.amt3.predictions - (1.96 * model.amt3.se), ymax = model.amt3.predictions + (1.96 * model.amt3.se)), fill = "lightblue") +
  geom_line(aes(Time, model.amt3.predictions), color = "blue", lty = 2) +
  geom_ribbon(aes(ymin = model.amt2.predictions - (1.96 * model.amt2.se), ymax = model.amt2.predictions + (1.96 * model.amt2.se)), fill = "pink") +
  geom_line(aes(Time, model.amt2.predictions), color = "red", lty = 2) +
  geom_ribbon(aes(ymin = model.amt.predictions - (1.96 * model.amt.se), ymax = model.amt.predictions + (1.96 * model.amt.se)), fill = "lightgreen") +
  geom_line(aes(Time, model.amt.predictions), color = "black", lty = 1) +
  geom_point(alpha = 0.3)


### 로그 그래프 생성 ----
mod.1 = quantity.x2 ~ Time + Intervention + Post.intervention.time + Intervention.2 + Post.intervention.2.time

fx = function(pval, qval) {
  summary(gls(mod.1, data = total_amt, correlation = corARMA(p = pval, q = qval, form = ~ Time), method = "ML"))$AIC
}

p = summary(gls(mod.1, data = total_amt, method = "ML"))$AIC
message(str_c("AIC Uncorrelated model = ", p))

autocorrel = expand.grid(pval = 0:5, qval = 0:5)

for (i in 2:nrow(autocorrel)) {
  p[i] = try(summary(gls(mod.1, data = total_amt, correlation = corARMA(p = autocorrel$pval[i], q = autocorrel$qval[i], form = ~ Time), method = "ML"))$AIC)
}

autocorrel <- autocorrel %>%
  mutate(AIC = as.numeric(p)) %>%
  arrange(AIC)

autocorrel

model.amt = gls(quantity.x2 ~ Time + Intervention + Post.intervention.time + Intervention.2 + Post.intervention.2.time, data = total_amt, method = "ML", correlation = corARMA(p = 2, q = 4, form = ~ Time))

# 모델 요약
summary(model.amt)

total_amt <- total_amt %>% mutate(
  model.amt.predictions = predictSE.gls(model.amt, total_amt, se.fit = TRUE)$fit,
  model.amt.se = predictSE.gls(model.amt, total_amt, se.fit = TRUE)$se.fit
)

ggplot(total_amt, aes(Time, quantity.x2)) +
  geom_ribbon(aes(ymin = model.amt.predictions - (1.96 * model.amt.se), ymax = model.amt.predictions + (1.96 * model.amt.se)), fill = "lightgreen") +
  geom_line(aes(Time, model.amt.predictions), color = "black", lty = 1) +
  geom_point(alpha = 0.3)

# 반사실 그래프화
total_amt2 <- filter(total_amt, Time < 25)

model.amt2 = gls(quantity.x2 ~ Time, data = total_amt2, correlation = corARMA(p = 2, q = 4, form = ~ Time), method = "ML")

total_amt <- total_amt %>% mutate(
  model.amt2.predictions = predictSE.gls(model.amt2, newdata = total_amt, se.fit = TRUE)$fit,
  model.amt2.se = predictSE.gls(model.amt2, total_amt, se.fit = TRUE)$se.fit
)

total_amt3 <- filter(total_amt, Time < 49)
model.amt3 = gls(quantity.x2 ~ Time + Intervention + Post.intervention.time, data = total_amt3, correlation = corARMA(p = 2, q = 4, form = ~ Time), method = "ML")

total_amt <- total_amt %>% mutate(
  model.amt3.predictions = predictSE.gls(model.amt3, newdata = total_amt, se.fit = TRUE)$fit,
  model.amt3.se = predictSE.gls(model.amt3, total_amt, se.fit = TRUE)$se.fit
)

# 로그 -> 지수 변환 (quantity.x2를 사용할 때)
total_amt <- total_amt %>% 
  mutate(quantity.x2 = exp(quantity.x2),
         model.amt.predictions = exp(model.amt.predictions),
         model.amt.se = exp(model.amt.se),
         model.amt2.predictions = exp(model.amt2.predictions),
         model.amt2.se = exp(model.amt2.se),
         model.amt3.predictions = exp(model.amt3.predictions),
         model.amt3.se = exp(model.amt3.se))

# 로그 값 그래프화
ggplot(total_amt, aes(Time, quantity.x2)) +
  geom_ribbon(aes(ymin = model.amt3.predictions - (1.96 * model.amt3.se), ymax = model.amt3.predictions + (1.96 * model.amt3.se)), fill = "lightblue") +
  geom_line(aes(Time, model.amt3.predictions), color = "blue", lty = 2) +
  geom_ribbon(aes(ymin = model.amt2.predictions - (1.96 * model.amt2.se), ymax = model.amt2.predictions + (1.96 * model.amt2.se)), fill = "pink") +
  geom_line(aes(Time, model.amt2.predictions), color = "red", lty = 2) +
  geom_ribbon(aes(ymin = model.amt.predictions - (1.96 * model.amt.se), ymax = model.amt.predictions + (1.96 * model.amt.se)), fill = "lightgreen") +
  geom_line(aes(Time, model.amt.predictions), color = "black", lty = 1) +
  geom_point(alpha = 0.3)



