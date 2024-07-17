library(dplyr)
library(ggplot2)
library(nlme)
library(readxl)

## pt ----
pt <- readxl::read_xlsx("pt.xlsx")

# P와 Q 탐지
mod.1 = quantity.x ~ Time + Intervention + Post.intervention.time + Intervention.2 + Post.intervention.2.time

fx = function(pval, qval) {
  summary(gls(mod.1, data = pt, correlation = corARMA(p = pval, q = qval, form = ~ Time), method = "ML"))$AIC
}

p = summary(gls(mod.1, data = pt, method = "ML"))$AIC
message(str_c("AIC Uncorrelated model = ", p))

autocorrel = expand.grid(pval = 0:5, qval = 0:5)

for (i in 2:nrow(autocorrel)) {
  p[i] = try(summary(gls(mod.1, data = pt, correlation = corARMA(p = autocorrel$pval[i], q = autocorrel$qval[i], form = ~ Time), method = "ML"))$AIC)
}

autocorrel <- autocorrel %>%
  mutate(AIC = as.numeric(p)) %>%
  arrange(AIC)

autocorrel

model.pt = gls(quantity.x ~ Time + Intervention + Post.intervention.time + Intervention.2 + Post.intervention.2.time, data = pt, method = "ML",
               correlation = corARMA(p = 0, q = 1, form = ~ Time))

# 모델 요약
summary(model.pt)

pt <- pt %>% mutate(
  model.pt.predictions = predictSE.gls(model.pt, pt, se.fit = TRUE)$fit,
  model.pt.se = predictSE.gls(model.pt, pt, se.fit = TRUE)$se
)

ggplot(pt, aes(Time, quantity.x)) +
  geom_ribbon(aes(ymin = model.pt.predictions - (1.96 * model.pt.se), ymax = model.pt.predictions + (1.96 * model.pt.se)), fill = "lightgreen") +
  geom_line(aes(Time, model.pt.predictions), color = "black", lty = 1) +
  geom_point(alpha = 0.3)

# 반사실 그래프화
pt2 <- filter(pt, Time < 25)

model.pt2 = gls(quantity.x ~ Time, data = pt2, correlation = corARMA(p = 1, q = 0, form = ~ Time), method = "ML")

pt <- pt %>% mutate(
  model.pt2.predictions = predictSE.gls(model.pt2, newdata = pt, se.fit = TRUE)$fit,
  model.pt2.se = predictSE.gls(model.pt2, pt, se.fit = TRUE)$se
)

pt3 <- filter(pt, Time < 49)
model.pt3 = gls(quantity.x ~ Time + Intervention + Post.intervention.time, data = pt3, correlation = corARMA(p = 1, q = 0, form = ~ Time), method = "ML")

pt <- pt %>% mutate(
  model.pt3.predictions = predictSE.gls(model.pt3, newdata = pt, se.fit = TRUE)$fit,
  model.pt3.se = predictSE.gls(model.pt3, pt, se.fit = TRUE)$se
)

ggplot(pt, aes(Time, quantity.x)) +
  geom_ribbon(aes(ymin = model.pt3.predictions - (1.96 * model.pt.se), ymax = model.pt3.predictions + (1.96 * model.pt2.se)), fill = "lightblue") +
  geom_line(aes(Time, model.pt3.predictions), color = "blue", lty = 2) +
  geom_ribbon(aes(ymin = model.pt2.predictions - (1.96 * model.pt.se), ymax = model.pt2.predictions + (1.96 * model.pt2.se)), fill = "pink") +
  geom_line(aes(Time, model.pt2.predictions), color = "red", lty = 2) +
  geom_ribbon(aes(ymin = model.pt.predictions - (1.96 * model.pt.se), ymax = model.pt.predictions + (1.96 * model.pt.se)), fill = "lightgreen") +
  geom_line(aes(Time, model.pt.predictions), color = "black", lty = 1) +
  geom_point(alpha = 0.3)


### log 그래프 ----
# P와 Q 탐지
mod.1 = quantity.x2 ~ Time + Intervention + Post.intervention.time + Intervention.2 + Post.intervention.2.time

fx = function(pval, qval) {
  summary(gls(mod.1, data = pt, correlation = corARMA(p = pval, q = qval, form = ~ Time), method = "ML"))$AIC
}

p = summary(gls(mod.1, data = pt, method = "ML"))$AIC
message(str_c("AIC Uncorrelated model = ", p))

autocorrel = expand.grid(pval = 0:5, qval = 0:5)

for (i in 2:nrow(autocorrel)) {
  p[i] = try(summary(gls(mod.1, data = pt, correlation = corARMA(p = autocorrel$pval[i], q = autocorrel$qval[i], form = ~ Time), method = "ML"))$AIC)
}

autocorrel <- autocorrel %>%
  mutate(AIC = as.numeric(p)) %>%
  arrange(AIC)

autocorrel

model.pt = gls(quantity.x2 ~ Time + Intervention + Post.intervention.time + Intervention.2 + Post.intervention.2.time, data = pt, method = "ML",
               correlation = corARMA(p = 0, q = 1, form = ~ Time))

# 모델 요약
summary(model.pt)

pt <- pt %>% mutate(
  model.pt.predictions = predictSE.gls(model.pt, pt, se.fit = TRUE)$fit,
  model.pt.se = predictSE.gls(model.pt, pt, se.fit = TRUE)$se
)

ggplot(pt, aes(Time, quantity.x2)) +
  geom_ribbon(aes(ymin = model.pt.predictions - (1.96 * model.pt.se), ymax = model.pt.predictions + (1.96 * model.pt.se)), fill = "lightgreen") +
  geom_line(aes(Time, model.pt.predictions), color = "black", lty = 1) +
  geom_point(alpha = 0.3)

# 반사실 그래프화
pt2 <- filter(pt, Time < 25)

model.pt2 = gls(quantity.x2 ~ Time, data = pt2, correlation = corARMA(p = 1, q = 0, form = ~ Time), method = "ML")

pt <- pt %>% mutate(
  model.pt2.predictions = predictSE.gls(model.pt2, newdata = pt, se.fit = TRUE)$fit,
  model.pt2.se = predictSE.gls(model.pt2, pt, se.fit = TRUE)$se
)

pt3 <- filter(pt, Time < 49)
model.pt3 = gls(quantity.x2 ~ Time + Intervention + Post.intervention.time, data = pt3, correlation = corARMA(p = 1, q = 0, form = ~ Time), method = "ML")

pt <- pt %>% mutate(
  model.pt3.predictions = predictSE.gls(model.pt3, newdata = pt, se.fit = TRUE)$fit,
  model.pt3.se = predictSE.gls(model.pt3, pt, se.fit = TRUE)$se
)

# log -> exponential - 로그값 변환은 quantity.x2를 사용할때 사용
pt <- pt %>% 
  mutate(quantity.x2 = exp(quantity.x2),
         model.pt.predictions = exp(model.pt.predictions),
         model.pt.se = exp(model.pt.se),
         model.pt2.predictions = exp(model.pt2.predictions),
         model.pt2.se = exp(model.pt2.se),
         model.pt3.predictions = exp(model.pt3.predictions),
         model.pt3.se = exp(model.pt3.se))


# log 값 그래프화
ggplot(pt, aes(Time, quantity.x2)) +
  geom_ribbon(aes(ymin = model.pt3.predictions - (1.96 * model.pt.se), ymax = model.pt3.predictions + (1.96 * model.pt2.se)), fill = "lightblue") +
  geom_line(aes(Time, model.pt3.predictions), color = "blue", lty = 2) +
  geom_ribbon(aes(ymin = model.pt2.predictions - (1.96 * model.pt.se), ymax = model.pt2.predictions + (1.96 * model.pt2.se)), fill = "pink") +
  geom_line(aes(Time, model.pt2.predictions), color = "red", lty = 2) +
  geom_ribbon(aes(ymin = model.pt.predictions - (1.96 * model.pt.se), ymax = model.pt.predictions + (1.96 * model.pt.se)), fill = "lightgreen") +
  geom_line(aes(Time, model.pt.predictions), color = "black", lty = 1) +
  geom_point(alpha = 0.3)


## pres 데이터 처리 ----
pres <- readxl::read_xlsx("pres.xlsx")

# P와 Q 탐지
mod.1 = quantity.x ~ Time + Intervention + Post.intervention.time + Intervention.2 + Post.intervention.2.time

fx = function(pval, qval) {
  summary(gls(mod.1, data = pres, correlation = corARMA(p = pval, q = qval, form = ~ Time), method = "ML"))$AIC
}

p = summary(gls(mod.1, data = pres, method = "ML"))$AIC
message(str_c("AIC Uncorrelated model = ", p))

autocorrel = expand.grid(pval = 0:5, qval = 0:5)

for (i in 2:nrow(autocorrel)) {
  p[i] = try(summary(gls(mod.1, data = pres, correlation = corARMA(p = autocorrel$pval[i], q = autocorrel$qval[i], form = ~ Time), method = "ML"))$AIC)
}

autocorrel <- autocorrel %>%
  mutate(AIC = as.numeric(p)) %>%
  arrange(AIC)

autocorrel

model.pres = gls(quantity.x ~ Time + Intervention + Post.intervention.time + Intervention.2 + Post.intervention.2.time, data = pres, method = "ML",
                 correlation = corARMA(p = 0, q = 1, form = ~ Time))

# 모델 요약
summary(model.pres)

pres <- pres %>% mutate(
  model.pres.predictions = predictSE.gls(model.pres, pres, se.fit = TRUE)$fit,
  model.pres.se = predictSE.gls(model.pres, pres, se.fit = TRUE)$se
)

ggplot(pres, aes(Time, quantity.x)) +
  geom_ribbon(aes(ymin = model.pres.predictions - (1.96 * model.pres.se), ymax = model.pres.predictions + (1.96 * model.pres.se)), fill = "lightgreen") +
  geom_line(aes(Time, model.pres.predictions), color = "black", lty = 1) +
  geom_point(alpha = 0.3)

# 반사실 그래프화
pres2 <- filter(pres, Time < 25)

model.pres2 = gls(quantity.x ~ Time, data = pres2, correlation = corARMA(p = 1, q = 0, form = ~ Time), method = "ML")

pres <- pres %>% mutate(
  model.pres2.predictions = predictSE.gls(model.pres2, newdata = pres, se.fit = TRUE)$fit,
  model.pres2.se = predictSE.gls(model.pres2, pres, se.fit = TRUE)$se
)

pres3 <- filter(pres, Time < 49)
model.pres3 = gls(quantity.x ~ Time + Intervention + Post.intervention.time, data = pres3, correlation = corARMA(p = 1, q = 0, form = ~ Time), method = "ML")

pres <- pres %>% mutate(
  model.pres3.predictions = predictSE.gls(model.pres3, newdata = pres, se.fit = TRUE)$fit,
  model.pres3.se = predictSE.gls(model.pres3, pres, se.fit = TRUE)$se
)

# 그래프 생성
ggplot(pres, aes(Time, quantity.x)) +
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
  summary(gls(mod.1, data = pres, correlation = corARMA(p = pval, q = qval, form = ~ Time), method = "ML"))$AIC
}

p = summary(gls(mod.1, data = pres, method = "ML"))$AIC
message(str_c("AIC Uncorrelated model = ", p))

autocorrel = expand.grid(pval = 0:5, qval = 0:5)

for (i in 2:nrow(autocorrel)) {
  p[i] = try(summary(gls(mod.1, data = pres, correlation = corARMA(p = autocorrel$pval[i], q = autocorrel$qval[i], form = ~ Time), method = "ML"))$AIC)
}

autocorrel <- autocorrel %>%
  mutate(AIC = as.numeric(p)) %>%
  arrange(AIC)

autocorrel

model.pres = gls(quantity.x2 ~ Time + Intervention + Post.intervention.time + Intervention.2 + Post.intervention.2.time, data = pres, method = "ML",
                 correlation = corARMA(p = 0, q = 1, form = ~ Time))

# 모델 요약
summary(model.pres)

pres <- pres %>% mutate(
  model.pres.predictions = predictSE.gls(model.pres, pres, se.fit = TRUE)$fit,
  model.pres.se = predictSE.gls(model.pres, pres, se.fit = TRUE)$se
)

ggplot(pres, aes(Time, quantity.x2)) +
  geom_ribbon(aes(ymin = model.pres.predictions - (1.96 * model.pres.se), ymax = model.pres.predictions + (1.96 * model.pres.se)), fill = "lightgreen") +
  geom_line(aes(Time, model.pres.predictions), color = "black", lty = 1) +
  geom_point(alpha = 0.3)

# 반사실 그래프화
pres2 <- filter(pres, Time < 25)

model.pres2 = gls(quantity.x2 ~ Time, data = pres2, correlation = corARMA(p = 1, q = 0, form = ~ Time), method = "ML")

pres <- pres %>% mutate(
  model.pres2.predictions = predictSE.gls(model.pres2, newdata = pres, se.fit = TRUE)$fit,
  model.pres2.se = predictSE.gls(model.pres2, pres, se.fit = TRUE)$se
)

pres3 <- filter(pres, Time < 49)
model.pres3 = gls(quantity.x2 ~ Time + Intervention + Post.intervention.time, data = pres3, correlation = corARMA(p = 1, q = 0, form = ~ Time), method = "ML")

pres <- pres %>% mutate(
  model.pres3.predictions = predictSE.gls(model.pres3, newdata = pres, se.fit = TRUE)$fit,
  model.pres3.se = predictSE.gls(model.pres3, pres, se.fit = TRUE)$se
)

# 로그 -> 지수 변환 (quantity.x2를 사용할 때)
pres <- pres %>% 
  mutate(quantity.x2 = exp(quantity.x2),
         model.pres.predictions = exp(model.pres.predictions),
         model.pres.se = exp(model.pres.se),
         model.pres2.predictions = exp(model.pres2.predictions),
         model.pres2.se = exp(model.pres2.se),
         model.pres3.predictions = exp(model.pres3.predictions),
         model.pres3.se = exp(model.pres3.se))

# 로그 값 그래프화
ggplot(pres, aes(Time, quantity.x2)) +
  geom_ribbon(aes(ymin = model.pres3.predictions - (1.96 * model.pres3.se), ymax = model.pres3.predictions + (1.96 * model.pres3.se)), fill = "lightblue") +
  geom_line(aes(Time, model.pres3.predictions), color = "blue", lty = 2) +
  geom_ribbon(aes(ymin = model.pres2.predictions - (1.96 * model.pres2.se), ymax = model.pres2.predictions + (1.96 * model.pres2.se)), fill = "pink") +
  geom_line(aes(Time, model.pres2.predictions), color = "red", lty = 2) +
  geom_ribbon(aes(ymin = model.pres.predictions - (1.96 * model.pres.se), ymax = model.pres.predictions + (1.96 * model.pres.se)), fill = "lightgreen") +
  geom_line(aes(Time, model.pres.predictions), color = "black", lty = 1) +
  geom_point(alpha = 0.3)


## amt 데이터 처리 ----
amt <- readxl::read_xlsx("amt.xlsx")

# P와 Q 탐지
mod.1 = quantity.x ~ Time + Intervention + Post.intervention.time + Intervention.2 + Post.intervention.2.time

fx = function(pval, qval) {
  summary(gls(mod.1, data = amt, correlation = corARMA(p = pval, q = qval, form = ~ Time), method = "ML"))$AIC
}

p = summary(gls(mod.1, data = amt, method = "ML"))$AIC
message(str_c("AIC Uncorrelated model = ", p))

autocorrel = expand.grid(pval = 0:5, qval = 0:5)

for (i in 2:nrow(autocorrel)) {
  p[i] = try(summary(gls(mod.1, data = amt, correlation = corARMA(p = autocorrel$pval[i], q = autocorrel$qval[i], form = ~ Time), method = "ML"))$AIC)
}

autocorrel <- autocorrel %>%
  mutate(AIC = as.numeric(p)) %>%
  arrange(AIC)

autocorrel

model.amt = gls(quantity.x ~ Time + Intervention + Post.intervention.time + Intervention.2 + Post.intervention.2.time, data = amt, method = "ML",
                correlation = corARMA(p = 0, q = 1, form = ~ Time))

# 모델 요약
summary(model.amt)

amt <- amt %>% mutate(
  model.amt.predictions = predictSE.gls(model.amt, amt, se.fit = TRUE)$fit,
  model.amt.se = predictSE.gls(model.amt, amt, se.fit = TRUE)$se.fit
)

ggplot(amt, aes(Time, quantity.x)) +
  geom_ribbon(aes(ymin = model.amt.predictions - (1.96 * model.amt.se), ymax = model.amt.predictions + (1.96 * model.amt.se)), fill = "lightgreen") +
  geom_line(aes(Time, model.amt.predictions), color = "black", lty = 1) +
  geom_point(alpha = 0.3)

# 반사실 그래프화
amt2 <- filter(amt, Time < 25)

model.amt2 = gls(quantity.x ~ Time, data = amt2, correlation = corARMA(p = 4, q = 0, form = ~ Time), method = "ML")

amt <- amt %>% mutate(
  model.amt2.predictions = predictSE.gls(model.amt2, newdata = amt, se.fit = TRUE)$fit,
  model.amt2.se = predictSE.gls(model.amt2, amt, se.fit = TRUE)$se.fit
)

amt3 <- filter(amt, Time < 49)
model.amt3 = gls(quantity.x ~ Time + Intervention + Post.intervention.time, data = amt3, correlation = corARMA(p = 4, q = 0, form = ~ Time), method = "ML")

amt <- amt %>% mutate(
  model.amt3.predictions = predictSE.gls(model.amt3, newdata = amt, se.fit = TRUE)$fit,
  model.amt3.se = predictSE.gls(model.amt3, amt, se.fit = TRUE)$se.fit
)

# 그래프 생성
ggplot(amt, aes(Time, quantity.x)) +
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
  summary(gls(mod.1, data = amt, correlation = corARMA(p = pval, q = qval, form = ~ Time), method = "ML"))$AIC
}

p = summary(gls(mod.1, data = amt, method = "ML"))$AIC
message(str_c("AIC Uncorrelated model = ", p))

autocorrel = expand.grid(pval = 0:5, qval = 0:5)

for (i in 2:nrow(autocorrel)) {
  p[i] = try(summary(gls(mod.1, data = amt, correlation = corARMA(p = autocorrel$pval[i], q = autocorrel$qval[i], form = ~ Time), method = "ML"))$AIC)
}

autocorrel <- autocorrel %>%
  mutate(AIC = as.numeric(p)) %>%
  arrange(AIC)

autocorrel

model.amt = gls(quantity.x2 ~ Time + Intervention + Post.intervention.time + Intervention.2 + Post.intervention.2.time, data = amt, method = "ML",
                correlation = corARMA(p = 0, q = 1, form = ~ Time))

# 모델 요약
summary(model.amt)

amt <- amt %>% mutate(
  model.amt.predictions = predictSE.gls(model.amt, amt, se.fit = TRUE)$fit,
  model.amt.se = predictSE.gls(model.amt, amt, se.fit = TRUE)$se.fit
)

ggplot(amt, aes(Time, quantity.x2)) +
  geom_ribbon(aes(ymin = model.amt.predictions - (1.96 * model.amt.se), ymax = model.amt.predictions + (1.96 * model.amt.se)), fill = "lightgreen") +
  geom_line(aes(Time, model.amt.predictions), color = "black", lty = 1) +
  geom_point(alpha = 0.3)

# 반사실 그래프화
amt2 <- filter(amt, Time < 25)

model.amt2 = gls(quantity.x2 ~ Time, data = amt2, correlation = corARMA(p = 2, q = 4, form = ~ Time), method = "ML")

amt <- amt %>% mutate(
  model.amt2.predictions = predictSE.gls(model.amt2, newdata = amt, se.fit = TRUE)$fit,
  model.amt2.se = predictSE.gls(model.amt2, amt, se.fit = TRUE)$se.fit
)

amt3 <- filter(amt, Time < 49)
model.amt3 = gls(quantity.x2 ~ Time + Intervention + Post.intervention.time, data = amt3, correlation = corARMA(p = 2, q = 4, form = ~ Time), method = "ML")

amt <- amt %>% mutate(
  model.amt3.predictions = predictSE.gls(model.amt3, newdata = amt, se.fit = TRUE)$fit,
  model.amt3.se = predictSE.gls(model.amt3, amt, se.fit = TRUE)$se.fit
)

# 로그 -> 지수 변환 (quantity.x2를 사용할 때)
amt <- amt %>% 
  mutate(quantity.x2 = exp(quantity.x2),
         model.amt.predictions = exp(model.amt.predictions),
         model.amt.se = exp(model.amt.se),
         model.amt2.predictions = exp(model.amt2.predictions),
         model.amt2.se = exp(model.amt2.se),
         model.amt3.predictions = exp(model.amt3.predictions),
         model.amt3.se = exp(model.amt3.se))

# 로그 값 그래프화
ggplot(amt, aes(Time, quantity.x2)) +
  geom_ribbon(aes(ymin = model.amt3.predictions - (1.96 * model.amt3.se), ymax = model.amt3.predictions + (1.96 * model.amt3.se)), fill = "lightblue") +
  geom_line(aes(Time, model.amt3.predictions), color = "blue", lty = 2) +
  geom_ribbon(aes(ymin = model.amt2.predictions - (1.96 * model.amt2.se), ymax = model.amt2.predictions + (1.96 * model.amt2.se)), fill = "pink") +
  geom_line(aes(Time, model.amt2.predictions), color = "red", lty = 2) +
  geom_ribbon(aes(ymin = model.amt.predictions - (1.96 * model.amt.se), ymax = model.amt.predictions + (1.96 * model.amt.se)), fill = "lightgreen") +
  geom_line(aes(Time, model.amt.predictions), color = "black", lty = 1) +
  geom_point(alpha = 0.3)

