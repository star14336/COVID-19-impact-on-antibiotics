
#패키지 ----
library(tidyverse)
library(ggplot2)
library(lubridate)

# Data screening -----
# 반복 작업을 위한 함수 정의
process_med_file <- function(file_path) {
  med_df <- read_csv(file_path)
  result <- med_df %>%
    mutate(USE_YEAR = factor(USE_YEAR),
           USE_MONTH = factor(USE_MONTH)) %>% 
    group_by(WK_COMPN_4, USE_YEAR, USE_MONTH) %>%
    summarise(pres = sum(PRSCRPTN_TNDN_CNT),
              pt = sum(PATIENT_CNT))
  return(result)
} #pres는 처방수, pt는 환자수


# 결과를 저장할 리스트 생성
MEDICINE_list <- list()

# MEDINE(1)부터 MEDINE(70)까지의 파일에 대해 반복 작업 수행
for (i in 1:70) {
       file_path <- paste0("MEDICINE/MEDICINE (", i, ").csv")
    if(!file.exists(file_path)){next}
    result <- process_med_file(file_path)
    MEDICINE_list[[i]] <- result
}

# 결과를 하나의 데이터프레임으로 병합
MEDICINE_result <- do.call(rbind, MEDICINE_list)

# 결과 확인
print(MEDICINE_result)



# Medication name ----
medi_inform <- read_csv("MEDICINE/MEDICINE_CODE_240301/medi.csv") #data loading

medi_inform <- medi_inform %>% 
  mutate(WK_COMPN_4 = substr(jo,1,4)) %>% 
  select(-jo)

length(unique(medi_inform$WK_COMPN_4))

# jo에서 중복된 번호를 제거 - 2251개
medi_inform <- medi_inform[-which(duplicated(medi_inform$WK_COMPN_4)), ]

medi_inform <- medi_inform %>% 
  mutate(WK_COMPN_4 = factor(WK_COMPN_4))

# left join
MEDICINE_result <- MEDICINE_result %>% 
  mutate(WK_COMPN_4 = factor(WK_COMPN_4))
  
medicine_result2 <- left_join(MEDICINE_result, medi_inform, by='WK_COMPN_4')

head(medicine_result2$pt)

colSums(is.na(medicine_result2)) #결측치 확인(과거에 사용되었지만 현재 사라진 약제)

###결측치 검사(없어진 약제들) -> 날려버리기(omit을 사용해도 됨, 어차피 같은 열에서 결측치가 있으니까)
medicine_result2 <- na.omit(medicine_result2)

colSums(is.na(medicine_result2))

