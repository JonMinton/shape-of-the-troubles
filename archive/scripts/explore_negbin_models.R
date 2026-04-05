# Structural model --------------------------------------------------------


# Improvements 1922-1955
# War 1939-1945 
# age 35+ ageing effect
# Adulthood onset effect (18+)
# post 1972 effect 


# Model assumptions : 
# Troubles exposure starts at age 16
# Troubles exposure peaks at age 20
# Troubles exposures diminishes to age 37

expos_strt <- 16
expos_peak <- 20
expos_end <- 37

age_expos <- function(this_age, expos_strt, expos_peak, expos_end){
  out <- 0
  if (this_age >= expos_strt & this_age <= expos_end){
    if (this_age <= expos_peak){
      rng <- expos_peak - expos_strt
      out <- (this_age - expos_strt) / rng
    } else {
      rng <- expos_end - expos_peak
      out <- (expos_end - this_age) / rng
    }
  }
  out 
}

dta_nir %>% 
  filter(age >=15 & age <= 45) %>% 
  mutate(P = year - min(year)) %>% 
  mutate(
    improvement_phase = ifelse(year < 1955, T, F),
    yrs_since_start_improvement = year - 1922
  ) %>% 
  mutate(
    ww2 = ifelse(year >= 1939 & year <= 1945, T, F) 
  ) %>% 
  mutate(
    age_since_35 = age - 35,
    age_since_35 = ifelse(age_since_35 < 0, 0, age_since_35)
  ) %>% 
  mutate(
    is_adult = age >= 18
  ) %>% 
  mutate(
    flg_72_77 = year >= 1972 & year <= 1977, # 1972-1977
    flg_78_83 = year >= 1978 & year <= 1983,# 1978-1983
    flg_84_89 = year >= 1984 & year <= 1989,# 1984-1989
    flg_90_95 = year >= 1990 & year <= 1995,# 1990-1995
    flg_96_00 = year >= 1996 & year <= 2000# 1996-2000
  ) %>% 
  mutate(
    trbls_lim = ifelse(year < 1972, 0, 1),
    trbls_0_05pc = ifelse(year < 1972, 0, (1 - 0.0005) ^ (year - 1972)),
    trbls_0_10pc = ifelse(year < 1972, 0, (1 - 0.0010) ^ (year - 1972)),
    trbls_0_25pc = ifelse(year < 1972, 0, (1 - 0.0025) ^ (year - 1972)),
    trbls_0_5pc = ifelse(year < 1972, 0, (1 - 0.005) ^ (year - 1972)),
    trbls_1pc = ifelse(year < 1972, 0, (1 - 0.01) ^ (year - 1972)),
    trbls_1_5pc = ifelse(year < 1972, 0, (1 - 0.015) ^ (year - 1972)),
    trbls_2pc = ifelse(year < 1972, 0, (1 - 0.02) ^ (year - 1972)),
    trbls_4pc = ifelse(year < 1972, 0, (1 - 0.04) ^ (year - 1972)),
    trbls_5pc = ifelse(year < 1972, 0, (1 - 0.05) ^ (year - 1972)),
    trbls_6pc = ifelse(year < 1972, 0, (1 - 0.06) ^ (year - 1972)),
    trbls_7pc = ifelse(year < 1972, 0, (1 - 0.07) ^ (year - 1972)),
    trbls_8pc = ifelse(year < 1972, 0, (1 - 0.08) ^ (year - 1972)),
    trbls_9pc = ifelse(year < 1972, 0, (1 - 0.09) ^ (year - 1972)),
    trbls_10pc = ifelse(year < 1972, 0, (1 - 0.10) ^ (year - 1972))
  ) %>% 
  mutate(
    trbls_expos_age = purrr::map_dbl(
      age, age_expos,
      expos_strt = expos_strt, expos_peak = expos_peak, expos_end = expos_end
    ),
    expos_1pc = trbls_expos_age * trbls_1pc,
    expos_2pc = trbls_expos_age * trbls_2pc,
    expos_4pc = trbls_expos_age * trbls_4pc,
    expos_5pc = trbls_expos_age * trbls_5pc,
    expos_6pc = trbls_expos_age * trbls_6pc,
    expos_7pc = trbls_expos_age * trbls_7pc,
    expos_8pc = trbls_expos_age * trbls_8pc,
    expos_9pc = trbls_expos_age * trbls_9pc,
    expos_10pc = trbls_expos_age * trbls_10pc
  ) -> dta_wth_flags

dta_wth_flags %>% 
  filter(sex == "male") -> dta_male_wth_flags
dta_wth_flags %>% 
  filter(sex == "female") -> dta_female_wth_flags


# No interaction
glm.nb(
  deaths ~ age + P + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_a_y

# Interaction
glm.nb(
  deaths ~ age * P + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_ay

# Poly -age2
glm.nb(
  deaths ~ poly(age, 2) + P + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_aa_y

# Poly -age3
glm.nb(
  deaths ~ poly(age, 3) + P + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_aaa_y


# Poly -year2
glm.nb(
  deaths ~ age + poly(P, 2) + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_a_yy

# Poly -year3
glm.nb(
  deaths ~ age + poly(P, 2) + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_a_yyy

# Poly = age2 year2
glm.nb(
  deaths ~ poly(age, 2) + poly(P, 2) + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_aa_yy

# Poly age2*year2
glm.nb(
  deaths ~ poly(age, 2) * poly(P, 2) + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_aayy


# Model age 35+ separately
glm.nb(
  deaths ~ age * P + age_since_35 + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_ay_35plus

glm.nb(
  deaths ~ age + P + age_since_35 + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_a_y_35plus

AIC(mdl_a_y, mdl_ay, mdl_a_y_35plus, mdl_ay_35plus)

# Adulthood effect
glm.nb(
  deaths ~ age + P + is_adult + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_a_y_adlt

glm.nb(
  deaths ~ age * P + is_adult + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_ay_adlt

glm.nb(
  deaths ~ age * P + is_adult + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_ay_adlt

glm.nb(
  deaths ~ age * P + age_since_35 + is_adult + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_ay_35plus_adlt

glm.nb(
  deaths ~ age + P + age_since_35 + is_adult + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_a_y_35plus_adlt

AIC(mdl_a_y, mdl_ay, mdl_a_y_adlt, mdl_ay_adlt, 
    mdl_a_y_35plus, mdl_ay_35plus, mdl_a_y_35plus_adlt, mdl_ay_35plus_adlt)

BIC(mdl_a_y, mdl_ay, mdl_a_y_adlt, mdl_ay_adlt, 
    mdl_a_y_35plus, mdl_ay_35plus, mdl_a_y_35plus_adlt, mdl_ay_35plus_adlt)

# BIC is more conservative, and suggests against adding interaction effects 

# Best BIC so far is 
# mdl_a_y_35plus_adlt

glm.nb(
  deaths ~ age + P + age_since_35 + is_adult + ww2 + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_a_y_35plus_adlt_ww2

BIC(mdl_a_y, mdl_ay, mdl_a_y_adlt, mdl_ay_adlt, 
    mdl_a_y_35plus, mdl_ay_35plus, mdl_a_y_35plus_adlt, mdl_ay_35plus_adlt,
    mdl_a_y_35plus_adlt_ww2
)

# Improvement phase
glm.nb(
  deaths ~ age + P + age_since_35 + is_adult + ww2 + I(yrs_since_start_improvement * improvement_phase) + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_a_y_35plus_adlt_ww2_imrv

AIC(mdl_a_y, mdl_ay, mdl_a_y_adlt, mdl_ay_adlt, 
    mdl_a_y_35plus, mdl_ay_35plus, mdl_a_y_35plus_adlt, mdl_ay_35plus_adlt,
    mdl_a_y_35plus_adlt_ww2, mdl_a_y_35plus_adlt_ww2_imrv
)


BIC(mdl_a_y, mdl_ay, mdl_a_y_adlt, mdl_ay_adlt, 
    mdl_a_y_35plus, mdl_ay_35plus, mdl_a_y_35plus_adlt, mdl_ay_35plus_adlt,
    mdl_a_y_35plus_adlt_ww2, mdl_a_y_35plus_adlt_ww2_imrv
)

# Suggests a further improvement now using both AIC and BIC

# Now to add flags for 
# 1972-1977
# 1978-1983
# 1984-1989
# 1990-1995
# 1996-2000

glm.nb(
  deaths ~ age + P + age_since_35 + is_adult + ww2 + I(yrs_since_start_improvement * improvement_phase) + 
    + flg_72_77 + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_a_y_35plus_adlt_ww2_imrv_flg01

glm.nb(
  deaths ~ age + P + age_since_35 + is_adult + ww2 + I(yrs_since_start_improvement * improvement_phase) + 
    + flg_72_77 + flg_78_83 + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_a_y_35plus_adlt_ww2_imrv_flg02

glm.nb(
  deaths ~ age + P + age_since_35 + is_adult + ww2 + I(yrs_since_start_improvement * improvement_phase) + 
    + flg_72_77 + flg_78_83 + flg_84_89 + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_a_y_35plus_adlt_ww2_imrv_flg03

AIC(mdl_a_y, mdl_ay, mdl_a_y_adlt, mdl_ay_adlt, 
    mdl_a_y_35plus, mdl_ay_35plus, mdl_a_y_35plus_adlt, mdl_ay_35plus_adlt,
    mdl_a_y_35plus_adlt_ww2, mdl_a_y_35plus_adlt_ww2_imrv,
    mdl_a_y_35plus_adlt_ww2_imrv_flg01,
    mdl_a_y_35plus_adlt_ww2_imrv_flg02,
    mdl_a_y_35plus_adlt_ww2_imrv_flg03
)


BIC(mdl_a_y, mdl_ay, mdl_a_y_adlt, mdl_ay_adlt, 
    mdl_a_y_35plus, mdl_ay_35plus, mdl_a_y_35plus_adlt, mdl_ay_35plus_adlt,
    mdl_a_y_35plus_adlt_ww2, mdl_a_y_35plus_adlt_ww2_imrv,
    mdl_a_y_35plus_adlt_ww2_imrv_flg01,
    mdl_a_y_35plus_adlt_ww2_imrv_flg02,
    mdl_a_y_35plus_adlt_ww2_imrv_flg03
)


summary(mdl_a_y_35plus_adlt_ww2_imrv_flg01)


# Now to look at the same model spec for females

glm.nb(
  deaths ~ age + P + age_since_35 + is_adult + ww2 + I(yrs_since_start_improvement * improvement_phase) + 
    + flg_72_77 + offset(log(exposure)),
  data = dta_female_wth_flags
) -> mdl_a_y_35plus_adlt_ww2_imrv_flg01_female

summary(mdl_a_y_35plus_adlt_ww2_imrv_flg01_female)


# Now to build some models that assume either a 1%, 2%, 5% or 10% decay in 
#effect after 1972

glm.nb(
  deaths ~ age + P + age_since_35 + is_adult + ww2 + I(yrs_since_start_improvement * improvement_phase) + 
    + trbls_lim + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_a_y_35plus_adlt_ww2_imrv_trblslim

glm.nb(
  deaths ~ age + P + age_since_35 + is_adult + ww2 + I(yrs_since_start_improvement * improvement_phase) + 
    + trbls_0_05pc + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_a_y_35plus_adlt_ww2_imrv_trbls0_05pc

glm.nb(
  deaths ~ age + P + age_since_35 + is_adult + ww2 + I(yrs_since_start_improvement * improvement_phase) + 
    + trbls_0_10pc + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_a_y_35plus_adlt_ww2_imrv_trbls0_10pc

glm.nb(
  deaths ~ age + P + age_since_35 + is_adult + ww2 + I(yrs_since_start_improvement * improvement_phase) + 
    + trbls_0_25pc + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_a_y_35plus_adlt_ww2_imrv_trbls0_25pc

glm.nb(
  deaths ~ age + P + age_since_35 + is_adult + ww2 + I(yrs_since_start_improvement * improvement_phase) + 
    + trbls_0_5pc + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_a_y_35plus_adlt_ww2_imrv_trbls0_5pc

glm.nb(
  deaths ~ age + P + age_since_35 + is_adult + ww2 + I(yrs_since_start_improvement * improvement_phase) + 
    + trbls_1pc + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_a_y_35plus_adlt_ww2_imrv_trbls1pc

glm.nb(
  deaths ~ age + P + age_since_35 + is_adult + ww2 + I(yrs_since_start_improvement * improvement_phase) + 
    + trbls_1_5pc + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_a_y_35plus_adlt_ww2_imrv_trbls1_5pc

glm.nb(
  deaths ~ age + P + age_since_35 + is_adult + ww2 + I(yrs_since_start_improvement * improvement_phase) + 
    + trbls_2pc + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_a_y_35plus_adlt_ww2_imrv_trbls2pc

glm.nb(
  deaths ~ age + P + age_since_35 + is_adult + ww2 + I(yrs_since_start_improvement * improvement_phase) + 
    + trbls_5pc + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_a_y_35plus_adlt_ww2_imrv_trbls5pc

glm.nb(
  deaths ~ age + P + age_since_35 + is_adult + ww2 + I(yrs_since_start_improvement * improvement_phase) + 
    + trbls_10pc + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_a_y_35plus_adlt_ww2_imrv_trbls10pc

AIC(mdl_a_y, mdl_ay, mdl_a_y_adlt, mdl_ay_adlt, 
    mdl_a_y_35plus, mdl_ay_35plus, mdl_a_y_35plus_adlt, mdl_ay_35plus_adlt,
    mdl_a_y_35plus_adlt_ww2, mdl_a_y_35plus_adlt_ww2_imrv,
    mdl_a_y_35plus_adlt_ww2_imrv_flg01,
    mdl_a_y_35plus_adlt_ww2_imrv_flg02,
    mdl_a_y_35plus_adlt_ww2_imrv_flg03,
    mdl_a_y_35plus_adlt_ww2_imrv_trblslim,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls0_05pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls0_10pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls0_25pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls0_5pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls1pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls1_5pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls2pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls5pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls10pc
)


BIC(mdl_a_y, mdl_ay, mdl_a_y_adlt, mdl_ay_adlt, 
    mdl_a_y_35plus, mdl_ay_35plus, mdl_a_y_35plus_adlt, mdl_ay_35plus_adlt,
    mdl_a_y_35plus_adlt_ww2, mdl_a_y_35plus_adlt_ww2_imrv,
    mdl_a_y_35plus_adlt_ww2_imrv_flg01,
    mdl_a_y_35plus_adlt_ww2_imrv_flg02,
    mdl_a_y_35plus_adlt_ww2_imrv_flg03,
    mdl_a_y_35plus_adlt_ww2_imrv_trblslim,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls0_05pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls0_10pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls0_25pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls0_5pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls1pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls1_5pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls2pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls5pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls10pc
)

# Both suggest 1% has better fit than alternatives 

# 0.1% better still.

# This suggests to me the baseline model hasn't accounted enough for
# the collapse in improvment in male mortality after the improvement phase. 


# What if I re-do without a separate P coefficient


glm.nb(
  deaths ~ age + age_since_35 + is_adult + ww2 + I(yrs_since_start_improvement * improvement_phase) + 
    + trbls_lim + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_a_35plus_adlt_ww2_imrv_trblslim

glm.nb(
  deaths ~ age + age_since_35 + is_adult + ww2 + I(yrs_since_start_improvement * improvement_phase) + 
    + trbls_0_05pc + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_a_35plus_adlt_ww2_imrv_trbls0_05pc

glm.nb(
  deaths ~ age + age_since_35 + is_adult + ww2 + I(yrs_since_start_improvement * improvement_phase) + 
    + trbls_0_10pc + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_a_35plus_adlt_ww2_imrv_trbls0_10pc

glm.nb(
  deaths ~ age + age_since_35 + is_adult + ww2 + I(yrs_since_start_improvement * improvement_phase) + 
    + trbls_0_25pc + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_a_35plus_adlt_ww2_imrv_trbls0_25pc

glm.nb(
  deaths ~ age + age_since_35 + is_adult + ww2 + I(yrs_since_start_improvement * improvement_phase) + 
    + trbls_0_5pc + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_a_35plus_adlt_ww2_imrv_trbls0_5pc

glm.nb(
  deaths ~ age + age_since_35 + is_adult + ww2 + I(yrs_since_start_improvement * improvement_phase) + 
    + trbls_1pc + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_a_35plus_adlt_ww2_imrv_trbls1pc

glm.nb(
  deaths ~ age + age_since_35 + is_adult + ww2 + I(yrs_since_start_improvement * improvement_phase) + 
    + trbls_1_5pc + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_a_35plus_adlt_ww2_imrv_trbls1_5pc

glm.nb(
  deaths ~ age + age_since_35 + is_adult + ww2 + I(yrs_since_start_improvement * improvement_phase) + 
    + trbls_2pc + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_a_35plus_adlt_ww2_imrv_trbls2pc

glm.nb(
  deaths ~ age + age_since_35 + is_adult + ww2 + I(yrs_since_start_improvement * improvement_phase) + 
    + trbls_5pc + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_a_35plus_adlt_ww2_imrv_trbls5pc

glm.nb(
  deaths ~ age + age_since_35 + is_adult + ww2 + I(yrs_since_start_improvement * improvement_phase) + 
    + trbls_10pc + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_a_35plus_adlt_ww2_imrv_trbls10pc

AIC(mdl_a_y, mdl_ay, mdl_a_y_adlt, mdl_ay_adlt, 
    mdl_a_y_35plus, mdl_ay_35plus, mdl_a_y_35plus_adlt, mdl_ay_35plus_adlt,
    mdl_a_y_35plus_adlt_ww2, mdl_a_y_35plus_adlt_ww2_imrv,
    mdl_a_y_35plus_adlt_ww2_imrv_flg01,
    mdl_a_y_35plus_adlt_ww2_imrv_flg02,
    mdl_a_y_35plus_adlt_ww2_imrv_flg03,
    mdl_a_y_35plus_adlt_ww2_imrv_trblslim,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls0_05pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls0_10pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls0_25pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls0_5pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls1pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls1_5pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls2pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls5pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls10pc,
    mdl_a_35plus_adlt_ww2_imrv_trblslim,
    mdl_a_35plus_adlt_ww2_imrv_trbls0_05pc,
    mdl_a_35plus_adlt_ww2_imrv_trbls0_10pc,
    mdl_a_35plus_adlt_ww2_imrv_trbls0_25pc,
    mdl_a_35plus_adlt_ww2_imrv_trbls0_5pc,
    mdl_a_35plus_adlt_ww2_imrv_trbls1pc,
    mdl_a_35plus_adlt_ww2_imrv_trbls1_5pc,
    mdl_a_35plus_adlt_ww2_imrv_trbls2pc,
    mdl_a_35plus_adlt_ww2_imrv_trbls5pc,
    mdl_a_35plus_adlt_ww2_imrv_trbls10pc
    
)


BIC(mdl_a_y, mdl_ay, mdl_a_y_adlt, mdl_ay_adlt, 
    mdl_a_y_35plus, mdl_ay_35plus, mdl_a_y_35plus_adlt, mdl_ay_35plus_adlt,
    mdl_a_y_35plus_adlt_ww2, mdl_a_y_35plus_adlt_ww2_imrv,
    mdl_a_y_35plus_adlt_ww2_imrv_flg01,
    mdl_a_y_35plus_adlt_ww2_imrv_flg02,
    mdl_a_y_35plus_adlt_ww2_imrv_flg03,
    mdl_a_y_35plus_adlt_ww2_imrv_trblslim,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls0_05pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls0_10pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls0_25pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls0_5pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls1pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls1_5pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls2pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls5pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls10pc,
    mdl_a_35plus_adlt_ww2_imrv_trblslim,
    mdl_a_35plus_adlt_ww2_imrv_trbls0_05pc,
    mdl_a_35plus_adlt_ww2_imrv_trbls0_10pc,
    mdl_a_35plus_adlt_ww2_imrv_trbls0_25pc,
    mdl_a_35plus_adlt_ww2_imrv_trbls0_5pc,
    mdl_a_35plus_adlt_ww2_imrv_trbls1pc,
    mdl_a_35plus_adlt_ww2_imrv_trbls1_5pc,
    mdl_a_35plus_adlt_ww2_imrv_trbls2pc,
    mdl_a_35plus_adlt_ww2_imrv_trbls5pc,
    mdl_a_35plus_adlt_ww2_imrv_trbls10pc
    
)


# No, removing P makes it much worse. 

# Alternative assumption is that there is both a step
# and also an exponential decay component to the additional mort

# Step + 0.5%
glm.nb(
  deaths ~ age + P + age_since_35 + is_adult + ww2 + I(yrs_since_start_improvement * improvement_phase) + 
    + trbls_lim + trbls_0_5pc + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_a_y_35plus_adlt_ww2_imrv_trblslim_plus_0_5pc

# Step + 1%
glm.nb(
  deaths ~ age + P + age_since_35 + is_adult + ww2 + I(yrs_since_start_improvement * improvement_phase) + 
    + trbls_lim + trbls_1pc + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_a_y_35plus_adlt_ww2_imrv_trblslim_plus_1pc

# Step + 2%
glm.nb(
  deaths ~ age + P + age_since_35 + is_adult + ww2 + I(yrs_since_start_improvement * improvement_phase) + 
    + trbls_lim + trbls_2pc + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_a_y_35plus_adlt_ww2_imrv_trblslim_plus_2pc

# Step + 5%
glm.nb(
  deaths ~ age + P + age_since_35 + is_adult + ww2 + I(yrs_since_start_improvement * improvement_phase) + 
    + trbls_lim + trbls_5pc + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_a_y_35plus_adlt_ww2_imrv_trblslim_plus_5pc

# Step + 10%
glm.nb(
  deaths ~ age + P + age_since_35 + is_adult + ww2 + I(yrs_since_start_improvement * improvement_phase) + 
    + trbls_lim + trbls_10pc + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_a_y_35plus_adlt_ww2_imrv_trblslim_plus_10pc

# What about the initial flag?

# Step + init flag
glm.nb(
  deaths ~ age + P + age_since_35 + is_adult + ww2 + I(yrs_since_start_improvement * improvement_phase) + 
    + trbls_lim + flg_72_77 + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_a_y_35plus_adlt_ww2_imrv_trblslim_plus_flg01

AIC(mdl_a_y, mdl_ay, mdl_a_y_adlt, mdl_ay_adlt, 
    mdl_a_y_35plus, mdl_ay_35plus, mdl_a_y_35plus_adlt, mdl_ay_35plus_adlt,
    mdl_a_y_35plus_adlt_ww2, mdl_a_y_35plus_adlt_ww2_imrv,
    mdl_a_y_35plus_adlt_ww2_imrv_flg01,
    mdl_a_y_35plus_adlt_ww2_imrv_flg02,
    mdl_a_y_35plus_adlt_ww2_imrv_flg03,
    mdl_a_y_35plus_adlt_ww2_imrv_trblslim,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls0_05pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls0_10pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls0_25pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls0_5pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls1pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls1_5pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls2pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls5pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls10pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trblslim_plus_0_5pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trblslim_plus_1pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trblslim_plus_2pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trblslim_plus_5pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trblslim_plus_10pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trblslim_plus_flg01
    
)


BIC(mdl_a_y, mdl_ay, mdl_a_y_adlt, mdl_ay_adlt, 
    mdl_a_y_35plus, mdl_ay_35plus, mdl_a_y_35plus_adlt, mdl_ay_35plus_adlt,
    mdl_a_y_35plus_adlt_ww2, mdl_a_y_35plus_adlt_ww2_imrv,
    mdl_a_y_35plus_adlt_ww2_imrv_flg01,
    mdl_a_y_35plus_adlt_ww2_imrv_flg02,
    mdl_a_y_35plus_adlt_ww2_imrv_flg03,
    mdl_a_y_35plus_adlt_ww2_imrv_trblslim,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls0_05pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls0_10pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls0_25pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls0_5pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls1pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls1_5pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls2pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls5pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls10pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trblslim_plus_0_5pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trblslim_plus_1pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trblslim_plus_2pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trblslim_plus_5pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trblslim_plus_10pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trblslim_plus_flg01
)


# Now to use the age-specific exposure flags instead

glm.nb(
  deaths ~ age + P + age_since_35 + is_adult + ww2 + I(yrs_since_start_improvement * improvement_phase) + 
    + expos_1pc + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_a_y_35plus_adlt_ww2_imrv_expos1pc

glm.nb(
  deaths ~ age + P + age_since_35 + is_adult + ww2 + I(yrs_since_start_improvement * improvement_phase) + 
    + expos_2pc + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_a_y_35plus_adlt_ww2_imrv_expos2pc

glm.nb(
  deaths ~ age + P + age_since_35 + is_adult + ww2 + I(yrs_since_start_improvement * improvement_phase) + 
    + expos_5pc + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_a_y_35plus_adlt_ww2_imrv_expos5pc

glm.nb(
  deaths ~ age + P + age_since_35 + is_adult + ww2 + I(yrs_since_start_improvement * improvement_phase) + 
    + expos_1pc + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_a_y_35plus_adlt_ww2_imrv_expos1pc


glm.nb(
  deaths ~ poly(age, 2) * poly(P, 2) + age_since_35 + is_adult + ww2 + I(yrs_since_start_improvement * improvement_phase) + 
    + expos_1pc + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_aayy_35plus_adlt_ww2_imrv_expos1pc

glm.nb(
  deaths ~ poly(age, 2) * poly(P, 2) + age_since_35 + is_adult + ww2 + I(yrs_since_start_improvement * improvement_phase) + 
    + expos_2pc + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_aayy_35plus_adlt_ww2_imrv_expos2pc

glm.nb(
  deaths ~ poly(age, 2) * poly(P, 2) + age_since_35 + is_adult + ww2 + I(yrs_since_start_improvement * improvement_phase) + 
    + expos_4pc + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_aayy_35plus_adlt_ww2_imrv_expos4pc

glm.nb(
  deaths ~ poly(age, 2) * poly(P, 2) + age_since_35 + is_adult + ww2 + I(yrs_since_start_improvement * improvement_phase) + 
    + expos_5pc + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_aayy_35plus_adlt_ww2_imrv_expos5pc

glm.nb(
  deaths ~ poly(age, 2) * poly(P, 2) + age_since_35 + is_adult + ww2 + I(yrs_since_start_improvement * improvement_phase) + 
    + expos_6pc + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_aayy_35plus_adlt_ww2_imrv_expos6pc

glm.nb(
  deaths ~ poly(age, 2) * poly(P, 2) + age_since_35 + is_adult + ww2 + I(yrs_since_start_improvement * improvement_phase) + 
    + expos_7pc + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_aayy_35plus_adlt_ww2_imrv_expos7pc

glm.nb(
  deaths ~ poly(age, 2) * poly(P, 2) + age_since_35 + is_adult + ww2 + I(yrs_since_start_improvement * improvement_phase) + 
    + expos_8pc + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_aayy_35plus_adlt_ww2_imrv_expos8pc

glm.nb(
  deaths ~ poly(age, 2) * poly(P, 2) + age_since_35 + is_adult + ww2 + I(yrs_since_start_improvement * improvement_phase) + 
    + expos_9pc + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_aayy_35plus_adlt_ww2_imrv_expos9pc

glm.nb(
  deaths ~ poly(age, 2) * poly(P, 2) + age_since_35 + is_adult + ww2 + I(yrs_since_start_improvement * improvement_phase) + 
    + expos_10pc + offset(log(exposure)),
  data = dta_male_wth_flags
) -> mdl_aayy_35plus_adlt_ww2_imrv_expos10pc

update(mdl_a_y_35plus_adlt_ww2_imrv_expos1pc, . ~ . + trbls_lim) -> mdl_a_y_35plus_adlt_ww2_imrv_lim_expos1pc
update(mdl_a_y_35plus_adlt_ww2_imrv_expos2pc, . ~ . + trbls_lim) -> mdl_a_y_35plus_adlt_ww2_imrv_lim_expos2pc
update(mdl_a_y_35plus_adlt_ww2_imrv_expos5pc, . ~ . + trbls_lim) -> mdl_a_y_35plus_adlt_ww2_imrv_lim_expos5pc

update(mdl_a_y_35plus_adlt_ww2_imrv_lim_expos1pc, . ~ . + age:P) -> mdl_ay_35plus_adlt_ww2_imrv_lim_expos1pc
update(mdl_a_y_35plus_adlt_ww2_imrv_lim_expos2pc, . ~ . + age:P) -> mdl_ay_35plus_adlt_ww2_imrv_lim_expos2pc
update(mdl_a_y_35plus_adlt_ww2_imrv_lim_expos5pc, . ~ . + age:P) -> mdl_ay_35plus_adlt_ww2_imrv_lim_expos5pc


AIC(mdl_a_y, mdl_ay, mdl_a_y_adlt, mdl_ay_adlt, 
    mdl_aa_y, mdl_aaa_y,
    mdl_a_yy, mdl_a_yyy,
    mdl_aa_yy, mdl_aayy,
    mdl_a_y_35plus, mdl_ay_35plus, mdl_a_y_35plus_adlt, mdl_ay_35plus_adlt,
    mdl_a_y_35plus_adlt_ww2, mdl_a_y_35plus_adlt_ww2_imrv,
    mdl_a_y_35plus_adlt_ww2_imrv_flg01,
    mdl_a_y_35plus_adlt_ww2_imrv_flg02,
    mdl_a_y_35plus_adlt_ww2_imrv_flg03,
    mdl_a_y_35plus_adlt_ww2_imrv_trblslim,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls0_05pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls0_10pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls0_25pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls0_5pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls1pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls1_5pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls2pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls5pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls10pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trblslim_plus_0_5pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trblslim_plus_1pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trblslim_plus_2pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trblslim_plus_5pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trblslim_plus_10pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trblslim_plus_flg01,
    mdl_a_y_35plus_adlt_ww2_imrv_expos1pc,
    mdl_a_y_35plus_adlt_ww2_imrv_expos2pc,
    mdl_a_y_35plus_adlt_ww2_imrv_expos5pc,
    mdl_a_y_35plus_adlt_ww2_imrv_lim_expos1pc,
    mdl_a_y_35plus_adlt_ww2_imrv_lim_expos2pc,
    mdl_a_y_35plus_adlt_ww2_imrv_lim_expos5pc,
    mdl_aayy_35plus_adlt_ww2_imrv_expos1pc,
    mdl_aayy_35plus_adlt_ww2_imrv_expos2pc,
    mdl_aayy_35plus_adlt_ww2_imrv_expos4pc,
    mdl_aayy_35plus_adlt_ww2_imrv_expos5pc,
    mdl_aayy_35plus_adlt_ww2_imrv_expos6pc,
    mdl_aayy_35plus_adlt_ww2_imrv_expos7pc,
    mdl_aayy_35plus_adlt_ww2_imrv_expos8pc,
    mdl_aayy_35plus_adlt_ww2_imrv_expos9pc,
    mdl_aayy_35plus_adlt_ww2_imrv_expos10pc,
    mdl_ay_35plus_adlt_ww2_imrv_lim_expos1pc,
    mdl_ay_35plus_adlt_ww2_imrv_lim_expos2pc,
    mdl_ay_35plus_adlt_ww2_imrv_lim_expos5pc
    
)


BIC(mdl_a_y, mdl_ay, mdl_a_y_adlt, mdl_ay_adlt, 
    mdl_aa_y, mdl_aaa_y,
    mdl_a_yy, mdl_a_yyy,
    mdl_aa_yy, mdl_aayy,
    mdl_a_y_35plus, mdl_ay_35plus, mdl_a_y_35plus_adlt, mdl_ay_35plus_adlt,
    mdl_a_y_35plus_adlt_ww2, mdl_a_y_35plus_adlt_ww2_imrv,
    mdl_a_y_35plus_adlt_ww2_imrv_flg01,
    mdl_a_y_35plus_adlt_ww2_imrv_flg02,
    mdl_a_y_35plus_adlt_ww2_imrv_flg03,
    mdl_a_y_35plus_adlt_ww2_imrv_trblslim,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls0_05pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls0_10pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls0_25pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls0_5pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls1pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls1_5pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls2pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls5pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trbls10pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trblslim_plus_0_5pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trblslim_plus_1pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trblslim_plus_2pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trblslim_plus_5pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trblslim_plus_10pc,
    mdl_a_y_35plus_adlt_ww2_imrv_trblslim_plus_flg01,
    mdl_a_y_35plus_adlt_ww2_imrv_expos1pc,
    mdl_a_y_35plus_adlt_ww2_imrv_expos2pc,
    mdl_a_y_35plus_adlt_ww2_imrv_expos5pc,
    mdl_a_y_35plus_adlt_ww2_imrv_lim_expos1pc,
    mdl_a_y_35plus_adlt_ww2_imrv_lim_expos2pc,
    mdl_a_y_35plus_adlt_ww2_imrv_lim_expos5pc,
    mdl_aayy_35plus_adlt_ww2_imrv_expos1pc,
    mdl_aayy_35plus_adlt_ww2_imrv_expos2pc,
    mdl_aayy_35plus_adlt_ww2_imrv_expos4pc,
    mdl_aayy_35plus_adlt_ww2_imrv_expos5pc,
    mdl_aayy_35plus_adlt_ww2_imrv_expos6pc,
    mdl_aayy_35plus_adlt_ww2_imrv_expos7pc,
    mdl_aayy_35plus_adlt_ww2_imrv_expos8pc,
    mdl_aayy_35plus_adlt_ww2_imrv_expos9pc,
    mdl_aayy_35plus_adlt_ww2_imrv_expos10pc,
    mdl_ay_35plus_adlt_ww2_imrv_lim_expos1pc,
    mdl_ay_35plus_adlt_ww2_imrv_lim_expos2pc,
    mdl_ay_35plus_adlt_ww2_imrv_lim_expos5pc
)

# AS this has now found a u-shape in BIC with different decay rates , and this is 
# just a single parameter, I can use optimize (not optim) to solve 


find_decay <- function(k, what = "AIC"){
  
  dta_nir %>% 
    filter(age >=15 & age <= 45) %>% 
    mutate(P = year - min(year)) %>% 
    mutate(
      improvement_phase = ifelse(year < 1955, T, F),
      yrs_since_start_improvement = year - 1922
    ) %>% 
    mutate(
      ww2 = ifelse(year >= 1939 & year <= 1945, T, F) 
    ) %>% 
    mutate(
      age_since_35 = age - 35,
      age_since_35 = ifelse(age_since_35 < 0, 0, age_since_35)
    ) %>% 
    mutate(
      is_adult = age >= 18
    ) %>% 
    mutate(
      flg_72_77 = year >= 1972 & year <= 1977, # 1972-1977
      flg_78_83 = year >= 1978 & year <= 1983,# 1978-1983
      flg_84_89 = year >= 1984 & year <= 1989,# 1984-1989
      flg_90_95 = year >= 1990 & year <= 1995,# 1990-1995
      flg_96_00 = year >= 1996 & year <= 2000# 1996-2000
    ) %>% 
    mutate(
      trbls_kpc = ifelse(year < 1972, 0, (1 - k) ^ (year - 1972))
    ) %>% 
    mutate(
      trbls_expos_age = purrr::map_dbl(
        age, age_expos,
        expos_strt = expos_strt, expos_peak = expos_peak, expos_end = expos_end
      ),
      expos_kpc = trbls_expos_age * trbls_kpc
    ) %>% 
    filter(sex == "male") -> dta_male_wth_flags
  
  glm.nb(
    deaths ~ poly(age, 2) * poly(P, 2) + age_since_35 + is_adult + ww2 + I(yrs_since_start_improvement * improvement_phase) + 
      + expos_kpc + offset(log(exposure)),
    data = dta_male_wth_flags
  ) -> mdl
  if (what == "AIC") {out <- AIC(mdl)} else if (what == "BIC") {out <- BIC(mdl)}
  out
}

optimize(find_decay, lower = 0, upper = 1)
# Using AIC: 0.03996742
optimize(find_decay, lower = 0, upper = 1, what = "BIC")
# Using BIC: 0.03996742

# So use decay rate of 4% 

mdl_best <- mdl_aayy_35plus_adlt_ww2_imrv_expos4pc

dta_male_wth_flags %>% 
  modelr::add_predictions(mdl_best) %>% 
  dplyr::select(year, age, sex, deaths, exposure, mr, lmr, pred ) %>%
  mutate(pred_dths = pred * log(exposure)) %>% 
  dplyr::select(year, age, deaths, pred_dths) %>% 
  gather(key = "type", value = "value", deaths:pred_dths) -> pred_actual

pred_actual %>% 
  levelplot(
    value ~ year * age | type, data = .,
    col.regions = qual_col,
    #    at = seq(from = -4.5, to = -1.5, by = 0.05),
    scales = list(alternating = 3),
    aspect= "iso"
  )

# The model underestimates the number of deaths up until around 1935,
# and overestimates the number of deaths after around 1955. 
# Let's look at a plot of residuals

pred_actual %>% 
  spread(type, value) %>% 
  mutate(residuals = pred_dths - deaths) %>% 
  levelplot(
    residuals ~ year * age, data = .,
    col.regions = rdbu_col,
    at = seq(from = -35, to = 35, by = 0.5),
    scales = list(
      alternating = 3,
      x = list(at = seq(1925, 2010, by = 5), rot = 90)
    ),
    aspect= "iso"
  )

# Add an additional flag indicating 1952 onwards 

mdl_best %>% 
  update(. ~ . + I(year >= 1952)) -> mdl_best_52flg


dta_male_wth_flags %>% 
  modelr::add_predictions(mdl_best_52flg) %>% 
  dplyr::select(year, age, sex, deaths, exposure, mr, lmr, pred ) %>%
  mutate(pred_dths = pred * log(exposure)) %>% 
  dplyr::select(year, age, deaths, pred_dths) %>% 
  gather(key = "type", value = "value", deaths:pred_dths) -> pred_actual

pred_actual %>% 
  levelplot(
    value ~ year * age | type, data = .,
    col.regions = qual_col,
    #    at = seq(from = -4.5, to = -1.5, by = 0.05),
    scales = list(alternating = 3),
    aspect= "iso"
  )

# The model underestimates the number of deaths up until around 1935,
# and overestimates the number of deaths after around 1955. 
# Let's look at a plot of residuals

pred_actual %>% 
  spread(type, value) %>% 
  mutate(residuals = pred_dths - deaths) %>% 
  levelplot(
    residuals ~ year * age, data = .,
    col.regions = rdbu_col,
    at = seq(from = -35, to = 35, by = 0.5),
    scales = list(
      alternating = 3,
      x = list(at = seq(1925, 2010, by = 5), rot = 90)
    ),
    aspect= "iso"
  )

