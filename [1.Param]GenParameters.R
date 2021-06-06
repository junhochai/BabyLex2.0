setwd("shortCDI_purrr_edition")

devtools::load_all(".")

# call.pkgs calls the requisite packages,
# or you can include more packages. see "pacs" for packages called.
call.pkgs()

rm(list=ls())
plan(multiprocess, gc=TRUE)

# tic("Parameters")
# Config
# set.seed(49)

# setwd("/Users/junho/Google\ Drive/PhD/Current\ Project/shortCDIr")

gen_list <- c("Female", "Male")
lang <- "Norwegian" #"English (American)" # "Greek (Cypriot)" #
#"Norwegian"#"Mandarin (Beijing)"#
type <- "WG" #"WS" # WS or WG
resp <- "production" # pick between comprehension or production
d_poly <- 3
poly_mode <- "Flexi" #"Fixed" #"Fixed" # or
criteria <- "incl.anyFinite" # or "excl.Inf" — for evaluation of log = -inf

# Prepare data
prep_data() # Extract data from wordbank
admin_ws <- admin_ws %>% filter(!(age < 11 & production > 100)) # extra filter for extreme outliers

admin_data_all <- data_in() #  merging admin_ws and data_ws

# # checking
# check <- admin_data_all %>% group_by(data_id, age) %>% summarise(sum = get(resp), value = sum(value)) %>% distinct()
# plot(check$sum-check$value)

word_dist <- perc_word() # percentage for each word in each age
word_dist$perc <- round(word_dist$perc, digits=2) # rounding

default_test <- c(num_words, 400, 200, 100, 50, 25, 10, 5)
test <- c(num_words, default_test[default_test < num_words])

# Convert admin_data_all and word_dist for purr and furr
data_purr <- admin_data_all %>% group_by(sex, value, age, num_item_id) %>%
  nest() # main set
get_order <- function(df){order(df$perc)} # percentage word
word_purr <- word_dist %>% select(-c("num_item_id")) %>% group_by(age) %>% nest() %>%
  mutate(data = future_map(data, get_order, .progress = T))

# Modeling
#Mod# Maximum likelihood estimation
item_threshold <- 5 # min amount of data point needed to fit mle

mle_purr <- data_purr
mle_purr$model <- future_pmap(list(data_purr$data), mle_model, .progress = T)

mle_purr$mean <- foreach(i = 1:nrow(mle_purr), .combine = rbind) %do% {
  try(coef(mle_purr$model[[i]])[1], silent=TRUE) %>% as.numeric()
}
mle_purr$sd  <- foreach(i = 1:nrow(mle_purr), .combine = rbind) %do% {
  try(coef(mle_purr$model[[i]])[2], silent=TRUE) %>% as.numeric()
}

mle_purr <- mle_purr %>% select(-c("model"))
mle_purr$mean[which(is.na(mle_purr$mean))] <- NaN
mle_purr$sd[which(is.na(mle_purr$sd))] <- NaN

# complete_data <- mle_purr %>% unnest() %>% select(-c(mean,sd))

#Mod# poly 3 or flexi
prep_purr <- mle_purr %>% select(-c("data")) %>%
  group_by(sex,value,age) %>% nest()

prep_purr$data2 <- pmap(list(prep_purr$age,prep_purr$data), fill_in_and_sort)
prep_purr <- prep_purr %>%
  select(-c("data"))

# complete_data <- complete_data %>% right_join(prep_purr %>% unnest(),
#                                               by=c("sex","value","age","num_item_id"))

idpoly <- poly_cfg(mode = poly_mode) # polynomial flexible, f(MAD)
# idpoly <- as.matrix(rep(1, length(age)))

poly_purr <- prep_purr
poly_purr$lm_mean <- pmap(list(prep_purr$age, prep_purr$data2), poly_fit_mean)
poly_purr$lm_sd <- pmap(list(prep_purr$age, prep_purr$data2), poly_fit_sd)
poly_purr <- poly_purr %>% unnest %>% nest(-c(sex,value,age,num_item_id))

# complete_data <- complete_data %>% right_join(poly_purr %>% unnest() %>%
#                                                 select("sex","value","age","num_item_id",
#                                                        "lm_mean","lm_sd"),
#                                               by=c("sex","value","age","num_item_id"))

#Mod# log PDF of normal distribution
log_pdf_purr_raw <- poly_purr
log_pdf_purr_raw$log  <- future_map(poly_purr$data, log_pdf, .progress = T)
log_pdf_purr_raw <- log_pdf_purr_raw %>%
  select(-data)

# Filter log dnorm pdf
log_pdf_purr <- log_pdf_purr_raw
log_pdf_purr$filt <- filter_log_pdf(log_pdf_purr_raw, criteria) # "excl.Inf" or
log_pdf_purr <- log_pdf_purr %>% filter(filt == 1)

#Mod# norms: basis for all items
all_basis <- log_pdf_purr %>% group_by(sex,value,age) %>% summarise(data = sum_log(log))

#Mod# Get Bmin (np_param) & slope for real-data simulation
param <- all_basis %>% group_by(sex,value,age) %>% summarise(B = max_basis(data))
np_param <- param %>% subset(value == 0)
slope <- param %>% group_by(sex,age) %>% summarise(slope = get_slope(B))

# complete_data <- mle_purr %>% unnest() %>% select(-c(mean,sd)) %>%
#   right_join(prep_purr %>% unnest(), by=c("sex","value","age","num_item_id")) %>%
#   right_join(poly_purr %>% unnest() %>% select("sex","value","age","num_item_id","lm_mean","lm_sd"),
#              by=c("sex","value","age","num_item_id")) %>%
#   right_join(log_pdf_purr %>% select(-filt), by=c("sex","value","age","num_item_id")) %>%
#   right_join(param, by = c("sex","value","age")) %>%
#   right_join(slope, by = c("sex","age"))

# save.rds("complete_data", poly_mode, criteria)

if (type == "WS") {
  save.csv("admin_data_all")
  save.csv("param", poly_mode, criteria)#,"temp") # temporary
  save.csv("np_param", poly_mode, criteria)
  save.csv("slope", poly_mode, criteria)
  save.rds("log_pdf_purr_raw", poly_mode, criteria)
} else if (type == "WG") {
  save.csv("admin_data_all", resp)
  save.csv("param", resp, poly_mode, criteria)#,"temp") # temporary
  save.csv("np_param", resp, poly_mode, criteria)
  save.csv("slope", resp, poly_mode, criteria)
  save.rds("log_pdf_purr_raw", resp, poly_mode, criteria)
}

for (gen in c("Female", "Male")) {
  
  slope2 = slope %>% ungroup() %>% filter(sex == gen) %>% 
    select(age, slope) %>% spread(age, slope)
  Bmin = np_param %>% ungroup() %>% filter(sex == gen) %>% 
    select(age, B) %>% spread(age, B)
  poly_unnest <- poly_purr %>% unnest()
  
  lm_p_mean = poly_unnest %>% ungroup() %>% filter(sex == gen & value == 1) %>% 
    select(age, lm_mean, num_item_id) %>% spread(age, lm_mean) %>% 
    mutate(word_id = num_item_id) %>%
    select(-num_item_id)
  lm_p_sd = poly_unnest %>% ungroup() %>% filter(sex == gen & value == 1) %>% 
    select(age, lm_sd, num_item_id) %>% spread(age, lm_sd) %>% 
    mutate(word_id = num_item_id) %>%
    select(-num_item_id)
  lm_np_mean = poly_unnest %>% ungroup() %>% filter(sex == gen & value == 0) %>% 
    select(age, lm_mean, num_item_id) %>% spread(age, lm_mean) %>% 
    mutate(word_id = num_item_id) %>%
    select(-num_item_id)
  lm_np_sd = poly_unnest %>% ungroup() %>% filter(sex == gen & value == 0) %>% 
    select(age, lm_sd, num_item_id) %>% spread(age, lm_sd) %>% 
    mutate(word_id = num_item_id) %>%
    select(-num_item_id)
  
  if (type == "WS") {
    fname1 <- paste("p_m", gen, lang, type, sep="_")
    fname1 <- paste(fname1, "csv", sep = ".")
    fname2 <- paste("p_sd", gen, lang, type, sep="_")
    fname2 <- paste(fname2, "csv", sep = ".")
    fname3 <- paste("np_m", gen, lang, type, sep="_")
    fname3 <- paste(fname3, "csv", sep = ".")
    fname4 <- paste("np_sd", gen, lang, type, sep="_")
    fname4 <- paste(fname4, "csv", sep = ".")
    fname5 <- paste("BMin", gen, lang, type, sep="_")
    fname5 <- paste(fname5, "csv", sep = ".")
    fname6 <- paste("Slope", gen, lang, type, sep="_")
    fname6 <- paste(fname6, "csv", sep = ".")
  } else if (type == "WG") {
    fname1 <- paste("p_m", gen, lang, type, resp, sep="_")
    fname1 <- paste(fname1, "csv", sep = ".")
    fname2 <- paste("p_sd", gen, lang, type, resp, sep="_")
    fname2 <- paste(fname2, "csv", sep = ".")
    fname3 <- paste("np_m", gen, lang, type, resp, sep="_")
    fname3 <- paste(fname3, "csv", sep = ".")
    fname4 <- paste("np_sd", gen, lang, type, resp, sep="_")
    fname4 <- paste(fname4, "csv", sep = ".")
    fname5 <- paste("BMin", gen, lang, type, resp, sep="_")
    fname5 <- paste(fname5, "csv", sep = ".")
    fname6 <- paste("Slope", gen, lang, type, resp, sep="_")
    fname6 <- paste(fname6, "csv", sep = ".")
  }
  
  write.csv(lm_p_mean, fname1)
  write.csv(lm_p_sd, fname2)
  write.csv(lm_np_mean, fname3)
  write.csv(lm_np_sd, fname4)
  write.csv(Bmin, fname5)
  write.csv(slope2, fname6)
  
}

fname7 <- paste("word_list", lang, type, resp, sep="_")
fname7 <- paste(fname7, "csv", sep = ".")
wordonly <- items_ws %>% mutate(word = definition, word_id = num_item_id) %>%
  select(word, word_id)
write_csv(wordonly, fname7)


### END of Modeling ###



