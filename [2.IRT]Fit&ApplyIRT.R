setwd("shortCDI_purrr_edition")

devtools::load_all(".")

# call.pkgs calls the requisite packages,
# or you can include more packages. see "pacs" for packages called.
call.pkgs()

# clear environment
# rm(list=ls())

# setwd("/Users/junho/Google\ Drive/PhD/Current\ Project/shortCDIr")

# Config
set.seed(49)

lang <- "Norwegian" # Norwegian
type <- "WG" # WS or WG
resp <- "production" # pick between comprehension or production
criteria <- "incl.anyFinite" # or "excl.Inf" — for evaluation of log = -inf
poly_mode <- "Fixed" #"Flexi" # or
cat_cfg(mode = "NoPreCAT") # NoPreCAT

if (type == "WG") {
  call.csv("admin_data_all", resp)
} else if (type == "WS") {
  call.csv("admin_data_all")
}

num_words <- max(admin_data_all$num_item_id)
test <- c(num_words, 400, 200, 100, 50, 25, 10, 5)

# # Fit IRT (first time)
# data_irt_purr <- admin_data_all %>% #filter(between(age, 16, 30)) %>%
# select(value,num_item_id,age,sex,data_id) %>%
#   spread(num_item_id,value) %>% nest(-c(age, sex)) %>%
#   mutate(data = future_map(data, remove_dataID, .progress=T)) %>%
#   mutate(data = future_map(data, filter_response, .progress=T))
# data_irt_purr <- data_irt_purr %>%
#   mutate(irt = future_map(data, fit_item_mirt, .progress=T))

# Fit IRT (overall)
data_irt <- admin_data_all %>% #filter(between(age, 16, 30)) %>%
  select(value,num_item_id,data_id) %>%
  spread(num_item_id,value) %>% select(-data_id)
data_irt <- Filter(var,data_irt)
items <- mirt(data_irt, 1, itemtype = "2PL", method = "EM", SE=TRUE)

# Extract parameters only (a1, d, g, u)
for (i in 1:(length(items@ParObjects$pars) - 1)){
  print(i)
  if (i == 1) {
    irtparam <- coef(items)[[i]][1,1:4] 
  } else {
    irtparam <- rbind(irtparam, coef(items)[[i]][1,1:4]) 
  }
}
rownames(irtparam) <- 1:nrow(irtparam)

IRT_Parameters <- irtparam

if (type == "WG") {
  save.csv("IRT_Parameters", resp)
} else if (type == "WS") {
  save.csv("IRT_Parameters")
}

if (type == "WG") {
  save.rds("data_irt_purr", resp, "irtMod")
} else if (type == "WS") {
  save.rds("data_irt_purr", "irtMod")
}

if (type == "WS"){
  admin_irt <- admin_data_all %>%
    spread(num_item_id,value) %>% nest(-c(data_id,age,sex,production))
} else if (type == "WG") {
  admin_irt <- admin_data_all %>%
    spread(num_item_id,value) %>% nest(-c(data_id,age,sex,all_of(resp)))
}

if (type == "WG") {
  call.rds("data_irt_purr", resp, "irtMod")
} else if (type == "WS") {
  call.rds("data_irt_purr", "irtMod")
}

# Apply IRT to database
data_irt_purr <- data_irt_purr %>% modify_if(is.factor,as.character) %>% filter(!is.na(sex))
admin_irt <- admin_irt %>% modify_if(is.factor,as.character) %>% filter(!is.na(sex))

# One Block
#### Run 50 words ####
mirt_cat <- function(dfa,dfx,dfd){
  design <- list(min_items = pmin(ncol(
    with(data_irt_purr, data[age==dfa & sex==dfx])[[1]]), 50),
    max_items = pmin(ncol(
      with(data_irt_purr, data[age==dfa & sex==dfx])[[1]]), 50))
  mirtCAT(mo =  with(data_irt_purr, irt[age==dfa & sex==dfx])[[1]],
          local_pattern = dfd, criteria = "MI", method = "WLE",
          start_item = "MI", design = design, preCAT = CAT_mode[[2]])#, cl = cl)
}

select_item_irt_purr <- admin_irt %>%
  mutate(result = future_pmap(list(admin_irt$age,admin_irt$sex,admin_irt$data),
                              mirt_cat, .progress=T))
if (type == "WG") {
  save.rds("select_item_irt_purr", resp,"itemsCAT")
} else if (type == "WS") {
  save.rds("select_item_irt_purr","itemsCAT")
}

# Chunks
admin_irt_1 <- admin_irt[1:572,]
admin_irt_2 <- admin_irt[573:1145,]
admin_irt_3 <- admin_irt[1146:1718,]
admin_irt_4 <- admin_irt[1719:2291,]
admin_irt_5 <- admin_irt[2292:2922,]

select_item_irt_purr_1 <- admin_irt_1 %>%
  mutate(result = future_pmap(list(admin_irt_1$age,admin_irt_1$sex,admin_irt_1$data), mirt_cat, .progress=T))

if (type == "WG") {
  save.rds("select_item_irt_purr_1", resp,"itemsCAT")
} else if (type == "WS") {
  save.rds("select_item_irt_purr_1","itemsCAT")
}

select_item_irt_purr_2 <- admin_irt_2 %>%
  mutate(result = future_pmap(list(admin_irt_2$age,admin_irt_2$sex,admin_irt_2$data), mirt_cat, .progress=T))

if (type == "WG") {
  save.rds("select_item_irt_purr_2", resp,"itemsCAT")
} else if (type == "WS") {
  save.rds("select_item_irt_purr_2","itemsCAT")
}

select_item_irt_purr_3 <- admin_irt_3 %>%
  mutate(result = future_pmap(list(admin_irt_3$age,admin_irt_3$sex,admin_irt_3$data), mirt_cat, .progress=T))

if (type == "WG") {
  save.rds("select_item_irt_purr_3", resp,"itemsCAT")
} else if (type == "WS") {
  save.rds("select_item_irt_purr_3","itemsCAT")
}

select_item_irt_purr_4 <- admin_irt_4 %>%
  mutate(result = future_pmap(list(admin_irt_4$age,admin_irt_4$sex,admin_irt_4$data), mirt_cat, .progress=T))

if (type == "WG") {
  save.rds("select_item_irt_purr_4", resp,"itemsCAT")
} else if (type == "WS") {
  save.rds("select_item_irt_purr_4","itemsCAT")
}

select_item_irt_purr_5 <- admin_irt_5 %>%
  mutate(result = future_pmap(list(admin_irt_5$age,admin_irt_5$sex,admin_irt_5$data), mirt_cat, .progress=T))

if (type == "WG") {
  save.rds("select_item_irt_purr_5", resp,"itemsCAT")
} else if (type == "WS") {
  save.rds("select_item_irt_purr_5","itemsCAT")
}

### comb <- bind_rows(select_item_irt_purr_1,select_item_irt_purr_2,...)

### benchmarking IRT Norwegian CDI-WG Production
admin_irt_sub <- admin_irt %>% filter(data_id %in% seed_data_id$data_id)
tic()
select_item_irt_purr <- admin_irt_sub %>%
  mutate(result = future_pmap(list(admin_irt_sub$age,admin_irt_sub$sex,admin_irt_sub$data),
                              mirt_cat, .progress=T))
toc()
if (type == "WG") {
  save.rds("select_item_irt_purr", resp,"itemsCAT")
} else if (type == "WS") {
  save.rds("select_item_irt_purr","itemsCAT")
}
