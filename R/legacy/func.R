call.pkgs <- function(...){pacs <<- c("wordbankr","plyr","dplyr","tidyr","tictoc","stats4","openxlsx","mirt","mirtCAT",
                                  "parallel","doParallel","sweep","purrr","future","furrr","tidyr",...)
  invisible(lapply(pacs,library,character.only=TRUE))
  cat("The summoning is complete.")}

# Configuration for CAT simulations
cat_cfg <- function(mode = "NoPreCAT") {
  if (mode == "NoPreCAT"){
    CAT_mode <<- list("NoPreCAT", list())
  } else if (mode == "PreCATFlex"){
    CAT_mode <<- list("PreCATFlex", list(min_items = 0, max_items = 10,
                                         criteria = "MI", method = "WLE",
                                         response_variance = TRUE))
  } else if (mode == "PreCAT") {
    CAT_mode <<- list("PreCAT", list(min_items = 10, max_items = 10,
                                     criteria = "MI", method = "WLE"))
  }
}

# Get data from word bank
prep_data <- function(){
  admin_ws <<- get_administration_data(lang, type)
  if(lang == "English (American)"){
    admin_ws <<- admin_ws %>% filter(norming=="TRUE")
  }
  data_ws <<- get_instrument_data(lang, type)
  if(lang == "English (American)"){
    data_ws <<- data_ws %>% filter(data_id %in% admin_ws$data_id)
  }
  items_ws <<- get_item_data(lang, type)

  # reduce datasets
  items_ws <<- subset(items_ws, type=="word")
  num_words <<- nrow(items_ws)
  data_ws <<- subset(data_ws, num_item_id < num_words+1)
  if (type == "WS") {
    data_ws$value[data_ws$value == "0p0r0o0d0u0c0e0s0"] <- 1
    data_ws$value[data_ws$value == "produces"] <- 1
    data_ws$value[is.na(data_ws$value)] <- 0
    data_ws$value[data_ws$value == ""] <- 0
  } else { # WG
    data_ws$value[is.na(data_ws$value)] <- 0
    data_ws$value[data_ws$value == ""] <- 0
    data_ws$value[data_ws$value != 0] <- 1
  }
  data_ws$value <<- as.numeric(data_ws$value)
}


# Prepare data for simulation (by merging admin_ws and data_ws)
data_in <- function(){
  if (type == "WS") {
    inner_join(data_ws, select(admin_ws, age, data_id, sex, production), by="data_id")
  } else if (type == "WG") {
    inner_join(data_ws, select(admin_ws, age, data_id, sex, comprehension), by="data_id")
  }
}

# Configuration for polynomial
poly_cfg <- function(mode = "Flexi"){

  age <<- admin_data_all$age %>% unique %>% sort
  ageN <- count(admin_data_all,age)

  # Measures size
  if (median(as.matrix(ageN[,2])) > 200){
    size <- "large"
  } else if (median(as.matrix(ageN[,2])) <= 200 & median(as.matrix(ageN[,2])) > 100){
    size <- "medium"
  } else if (median(as.matrix(ageN[,2])) <= 100 & median(as.matrix(ageN[,2])) > 50){
    size <- "small"
  } else if (median(as.matrix(ageN[,2])) <= 50) {
    size <- "tiny"
  }
  print(size)

  if (mode == "Fixed"){
    matrix(3, length(age), 1, dimnames = list(c(age), c("poly")))

  } else if (mode == "Flexi"){
    # Flexi Poly
    df_mad <- admin_data_all %>% select(age,production)
    df_mad <- aggregate(. ~ age, df_mad, function(x) c(median = median(x), mad = mad(x)))
    temp_df <- cbind(df_mad[,1],df_mad$production[,2]) %>%
      data.frame %>%
      mutate(poly = ifelse(X2 < 100, 1, 3))
    # Extract poly num only
    as.matrix(temp_df[,3])
  }
}

# Get percentages for each word in each age
perc_word <- function(){
  temp <- select(admin_data_all,num_item_id,value,age)
  word_dist <<- temp %>% group_by(age,num_item_id) %>% summarise(perc = mean(value))
}


# Model for MLE
LL <- function(m, sd) {
  R = dnorm(scores, m, sd)
  -sum(log(R))
}

# Apply mle model
mle_model <- function(dfdata,...){
  if (nrow(dfdata) > 5) {
    if (type == "WS") {
      scores <<- dfdata$production
    } else {
      scores <<- dfdata$comprehension
    }
    mle(LL, start = list(m = mean(scores), sd = sd(scores)), method = "L-BFGS-B")
  } else{
    NaN
  }
}

# Fill in "missing" words
fill_in <- function(dfage,df){
  # ex <- data.frame(1:num_words)
  sorted <<- data.frame(word_purr$data[which(word_purr$age == dfage)])
  colnames(sorted) <- "num_item_id"
  right_join(df,sorted, by = "num_item_id")
}

# Fit degree of polynomial
poly_fit_mean <- function(dfage,df,...){
  d_poly <<- idpoly[which(age == dfage)]
  if (any(is.finite(df$mean))) {
    model <<- lm(df$mean~poly(c(1:num_words), d_poly, raw = TRUE))
    pmax(20, predict(model, newdata=data.frame(c(1:num_words))))
  } else {
    NaN
  }
}

poly_fit_sd <- function(dfage,df,...){
  d_poly <<- idpoly[which(age == dfage)]
  if (any(is.finite(df$sd))) {
    model <<- lm(df$sd~poly(c(1:num_words), d_poly, raw = TRUE))
    pmax(20, predict(model, newdata=data.frame(c(1:num_words))))
  } else {
    NaN
  }
}

# log PDF of normal distribution
log_pdf <- function(df){
  log(dnorm(c(0,1:num_words),df$lm_mean,df$lm_sd),base = exp(1))
}

# Filter log dnorm pdf
filter_log_pdf <- function(df, criteria){
  if(criteria == "excl.Inf"){
    # Exclude all rows with -inf
    df %>% mutate(filt = as.numeric(map(df$log,sum))) %>%
      filter(is.finite(filt))
  } else if (criteria == "incl.anyFinite"){
    # Filter rows that have any finite numbers
    df %>% mutate(filt = map(df$log,ls.is.finite)) %>%
      filter(filt==TRUE)
  } else {
    cat("Input error.")
  }
}

# Filter log dnorm pdf by Rows
ls.is.finite <- function(df){
  ifelse(any(is.finite(unlist(df)))==TRUE,TRUE,FALSE)}

# Replace log dnorm pdf by Case

# sum of log (basis)
sum_log <- function(df){
  list(reduce(df,`+`))
}

# map(df, function(df){df[which(is.finite(df))]})

# max basis
max_basis <- function(df){
  as.numeric(which(unlist(df) == max(unlist(df))))
}

# calculate slope
get_slope <- function(df){
  diff(df)/num_words
}

# save csv
save.csv <- function(df,...){
  fname <- paste(lang,type,df,...,sep="_")
  write.csv(get(df),paste(fname,"csv",sep="."))
}

# save rds
save.rds <- function(df,...){
  fname <- paste(lang,type,df,...,sep="_")
  saveRDS(get(df),file=paste(fname,"rds",sep="."))
}

# read csv
call.csv <- function(df,...){
  fname <- paste(lang,type,df,...,sep="_")
  assign(df, read.csv(paste(fname,"csv",sep="."))[,-1],envir = .GlobalEnv)
}

# read rds
call.rds <- function(df,...){
  fname <- paste(lang,type,df,...,sep="_")
  assign(df, readRDS(paste(fname,"rds",sep=".")), envir = .GlobalEnv)
}

# Babylex (random list)
randrowsumB <- function(df){
 df[sample(nrow(df), test[i]),]$log %>% sum_log() %>% max_basis()
}

# Babylex-IRT (mirt-cat)
# Config for IRT
remove_dataID <- function(df){df %>% select(-c(data_id))}

filter_response <- function(df){Filter(var,df)}

# Fit data to IRT
fit_item_mirt <- function(df){mirt(df, 1, itemtype = "2PL", method = "EM")}

# Apply IRT to database
mirt_cat <- function(dfa,dfx,dfd){
  design <- list(min_items = pmin(ncol(
    # data_irt_purr$data[which(data_irt_purr$age==dfa &
    #                            data_irt_purr$sex==dfx)]
    with(data_irt_purr, data[age==dfa & sex==dfx])[[1]]), test[1]),
    max_items = pmin(ncol(
      # data_irt_purr$data[which(data_irt_purr$age==dfa &
      #                                    data_irt_purr$sex==dfx)]
      with(data_irt_purr, data[age==dfa & sex==dfx])[[1]]), test[1]))
  mirtCAT(mo =  with(data_irt_purr, irt[age==dfa & sex==dfx])[[1]],
          local_pattern = dfd, criteria = "MI", method = "WLE",
          start_item = "MI", design = design, preCAT = CAT_mode[[2]])#, cl = cl)
}

# Compute B for IRT test
IRTrowsumB <- function(dfref,dftar){
  dftar[dfref[["items_answered"]] # Get item_answered from the results
        [1:pmin(length(dfref[["items_answered"]]), # Pick between test[i] and
                test[i])],]$log %>%     # length of "items_ans.." for safety.
    sum_log() %>%
    max_basis()
}

# Real-data simulations - Score estimation
get_newB <- function(df){
  (unlist(df[callcolm]) - df$B)/df$slope
}

get_reg <- function(df){
  cor(unlist(df[callcolmB]),df$production)
}

get_stderr <- function(df){
  mean(sd((unlist(df$production)/num_words) - (unlist(df[callcolmNewB])/num_words)))
}

get_rel <- function(df){
  1-mean(df^2)
}

# Analyses Branch:
analyse <- function(group,...){
  final_purr <<- newB_purr %>%
    ungroup %>% select(-c(B,slope,...)) %>% group_by_at(group) %>% nest()
  # Get reg, stdev, rel
  col <- ncol(final_purr)
  ind <- 0
  for(i in 1:length(test)){
    print(i)
    callcolmB <<- paste("B",test[i], sep= "_")
    callcolmNewB <<- paste("newB",test[i], sep= "_")

    colmreg <<- paste(test[i], "reg" ,sep= "_")
    colmstderr <<- paste(test[i], "stderr" ,sep= "_")
    colmrel <<- paste(test[i], "rel" ,sep= "_")

    final_purr[colmreg] <<- (final_purr %>% mutate(map(final_purr$data, get_reg)))[col+i+ind]
    final_purr[colmstderr] <<- (final_purr %>% mutate(map(final_purr$data, get_stderr)))[col+1+i+ind]
    final_purr[colmrel] <<- as.matrix(unlist((final_purr %>% mutate(map(final_purr$data, get_stderr)))[col+1+i+ind])) %>%
      apply(1,get_rel)

    ind <- ind + 2
  }
}

# Incase of insufficient sample size per age.
gen_age_grp <- function(df)
  if(all(unique(df$age) %in% c(16:30))==TRUE){
    df %>% mutate(age = case_when(between(age,16,18)~1618,
                                  between(age,19,21)~1921,
                                  between(age,22,24)~2224,
                                  between(age,25,27)~2527,
                                  between(age,28,30)~2830))
  }else if(all(unique(df$age) %in% c(18:36))==TRUE){
    df %>%  mutate(age = case_when(between(age,18,21)~"18 - 21",
                                   between(age,22,24)~"22 - 24",
                                   between(age,25,27)~"25 - 27",
                                   between(age,28,30)~"28 - 30",
                                   between(age,31,33)~"31 - 33",
                                   between(age,34,36)~"34 - 36"))
  }else if(all(unique(df$age) %in% c(16:36))==TRUE){
    df %>% mutate(age = case_when(between(age,16,18)~"16 - 18",
                                  between(age,19,21)~"19 - 21",
                                  between(age,22,24)~"22 - 24",
                                  between(age,25,27)~"25 - 27",
                                  between(age,28,30)~"28 - 30",
                                  between(age,31,33)~"31 - 33",
                                  between(age,34,36)~"34 - 36"))} else {print("Age needs attention")}

