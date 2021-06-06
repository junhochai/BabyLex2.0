call.pkgs <- function(...){pacs <<- c("wordbankr","plyr","dplyr","tidyr","stats4","openxlsx","mirt","mirtCAT",
                                  "sweep","purrr","future","furrr","tidyverse","tictoc","parallel","doParallel",...) #"tictoc","parallel","doParallel",
  invisible(lapply(pacs,library,character.only=TRUE))
  cat("The summoning is complete.")}

# Get data from word bank
prep_data <- function(){
  admin_ws <<- get_administration_data(lang, type)
  if(lang == "English (American)"){
    admin_ws <<- admin_ws #%>% filter(norming=="TRUE")
  }
  data_ws <<- get_instrument_data(lang, type)
  if(lang == "English (American)"){
    data_ws <<- data_ws %>% filter(data_id %in% admin_ws$data_id)
  }
  items_ws <<- get_item_data(lang, type)

  # For matching
  # if(lang == "Norwegian" & type == "WG"){
  #   items_ws <<- items_ws %>% mutate(definition = str_remove(items_ws$definition, pattern = " "))
  #   sub_cdi <<- read.csv("cdi_df.csv")
  #   items_ws <<- items_ws %>% filter(definition %in% sub_cdi$lemma)
  # }

  # reduce datasets
  items_ws <<- subset(items_ws, type=="word")
  num_words <<- nrow(items_ws)
  data_ws <<- data_ws %>% filter(num_item_id %in% items_ws$num_item_id)
  if (type == "WS" | (type == "WG" & resp == "production")) {
    data_ws <- data_ws %>% 
      mutate(value = ifelse(value %in% c("produces", "yes", "sometimes", "often"),
                            1,
                            0))
  } else if (type == "WG" & resp == "comprehension") { # WG
    data_ws <- data_ws %>% 
      mutate(value = ifelse(value %in% c("no", "never", "not yet", "") | is.na(value),
                            0,
                            1))
  }
  data_ws$value <<- as.numeric(data_ws$value)
}


# Prepare admin_data_all for simulation (by merging admin_ws and data_ws)
data_in <- function(){
  if (type == "WS") {
    inner_join(data_ws, select(admin_ws, age, data_id, sex, production), by="data_id")
  } else if (type == "WG") {
    inner_join(data_ws, select(admin_ws, age, data_id, sex, all_of(resp)), by="data_id")
  }
}

# Configuration for polynomial
### should consider ratio: df_mad$production[,2]/df_mad$production[,1]
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
    if(type == "WS"){
      # Flexi Poly
      df_mad <- admin_data_all %>% ungroup() %>% select(age,production)
      df_mad <- aggregate(. ~ age, df_mad, function(x) c(median = median(x), mad = mad(x)))
      temp_df <- cbind(df_mad[,1],df_mad$production[,2]) %>%
        data.frame %>%
        mutate(poly = ifelse(X2 < 100, 1, 3))
      # Extract poly num only
      as.matrix(temp_df[,3])
    } else if (type == "WG" & resp == "comprehension"){
      # Flexi Poly
      df_mad <- admin_data_all %>% select(age,all_of(resp))
      df_mad <- aggregate(. ~ age, df_mad, function(x) c(median = median(x), mad = mad(x)))
      temp_df <- cbind(df_mad[,1],
                       (df_mad %>% select(all_of(resp)) %>% data.frame)[,1][,2]) %>% # dynamic columns
        data.frame %>%
        mutate(poly = ifelse(X2 < 100, 1, 3))
      # Extract poly num only
      as.matrix(temp_df[,3])
      
    } else if (type == "WG" & resp == "production"){
      # # Flexi Poly Pilot
      # df_mad <- admin_data_all %>% select(age,all_of(resp))
      # df_mad <- aggregate(. ~ age, df_mad, function(x) c(median = median(x), mad = mad(x)))
      # temp_df <- cbind(df_mad[,1],
      #                  (df_mad %>% select(all_of(resp)) %>% data.frame)[,1][,2]) %>% # dynamic columns
      #   data.frame %>%
      #   mutate(poly = ifelse(df_mad$production[,2]/df_mad$production[,1] >= 1 |
      #                        is.na(df_mad$production[,2]/df_mad$production[,1]),
      #                        1, 3))
      # # Extract poly num only
      # as.matrix(temp_df[,3])
      # as.matrix(rep(1, length(age))) # Fixed Poly
      # Flexi Poly Original
      df_mad <- admin_data_all %>% select(age,all_of(resp))
      df_mad <- aggregate(. ~ age, df_mad, function(x) c(median = median(x), mad = mad(x)))
      temp_df <- cbind(df_mad[,1],
                       (df_mad %>% select(all_of(resp)) %>% data.frame)[,1][,2]) %>% # dynamic columns
        data.frame %>%
        mutate(poly = ifelse(X2 < 100, 1, 3))
      # Extract poly num only
      as.matrix(temp_df[,3])
    }
  }
}

# Get percentages for each word in each age
perc_word <- function(){
  temp <- select(admin_data_all,num_item_id,value,age)
  word_dist <<- temp %>% dplyr::group_by(age,num_item_id) %>% dplyr::summarise(perc = mean(value))
}


# Model for MLE
LL <- function(m, sd) {
  R = dnorm(scores, m, sd)
  -sum(log(R))
}

# Apply mle model
mle_model <- function(dfdata,...){
  # i <<- i + 1 # troubleshooting
  # print(i) # troubleshooting
  if (type == "WS") { # check CDI type
    scores <<- dfdata$production
    scores <<- scores[scores != 0] # remove zeros
    check <<- scores[scores != 0] 
    if (length(check) > item_threshold) {
      mle(LL, start = list(m = mean(scores), sd = sd(scores)),
          method = "L-BFGS-B")
    } else{
      NaN
    } 
  } else if (type == "WG") { # check CDI type
    if (resp == "comprehension"){
      scores <<- dfdata$comprehension
      scores <<- scores[scores != 0] # remove zeros
      check <<- scores[scores != 0] 
    } else if (resp == "production"){
      scores <<- dfdata$production
      scores <<- scores[scores != 0] # remove zeros
      check <<- scores[scores != 0]
    }
    if (length(check) > item_threshold) { 
      mle(LL, start = list(m = mean(scores), sd = sd(scores)))#, 
      # method = "L-BFGS-B")
    } else{
      NaN
    }
  }
} #40 45 50 55

# Debug MLE debug_mle(data_purr)
debug_mle <- function(df) for (i in 1:nrow(df)){
  print(i)
  print(try(coef(mle_model(df$data[[i]]))[1]) %>% as.numeric())
}

# Fill in "missing" words and Reorder according to word_purr
fill_in_and_sort <- function(dfage,df){
  # ex <- data.frame(1:num_words)
  sorted <<- word_dist$num_item_id %>% unique() %>% data.frame()
  colnames(sorted) <- "num_item_id"
  full_join(sorted, df, by = "num_item_id")
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
  log(dnorm(c(0,1:num_words),df$lm_mean,df$lm_sd), base = exp(1))
}

# Filter log dnorm pdf
filter_log_pdf <- function(df, criteria){
  if(criteria == "excl.Inf"){
    # Exclude all rows with -inf
    # df %>% mutate(., filt = as.numeric(map(df$log,sum))) %>%
    #   filter(is.finite(filt))
    df$filt <- is.finite(as.numeric(map(df$log,sum)))
  } else if (criteria == "incl.anyFinite"){
    # Filter rows that have any finite numbers
    # df %>% mutate(., filt = map(df$log,ls.is.finite)) %>%
    #   filter(filt==TRUE)
    df$filt <- as.numeric(map(df$log,ls.is.finite))
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
  list(reduce(df,`+`) + 1)
}

# map(df, function(df){df[which(is.finite(df))]})

# max basis
max_basis <- function(df){
  as.numeric(which(unlist(df) == max(unlist(df), na.rm = T)))
}

# max basis
max_basis_test <- function(df, value){
  if (value == 0){
    as.numeric(which(unlist(df) == max(unlist(df), na.rm = T)))
  } else if (value == 1){
    as.numeric(which(unlist(df) == max(unlist(df), na.rm = T))) + 1
  }
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

# Obsolete # Babylex (random list) #
# randrowsumB <- function(df){
#  # df[sample(nrow(df), test[i]),]$log %>% sum_log() %>% max_basis()
#   df[rseed,]$log %>% sum_log() %>% max_basis()
# }

####### Babylex-IRT (mirt-cat)
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

# Config for IRT
remove_dataID <- function(df){df %>% select(-c(data_id))}

filter_response <- function(df){Filter(var,df)}

# Fit data to IRT
fit_item_mirt <- function(df){mirt(df, 1, itemtype = "2PL", method = "EM", SE=TRUE)}

# Apply IRT to database
mirt_cat <- function(dfa,dfx,dfd){
  design <- list(min_items = pmin(ncol(
    with(data_irt_purr, data[age==dfa & sex==dfx])[[1]]), test[1]),
    max_items = pmin(ncol(
      with(data_irt_purr, data[age==dfa & sex==dfx])[[1]]), test[1]))
  mirtCAT(mo =  with(data_irt_purr, irt[age==dfa & sex==dfx])[[1]],
          local_pattern = dfd, criteria = "MI", method = "WLE",
          start_item = "MI", design = design, preCAT = CAT_mode[[2]])#, cl = cl)
}
####### END ####### Babylex-IRT (mirt-cat)

# Compute B for IRT or Base or SF
getrowsumB <- function(dfref,dftar){
  # if (test_mode == "IRT") {
  #   dftar[dfref[["items_answered"]] # Get item_answered from the results
  #         [1:pmin(length(dfref[["items_answered"]]), # Pick between test[i] and
  #                 test[i])],]$log %>%     # length of "items_ans.." for safety.
  #     sum_log() %>%
  #     max_basis()
  if (test_mode == "IRT") {
    dftar[dfref # Get item_answered from the results
          [1:pmin(length(dfref), # Pick between test[i] and
                  test[i])],]$log %>%     # length of "items_ans.." for safety.
       sum_log() %>%
       max_basis()
  } else if (test_mode == "Base" | test_mode == "SF" ) {
    dftar[dfref,]$log %>% # get seed or get SF
       sum_log() %>%
       max_basis()
  }
}

# Real-data simulations - Score estimation
get_newB <- function(df){
  (unlist(df[callcolm]) - df$B)/df$slope
}
get_reg <- function(df){ # final_purr$data
  if(type == "WS"){
  cor(unlist(df[callcolmB]),df$production)
  } else if(type == "WG"){
  cor(unlist(df[callcolmB]),df %>% select(all_of(resp)))
  }
}
get_stderr <- function(df){
  if(type == "WS"){
  mean(sd((unlist(df$production)/num_words) - (unlist(df[callcolmNewB])/num_words)))
  } else if(type == "WG"){
    mean(sd((unlist(df %>% select(all_of(resp)))/num_words) - (unlist(df[callcolmNewB])/num_words)))
  }
}
get_rel <- function(df){
  1-mean(df^2)
}

######### Obsolete
# # Analyses Branch:
# analyse <- function(group,...){
#   final_purr <<- newB_purr %>%
#     ungroup %>% select(-c(B,slope,...)) %>% group_by_at(group) %>% nest()
#   # Get reg, stdev, rel
#   col <- ncol(final_purr)
#   ind <- 0
#   for(i in 1:length(test)){
#     print(i)
#     callcolmB <<- paste("B",test[i], sep= "_")
#     callcolmNewB <<- paste("newB",test[i], sep= "_")
#
#     colmreg <<- paste(test[i], "reg" ,sep= "_")
#     colmstderr <<- paste(test[i], "stderr" ,sep= "_")
#     colmrel <<- paste(test[i], "rel" ,sep= "_")
#
#     #CHECKCHECKCHECLCHECK
#     final_purr[colmreg] <<- unlist(map(final_purr$data, get_reg)) #cor(unlist(df[callcolmB]),df$production)
#     # final_purr[colmreg] <<- (final_purr %>% mutate(map(final_purr$data, get_reg)))[col+i+ind]
#     final_purr[colmstderr] <<- unlist(map(final_purr$data, get_stderr))
#     # final_purr[colmstderr] <<- (final_purr %>% mutate(map(final_purr$data, get_stderr)))[col+1+i+ind]
#     final_purr[colmrel] <<- as.matrix(unlist(map(final_purr$data, get_stderr))) %>%
#       apply(1,get_rel)
#     # final_purr[colmrel] <<- as.matrix(unlist((final_purr %>% mutate(map(final_purr$data, get_stderr)))[col+1+i+ind])) %>%
#       # apply(1,get_rel)
#
#     ind <- ind + 2
#   }
# }

# # In case of insufficient sample size per age.
compute_age <- function(){if(all(unique(final_df$age) %in% c(16,30))==TRUE){
  final_df %>% ungroup() %>% mutate(age = case_when(between(age,16,18)~"16 - 18",
                                                                    between(age,19,21)~"19 - 21",
                                                                    between(age,22,24)~"22 - 24",
                                                                    between(age,25,27)~"25 - 27",
                                                                    between(age,28,30)~"28 - 30"))
}else if(all(unique(final_df$age) %in% c(18,36))==TRUE){
  final_df %>% ungroup() %>% mutate(age = case_when(between(age,18,21)~"18 - 21",
                                                                    between(age,22,24)~"22 - 24",
                                                                    between(age,25,27)~"25 - 27",
                                                                    between(age,28,30)~"28 - 30",
                                                                    between(age,31,33)~"31 - 33",
                                                                    between(age,34,36)~"34 - 36"))
}else if(all(unique(final_df$age) %in% c(18:30))==TRUE){
  final_df %>% ungroup() %>% mutate(age = case_when(between(age,18,21)~"18 - 21",
                                                                    between(age,22,24)~"22 - 24",
                                                                    between(age,25,27)~"25 - 27",
                                                                    between(age,28,30)~"28 - 30"))
}else if(all(unique(final_df$age) %in% c(16,36))==TRUE){
  final_df %>% ungroup() %>% mutate(age = case_when(between(age,16,18)~"16 - 18",
                                                                    between(age,19,21)~"19 - 21",
                                                                    between(age,22,24)~"22 - 24",
                                                                    between(age,25,27)~"25 - 27",
                                                                    between(age,28,30)~"28 - 30",
                                                                    between(age,31,33)~"31 - 33",
                                                                    between(age,34,36)~"34 - 36"))} else {print("Age needs attention")}
}
gen_age_grp <- function(df)
  if(all(unique(df$age) %in% c(16:30))==TRUE){
    df %>% mutate(age = case_when(between(age,16,18)~1618,
                                  between(age,19,21)~1921,
                                  between(age,22,24)~2224,
                                  between(age,25,27)~2527,
                                  between(age,28,30)~2830))
  }else if(all(unique(df$age) %in% c(18:36))==TRUE){
    df %>%  mutate(age = case_when(between(age,18,21)~1821,
                                   between(age,22,24)~2224,
                                   between(age,25,27)~2527,
                                   between(age,28,30)~2830,
                                   between(age,31,33)~3133,
                                   between(age,34,36)~3436))
  }else if(all(unique(df$age) %in% c(16:36))==TRUE){
    df %>% mutate(age = case_when(between(age,16,18)~1618,
                                  between(age,19,21)~1921,
                                  between(age,22,24)~2224,
                                  between(age,25,27)~2527,
                                  between(age,28,30)~2830,
                                  between(age,31,33)~3133,
                                  between(age,34,36)~3436))
  }else if(all(unique(df$age) %in% c(12:36))==TRUE){
    df %>% mutate(age = case_when(between(age,12,15)~1215,
                                  between(age,16,18)~1618,
                                  between(age,19,21)~1921,
                                  between(age,22,24)~2224,
                                  between(age,25,27)~2527,
                                  between(age,28,30)~2830,
                                  between(age,31,33)~3133,#))} else {print("Age needs attention")}#,
                                  between(age,34,36)~3436))} else {print("Age needs attention")}

# }else if(all(unique(df$age) %in% c(12:36))==TRUE){
#   df %>% mutate(age = case_when(between(age,12,18)~"12 - 18",
#                                 between(age,19,21)~"19 - 21",
#                                 between(age,22,24)~"22 - 24",
#                                 between(age,25,27)~"25 - 27",
#                                 between(age,28,30)~"28 - 30",
#                                 between(age,31,36)~"31 - 36"))} else {print("Age needs attention")}#,
#                                 # between(age,34,36)~"34 - 36"))} else {print("Age needs attention")}

# bs <- function(...){temp <- iSpline(...)
# class(temp) <- c("bs", "basis", "matrix")
# temp}

bs <- function(...){temp <- mSpline(...)
class(temp) <- c("bs", "basis", "matrix")
attr(temp, "x") <- NULL
temp}

# bs <- function(...){temp <- bspline(..., ndx = 400)
# class(temp) <- c("bs", "basis", "matrix")
# attr(temp, "x") <- NULL
# temp}
