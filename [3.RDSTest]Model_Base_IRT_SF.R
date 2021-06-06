setwd("shortCDI_purrr_edition")

devtools::load_all(".")

# call.pkgs calls the requisite packages,
# or you can include more packages. print "pacs" for packages called.
call.pkgs()

# Clean environment
# rm(list=ls())

# Config
set.seed(49)

lang <- "Norwegian" #"Norwegian" #"Mandarin (Beijing)"#"English (American)" # #"Danish" #  "Italian" #
type <- "WS"
resp <- "production" # pick between comprehension or production
criteria <- "incl.anyFinite" #"excl.Inf" # or  — for evaluation of log = -inf
poly_mode <- "Flexi" #"Fixed" # Flexi" #"Flexi" # or
cat_cfg(mode = "NoPreCAT") # NoPreCAT

test_mode <-  "IRT" # Option 1) "Base" - Original Babylex Model or 2) "IRT" - Babylex-IRT
irt_mode <- "bywhole" #"bywhole" #"byagesex" #
model <- "babylex"
print(test_mode)
print(irt_mode)

if (type == "WS") {
  call.csv("admin_data_all")
  call.csv("np_param", poly_mode, criteria)
  call.csv("slope", poly_mode, criteria)
  call.rds("log_pdf_purr_raw", poly_mode, criteria)
} else if (type == "WG") {
  call.csv("admin_data_all", resp)
  call.csv("np_param", resp, poly_mode, criteria)
  call.csv("slope", resp, poly_mode, criteria)
  call.rds("log_pdf_purr_raw", resp, poly_mode, criteria)
}


num_words <- max(log_pdf_purr_raw$num_item_id)
default_test <- c(num_words, 400, 200, 100, 50, 25, 10, 5)
test <- c(num_words, default_test[default_test < num_words])

log_pdf_purr_raw <- log_pdf_purr_raw %>% modify_if(is.factor,as.character)
# Filter log dnorm pdf
log_pdf_purr <- log_pdf_purr_raw
log_pdf_purr$filt <- filter_log_pdf(log_pdf_purr_raw, criteria) # "excl.Inf" or
log_pdf_purr <- log_pdf_purr %>% filter(filt == 1)

np_param <- np_param %>% modify_if(is.factor,as.character)
slope <- slope %>% modify_if(is.factor,as.character)

# Prep data: map basis to item responses of each data_id
admin_data_basis <- admin_data_all %>% #filter(between(age, 16, 30)) %>%
  inner_join(log_pdf_purr,by=c("sex","age","value","num_item_id")) %>% select(-value) %>%
  inner_join(np_param %>% select(-value),by=c("age","sex")) %>%
  inner_join(slope,by=c("age","sex")) %>% group_by(data_id) %>%
  nest(c(num_item_id,log))

if (test_mode == "IRT") {
  # IRT parameters
  if (type == "WS") {
    call.rds("data_irt_purr", "irtMod")
  } else if (type == "WG"){
    call.rds("data_irt_purr", resp, "irtMod")
  }
  if (irt_mode == "byagesex"){
    if (type == "WS") {
      call.rds("select_item_irt_purr", "itemsCAT")
    } else if (type == "WG"){
      call.rds("select_item_irt_purr", resp, "itemsCAT")
    }
  } else if(irt_mode == "bywhole"){
    call.rds("rem_irt","itemsCAT")
    select_item_irt_purr <- rem_irt
  }
#id 129261 
  
  word_ext <- data_irt_purr %>%
    mutate(items_subset = map(data_irt_purr$data, names)) %>%
    select(-c(data,irt))

  admin_reven <- admin_data_all %>% nest(value,num_item_id) %>%
    inner_join(word_ext, by = c("age","sex"))

  items_answered <- select_item_irt_purr %>%
    mutate(result = map(select_item_irt_purr$result, function(x){x[["items_answered"]]})) %>%
    select(data_id, result)

  reven <- inner_join(admin_reven, items_answered, by="data_id") %>%
  # reven <- reven %>% mutate(result = pmap(list(reven$items_subset, reven$result),
  #                                         function(x, y){x[y]})) %>%
    select(-c(items_subset,data))

  test_B_purr <- admin_data_basis %>% select(-production) %>%
    inner_join(reven, by = c("data_id", "age", "sex"))

} else if (test_mode == "SF") {
  test_B_purr <- admin_data_basis

  sfname <- paste("CDI_SF_items",type,lang,sep="_")
  assign("item_sf", (read.csv(paste(sfname,"csv",sep="."))))
  item_sf <- item_sf[sample(nrow(item_sf),100),]

} else if (test_mode == "Base") {
  test_B_purr <- admin_data_basis
}

# ## Norwegian babylex tablet-based SF
# uni_lemma <- c("lion","candy","moose","scissors","tiger","goose","dog","owl","cow","elephant","car","airplane","horse",
#                "giraffe","cat","pasta","shorts","banana","pen/pencil","apple","train","truck","penguin","zipper")
# norsksf <- right_join(comp, cbind(uni_lemma) %>% data.frame,by="uni_lemma") %>% filter(definition != "spagetti")
#
# test_B_purr <- admin_data_basis
#
# item_sf <- norsksf

# Estimate scores with words selected by IRT, SF or Base Model (Random)
if (test_mode %in% c("IRT","SF")) {
  for(i in 1:length(test)){
    print(paste("test",i,sep="_"))
    colm <- paste("B",test[i], sep= "_")
    
    if (test_mode == "IRT") {
      test_B_purr[[colm]] <- pmap(list(test_B_purr$result,test_B_purr$data),
                                  getrowsumB) %>% unlist()  # Get items from IRT, sum and max basis
    } else if (test_mode == "SF") {
      test_B_purr[[colm]] <- pmap(list(list(item_sf$num_item_id),test_B_purr$data),
                                  getrowsumB) %>% unlist() # Get items from rseed, sum and max basis
    }
  }

  newB_purr <- test_B_purr
  # Get newB for each item lengths
  for(i in 1:length(test)){
    print(i)
    callcolm <- paste("B",test[i], sep= "_")
    colm <- paste("newB", test[i], sep= "_")
    newB_purr[colm] <- get_newB(newB_purr)
  }

} else if (test_mode == "Base") {

  redo <- 10
  for (n in 1:redo) {
    set.seed(49+n)
    print(paste("redo",n,"@B",sep="_"))
    for(i in 1:length(test)){
      print(paste("test",i,sep="_"))
      colm <- paste("B",test[i],n, sep= "_")
      # test_B_purr[[colm]] <- pmap(list(list(sample(num_words, test[i])),test_B_purr$data),
      test_B_purr[[colm]] <- pmap(list(list(sample(num_words, test[i])),test_B_purr$data),
                                  getrowsumB) %>% unlist()  # Get randomly sampled items, sum and max basis
    }
  }

  newB_purr <- test_B_purr

  for (n in 1:redo) {
    print(paste("redo",n,"@newB",sep="_"))
    # Get newB for each item lengths
    for(i in 1:length(test)){
      print(i)
      callcolm <- paste("B",test[i],n, sep= "_")
      colm <- paste("newB", test[i],n, sep= "_")
      newB_purr[[colm]] <- get_newB(newB_purr)
    }
  }
}

# analyses branch by gender (can be easily modified for by age group):
if((type == "WG" & resp == "production") | type == "WS"){
  if(test_mode == "Base"){
    final_df <- newB_purr %>% select(-c(data,data_id,filt,B,slope)) %>% filter(!is.na(sex))
    final_df <- final_df %>% gather("key","scores",names(final_df[,4:ncol(final_df)])) %>%
      separate("key", c("type","test","redo"),sep="_") %>% #group_by(type,test,redo) %>%
      #mutate(scores = (scores - min(scores))/(max(scores)- min(scores))*num_words) %>%
      filter(type=="newB")
  } else if(test_mode == "IRT"){
    final_df <- newB_purr %>% ungroup() %>% select(-c(data,data_id,filt,B,slope,result)) %>% filter(!is.na(sex))
    final_df <- final_df %>% gather("key","scores",names(.[,4:ncol(.)])) %>%
      separate("key", c("type","test"),sep="_") %>%# group_by(type,test) %>%
      #mutate(scores = (scores - min(scores))/(max(scores)- min(scores))*num_words) %>%
      filter(type=="newB")
  } else if(test_mode == "SF"){
    final_df <- newB_purr %>% select(-c(data,data_id,filt,B,slope)) %>% filter(!is.na(sex))
    final_df <- final_df %>% gather("key","scores",names(final_df[,4:ncol(final_df)])) %>%
      separate("key", c("type","test"),sep="_")
  }

} else if (type == "WG" & resp == "comprehension"){

  if(test_mode == "Base"){
    final_df <- newB_purr %>% select(-c(data,data_id,filt,B,slope)) %>% filter(!is.na(sex))
    final_df <- final_df %>% gather("key","scores",names(final_df[,4:ncol(final_df)])) %>%
      separate("key", c("type","test","redo"),sep="_") %>% #group_by(type,test,redo) %>%
      #mutate(scores = (scores - min(scores))/(max(scores)- min(scores))*num_words) %>%
      filter(type=="newB")
  } else if(test_mode == "IRT"){
    final_df <- newB_purr %>% select(-c(data,data_id,filt,B,slope,result)) %>% filter(!is.na(sex))
    final_df <- final_df %>% gather("key","scores",names(.[,4:ncol(.)])) %>%
      separate("key", c("type","test"),sep="_") %>%# group_by(type,test) %>%
      #mutate(scores = (scores - min(scores))/(max(scores)- min(scores))*num_words) %>%
      filter(type=="newB")
  } else if(test_mode == "SF"){
    final_df <- newB_purr %>% select(-c(data,data_id,filt,B,slope)) %>% filter(!is.na(sex))
    final_df <- final_df %>% gather("key","scores",names(final_df[,4:ncol(final_df)])) %>%
      separate("key", c("type","test"),sep="_")
  }
}
# # Generate tables
# final_data <- df %>%
#   gather(colnames(df[(ncol(df)-length(test)*3+1):ncol(df)]), key= "test", value = "value") %>%
#   separate(test,c("test","type")) %>% spread(type,value)
# final_data$test <- as.numeric(final_data$test)
# final_data <- final_data[order(-final_data$test),]

## Tweaking computation # now by sex and not by age
if((type == "WG" & resp == "production") | type == "WS"){
  if(test_mode == "Base"){
    final_df <- final_df %>% mutate(scores = ifelse(scores == "NaN", 0, scores)) %>%
      group_by(sex,test,redo,type) %>%
      summarise(cor = cor(production,scores),
                sd = sd(production/num_words - scores/num_words),
                rel = 1-mean(sd(production/num_words - scores/num_words)^2),
                std_err = mean(sd)) %>%
      ungroup() %>% select(-redo) %>% na.omit() %>% group_by(sex, test, type) %>%
      summarise_all(list(mean))
  } else if(test_mode %in% c("SF", "IRT")){
    final_df <- final_df %>% mutate(scores = ifelse(scores == "NaN", 0, scores)) %>%
      group_by(sex,test,type) %>%
      summarise(cor = cor(production,scores),
                sd = sd(production/num_words - scores/num_words),
                rel = 1-mean(sd(production/num_words - scores/num_words)^2),
                std_err = mean(sd)) %>%
      ungroup()
  }
} else if (type == "WG" & resp == "comprehension"){
  if(test_mode == "Base"){
    final_df <- final_df %>% group_by(sex,test,redo,type) %>%
      summarise(cor = cor(comprehension,scores),
                sd = sd(comprehension/num_words - scores/num_words),
                rel = 1-mean(sd(comprehension/num_words - scores/num_words)^2),
                std_err = mean(sd)) %>%
      ungroup() %>% select(-redo) %>% na.omit() %>% group_by(sex, test, type) %>%
      summarise_all(list(mean))
  } else if(test_mode %in% c("SF", "IRT")){
    final_df <- final_df %>% group_by(sex,test,type) %>%
      summarise(cor = cor(comprehension,scores),
                sd = sd(comprehension/num_words - scores/num_words),
                rel = 1-mean(sd(comprehension/num_words - scores/num_words)^2),
                std_err = mean(sd)) %>%
      ungroup()
  }
}

######## For reporting ########
byoverall <- final_df %>% filter(type == "newB") %>%
  group_by(test) %>% summarise_at(vars("cor","std_err","rel"),mean)
# visualise
ggplot(byoverall, aes(y = cor, x = factor(test, levels = c(5, 10, 25, 50, 100, 200, 400, num_words)))) +
  geom_point() + jtools::theme_apa() #+ ylim(0.7,1)

bysex <- final_df %>% filter(type == "newB") %>%
  group_by(sex,test) %>% summarise_at(vars("cor","std_err","rel"),mean)
# visualise
ggplot(bysex, aes(y = cor, x = factor(test, levels = c(5, 10, 25, 50, 100, 200, 400, num_words)))) +
  geom_point() + facet_wrap(~sex) + jtools::theme_apa() #+ ylim(0.7,1)

dfbabylexbase <- bysex

ggplot(rbind((bysexbase %>% mutate(type = "base")), (bysexirt %>% mutate(type = "irt"))), aes(y = cor, col = type,x = factor(test, levels = c(5, 10, 25, 50, 100, 200, 400, num_words)))) +
  geom_point() + facet_wrap(~sex) + jtools::theme_apa() + ylim(0.7,1) + geom_hline(yintercept = .95)

ggplot(rbind((bysexbase %>% mutate(type = "base")), 
             (bysexirt %>% mutate(type = "irt"))) %>% 
         ungroup() %>% group_by(type,test) %>% 
         summarise(cor = mean(cor), 
                   std_err = mean(std_err), 
                   rel = mean(rel)), aes(y = cor, col = type,x = factor(test, levels = c(5, 10, 25, 50, 100, 200, 400, num_words)))) +
  geom_point() + jtools::theme_apa() + ylim(0.7,1) + geom_hline(yintercept = .95)

dfbabylexbase %<>%
  mutate(mode = paste("Babylex",sep="-")) %>%
  mutate(language = lang) #%>% select(-c(model,type))


bysex_lang <- rbind(dfbabylexbase %>% data.frame(),
                    dfbabylexIRT %>% data.frame(),
                    byoverall %>% data.frame())

View(colSums(cbind(obj$cor-bysex_1$cor%>%data.frame(),obj$cor-bysex_2$cor%>%data.frame(),obj$cor-bysex_3$cor%>%data.frame(),obj$cor-bysex_4$cor%>%data.frame(),obj$cor-bysex_5$cor%>%data.frame())))

byage <- if(all(unique(final_df$age) %in% c(16:30))==TRUE){
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
byage <- byage %>% group_by(age,test,sex) %>%
  summarise_at(vars("cor","std_err","rel"),mean)


print(paste(lang, type, model, test_mode, sep=" - "))

fname_ag <- paste("byage", lang, model, test_mode, sep="_")
fname_sx <- paste("bysex", lang, model, test_mode, sep="_")
assign(fname_ag, byage %>%
         mutate(language = lang) %>%
         mutate(model = model) %>%
         mutate(type = test_mode))
assign(fname_sx, bysex %>%
         mutate(language = lang) %>%
         mutate(model = model) %>%
         mutate(type = test_mode))


# ggplot(data = eng_df %>%
#          filter(!test %in% c(num_words,"400","200") & type != "IRT-ReverseEngineered"),
#        aes(y=cor,x=test,group = interaction(type,sex),col=sex,shape=type)) +
#   geom_point(size = 2.5) + geom_line(linetype=2, size = .5) + scale_color_manual(values = c("red","blue"))+
#   xlab("Length") + ylab("Correlation coefficient") +
#   ylim(.75,1) + jtools::theme_apa() +geom_hline(yintercept=.95,linetype = 2,col="gray")

### benchmarking IRT Norwegian CDI-WG Production
set.seed(131)
seed_data_id <- newB_purr %>% group_by(age,sex) %>% sample_n(3) %>% select(data_id)
set.seed(131)
final_df <- newB_purr %>% group_by(age,sex) %>% sample_n(3) %>% select(-c(data,data_id,filt,B,slope)) %>% filter(!is.na(sex))
final_df <- final_df %>% gather("key","scores",names(final_df[,4:ncol(final_df)])) %>%
  separate("key", c("type","test","redo"),sep="_") %>% #group_by(type,test,redo) %>%
  #mutate(scores = (scores - min(scores))/(max(scores)- min(scores))*num_words) %>%
  filter(type=="newB")
final_df <- final_df %>% group_by(age,sex,test,redo,type) %>%
  summarise(cor = cor(production,scores),
            sd = sd(production/num_words - scores/num_words),
            rel = 1-mean(sd(production/num_words - scores/num_words)^2),
            std_err = mean(sd)) %>%
  ungroup() %>% select(-redo) %>% na.omit() %>% group_by(age, sex, test, type) %>%
  summarise_all(list(mean))

######## For reporting ########
bysex_nor_base_prod <- final_df %>% filter(type == "newB") %>%
  group_by(sex,test) %>% summarise_at(vars("cor","std_err","rel"),mean)
# visualise
gbysex_nor_base_prod  <- ggplot(bysex_nor_base_prod, aes(y = cor, x = factor(test, levels = c(num_words, 200, 100, 50, 25, 10, 5)))) +
  geom_point() + facet_wrap(~sex) + jtools::theme_apa()
gbysex_nor_base_prod

ggplot(final_df, aes( y = production-scores,
                      x = factor(test, levels = c("395", "200",
                                                  "100", "50",
                                                  "25", "10",
                                                  "5")))) +geom_point()
