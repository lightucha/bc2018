
#####################################################################################
### Project     : Big Contest 2018
### Script      : Prediction_2Step.R
### Description : Step1 - Retained / Churn Prediction 
###               Step2 - Week / Month / 2Month Prediction
#####################################################################################

#####################################################################################
### Setting up Environment
#####################################################################################

# Load libraries
pkgs <- c("data.table", "dplyr",  "tidyr", "caret", "randomForest", "xgboost", "ggplot2")
sapply(pkgs, require, character.only = T)

# Load data
activity <- fread("data/futureleague/original/train_activity.csv", stringsAsFactors = F)
guild    <- fread("data/futureleague/reform/train_guild_new.csv", stringsAsFactors = F)
label    <- fread("data/futureleague/reform/train_label.csv", stringsAsFactors = F)
payment  <- fread("data/futureleague/original/train_payment.csv", stringsAsFactors = F)
trade    <- fread("data/futureleague/original/train_trade.csv", stringsAsFactors = F)

# Set memory limit
memory.limit(80000)


#####################################################################################
### Preprocessing
#####################################################################################

# Create payment variables
pay <- payment %>% 
  mutate(payment_amount_new = (payment_amount - min(payment_amount))/(max(payment_amount) - min(payment_amount))) %>%
  group_by(acc_id) %>% summarize(sum_payment = sum(payment_amount))
head(pay)


# Create guild varibles
guild_join <- guild %>% group_by(acc_id) %>% 
  summarize(join_num = n())
head(guild_join)


# Create time variables
time <- activity %>% 
  mutate(play_time_new = (play_time - min(play_time))/(max(play_time) - min(play_time))) %>%
  group_by(acc_id) %>%
  summarize(wk_num = n(),
            acc_num = sum(cnt_dt),
            acc_mean = mean(cnt_dt),
            time_sum = sum(play_time_new),
            time_wk_mean = mean(play_time_new)) %>%
  mutate(time_acc_mean = time_sum/acc_num) %>%
  select(acc_id, wk_num, acc_num, time_sum)
head(time)


# Create exp variables
exp <- activity %>% select(acc_id, npc_exp, npc_hongmun, quest_exp, quest_hongmun, item_hongmun) %>%
  mutate(npc_exp_new = (npc_exp - min(npc_exp))/(max(npc_exp) - min(npc_exp)),
         npc_hongmun_new = (npc_hongmun - min(npc_hongmun))/(max(npc_hongmun) - min(npc_hongmun)),
         quest_exp_new = (quest_exp - min(quest_exp))/(max(quest_exp) - min(quest_exp)),
         quest_hongmun_new = (quest_hongmun - min(quest_hongmun))/(max(quest_hongmun) - min(quest_hongmun)),
         item_hongmun_new = (item_hongmun - min(item_hongmun))/(max(item_hongmun) - min(item_hongmun))) %>%
  group_by(acc_id) %>%
  summarize(npc_exp_sum = sum(npc_exp_new),
            npc_hongmun_sum = sum(npc_hongmun_new),
            quest_exp_sum = sum(quest_exp_new),
            quest_hongmun_sum = sum(quest_hongmun_new),
            item_hongmun_sum = sum(item_hongmun_new))
head(exp)


# Create battle variables
battle_temp <- apply(activity %>% select(duel_cnt, duel_win, partybattle_cnt, partybattle_win), 2,
                     function(x) {(x - min(x))/(max(x) - min(x))}) %>% as.data.frame() %>%
  cbind(activity[,c(1,2,3)]) %>% select(acc_id, wk, cnt_dt, everything())

battle <- battle_temp %>% group_by(acc_id) %>%
  summarize(duel_cnt_sum = sum(duel_cnt),
            duel_win_sum = sum(duel_win),
            partybattle_cnt_sum = sum(partybattle_cnt),
            partybattle_win_sum = sum(partybattle_win))
head(battle)


# Create dungeon variables
dungeon_temp <- apply(activity[,c(16:29)], 2, function(x) { (x - min(x))/(max(x) - min(x)) }) %>% 
  as.data.frame() %>% cbind(activity[,c(1,2,3)]) %>% select(acc_id, wk, cnt_dt, everything())

dungeon <- dungeon_temp %>%
  mutate(enter_inzone = cnt_enter_inzone_solo + cnt_enter_inzone_normal,
         enter_skilled = cnt_enter_inzone_skilled,
         enter_raid = cnt_enter_raid + cnt_enter_raid_light,
         enter_bam = cnt_enter_bam,
         clear_inzone = cnt_clear_inzone_solo + cnt_clear_inzone_normal,
         clear_skilled = cnt_clear_inzone_skilled,
         clear_raid = cnt_clear_raid + cnt_clear_raid_light,
         clear_bam = cnt_clear_bam) %>%
  group_by(acc_id) %>%
  summarize(enter_inzone_sum = sum(enter_inzone),
            enter_skilled_sum = sum(enter_skilled),
            enter_raid_sum = sum(enter_raid),
            enter_bam_sum = sum(enter_bam),
            clear_inzone_sum = sum(clear_inzone),
            clear_skilled_sum = sum(clear_skilled),
            clear_raid_sum = sum(clear_raid),
            clear_bam_sum = sum(clear_bam))
head(dungeon)


# Create chat variables
chat <- activity %>% select(acc_id, normal_chat, whisper_chat, district_chat, party_chat, guild_chat, faction_chat) %>%
  mutate(normal_chat_new = (normal_chat - min(normal_chat))/(max(normal_chat) - min(normal_chat)),
         whisper_chat_new = (whisper_chat - min(whisper_chat))/(max(whisper_chat) - min(whisper_chat)),
         district_chat_new = (district_chat - min(district_chat))/(max(district_chat) - min(district_chat)),
         party_chat_new = (party_chat - min(party_chat))/(max(party_chat) - min(party_chat)),
         guild_chat_new = (guild_chat - min(guild_chat))/(max(guild_chat) - min(guild_chat)),
         faction_chat_new = (faction_chat - min(faction_chat))/(max(faction_chat) - min(faction_chat))) %>%
  group_by(acc_id) %>%
  summarize(normal_chat_sum = sum(normal_chat_new),
            whisper_chat_sum = sum(whisper_chat_new),
            district_chat_sum = sum(district_chat_new),
            party_chat_sum = sum(party_chat_new),
            guild_chat_sum = sum(guild_chat_new),
            faction_chat_sum = sum(faction_chat_new)) 
head(chat)


# Create etc variables
etc <- activity %>% select(acc_id, cnt_use_buffitem, gathering_cnt, making_cnt, game_combat_time, get_money) %>%
  mutate(cnt_use_buffitem_new = (cnt_use_buffitem - min(cnt_use_buffitem))/(max(cnt_use_buffitem) - min(cnt_use_buffitem)),
         gathering_cnt_new = (gathering_cnt - min(gathering_cnt))/(max(gathering_cnt) - min(gathering_cnt)),
         making_cnt_new = (making_cnt - min(making_cnt))/(max(making_cnt) - min(making_cnt)),
         game_combat_time_new = (game_combat_time - min(game_combat_time))/(max(game_combat_time) - min(game_combat_time)),
         get_money_new = (get_money - min(get_money))/(max(get_money) - min(get_money))) %>%
  group_by(acc_id) %>%
  summarize(cnt_use_buffitem_sum = sum(cnt_use_buffitem_new),
            gathering_cnt_sum = sum(gathering_cnt_new),
            making_cnt_sum = sum(making_cnt_new),
            game_combat_time_sum = sum(game_combat_time_new),
            get_money_sum = sum(get_money_new))
head(etc)


# Create trade variables
trade_new <- trade %>%
  gather(key = "role", value = "acc_id", source_acc_id, target_acc_id) %>%
  mutate(role = ifelse(role == "source_acc_id", "sell", "buy"),
         item_amount_new = (item_amount - min(item_amount))/(max(item_amount) - min(item_amount)))

trade_num <- trade_new %>% group_by(acc_id, role, item_type) %>%
  summarize(n_obs = n()) %>% ungroup() %>%
  mutate(type = paste0(role, "_", item_type)) %>%
  select(-role, -item_type) %>%
  spread(key = type, value = n_obs)
trade_num[is.na(trade_num)] <- 0
head(trade_num)


#####################################################################################
### Clustering
#####################################################################################

# Merge data for clustering
game_temp <- label %>%
  left_join(pay, by = "acc_id") %>%
  left_join(guild_join, by = "acc_id") %>%
  left_join(time, by = "acc_id") %>%
  left_join(exp, by = "acc_id") %>%
  left_join(battle, by = "acc_id") %>%
  left_join(dungeon, by = "acc_id") %>%
  left_join(chat, by = "acc_id") %>%
  left_join(etc, by = "acc_id") %>%
  left_join(trade_num, by = "acc_id")
game_temp[is.na(game_temp)] <- 0

game_cluster <- apply(game_temp[,3:47], 2, function(x) {scale(x)}) %>% data.frame()


# Make Cluster
set.seed(9866)
k <- 6
kmeans_result <- kmeans(game_cluster, centers = k)


#####################################################################################
### Data Merge
#####################################################################################

# Merge it!
game <- label %>%
  left_join(pay, by = "acc_id") %>%
  left_join(guild_join, by = "acc_id") %>%
  left_join(time, by = "acc_id") %>%
  left_join(exp, by = "acc_id") %>%
  left_join(dungeon, by = "acc_id") %>%
  left_join(chat, by = "acc_id") %>%
  left_join(etc, by = "acc_id") %>%
  left_join(trade_num, by = "acc_id") %>%
  mutate(cluster = kmeans_result$cluster %>% factor())
game[is.na(game)] <- 0
summary(game)


#####################################################################################
### Data Split
#####################################################################################

# Split data
set.seed(1990)
train_id <- createDataPartition(game$label, p = 0.75, list = F)
train    <- game[train_id,]
test     <- game[-train_id,]

result <- test %>% select(acc_id, label)


#####################################################################################
### Step 1 - Random Forest
#####################################################################################

# Preprocessing
train_step1 <- train %>%
  mutate(label = ifelse(label == "retained", "retained", "churn") %>% factor())
test_step1 <- test %>% 
  mutate(label = ifelse(label == "retained", "retained", "churn") %>% factor())


# Select variables
formula1 <- formula(label ~ . - acc_id)
formula2 <- formula(label ~ item_hongmun_sum + wk_num + acc_num + time_sum + clear_raid_sum + 
                      guild_chat_sum + enter_raid_sum + quest_hongmun_sum + buy_money + 
                      get_money_sum + party_chat_sum + game_combat_time_sum + cluster + 
                      npc_hongmun_sum + whisper_chat_sum + sell_money + join_num + 
                      cnt_use_buffitem_sum + enter_inzone_sum + clear_inzone_sum + sell_grocery +
                      sum_payment + npc_exp_sum + district_chat_sum + quest_exp_sum + 
                      making_cnt_sum + normal_chat_sum + enter_skilled_sum + buy_grocery + clear_skilled_sum)


# Train randomforest model
rf_step1.fit1 <- randomForest(formula1, data = train_step1)
rf_step1.fit2 <- randomForest(formula2, data = train_step1)


# Predict
rf_step1.pred1 <- predict(rf_step1.fit1, test)
rf_step1.pred2 <- predict(rf_step1.fit2, test)


# Result
confusionMatrix(rf_step1.pred1, test_step1$label, mode = "everything")
confusionMatrix(rf_step1.pred2, test_step1$label, mode = "everything")


# Save Result
result$step1 <- rf_step1.pred2


# Check variables importance
varImp(rf_step1.fit1) %>% mutate(Variable = row.names(.)) %>%
  arrange(desc(Overall)) %>% select(Variable, Overall)
varImpPlot(rf_step1.fit1)

varImp(rf_step1.fit2) %>% mutate(Variable = row.names(.)) %>%
  arrange(desc(Overall)) %>% select(Variable, Overall)
varImpPlot(rf_step1.fit2)



# 10-fold Cross-Validation
rf.ctrl <- trainControl(method = "repeatedcv",
                        repeats = 5,
                        summaryFunction = twoClassSummary,
                        classProb = T,
                        savePredictions = T)
rf.grid <- expand.grid()

rf.tune1 <- train(x = train %>% select(-label),
                  y = train$label,
                  method = "rf",
                  metric = "ROC",
                  trControl = rf.ctrl)

rf.tune2 <- train(x = train %>% 
                    select(item_hongmun_sum, wk_num, acc_num, time_sum, clear_raid_sum, 
                           guild_chat_sum, enter_raid_sum, quest_hongmun_sum, buy_money, 
                           get_money_sum, party_chat_sum, game_combat_time_sum, cluster1,
                           cluster2, cluster3, cluster4, cluster5, cluster6,
                           npc_hongmun_sum, whisper_chat_sum, sell_money, join_num, 
                           cnt_use_buffitem_sum, enter_inzone_sum, clear_inzone_sum, sell_grocery,
                           sum_payment, npc_exp_sum, district_chat_sum, quest_exp_sum, 
                           making_cnt_sum, normal_chat_sum, enter_skilled_sum, buy_grocery, clear_skilled_sum),
                  y = train$label,
                  method = "rf",
                  metric = "ROC",
                  trControl = rf.ctrl)


#####################################################################################
### Step 2 - Random Forest
#####################################################################################

# Preprocessing
train_step2 <- train %>%
  filter(label != "retained") %>%
  mutate(label = factor(label))

test_step2 <- test %>% select(-label) %>%
  inner_join(result %>% filter(step1 == "churn") %>% select(acc_id, label), by = "acc_id")


# Select variables
formula3 <- formula(label ~ game_combat_time_sum + npc_exp_sum + time_sum + get_money_sum +
                      acc_num + cnt_use_buffitem_sum + quest_exp_sum + npc_hongmun_sum + 
                      quest_hongmun_sum + enter_inzone_sum + sell_money + clear_inzone_sum +
                      whisper_chat_sum + item_hongmun_sum + sell_grocery + party_chat_sum +
                      wk_num + normal_chat_sum + making_cnt_sum + buy_grocery + cluster +
                      guild_chat_sum + join_num + buy_money + enter_raid_sum + sum_payment +
                      clear_raid_sum + district_chat_sum)


# Train randomforest model
rf_step2.fit1 <- randomForest(formula1, data = train_step2)
rf_step2.fit2 <- randomForest(formula3, data = train_step2)


# Predict
rf_step2.pred1 <- predict(rf_step2.fit1, test_step2)
rf_step2.pred2 <- predict(rf_step2.fit2, test_step2)


# Save result
result2 <- result %>% 
  left_join(test_step2 %>% 
              mutate(step2_all = rf_step2.pred1) %>% 
              select(acc_id, step2_all),  by = "acc_id") %>% 
  left_join(test_step2 %>% 
              mutate(step2_reduced = rf_step2.pred2) %>% 
              select(acc_id, step2_reduced),  by = "acc_id") %>%
  mutate(step2_all = as.character(step2_all),
         step2_reduced = as.character(step2_reduced),
         label = factor(label))
result2[is.na(result2)] <- "retained"
result2$step2_all <- factor(result2$step2_all)
result2$step2_reduced <- factor(result2$step2_reduced)


# Result
confusionMatrix(result2$step2_all, result2$label, mode = "everything")
confusionMatrix(result2$step2_all, result2$label, mode = "everything")$byClass[,"F1"] %>% mean()

confusionMatrix(result2$step2_reduced, result2$label, mode = "everything")
confusionMatrix(result2$step2_reduced, result2$label, mode = "everything")$byClass[,"F1"] %>% mean()


# Check variables importance
varImp(rf_step2.fit1) %>% mutate(Variable = row.names(.)) %>%
  arrange(desc(Overall)) %>% select(Variable, Overall)
varImpPlot(rf_step2.fit1)

varImp(rf_step2.fit2) %>% mutate(Variable = row.names(.)) %>%
  arrange(desc(Overall)) %>% select(Variable, Overall)
varImpPlot(rf_step2.fit2)


#####################################################################################
### XGBoost - Resampling / 2Step / Grid Search
#####################################################################################

# Preprocessing
train_dummy <- model.matrix(~ cluster -1, train)
test_dummy  <- model.matrix(~ cluster -1, test)

train_xgb <- train %>% mutate(label = ifelse(label != "retained", 1, 0)) %>%
  select(-cluster, -acc_id) %>% bind_cols(as.data.frame(train_dummy))
test_xgb  <- test %>% mutate(label = ifelse(label != "retained", 1, 0)) %>%
  select(-cluster, -acc_id) %>% bind_cols(as.data.frame(test_dummy))


# Train xgboost model
xgb.fit1 <- xgboost(data = train_xgb %>% select(-label) %>% as.matrix(), label = train_xgb$label,
                    nrounds = 2,
                    objective = "binary:logistic")


xgb.fit2 <- xgboost(data = train_xgb %>% 
                      select(item_hongmun_sum, wk_num, acc_num, time_sum, clear_raid_sum, 
                             guild_chat_sum, enter_raid_sum, quest_hongmun_sum, buy_money, 
                             get_money_sum, party_chat_sum, game_combat_time_sum, cluster1,
                             cluster2, cluster3, cluster4, cluster5, cluster6,
                             npc_hongmun_sum, whisper_chat_sum, sell_money, join_num, 
                             cnt_use_buffitem_sum, enter_inzone_sum, clear_inzone_sum, sell_grocery,
                             sum_payment, npc_exp_sum, district_chat_sum, quest_exp_sum, 
                             making_cnt_sum, normal_chat_sum, enter_skilled_sum, buy_grocery, clear_skilled_sum) %>% as.matrix(), 
                    label = train_xgb$label,
                    nrounds = 2,
                    objective = "binary:logistic")


# Predict
xgb.proc1 <- predict(xgb.fit1, test_xgb %>% select(-label) %>% as.matrix())
xgb.pred1 <- ifelse(xgb.proc1 > 0.5, "churn", "retained")

xgb.proc2 <- predict(xgb.fit2, test_xgb %>% select(item_hongmun_sum, wk_num, acc_num, time_sum, clear_raid_sum, 
                                                   guild_chat_sum, enter_raid_sum, quest_hongmun_sum, buy_money, 
                                                   get_money_sum, party_chat_sum, game_combat_time_sum, cluster1,
                                                   cluster2, cluster3, cluster4, cluster5, cluster6,
                                                   npc_hongmun_sum, whisper_chat_sum, sell_money, join_num, 
                                                   cnt_use_buffitem_sum, enter_inzone_sum, clear_inzone_sum, sell_grocery,
                                                   sum_payment, npc_exp_sum, district_chat_sum, quest_exp_sum, 
                                                   making_cnt_sum, normal_chat_sum, enter_skilled_sum, buy_grocery, clear_skilled_sum) %>% as.matrix())
xgb.pred2 <- ifelse(xgb.proc2 > 0.5, "churn", "retained")

# Result
confusionMatrix(factor(xgb.pred1), 
                (test_xgb %>%
                   mutate(label = ifelse(label == 1, "churn", "retained") %>% factor()))$label, 
                mode = "everything")
confusionMatrix(factor(xgb.pred2), 
                (test_xgb %>%
                   mutate(label = ifelse(label == 1, "churn", "retained") %>% factor()))$label, 
                mode = "everything")

data.frame(all = xgb.proc1,
           reduced = xgb.proc2) %>% 
  gather(key = "type", value = "pr") %>% 
  ggplot(aes(type, pr, fill = factor(type))) + geom_boxplot() +
  scale_fill_manual(name = "type",
                    values = c("darkorange", "steelblue"))


#####################################################################################
### Submit
#####################################################################################

# Load data
test_activity <- fread("data/futureleague/final/test_activity.csv", stringsAsFactors = F)
test_guild    <- fread("data/futureleague/final/test_guild_new.csv", stringsAsFactors = F)
test_label    <- fread("data/futureleague/final/test_label.csv", stringsAsFactors = F)
test_payment  <- fread("data/futureleague/final/test_payment.csv", stringsAsFactors = F)
test_trade    <- fread("data/futureleague/final/test_trade.csv", stringsAsFactors = F)


# Create payment variables
test_pay <- test_payment %>% 
  mutate(payment_amount_new = (payment_amount - min(payment_amount))/(max(payment_amount) - min(payment_amount))) %>%
  group_by(acc_id) %>% summarize(sum_payment = sum(payment_amount))
head(test_pay)


# Create guild varibles
test_guild_join <- test_guild %>% group_by(acc_id) %>% 
  summarize(join_num = n())
head(test_guild_join)


# Create time variables
test_time <- test_activity %>% 
  mutate(play_time_new = (play_time - min(play_time))/(max(play_time) - min(play_time))) %>%
  group_by(acc_id) %>%
  summarize(wk_num = n(),
            acc_num = sum(cnt_dt),
            acc_mean = mean(cnt_dt),
            time_sum = sum(play_time_new),
            time_wk_mean = mean(play_time_new)) %>%
  mutate(time_acc_mean = time_sum/acc_num) %>%
  select(acc_id, wk_num, acc_num, time_sum)
head(test_time)


# Create exp variables
test_exp <- test_activity %>% select(acc_id, npc_exp, npc_hongmun, quest_exp, quest_hongmun, item_hongmun) %>%
  mutate(npc_exp_new = (npc_exp - min(npc_exp))/(max(npc_exp) - min(npc_exp)),
         npc_hongmun_new = (npc_hongmun - min(npc_hongmun))/(max(npc_hongmun) - min(npc_hongmun)),
         quest_exp_new = (quest_exp - min(quest_exp))/(max(quest_exp) - min(quest_exp)),
         quest_hongmun_new = (quest_hongmun - min(quest_hongmun))/(max(quest_hongmun) - min(quest_hongmun)),
         item_hongmun_new = (item_hongmun - min(item_hongmun))/(max(item_hongmun) - min(item_hongmun))) %>%
  group_by(acc_id) %>%
  summarize(npc_exp_sum = sum(npc_exp_new),
            npc_hongmun_sum = sum(npc_hongmun_new),
            quest_exp_sum = sum(quest_exp_new),
            quest_hongmun_sum = sum(quest_hongmun_new),
            item_hongmun_sum = sum(item_hongmun_new))
head(test_exp)


# Create battle variables
test_battle_temp <- apply(test_activity %>% select(duel_cnt, duel_win, partybattle_cnt, partybattle_win), 2,
                     function(x) {(x - min(x))/(max(x) - min(x))}) %>% as.data.frame() %>%
  cbind(test_activity[,c(1,2,3)]) %>% select(acc_id, wk, cnt_dt, everything())

test_battle <- test_battle_temp %>% group_by(acc_id) %>%
  summarize(duel_cnt_sum = sum(duel_cnt),
            duel_win_sum = sum(duel_win),
            partybattle_cnt_sum = sum(partybattle_cnt),
            partybattle_win_sum = sum(partybattle_win))
head(battle)


# Create dungeon variables
test_dungeon_temp <- apply(test_activity[,c(16:29)], 2, function(x) { (x - min(x))/(max(x) - min(x)) }) %>% 
  as.data.frame() %>% cbind(test_activity[,c(1,2,3)]) %>% select(acc_id, wk, cnt_dt, everything())

test_dungeon <- test_dungeon_temp %>%
  mutate(enter_inzone = cnt_enter_inzone_solo + cnt_enter_inzone_normal,
         enter_skilled = cnt_enter_inzone_skilled,
         enter_raid = cnt_enter_raid + cnt_enter_raid_light,
         enter_bam = cnt_enter_bam,
         clear_inzone = cnt_clear_inzone_solo + cnt_clear_inzone_normal,
         clear_skilled = cnt_clear_inzone_skilled,
         clear_raid = cnt_clear_raid + cnt_clear_raid_light,
         clear_bam = cnt_clear_bam) %>%
  group_by(acc_id) %>%
  summarize(enter_inzone_sum = sum(enter_inzone),
            enter_skilled_sum = sum(enter_skilled),
            enter_raid_sum = sum(enter_raid),
            enter_bam_sum = sum(enter_bam),
            clear_inzone_sum = sum(clear_inzone),
            clear_skilled_sum = sum(clear_skilled),
            clear_raid_sum = sum(clear_raid),
            clear_bam_sum = sum(clear_bam))
head(test_dungeon)


# Create chat variables
test_chat <- test_activity %>% select(acc_id, normal_chat, whisper_chat, district_chat, party_chat, guild_chat, faction_chat) %>%
  mutate(normal_chat_new = (normal_chat - min(normal_chat))/(max(normal_chat) - min(normal_chat)),
         whisper_chat_new = (whisper_chat - min(whisper_chat))/(max(whisper_chat) - min(whisper_chat)),
         district_chat_new = (district_chat - min(district_chat))/(max(district_chat) - min(district_chat)),
         party_chat_new = (party_chat - min(party_chat))/(max(party_chat) - min(party_chat)),
         guild_chat_new = (guild_chat - min(guild_chat))/(max(guild_chat) - min(guild_chat)),
         faction_chat_new = (faction_chat - min(faction_chat))/(max(faction_chat) - min(faction_chat))) %>%
  group_by(acc_id) %>%
  summarize(normal_chat_sum = sum(normal_chat_new),
            whisper_chat_sum = sum(whisper_chat_new),
            district_chat_sum = sum(district_chat_new),
            party_chat_sum = sum(party_chat_new),
            guild_chat_sum = sum(guild_chat_new),
            faction_chat_sum = sum(faction_chat_new)) 
head(test_chat)


# Create etc variables
test_etc <- test_activity %>% select(acc_id, cnt_use_buffitem, gathering_cnt, making_cnt, game_combat_time, get_money) %>%
  mutate(cnt_use_buffitem_new = (cnt_use_buffitem - min(cnt_use_buffitem))/(max(cnt_use_buffitem) - min(cnt_use_buffitem)),
         gathering_cnt_new = (gathering_cnt - min(gathering_cnt))/(max(gathering_cnt) - min(gathering_cnt)),
         making_cnt_new = (making_cnt - min(making_cnt))/(max(making_cnt) - min(making_cnt)),
         game_combat_time_new = (game_combat_time - min(game_combat_time))/(max(game_combat_time) - min(game_combat_time)),
         get_money_new = (get_money - min(get_money))/(max(get_money) - min(get_money))) %>%
  group_by(acc_id) %>%
  summarize(cnt_use_buffitem_sum = sum(cnt_use_buffitem_new),
            gathering_cnt_sum = sum(gathering_cnt_new),
            making_cnt_sum = sum(making_cnt_new),
            game_combat_time_sum = sum(game_combat_time_new),
            get_money_sum = sum(get_money_new))
head(test_etc)


# Create trade variables
test_trade_new <- test_trade %>%
  gather(key = "role", value = "acc_id", source_acc_id, target_acc_id) %>%
  mutate(role = ifelse(role == "source_acc_id", "sell", "buy"),
         item_amount_new = (item_amount - min(item_amount))/(max(item_amount) - min(item_amount)))

test_trade_num <- test_trade_new %>% group_by(acc_id, role, item_type) %>%
  summarize(n_obs = n()) %>% ungroup() %>%
  mutate(type = paste0(role, "_", item_type)) %>%
  select(-role, -item_type) %>%
  spread(key = type, value = n_obs)
test_trade_num[is.na(test_trade_num)] <- 0
head(test_trade_num)

# Merge data for clustering
test_game_temp <- test_pay %>%
  left_join(test_guild_join, by = "acc_id") %>%
  left_join(test_time, by = "acc_id") %>%
  left_join(test_exp, by = "acc_id") %>%
  left_join(test_battle, by = "acc_id") %>%
  left_join(test_dungeon, by = "acc_id") %>%
  left_join(test_chat, by = "acc_id") %>%
  left_join(test_etc, by = "acc_id") %>%
  left_join(test_trade_num, by = "acc_id")
test_game_temp[is.na(test_game_temp)] <- 0

test_game_cluster <- apply(test_game_temp[,2:46], 2, function(x) {scale(x)}) %>% data.frame()


# Make Cluster
set.seed(9866)
k <- 6
test_kmeans_result <- kmeans(test_game_cluster, centers = k)


# Merge it!
test_game <- test_pay %>%
  left_join(test_guild_join, by = "acc_id") %>%
  left_join(test_time, by = "acc_id") %>%
  left_join(test_exp, by = "acc_id") %>%
  left_join(test_battle, by = "acc_id") %>%
  left_join(test_dungeon, by = "acc_id") %>%
  left_join(test_chat, by = "acc_id") %>%
  left_join(test_etc, by = "acc_id") %>%
  left_join(test_trade_num, by = "acc_id") %>%
  mutate(cluster = test_kmeans_result$cluster %>% factor())
test_game[is.na(test_game)] <- 0
summary(test_game)


# Preprocessing
test_result <- test_game %>% select(acc_id)


# Predict step 1
test_pred1 <- predict(rf_step1.fit2, test_game)
test_result$step1 <- test_pred1


# Predict step 2
test_test_step2 <- test %>% select(-label) %>%
  inner_join(result %>% filter(step1 == "churn") %>% select(acc_id, label), by = "acc_id")
test_pred2 <- predict(rf_step2.fit2, test_test_step2)

test_result2 <- test_result %>%
  left_join(test_test_step2 %>%
              mutate(step2 = test_pred2) %>% 
              select(acc_id, step2), by = "acc_id") %>%
  mutate(step2 = as.character(step2))
test_result2[is.na(test_result2)] <- "retained"


# Make submit data
submit_df <- test_result2 %>% select(acc_id, step2) %>% rename(label = step2); head(submit_df)


# Save it
write.csv(submit_df, "basic_submit.csv", row.names = F)


#####################################################################################
### TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST
#####################################################################################
