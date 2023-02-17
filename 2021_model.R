library(caret)
library(magrittr)
library(dplyr)
library(tidyr)
library(ggmap)
library(xgboost)
library(tidyverse)
library(mlr)

ncaa_tourney_results <- read.csv("C://Users/johnp/Documents/GitHub/March Madness/Data2021/MNCAATourneyDetailedResults.csv") %>% 
  mutate(team_id_diff = WTeamID - LTeamID,
         Team1 = case_when(team_id_diff < 0 ~ WTeamID,
                           team_id_diff > 0 ~ LTeamID),
         Team2 = case_when(team_id_diff > 0 ~ WTeamID,
                           team_id_diff < 0 ~ LTeamID),
         result = if_else(WTeamID == Team1, 1, 0))

seeds <- read.csv("C://Users/johnp/Documents/GitHub/March Madness/Data2021/MNCAATourneySeeds.csv")

seeds_cleaned <- seeds %>% 
  select(TeamID, Season, Seed) %>%
  mutate(seed_n = str_sub(Seed, 2, -1),
         seed_n = as.numeric(str_replace_all(seed_n, "[a-z]", "")),
         seed_region = str_sub(Seed, 1, 1))

team1_seeds <- seeds %>% set_colnames(c("Season", "T1Seed", "Team1ID"))
team2_seeds <- seeds %>% set_colnames(c("Season", "T2Seed", "Team2ID"))

Teams <- read.csv("C://Users/johnp/Documents/GitHub/March Madness/Data2021/MTeams.csv") %>% 
  mutate(TeamName = as.character(TeamName))

# Team Box Scores ---------------------------------------------------------

reg_season_box_scores <- read.csv("C://Users/johnp/Documents/GitHub/March Madness/Data2021/MRegularSeasonDetailedResults.csv")

win_stats <- reg_season_box_scores %>% 
  mutate(PA = (LFGM3 *3) + (LFGM - LFGM3) *2 + LFTM,
         PS = (WFGM3 *3) + (WFGM - WFGM3) *2 + WFTM,
         FGP = WFGM / WFGA, 
         FGP2 = (WFGM - WFGM3) / (WFGA - WFGA3),
         FGP3 = WFGM3 / WFGA3,
         FTP = WFTM / WFTA,
         ORP = WOR / (WOR + LDR), 
         DRP = WDR / (WDR + LOR),
         poss = WFGA + 0.475*WFTA - WOR + WTO,
         opp_poss = LFGA + 0.475*LFTA - LOR + LTO,
         off_rating = round((WScore / poss)*100, 2),
         def_rating = round((LScore / opp_poss)*100, 2), 
         net_rating = off_rating - def_rating,
         pace = 48*((poss+opp_poss)/(2*(240/5))),
         Outcome = 1) %>% 
  select(Season, TeamID = WTeamID, DayNum, Loc = WLoc, PS, PA, FGM = WFGM, FGA = WFGA, FGM3 = WFGM3, FGA3 = WFGA3,
         FTM = WFTM, FTA = WFTA, OR = WOR, DR = WDR, AST = WAst, TO = WTO,
         STL = WStl, BLK = WBlk, PF = WPF, FGP, FGP2, FGP3, FTP, ORP, DRP, poss, opp_poss, 
         off_rating, def_rating, net_rating, pace, Outcome)


los_stats <- reg_season_box_scores %>% 
  mutate(PS = (LFGM3 *3) + (LFGM - LFGM3) *2 + LFTM,
         PA = (WFGM3 *3) + (WFGM - WFGM3) *2 + WFTM,
         FGP = LFGM / LFGA, 
         FGP2 = (LFGM - LFGM3) / (LFGA - LFGA3),
         FGP3 = LFGM3 / LFGA3,
         FTP = LFTM / LFTA,
         ORP = LOR / (LOR + WDR), 
         DRP = LDR / (LDR + WOR),
         poss = WFGA + 0.475*WFTA - WOR + WTO,
         opp_poss = LFGA + 0.475*LFTA - LOR + LTO,
         off_rating = round((WScore / poss)*100, 2),
         def_rating = round((LScore / opp_poss)*100, 2), 
         net_rating = off_rating - def_rating,
         pace = 48*((poss+opp_poss)/(2*(240/5))),
         Outcome = 0) %>%
  select(Season, TeamID = LTeamID, DayNum, Loc = WLoc, PS, PA, FGM = LFGM, FGA = LFGA, FGM3 = LFGM3, FGA3 = LFGA3, 
         FTM = LFTM, FTA = LFTA, OR = LOR, DR = LDR, AST = LAst, TO = LTO,
         STL = LStl, BLK = LBlk, PF = LPF, FGP, FGP2, FGP3, FTP, ORP, DRP, poss, opp_poss, 
         off_rating, def_rating, net_rating, pace, Outcome) 

stats_all <- rbind(win_stats, los_stats)

stats_season <- stats_all %>% 
  select(-c(FGP, FGP2, FGP3, FTP, ORP, DRP)) %>% 
  group_by(Season, TeamID) %>% 
  summarise(G = n(), 
            W = sum(Outcome == 1),
            L = sum(Outcome == 0),
            W_last30D = sum(Outcome == 1 & DayNum > 100),
            L_last30D = sum(Outcome == 0 & DayNum > 100),
            W_H = sum(Outcome == 1 & Loc == "H"),
            W_A = sum(Outcome == 1 & Loc == "A"),
            W_N = sum(Outcome == 1 & Loc == "N"),
            PPG = mean(PS),
            PPG_last30D = mean(PS[DayNum > 100]),
            OPP_PPG = mean(PA),
            OPP_PPG_last30D = mean(PA[DayNum > 100]),
            poss_m = mean(poss),
            opp_poss_m = mean(opp_poss),
            off_rating_m = mean(off_rating),
            def_rating_m = mean(def_rating),
            net_rating_m = mean(net_rating),
            pace_m = mean(pace),
            off_rating_m_last30D = mean(off_rating[DayNum > 100]),
            def_rating_m_last30D = mean(def_rating[DayNum > 100]),
            net_rating_m_last30D = mean(net_rating[DayNum > 100]),
            FGP = sum(FGM)/sum(FGA),
            FGP3 = sum(FGM3)/sum(FGA3),
            FTP = sum(FTM)/sum(FTA),
            FGM_PG = mean(FGM),
            FGM3_PG = mean(FGM3),
            FTA_PG = mean(FTA),
            OR_PG = mean(OR),
            DR_PG = mean(DR),
            AST_PG = mean(AST),
            TO_PG = mean(TO),
            STL_PG = mean(STL),
            BLK_PG = mean(BLK),
            PF_PG = mean(PF)) %>% 
  ungroup() %>% 
  mutate(G_last30D = W_last30D + L_last30D,
         W_PCT = round(W/G, 2),
         L_PCT = 1 - W_PCT,
         W_PCT_last30D = round(W_last30D / G_last30D, 2),
         L_PCT_last30D = 1 - W_PCT_last30D) %>% 
  left_join(., Teams %>% select(TeamName, TeamID)) 


train <- ncaa_tourney_results %>% 
  left_join(., seeds_cleaned %>% select(Season, seed_n, TeamID), by = c("Season", "Team1" = "TeamID")) %>% 
  rename(T1Seed = seed_n) %>% 
  left_join(., seeds_cleaned %>% select(Season, seed_n, TeamID), by = c("Season", "Team2" = "TeamID")) %>% 
  rename(T2Seed = seed_n) %>% 
  select(Season, DayNum, Team1, Team2, result, T1Seed, T2Seed) %>% 
  left_join(., stats_season, by = c("Season", "Team1" = "TeamID")) %>% 
  left_join(., stats_season, by = c("Season", "Team2" = "TeamID"), suffix = c("_1", "_2")) %>% 
  mutate(seed_diff = T1Seed - T2Seed,
         w_diff = W_1 - W_2,
         l_diff = L_1 - L_2,
         w_last_30_diff = W_last30D_1 - W_last30D_2,
         l_last_30_diff = L_last30D_1 - L_last30D_2,
         ppg_diff = PPG_1 - PPG_2,
         ppg_last_30_diff = PPG_last30D_1 - PPG_last30D_2,
         opp_ppg_diff = OPP_PPG_1 - OPP_PPG_2,
         opp_ppg_last_30_diff = OPP_PPG_last30D_1 - OPP_PPG_last30D_2,
         poss_m_diff = poss_m_1 - poss_m_2,
         opp_poss_m_diff = opp_poss_m_1 - opp_poss_m_2,
         off_rating_m_diff = off_rating_m_1 - off_rating_m_2,
         off_rating_m_last_30_diff = off_rating_m_last30D_1 - off_rating_m_last30D_2,
         def_rating_m_diff = def_rating_m_1 - def_rating_m_2,
         def_rating_m_last_30_diff = def_rating_m_last30D_1 - def_rating_m_last30D_2,
         net_rating_m_diff = net_rating_m_1 - net_rating_m_2,
         net_rating_m_last_30_diff = net_rating_m_last30D_1 - net_rating_m_last30D_2,
         pace_m_diff = pace_m_1 - pace_m_2,
         fgp_diff = FGP_1 - FGP_2,
         fgp3_diff = FGP3_1 - FGP3_2,
         ftp_diff = FTP_1 - FTP_2,
         fgm_pg_diff = FGM_PG_1 - FGM_PG_2,
         fgm3_pg_diff = FGM3_PG_1 - FGM3_PG_2,
         fta_pg_diff = FTA_PG_1 - FTA_PG_2,
         or_pg_diff = OR_PG_1 - OR_PG_2,
         dr_pg_diff = DR_PG_1 - DR_PG_2,
         ast_pg_diff = AST_PG_1 - AST_PG_2,
         to_pg_diff = TO_PG_1 - TO_PG_2,
         stl_pg_diff = STL_PG_1 - STL_PG_2,
         blk_pg_diff = BLK_PG_1 - BLK_PG_2,
         pf_pg_diff = PF_PG_1 - PF_PG_2) %>% 
  select(1:7, 88:118, TeamName_1, TeamName_2)

# Model Setup -------------------------------------------------------------

train_dates <- train %>% 
  select(Season) %>% 
  filter(Season < 2019) 

test_dates <- train %>% 
  select(Season) %>% 
  filter(Season == 2019) 

features_train <- train %>% 
  filter(Season < 2019) %>% 
  select(-c(Season, DayNum, Team1, Team2, result, T1Seed, T2Seed, TeamName_1, TeamName_2))

features_test <- train  %>%
  filter(Season == 2019) %>% 
  select(-c(Season, DayNum, Team1, Team2, result, T1Seed, T2Seed, TeamName_1, TeamName_2))

label_train <- train %>% 
  filter(Season < 2019) %>%
  select(result)

label_test <- train %>% 
  filter(Season == 2019) %>%
  select(result)

# parameter search --------------------------------------------------------

train_data_with_label <- cbind(features_train, label_train %>% mutate(result = as.factor(result)))

trainTask <- makeClassifTask(data = train_data_with_label, target = "result")

# make the xgboost learner in mlr for para tuning

xgb_learner <- makeLearner(
  "classif.xgboost",
  predict.type = "prob",
  par.vals = list(
    objective = "reg:logistic",
    eval_metric = "error"
  )
)

xgb_params <- makeParamSet(
  makeIntegerParam("nrounds", lower = 10, upper = 40),
  makeIntegerParam("max_depth", lower = 2, upper = 6),
  makeNumericParam("eta", lower = .05, upper = .15),
  makeNumericParam("gamma", lower = 18, upper = 25)
)

control <- makeTuneControlRandom(maxit = 20)
resample_desc <- makeResampleDesc("CV", iters = 5)

tuned_params <- tuneParams(
  learner = xgb_learner,
  task = trainTask,
  resampling = resample_desc,
  par.set = xgb_params,
  control = control
)

# model -------------------------------------------------------------------

dtrain <- xgb.DMatrix(data = features_train %>% 
                        data.frame() %>%
                        data.matrix(.),
                      label = label_train %>%
                        data.frame() %>%
                        data.matrix(.), 
                      missing = NA)

dtest <- xgb.DMatrix(data = features_test %>% 
                       data.frame() %>%
                       data.matrix(.),
                     label = label_test %>%
                       data.frame() %>%
                       data.matrix(.), 
                     missing = NA)

watchlist <- list(train = dtrain, test = dtest)

param <- list(objective = "reg:logistic",
              eval_metric = "error")

learner_model <- xgb.train(data = dtrain,
                           params = param,
                           max_depth = 10,
                           eta = 0.15,
                           nthread = 8,
                           nrounds = 100,
                           watchlist = watchlist,
                           early_stopping_rounds = 20)

# learner_model <- xgb.train(data = dtrain, 
#                            params = param,
#                            max_depth = tuned_params$x$max_depth,
#                            eta = tuned_params$x$eta, 
#                            nthread = 8, 
#                            gamma = tuned_params$x$gamma,
#                            nrounds = tuned_params$x$nrounds,
#                            watchlist = watchlist)

pred <- data.frame(prob = predict(learner_model, newdata = dtest)) %>% 
  cbind(., label_test) %>% 
  mutate(prediced_WL = if_else(prob > .50, 1, 0)) %>% 
  cbind(., features_test)


# 2021 season data --------------------------------------------------------

seeds_2021 <- read.csv("C://Users/johnp/Documents/GitHub/March Madness/Data2021/2021Seeds.csv") %>%
  mutate(TeamName_1 = as.character(TeamName_1),
         TeamName_2 = as.character(TeamName_2)) %>% 
  left_join(., Teams %>% select(TeamID, TeamName), by = c("TeamName_1" = "TeamName")) %>% 
  rename(Team1 = TeamID) %>% 
  left_join(., Teams %>% select(TeamID, TeamName), by = c("TeamName_2" = "TeamName")) %>% 
  rename(Team2 = TeamID) %>% 
  filter(Rd == 32) %>% 
  select(-Rd, -Win)

test <- seeds_2021 %>% 
  left_join(., stats_season %>% filter(Season == 2020), by = c("Season", "TeamName_1" = "TeamName",
                                                               "Team1" = "TeamID")) %>% 
  left_join(., stats_season %>% filter(Season == 2020), by = c("Season", "TeamName_2" = "TeamName",
                                                               "Team2" = "TeamID"), 
            suffix = c("_1", "_2")) %>% 
  mutate(seed_diff = T1Seed - T2Seed,
         w_diff = W_1 - W_2,
         l_diff = L_1 - L_2,
         w_last_30_diff = W_last30D_1 - W_last30D_2,
         l_last_30_diff = L_last30D_1 - L_last30D_2,
         ppg_diff = PPG_1 - PPG_2,
         ppg_last_30_diff = PPG_last30D_1 - PPG_last30D_2,
         opp_ppg_diff = OPP_PPG_1 - OPP_PPG_2,
         opp_ppg_last_30_diff = OPP_PPG_last30D_1 - OPP_PPG_last30D_2,
         poss_m_diff = poss_m_1 - poss_m_2,
         opp_poss_m_diff = opp_poss_m_1 - opp_poss_m_2,
         off_rating_m_diff = off_rating_m_1 - off_rating_m_2,
         off_rating_m_last_30_diff = off_rating_m_last30D_1 - off_rating_m_last30D_2,
         def_rating_m_diff = def_rating_m_1 - def_rating_m_2,
         def_rating_m_last_30_diff = def_rating_m_last30D_1 - def_rating_m_last30D_2,
         net_rating_m_diff = net_rating_m_1 - net_rating_m_2,
         net_rating_m_last_30_diff = net_rating_m_last30D_1 - net_rating_m_last30D_2,
         pace_m_diff = pace_m_1 - pace_m_2,
         fgp_diff = FGP_1 - FGP_2,
         fgp3_diff = FGP3_1 - FGP3_2,
         ftp_diff = FTP_1 - FTP_2,
         fgm_pg_diff = FGM_PG_1 - FGM_PG_2,
         fgm3_pg_diff = FGM3_PG_1 - FGM3_PG_2,
         fta_pg_diff = FTA_PG_1 - FTA_PG_2,
         or_pg_diff = OR_PG_1 - OR_PG_2,
         dr_pg_diff = DR_PG_1 - DR_PG_2,
         ast_pg_diff = AST_PG_1 - AST_PG_2,
         to_pg_diff = TO_PG_1 - TO_PG_2,
         stl_pg_diff = STL_PG_1 - STL_PG_2,
         blk_pg_diff = BLK_PG_1 - BLK_PG_2,
         pf_pg_diff = PF_PG_1 - PF_PG_2) %>% 
  select(1:7, 86:116)

info <- test %>%
  select(-c(Season, Team1, Team2, T1Seed, T2Seed, TeamName_1, TeamName_2)) 

test_label <- test %>% mutate(result = NA) %>% select(result)

dtest <- xgb.DMatrix(data = test %>% 
                       select(-c(Season, Team1, Team2, T1Seed, T2Seed, TeamName_1, TeamName_2)) %>% 
                       data.frame() %>%
                       data.matrix(.),
                     label = test_label %>%
                       data.frame() %>%
                       data.matrix(.), 
                     missing = NA)

pred <- data.frame(prob = predict(learner_model, newdata = dtest)) %>% 
  cbind(., test_label) %>% 
  mutate(predicted_WL = if_else(prob > .50, 1, 0)) %>% 
  cbind(., test) %>% 
  left_join(., read.csv("C://Users/johnp/Documents/GitHub/March Madness/Data2021/2021Seeds.csv"))


