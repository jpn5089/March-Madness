library(caret)
library(magrittr)
library(dplyr)
library(tidyr)
library(ggmap)
library(xgboost)
library(tidyverse)

ncaa_tourney_results <- read.csv("C://Users/johnp/Documents/GitHub/March Madness/DataFiles/NCAATourneyDetailedResults.csv") %>% 
  mutate(team_id_diff = WTeamID - LTeamID,
         Team1 = case_when(team_id_diff < 0 ~ WTeamID,
                           team_id_diff > 0 ~ LTeamID),
         Team2 = case_when(team_id_diff > 0 ~ WTeamID,
                           team_id_diff < 0 ~ LTeamID),
         result = if_else(WTeamID == Team1, 1, 0))
         
seeds <- read.csv("C://Users/johnp/Documents/GitHub/March Madness/DataFiles/NCAATourneySeeds.csv")

seeds_cleaned <- seeds %>% 
  select(TeamID, Season, Seed) %>%
  mutate(seed_n = str_sub(Seed, 2, -1),
         seed_n = as.numeric(str_replace_all(seed_n, "[a-z]", "")),
         seed_region = str_sub(Seed, 1, 1))

team1_seeds <- seeds %>% set_colnames(c("Season", "T1Seed", "Team1ID"))
team2_seeds <- seeds %>% set_colnames(c("Season", "T2Seed", "Team2ID"))

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
  filter(Season < 2017) 

test_dates <- train %>% 
  select(Season) %>% 
  filter(Season == 2017) 

features_train <- train %>% 
  filter(Season < 2017) %>% 
  select(-c(Season, DayNum, Team1, Team2, result, T1Seed, T2Seed, TeamName_1, TeamName_2))

features_test <- train  %>%
  filter(Season == 2017) %>% 
  select(-c(Season, DayNum, Team1, Team2, result, T1Seed, T2Seed, TeamName_1, TeamName_2))

label_train <- train %>% 
  filter(Season < 2017) %>%
  select(result)

label_test <- train %>% 
  filter(Season == 2017) %>%
  select(result)

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
                           max_depth = 6,
                           eta = 0.25, 
                           nthread = 8, 
                           nrounds = 100,
                           watchlist = watchlist, 
                           early_stopping_rounds = 5)

pred <- data.frame(prob = predict(learner_model, newdata = dtest)) %>% 
  cbind(., label_test) %>% 
  mutate(prediced_WL = if_else(prob > .50, 1, 0)) %>% 
  cbind(., features_test)

#try differences of win %'s for last 30 days (and overall) instead of win/loss totals
#Add conference tourney data 
#Conference Champ 0/1

#create differences variables then train

#Get 2018 matchups and insert season_stats

# 2018 season data --------------------------------------------------------

seeds_2018 <- read.csv("C://Users/johnp/Documents/GitHub/March Madness/PrelimData2018/2018seeds.csv") %>%
  mutate(TeamName_1 = as.character(TeamName_1),
         TeamName_2 = as.character(TeamName_2)) %>% 
  left_join(., Teams %>% select(TeamID, TeamName), by = c("TeamName_1" = "TeamName")) %>% 
  rename(Team1 = TeamID) %>% 
  left_join(., Teams %>% select(TeamID, TeamName), by = c("TeamName_2" = "TeamName")) %>% 
  rename(Team2 = TeamID)

test <- seeds_2018 %>% 
  left_join(., stats_season %>% filter(Season == 2018), by = c("Season", "TeamName_1" = "TeamName",
                                                               "Team1" = "TeamID")) %>% 
  left_join(., stats_season %>% filter(Season == 2018), by = c("Season", "TeamName_2" = "TeamName",
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
  cbind(., test)

