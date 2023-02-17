# http://www.ncaa.com/stats/basketball-men/d1
# 
# http://blog.kaggle.com/2017/05/19/march-machine-learning-mania-1st-place-winners-interview-andrew-landgraf/
#https://www.kaggle.com/c/mens-machine-learning-competition-2018/data
#https://www.kaggle.com/memocyp/team-home-cities-by-season/notebook
library(dplyr)
library(tidyr)
library(ggmap)
library(xgboost)
library(tidyverse)

Teams <- read.csv("C://Users/johnp/Documents/GitHub/March Madness/DataFiles/Teams.csv") %>% 
  mutate(TeamName = as.character(TeamName))
GameCities <- read.csv("C://Users/johnp/Documents/GitHub/March Madness/DataFiles/GameCities.csv")
Cities <- read.csv("C://Users/johnp/Documents/GitHub/March Madness/DataFiles/Cities.csv")
rscr <- read.csv("C://Users/johnp/Documents/GitHub/March Madness/DataFiles/RegularSeasonCompactResults.csv")

team_info <- left_join(rscr %>% filter(WLoc == "H") %>% filter(Season >= 2010), GameCities) %>% 
  left_join(., Cities) %>% 
  left_join(., Teams, by = c("WTeamID" = "TeamID")) %>% 
  select(TeamID = WTeamID, TeamName, CityID, City, State) %>% 
  distinct() %>% 
  mutate(name = paste0(City, ", ", State))


lonlat <- geocode(unique(as.character(team_info$name))) 
lonlat_city <- lonlat %>% cbind(unique(team_info$name)) 

na <- lonlat %>% filter(is.na(lon)) %>% 
  separate()

# Team Box Scores ---------------------------------------------------------

ncaa_box_scores <- read.csv("C://Users/johnp/Documents/GitHub/March Madness/DataFiles/NCAATourneyDetailedResults.csv")

reg_season_box_scores <- read.csv("C://Users/johnp/Documents/GitHub/March Madness/DataFiles/RegularSeasonDetailedResults.csv")
reg_season_box_scores_2018 <- read.csv("C://Users/johnp/Documents/GitHub/March Madness/PrelimData2018/RegularSeasonDetailedResults_Prelim2018.csv")

seeds <- read.csv("C://Users/johnp/Documents/GitHub/March Madness/DataFiles/NCAATourneySeeds.csv") 

seeds_cleaned <- seeds %>% 
  select(TeamID, Season, Seed) %>%
  mutate(seed_n = str_sub(Seed, 2, -1),
         seed_n = as.numeric(str_replace_all(seed_n, "[a-z]", "")),
         seed_region = str_sub(Seed, 1, 1)) %>% 
  left_join(., Teams)

# stats -------------------------------------------------------------------

win_stats <- reg_season_box_scores_2018 %>% 
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
         
         
los_stats <- reg_season_box_scores_2018 %>% 
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

pdata <- stats_season %>% 
  left_join(., seeds_cleaned) %>% 
  select(seed_n, OR_PG) %>%
  filter(!is.na(seed_n))
 
ggplot(pdata, aes(seed_n, OR_PG)) +
  geom_point(colour = "navajowhite3") +
  geom_smooth(method = "lm", colour = "midnightblue") 


# Model -------------------------------------------------------------------

train_dates <- stats_all %>% 
  select(Season) %>% 
  filter(Season < 2017) 

test_dates <- stats_all %>% 
  select(Season) %>% 
  filter(Season == 2017) 

features_train <- stats_all %>% 
  filter(Season < 2017) %>% 
  select(-c(Season, TeamID, Outcome))

features_test <- stats_all  %>%
  filter(Season == 2017) %>% 
  select(-c(Season, TeamID, Outcome))

label_train <- stats_all %>% 
  filter(Season < 2017) %>%
  select(Outcome)

label_test <- stats_all %>% 
  filter(Season == 2017) %>%
  select(Outcome)

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

# predictions -------------------------------------------------------------

pred <- data.frame(prob = predict(learner_model, newdata = dtest)) %>% 
  cbind(., label_test) %>% 
  mutate(prediced_WL = if_else(prob > .50, 1, 0))
