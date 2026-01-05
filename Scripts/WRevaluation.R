##NFL WR Cluster Analysis
##Lee Jepsen

##clear environment
rm(list = ls())

##Load required packages
library(nflfastR)
library(nflreadr)
library(gsisdecoder)
library(readxl)
library(tidyverse)
library(dplyr)
library(factoextra)
library(cluster)
library(fmsb)
library(ggplot2)
library(ggimage)
library(ggrepel)
library(gt)
library(tidyr)
library(stringr)
library(scales)
library(writexl)

##importing 2024 WR stats
WR2024 <- read_excel("C:/Users/YourName/Desktop/WR2024td.xlsx")

#importing useful stats from fantasy football tracking
FF2024 <- read_excel("C:/Users/YourName/Desktop/FF2024wr.xlsx")

##Next variable to calculate is drop rate using FF2024 data frame
FF2024 <- FF2024 %>%
  mutate(DropRate = DROP/CATCHABLE)

##Selecting and Merging Variables from both data sets into one data frame
FF2024b <- FF2024 %>%
  select(Player, G, YDS, `RZ TGT`, `YAC/R`, `Y/R`, DropRate, `YBC/R`, TGT)

WR2024b <- WR2024 %>%
  select(Player, TD)

WRmerged <- FF2024b %>%
  left_join(WR2024b, by = "Player")

##extracting the team abbreviation from the player name column
WRmerged <- WRmerged %>%
  mutate(team_abbr = str_extract(Player, "(?<=\\()[A-Z]+(?=\\))"))

##removing the team abbreviation in name column completely
WRmerged <- WRmerged %>%
  mutate(Player = str_trim(str_remove(Player, "\\s*\\([A-Z]+\\)")))

##Jacksonville jaguars abbreviation is JAX not JAC changing it to JAX
WRmerged <- WRmerged %>%
  mutate(team_abbr = ifelse(team_abbr == "JAC", "JAX", team_abbr))

##Filtering WRs who played at least half of the season 
WRfiltered <- WRmerged %>%
  filter(G >= 8)

##Removing NA Values
WRClean <- na.omit(WRfiltered)

##Now that we have all the stats into one data frame, and filtered
##utilizing PAMS 
##First scaling the WR variables that I am using
##Scale all columns except Names and games. Names in particular so I can double
##check that when adding the cluster of each player to data frame it aligns 
##with that player
WRscaled <- WRClean %>%
  mutate(across(
    .cols = -c(Player, G, team_abbr),
    .fns = ~as.numeric(scale(.))
  ))

##Using the Elbow method and Silhouette method to determine optimal 
##number of clusters
##looking at both PAM and kmeans 
##elbow method kmeans
fviz_nbclust(select(WRscaled, -Player, -G, -team_abbr), kmeans, method = "wss") +
  labs(title = "elbow method for optimal clusters")

##Silhouette method kmeans
fviz_nbclust(select(WRscaled, -Player, -G, -team_abbr), kmeans, method = "silhouette") + 
  labs(title = "silhouette method")

##elbow method PAM
fviz_nbclust(select(WRscaled, -Player, -G, -team_abbr), pam, method = "wss") +
  labs(title = "elbow method for optimal clusters")

##Silhouette method PAM
fviz_nbclust(select(WRscaled, -Player, -G, -team_abbr), pam, method = "silhouette") + 
  labs(title = "silhouette method")


##From the both elbow methods it seems to become more gradual after k=4/5 
##choosing k=4 to start 
##Looking at the Silhouette method it suggests the optimal cluster to be 2
##Mainly suggesting a strong divide between 2 types of wide receivers
##Which may not give much detail, so I will continue with k=4
##running PAM clustering with k=4 and k=5
set.seed(27)
k4Results <- pam(select(WRscaled, -Player, -G, -team_abbr), k = 4)
summary(k4Results)
plot(k4Results)

##Seeing what k=5 looks like
set.seed(27)
k5Results <- pam(select(WRscaled, -Player, -G, -team_abbr), k = 5)
summary(k5Results)
plot(k5Results)


##Start to see overlapping here with clusters k=5
##k=4 looks pretty clean with distinct groupings and not much overlap
##comparing to k=5, In the first k=4 clustering model 
##I am also explaining about 68% of the total variability with 2 components 
##with this 2 dimension plot meaning condensing the data this way shows that my
##clusters may represent real groupings and not misled by overlaps or separations
##that are not real in the full dimensional space
##overall continuing with 4 different WR groupings where I have enough 
##variability in the groupings to label each cluster and not struggle to differentiate
##between limited number of groups or too many groups which may cause confusion

##Alright so Lets try to give some context to what these clusters mean
##adding cluster data to cleaned data frame
WRcomp <- WRClean %>%
  mutate(cluster = k4Results$clustering)

##Double checking that the clusters aligned with the right name
##They should since I did not manipulate the data frame once scaling 
##but for my sanity and best practice 
head(data.frame(Player = WRscaled$Player, Cluster = k4Results$clustering))

##Now what does each cluster represent
##see the average of each variable for the clusters
WRcomp %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

##visualize the data
##Creating dot plot that shows the cluster features 
##creating new df that includes scaled data and then adding the cluster to new df
scaledDF <- as.data.frame(select(WRscaled, -Player, -G, -team_abbr))
scaledDF$cluster <- WRcomp$cluster

##Creating cluster centers (means of scaled features)
ClusterCenters <- scaledDF %>%
  group_by(cluster) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  pivot_longer(-cluster, names_to = "feature", values_to = "value")

##creating plot of clusters
ggplot(ClusterCenters, aes(x = feature, y = value)) + 
  geom_point(size = 4, color = "blue") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  facet_wrap(~ cluster, ncol = 2, labeller = label_both) +
  theme_minimal() +
  labs(title = "Wide Receiver Profiles (Standardized Feature Centers)",
       x = "Feature", y = "Standardized Cluster Center") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 14, face = "bold"))

##identifying the type of wide receiver in each cluster
ClusterProfiles <- WRcomp %>%
  group_by(cluster) %>%
  summarise(avgYards = mean(YDS, na.rm = TRUE),
            avgYBC = mean(`YBC/R`, na.rm = TRUE),
            avgYAC = mean(`YAC/R`, na.rm = TRUE),
            avgYR = mean(`Y/R`, na.rm = TRUE),
            avgTGT = mean(TGT, na.rm = TRUE),
            avgRZ = mean(`RZ TGT`, na.rm = TRUE),
            avgTD = mean(TD, na.rm = TRUE),
            avgDR = mean(DropRate, na.rm = TRUE))


##Based on the averages of each variable and type of wide receiver
##Cluster 1: Elite production across the board ~ Alpha WR / do it all WR1s
##Cluster 2: Mid-level producers ~ efficient on deep/intermediate plays: Field Stretcher
##Cluster 3: low yardage/volume but high YAC guys ~ slot/gadget WR, Slot/Gadget WR
##Cluster 4: Very low volume but High Y/R and YBC ~ Boom or Bust Depth WR, low consistency
##but Vertical splash plays when connected

##Naming of each cluster
WRcomp$WRtype <- recode_factor(WRcomp$cluster,
                               `1` = "Do it all WR",
                               `2` = "Field-Stretcher",
                               `3` = "Slot/Gadget WR",
                               `4` = "Boom-or-Bust Depth WR")


##More inspecting of the data and visualizing cluster 1, produce a ranking of 
##cluster 1 since the better performing WRs are grouped in this cluster
##I would like to create a scatter plot of the Cluster 1 WR that has a 
##composite z score of the features vs their raw yards output from 2024 Season
##Filtering out Cluster 1 WRs
WRC1 <- WRcomp %>%
  filter(cluster == 1)

##Scaling the features across all WRs in cluster 1
##using all features except yards, which is y axis 
##DropRate is going to negatively impact the player
##creating zscore to represent well-rouded WR
zscoreFeatures <- c("DropRate", "RZ TGT", "TD", "Y/R", "YAC/R", "YBC/R", "TGT")

WRC1scaled <- WRC1 %>%
  mutate(across(all_of(zscoreFeatures), scale))

##overall score
WRC1scaled <- WRC1scaled %>%
  mutate(zscore = `RZ TGT` + TD + `Y/R` + `YAC/R` + `YBC/R` + TGT - DropRate)

##team logos from nflfastR
data("teams_colors_logos")

##merge logo info with team abbreviation in data set
WRC1scaledLogo <- WRC1scaled %>%
  left_join(teams_colors_logos %>% select(team_abbr, team_logo_espn),
            by = "team_abbr")

##Plot overall score vs yards with the logos
ggplot(WRC1scaledLogo, aes(x = zscore, y = YDS)) +
  geom_image(aes(image = team_logo_espn), size = 0.03, asp = 1.0) +
  geom_text_repel(aes(label = Player), size = 2, max.overlaps = 15) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  theme_minimal() +
  labs(title = "Cluster 1, Do It All WRs: Overall Score (Z score) vs Receiving Yards",
       caption = "Composite Z-score variables: Red Zone Targets + Touchdowns + Yards/Recption + 
       Yards After Catch/Reception + Yards Before Catch/Reception + Targets - DropRate",
       x = "Composite Z-Score",
       y = "Receiving Yards") +
  theme(plot.title = element_text(face = "bold"), 
        plot.caption = element_text(face = "bold"))

##Keenan Allen dropped out of the Graph because he is technically a 
##Free agent and is currently not signed on a team as of April 2025. He would 
##be on the bottom left side of the graph y = 744yds, x = -1.8zscore same 

##plotting TD scorers vs Yards after catch threats
ggplot(WRC1scaledLogo, aes(x = `RZ TGT`, y = `YAC/R`)) +
  geom_image(aes(image = team_logo_espn), size = 0.03, asp = 1.0) +
  geom_text_repel(aes(label = Player), size = 2, max.overlaps = 15) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  theme_minimal() +
  labs(title = "Cluster 1, Do It All WRs: Red Zone Threat vs Yards after Catch threat",
       x = "Red Zone Targets (Z-score)",
       y = "YAC per Reception (Z-score)") +
  theme(plot.title = element_text(face = "bold"))

##Next to visualize all the WR on a table I table all names with their team
##and cluster
##bringing in team logos, and headshots to table
##creating roster data frame to bring in headshots from nflreadr
Rosters <- load_rosters(season = 2024) %>%
  select(full_name, headshot_url)

##merging team logos from nflfastR
WRtable <- WRcomp %>%
  left_join(teams_colors_logos, by = "team_abbr")

##merging headshots from nflreadr
WRtable <- WRtable %>%
  left_join(Rosters, by = c("Player" = "full_name"))

##Creating table with headshots, logos and player name with their associated
##cluster
WRtable %>%
  arrange(cluster, Player) %>%
  select(headshot_url, Player, team_nick, cluster, team_logo_espn, WRtype) %>%
  gt() %>%
  text_transform(location = cells_body(columns = c(headshot_url)),
                 fn = function(x){
                   web_image(url = x, height = 40)
                 }
  ) %>%
  
  text_transform(locations = cells_body(vars(team_logo_espn)),
                 fn = function(x) {
                   web_image(url = x, height = 25)
                 }
  ) %>%
  cols_label(headshot_url = "",
             Player = "Player", 
             team_nick = "Team",
             cluster = "Cluster",
             team_logo_espn = "",
             WRtype = "WR Profile"
             ) %>%
  tab_header(
    title = md("*NFL WRs and their Cluster*"),
    subtitle = "2024/25 Cluster Analysis"
  ) %>%
  data_color(columns = vars(cluster),
             colors = scales::col_factor(
               palette = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728"),
               domain = NULL
             )
  )

##NA means the player is considered a Free Agent and not signed with a team 
##as of April 2025

#####Contracts
##looking at Contract efficiency by cluster
##understanding cluster value and determine which WRs are providing
##value to the team

##importing salary data as of April 2025
Salary25 <- read_excel("C:/Users/YourName/Desktop/Salary25.xlsx")

##Some names have periods, commas, apostrophes in them such as D.K. Metcalf so 
##names wont align when merging removing them from names
Salary25 <- Salary25 %>%
  mutate(PlayerClean = str_replace_all(Player, "[\\.\\,\\']", "") %>%
           str_squish()
  )

WRcomp <- WRcomp %>%
  mutate(PlayerClean = str_replace_all(Player, "[\\.\\,\\']", "") %>%
           str_squish()
  )

Salary25 <- Salary25 %>%
  mutate(PlayerClean = str_replace_all(PlayerClean, "\\b(II|III|IV|V|JR|Jr|Jr\\.|Sr|SR|Sr\\.)\\b", "") %>%
           str_squish()
  )

WRcomp <- WRcomp %>%
  mutate(PlayerClean = str_replace_all(PlayerClean, "\\b(II|III|IV|V|JR|Jr|Jr\\.|Sr|SR|Sr\\.)\\b", "") %>%
           str_squish()
  )

WRcomp <- WRcomp %>%
  mutate(PlayerClean = str_to_title(PlayerClean))

Salary25 <- Salary25 %>%
  mutate(PlayerClean = str_to_title(PlayerClean))

Salary25 <- Salary25 %>%
  mutate(PlayerClean = case_when(
    PlayerClean == "Scott Miller" ~ "Scotty Miller",
    PlayerClean == "Gabriel Davis" ~ "Gabe Davis",
    PlayerClean == "Josh Palmer" ~ "Joshua Palmer",
    TRUE ~ PlayerClean
  ))

##Merging both dataframes
Contracts <- left_join(WRcomp, Salary25, by = "PlayerClean")

ContractsA <- na.omit(Contracts)

##Cleaned all the different naming conventions from the data frame
##All that remain with no contract data are the 14 Free Agents 
##from the Salary data I imported where those players did not sign
##a contract when the Salary data was formed

##Now that we have Salary metrics lets see the breakdown of average salary 
##per WR type/cluster
SalaryByCluster <- ContractsA %>%
  group_by(cluster) %>%
  summarise(AvgSalary = mean(`Cash Spent ▾`, na.rm = TRUE),
            count = n()) %>%
  arrange(desc(AvgSalary))

##Bar Plot to visualize average salary per Cluster
ggplot(SalaryByCluster, aes(x = factor(cluster), y = AvgSalary)) +
  geom_col(fill = "forestgreen") +
  scale_y_continuous(labels = dollar_format(prefix = "$")) +
  labs(title ="Average Annual WR Salary by Cluster",
       x = "WR Cluster",
       y = "Average Annual Salary (Cash Spent)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

##WRs in cluster 1 are most expensive followed by clusters 2, 4, and then 3

##With the metrics in this data set I would like to try and find which performance 
##metrics best explain or correlate with how much WRs are paid and finding the 
##best value WRs
##Correlation analysis
Efficiency <- ContractsA %>%
  select(`Cash Spent ▾`, YDS, TD, TGT, `RZ TGT`, `YAC/R`, `Y/R`, `YBC/R`)

##Correlation matrix
CorMatrix <- cor(Efficiency, use = "complete.obs")
CorMatrix

##Correlation matrix says yards is the strongest predictor of salary, followed by Targets
##and red zone targets which makes sense given that the more the wide receiver produces
##the more likely they are to get paid, and the best overall stat being Yards 
##followed by who gets the most looks in the red zone and which receivers teams
##can rely on
##going to look into this further with the variables that I have and do a multi-variate
##Linear regression to test how well stats predict salary value
##multi-variate model
Lmodel <- lm(`Cash Spent ▾` ~ YDS + TD + `RZ TGT` + TGT + `YAC/R` + `Y/R` + `YBC/R`,
             data = ContractsA)
summary(Lmodel)

##based on multi-variate model , Yards, RZ targets, and targets are significant 
##variables, although targets was negative which may be due to multicollinearity
##with yards, my thinking is because of usage for wide receivers
##yards and targets can both be used to measure volume
##The correlation analysis and multi-variate linear regression point towards
##Yards and Red Zone Targets which I will use to evaluate WR contract efficiency

##Now lets dig into which Wide receivers provide the most value
ContractsA <- ContractsA %>%
  mutate(YardsperMillion = YDS / (`Cash Spent ▾` / 1e6),
         RzTGTperMillion = `RZ TGT` / (`Cash Spent ▾` / 1e6))

##rank WRs by yards per million
TopValueWRyards <- ContractsA %>%
  filter(!is.na(YardsperMillion), `Cash Spent ▾` > 0) %>%
  arrange(desc(YardsperMillion)) %>%
  select(PlayerClean, Team, cluster, YDS, `Cash Spent ▾`, YardsperMillion) %>%
  mutate(rank = row_number())

##Rank WRs by rz targets per million
TopValueWRrztgt <- ContractsA %>%
  filter(!is.na(RzTGTperMillion), `Cash Spent ▾` > 0) %>%
  arrange(desc(RzTGTperMillion)) %>%
  select(PlayerClean, Team, cluster, YDS, `Cash Spent ▾`, RzTGTperMillion) %>%
  mutate(rank = row_number())

##Going to add a composite score for value combining yards and red zone targets
##and subtracting drop rate from the value for each WR 
ContractsA <- ContractsA %>%
  mutate(ValueScore = scale(YDS) + scale(`RZ TGT`),
         Value = as.numeric(ValueScore) / (`Cash Spent ▾` / 1e6))

##Re-ranking WR based on ValueScore
TopValueWR <- ContractsA %>%
  filter(!is.na(Value), `Cash Spent ▾` > 0) %>%
  arrange(desc(Value)) %>%
  select(PlayerClean, Team, cluster, WRtype, YDS, `RZ TGT`, `Cash Spent ▾`, 
         ValueScore, Value) %>%
  mutate(rank = row_number())

##based on this ranking it tells me which WRs outperformed their contract
##I see a lot of WRs are actually in Cluster 1 being the more dominant WR type 
##also after seeing these results WRs that are still under rookie contracts 
##in particular last years rookies performed really well and was a dominant 
##WR draft class

##Creating a table to show the undervalued WRs and by cluster
##first rank the WR in each cluster
WRranked <- ContractsA %>%
  filter(!is.na(Value), `Cash Spent ▾` > 0) %>%
  group_by(cluster) %>%
  mutate(clusterRank = dense_rank(desc(Value))) %>%
  ungroup()

##create table for top 25 value for each cluster
##first create new dataframe with nfl athlete data and merge to nfl Ranked
##I previously did this but that data frame does not include new naming 
##conventions because I combined salary data and stats
##team branding logos
NFLteams <- nflfastR::teams_colors_logos %>%
  select(team_abbr, team_nick, team_color, team_logo_espn, team_name)

##merging the NFL team data and roster into WRranked dataframe
WRranked <- WRranked %>%
  left_join(NFLteams, by = "team_abbr") %>%
  left_join(Rosters, by = c("Player.x" = "full_name"))

##Create a top 25 undervalued WR per cluster/WR profile
Top25perCluster <- WRranked %>%
  filter(clusterRank <= 25) %>%
  arrange(cluster, clusterRank)

##create table
Top25perCluster %>%
  arrange(cluster, clusterRank) %>%
  select(headshot_url, Player.x, team_abbr, team_logo_espn, `Cash Spent ▾`, WRtype, 
         cluster, clusterRank) %>%
  gt() %>%
  text_transform(locations = cells_body(columns = c(headshot_url)),
                 fn = function(x) {
                   web_image(url = x, height = 40)
                 }) %>%
  text_transform(locations = cells_body(columns = c(team_logo_espn)),
                 fn = function(x) {
                   web_image(url = x, height = 30)
                 }) %>%
  fmt_currency(columns = `Cash Spent ▾`, currency = "USD") %>%
  cols_label(headshot_url = "",
             Player.x = "Player",
             team_abbr = "Team",
             team_logo_espn = "",
             `Cash Spent ▾` = "Annual Salary",
             cluster = "Cluster",
             clusterRank = "Rank in Cluster") %>%
  tab_header(title = md("**Top 25 Most Cost-Efficient (Undervalued) WRs per Cluster**"),
             subtitle = "Ranked by Value Score ((Yards + RZ TGTs) / Annual Salary)") %>%
  tab_options(table.font.size = px(13),
              heading.title.font.size = px(18),
              heading.subtitle.font.size = px(14))

##########
##########
##Re-doing this table but adjusting the Ranking by including Drop rate as a negative
##to the cluster ranking which will be (Yards + Targets - Drop rate) per $1M Spent
##Now lets dig into which Wide recievers provide the most value
ContractsB <- ContractsA %>%
  mutate(ValueScore = scale(YDS) + scale(`RZ TGT`) - scale(DropRate),
         Value = as.numeric(ValueScore) / (`Cash Spent ▾` / 1e6))

##Re-ranking WR based on ValueScore
TopValueWRB <- ContractsB %>%
  filter(!is.na(Value), `Cash Spent ▾` > 0) %>%
  arrange(desc(Value)) %>%
  select(PlayerClean, Team, cluster, WRtype, YDS, `RZ TGT`, `Cash Spent ▾`, 
         ValueScore, Value) %>%
  mutate(rank = row_number())

##Creating a table to show the undervalued WRs and by cluster
##first rank the WR in each cluster
WRrankedB <- ContractsB %>%
  filter(!is.na(Value), `Cash Spent ▾` > 0) %>%
  group_by(cluster) %>%
  mutate(clusterRank = dense_rank(desc(Value))) %>%
  ungroup()

##merging the NFL team data and roster into WRranked dataframe
WRrankedB <- WRrankedB %>%
  left_join(NFLteams, by = "team_abbr") %>%
  left_join(Rosters, by = c("Player.x" = "full_name"))

##Create a top 25 undervalued WR per cluster/WR profile
Top25perClusterB <- WRrankedB %>%
  filter(clusterRank <= 25) %>%
  arrange(cluster, clusterRank)

##create table
Top25perClusterB %>%
  arrange(cluster, clusterRank) %>%
  select(headshot_url, Player.x, team_abbr, team_logo_espn, `Cash Spent ▾`, WRtype, 
         cluster, clusterRank) %>%
  gt() %>%
  text_transform(locations = cells_body(columns = c(headshot_url)),
                 fn = function(x) {
                   web_image(url = x, height = 40)
                 }) %>%
  text_transform(locations = cells_body(columns = c(team_logo_espn)),
                 fn = function(x) {
                   web_image(url = x, height = 30)
                 }) %>%
  fmt_currency(columns = `Cash Spent ▾`, currency = "USD") %>%
  cols_label(headshot_url = "",
             Player.x = "Player",
             team_abbr = "Team",
             team_logo_espn = "",
             `Cash Spent ▾` = "Annual Salary",
             cluster = "Cluster",
             clusterRank = "Rank in Cluster") %>%
  tab_header(title = md("**Top 25 Most Cost-Efficient (Undervalued) WRs per Cluster**"),
             subtitle = "Ranked by Value Score ((Yards + RZ TGTs - Drop Rate) / Annual Salary)") %>%
  tab_options(table.font.size = px(13),
              heading.title.font.size = px(18),
              heading.subtitle.font.size = px(14))

##Create a top 50 most undervalued players table
##I will be following through with the table that penalizes players for drop rate
##I believe including this statistic reflects true value in WR and this brings in
##inefficiencies that reflect true value
##slicing the top 50 WRs across all 4 clusters
##global rank of top 50 undervalued WRs
Top50WRvalue <- WRrankedB %>%
  arrange(desc(Value)) %>%
  slice_head(n = 50)

##ranking top 50
Top50WRvalue <- Top50WRvalue %>%
  mutate(GlobalRank = row_number())

##create the table
Top50WRvalue %>%
  select(headshot_url, Player.x, team_name, team_logo_espn, `Cash Spent ▾`, WRtype, 
         cluster, Value, GlobalRank) %>%
  gt() %>%
  text_transform(
    locations = cells_body(columns = c(headshot_url)),
    fn = function(x) { web_image(url = x, height = 40) }
  ) %>%
  text_transform(
    locations = cells_body(columns = c(team_logo_espn)),
    fn = function(x) { web_image(url = x, height = 30) }
  ) %>%
  fmt_currency(columns = `Cash Spent ▾`, currency = "USD") %>%
  fmt_number(columns = Value, decimals = 2) %>%
  cols_label(
    headshot_url = "",
    Player.x = "Player",
    team_name = "Team",
    team_logo_espn = "",
    WRtype = "WR Profile",
    `Cash Spent ▾` = "Annual Salary",
    Value = "Value Score",
    GlobalRank = "Undervalued Rank"
  ) %>%
  tab_header(
    title = md("**Top 50 Most Undervalued WRs in the NFL**"),
    subtitle = "Based on Value Score ((Yards + RZ TGT - Drop Rate) / Annual Salary)"
  ) %>%
  tab_options(
    table.font.size = px(13),
    heading.title.font.size = px(18),
    heading.subtitle.font.size = px(14)
  )

##WR who shifted on the list with the incorporation of drop rate
##exposes players who have high volume but the drops can quietly kill drives 
##despite having big yardage 

##export Clustering for forecasting script
##Provided the exported file with Project Submission
ClusterResults <- WRcomp %>%
  select(Player, cluster, WRtype)

#write_xlsx(ClusterResults, "ClusterResults.xlsx")