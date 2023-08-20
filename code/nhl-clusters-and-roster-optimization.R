# NHL Clustering Analysis and Roster Optimization
# Laken Rivet

# load necessary libraries
library(tidyverse)
library(rvest)
library(cluster)
library(janitor)
library(openxlsx)
library(readxl)
library(corrplot)
library(factoextra)
library(FactoMineR)
library(NbClust)
library(RobStatTM)
library(DescTools)
library(gghighlight)
library(lpSolve)

# turn off scientific notation for larger numbers
options(scipen = 999)

# DATA ACQUISITION AND CLEANING

# Scraping 2020-2023 Skaters Basic Stats

target_pages <- c("https://www.hockey-reference.com/leagues/NHL_2021_skaters.html",
                  "https://www.hockey-reference.com/leagues/NHL_2022_skaters.html",
                  "https://www.hockey-reference.com/leagues/NHL_2023_skaters.html")

season <- c("2020-2021", "2021-2022", "2022-2023")

year = 0

basic_stats <- data.frame(t(seq(1,28,1)))

for (i in target_pages) {

  year = year + 1

  url <- read_html(i)

  values <- url %>%
    html_nodes(".poptip.right , .right+ .poptip , .poptip.right , .center+ .poptip , td , .left+ .poptip") %>%
    html_text()

  for (i in seq(1, length(split(values, ceiling(seq_along(values)/27))))) {
    div <- split(values, ceiling(seq_along(values)/27))
    if (i == 1) {
      vars <- div[i]
      vars <- append(unlist(vars), "Season")
      colnames(basic_stats) <- c(vars)
    }

    else if (i > 1) {
      player <- div[i]
      player <- append(unlist(player), season[year])
      player <- t(data.frame(player))
      colnames(player) <- c(vars)
      basic_stats <- rbind(basic_stats, player)
    }
  }
}

# Scraping 2020-2023 Skaters Advanced Stats

target_pages <- c("https://www.hockey-reference.com/leagues/NHL_2021_skaters-advanced.html",
                  "https://www.hockey-reference.com/leagues/NHL_2022_skaters-advanced.html",
                  "https://www.hockey-reference.com/leagues/NHL_2023_skaters-advanced.html")

season <- c("2020-2021", "2021-2022", "2022-2023")

year = 0

advanced_stats <- data.frame(t(seq(1,26,1)))

for (i in target_pages) {

  year = year + 1

  url <- read_html(i)

  values <- url %>%
    html_nodes(".poptip.right , .right+ .poptip , .poptip.right , .center+ .poptip , td , .left+ .poptip") %>%
    html_text()

  for (i in seq(1, length(split(values, ceiling(seq_along(values)/25))))) {
    div <- split(values, ceiling(seq_along(values)/25))
    if (i == 1) {
      vars <- div[i]
      vars <- append(unlist(vars), "Season")
      colnames(advanced_stats) <- c(vars)
    }

    else if (i > 1) {
      player <- div[i]
      player <- append(unlist(player), season[year])
      player <- t(data.frame(player))
      colnames(player) <- c(vars)
      advanced_stats <- rbind(advanced_stats, player)
    }
  }
}

# clean column names in basic stats
basic_stats <- basic_stats %>%
  clean_names() %>%
  rename(plusminus = x,
         ev_g = ev,
         pp_g = pp,
         sh_g = sh,
         gw_g = gw,
         ev_a = ev_2,
         pp_a = pp_2,
         sh_a = sh_2) %>%
  filter(!row_number() %in% c(1)) %>%
  mutate_at(c('s_percent','fo_percent'), ~na_if(.,'')) %>%
  separate(atoi, c("MINUTES", "SECONDS"), ":")

# identify columns to convert to numeric
colstoconvert <- c(2, 5:28)

# apply parse number to all specified columns (convert to numeric)
basic_stats[ , colstoconvert] <- apply(basic_stats[ ,colstoconvert], 2,
                                   function(x) parse_number(x))

# combine minutes and seconds to re-create avg toi in minutes
basic_stats <- basic_stats %>%
  mutate(atoi = MINUTES + (SECONDS/60)) %>%
  select(-c(MINUTES, SECONDS))

# clean advanced stats
advanced_stats <- advanced_stats %>%
  clean_names() %>%
  rename(e_plusminus = e) %>%
  filter(!row_number() %in% c(1)) %>%
  mutate_at(c('s_att', 'thru_percent', 'ff_percent_rel', 'pdo', 'oi_sh_percent'),
            ~na_if(.,'')) %>%
  separate(toi_60, c("MINUTES_60", "SECONDS_60"), ":") %>%
  separate(toi_ev, c("MINUTES_ev", "SECONDS_ev"), ":")

# identify columns to convert to numeric
colstoconvert <- c(2, 5:27)

# apply parse number to all specified columns (convert to numeric)
advanced_stats[ , colstoconvert] <- apply(advanced_stats[ ,colstoconvert], 2,
                                       function(x) parse_number(x))

# combine minutes and seconds to re-create toi_60 and toi_ev in minutes
advanced_stats <- advanced_stats %>%
  mutate(toi_60 = MINUTES_60 + (SECONDS_60/60)) %>%
  mutate(toi_ev = MINUTES_ev + (SECONDS_ev/60)) %>%
  select(-c(MINUTES_60, SECONDS_60, MINUTES_ev, SECONDS_ev))

# change pos to fwd and dmen for simpler merging
basic_stats$gen_pos[!grepl("D", basic_stats$pos)] <- "F"
basic_stats$gen_pos[grepl("D", basic_stats$pos)] <- "D"
advanced_stats$gen_pos[!grepl("D", advanced_stats$pos)] <- "F"
advanced_stats$gen_pos[grepl("D", advanced_stats$pos)] <- "D"

# merge data sets to create one master data set
all_stats <- full_join(basic_stats, advanced_stats, by = c('player', 'age',
                                                          'tm', 'pos',
                                                          'season', 'gp', 'gen_pos'))

# identify players with bad merge
problem <- all_stats[is.na(all_stats$g) | is.na(all_stats$cf), ]

# write for loop so pos for players is the same across both data sets
for (i in unique(problem$player)) {
  advanced_stats$pos[advanced_stats$player == i] <-
    basic_stats$pos[basic_stats$player == i]
}

# re-merge fixed data sets to create one master data set
all_stats <- full_join(basic_stats, advanced_stats, by = c('player', 'age',
                                                           'tm', 'pos',
                                                           'season', 'gp', 'gen_pos'))

# write final data set into xlsx file
write.xlsx(all_stats, file = "processed_data/all_stats.xlsx")

# read in data collected in earlier code
all_stats <- read_excel("processed_data/all_stats.xlsx")

# divide data into forwards and defense men
fwds <- all_stats[all_stats$gen_pos == "F", ]
dmen <- all_stats[all_stats$gen_pos == "D", ]

# create histogram of games played to determine cutoff point [FORWARDS]
f_gpdist <- ggplot(data = fwds, aes(x = gp)) +
  geom_histogram(color = "black", fill = "grey", alpha = 0.5) +
  geom_vline(xintercept = quantile(fwds$gp, probs = 0.25), color = "red", linetype = "dashed") +
  labs(title = "Distribution of Games Played - Forwards", x = "Games Played", y = "Count") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", linewidth = 0.25)) 

# display visualization [FORWARDS]
f_gpdist

# save visualization
ggsave(filename = "output/f_gpdist.pdf")

# create density plot of games played to determine cutoff point [FORWARDS]
f_gpdens <- ggplot(data = fwds, aes(x = gp)) +
  geom_density(alpha = 0.5, fill = "grey") +
  geom_vline(xintercept = quantile(fwds$gp, probs = 0.25), color = "red", linetype = "dashed") +
  labs(title = "Density Plot of Games Played - Forwards", x = "Games Played", y = "Density") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", linewidth = 0.25)) 

# display visualization [FORWARDS]
f_gpdens

# save visualization
ggsave(filename = "output/f_gpdens.pdf")

# calculate summary statistics of games played [FORWARDS]
summary(fwds$gp)

# reduce data set to players with more than 15 games played in a season
# retains 74.7% of data set [FORWARDS]
fwds <- fwds %>%
  filter(gp > 15)

# create histogram of games played to determine cutoff point [DEFENSEMEN]
d_gpdist <- ggplot(data = dmen, aes(x = gp)) +
  geom_histogram(color = "black", fill = "grey", alpha = 0.5) +
  geom_vline(xintercept = quantile(dmen$gp, probs = 0.25), color = "red", linetype = "dashed") +
  labs(title = "Distribution of Games Played - Defensemen", x = "Games Played", y = "Count") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", linewidth = 0.25)) 

# display visualization [DEFENSEMEN]
d_gpdist

# save visualization
ggsave(filename = "output/d_gpdist.pdf")

# create density plot of games played to determine cutoff point [DEFENSEMEN]
d_gpdens <- ggplot(data = dmen, aes(x = gp)) +
  geom_density(alpha = 0.5, fill = "grey") +
  geom_vline(xintercept = quantile(dmen$gp, probs = 0.25), color = "red", linetype = "dashed") +
  labs(title = "Density Plot of Games Played - Defensemen", x = "Games Played", y = "Density") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", linewidth = 0.25)) 

# display visualization [DEFENSEMEN]
d_gpdens

# save visualization
ggsave(filename = "output/d_gpdens.pdf")

# calculate summary statistics of games played [DEFENSEMEN]
summary(dmen$gp)

# reduce data set to players with more than or equal to 13 games played in a 
# season retains 75.7% of data set [DEFENSEMEN]
dmen <- dmen %>%
  filter(gp >= 13)

# drop faceoff related variables from defensemen data set
dmen <- dmen %>%
  select(-c(24:26))

# check for NA values in dmen
missingdmen <- dmen[!complete.cases(dmen), ]
# only missing values in s_att and thru_percent and only 2020-2021
# only TOT entries

# check for NA values in fwds
missingfwds <- fwds[!complete.cases(fwds), ]
# missing fo_percent, s_att, and thru_percent

# only missing fo_percent for zero fol and fow
# impute zeros for missing fo_percent
fwds$fo_percent[is.na(fwds$fo_percent)] <- 0.0

# write for loop to copy s_att and thru_percent from other entries in 2020-2021 
# and place in TOT row for forwards
for (i in seq(1, nrow(missingfwds))) {
  values <- fwds %>%
    filter(player == missingfwds$player[i] & season == "2020-2021") %>%
    summarise(s_att = sum(s_att, na.rm = TRUE), thru_percent = mean(thru_percent, na.rm = TRUE))
  
  fwds['s_att'][fwds['player']==missingfwds$player[i] & is.na(fwds['s_att'])] <- as.double(values[1])
  
  fwds['thru_percent'][fwds['player']==missingfwds$player[i] & is.na(fwds['thru_percent'])] <- as.double(values[2])
}

# check to make sure no more NA values in fwds
fwds[!complete.cases(fwds), ]

# drop weird forward (Alexander Barabanov 2020-2021)
fwds <- fwds %>%
  filter(!(player == "Alexander Barabanov" & season == "2020-2021"))

# final check
fwds[!complete.cases(fwds), ]

# repeat process for defensemen
for (i in seq(1, nrow(missingdmen))) {
  values <- dmen %>%
    filter(player == missingdmen$player[i] & season == "2020-2021") %>%
    summarise(s_att = sum(s_att, na.rm = TRUE), thru_percent = mean(thru_percent, na.rm = TRUE))
  
  dmen['s_att'][dmen['player']==missingdmen$player[i] & is.na(dmen['s_att'])] <- as.double(values[1])
  
  dmen['thru_percent'][dmen['player']==missingdmen$player[i] & is.na(dmen['thru_percent'])] <- as.double(values[2])
}

# check to make sure no more NA values in dmen
dmen[!complete.cases(dmen), ]

# drop weird dmen (Greg Pateryn and Jonas Siegenthaler 2020-2021)
dmen <- dmen %>%
  filter(!(player == "Greg Pateryn" & season == "2020-2021")) %>%
  filter(!(player == "Jonas Siegenthaler" & season == "2020-2021"))

# final check
dmen[!complete.cases(dmen), ]

# atoi and toi_60 are the same variable so drop one
fwds <- fwds %>%
  select(-c(toi_60))
dmen <- dmen %>%
  select(-c(toi_60))

# create standardized variables for forwards
fwds$stand_g <- fwds$g / fwds$toi
fwds$stand_a <- fwds$a / fwds$toi
fwds$stand_pts <- fwds$pts / fwds$toi
fwds$stand_plusminus <- fwds$plusminus / fwds$toi
fwds$stand_pim <- fwds$pim / fwds$toi
fwds$stand_ps <- fwds$ps / fwds$toi
fwds$stand_ev_g <- fwds$ev_g / fwds$toi
fwds$stand_pp_g <- fwds$pp_g / fwds$toi
fwds$stand_sh_g <- fwds$sh_g / fwds$toi
fwds$stand_gw_g <- fwds$gw_g / fwds$toi
fwds$stand_ev_a <- fwds$ev_a / fwds$toi
fwds$stand_pp_a <- fwds$pp_a / fwds$toi
fwds$stand_sh_a <- fwds$sh_a / fwds$toi
fwds$stand_s <- fwds$s / fwds$toi
fwds$stand_blk <- fwds$blk / fwds$toi
fwds$stand_hit <- fwds$hit / fwds$toi
fwds$stand_fow <- fwds$fow / fwds$toi
fwds$stand_fol <- fwds$fol / fwds$toi
fwds$stand_cf <- fwds$cf / fwds$toi
fwds$stand_ca <- fwds$ca / fwds$toi
fwds$stand_ff <- fwds$ff / fwds$toi
fwds$stand_fa <- fwds$fa / fwds$toi
fwds$stand_tk <- fwds$tk / fwds$toi
fwds$stand_gv <- fwds$gv / fwds$toi
fwds$stand_e_plusminus <- fwds$e_plusminus / fwds$toi
fwds$stand_s_att <- fwds$s_att / fwds$toi

# create standardized variables for dmen
dmen$stand_g <- dmen$g / dmen$toi
dmen$stand_a <- dmen$a / dmen$toi
dmen$stand_pts <- dmen$pts / dmen$toi
dmen$stand_plusminus <- dmen$plusminus / dmen$toi
dmen$stand_pim <- dmen$pim / dmen$toi
dmen$stand_ps <- dmen$ps / dmen$toi
dmen$stand_ev_g <- dmen$ev_g / dmen$toi
dmen$stand_pp_g <- dmen$pp_g / dmen$toi
dmen$stand_sh_g <- dmen$sh_g / dmen$toi
dmen$stand_gw_g <- dmen$gw_g / dmen$toi
dmen$stand_ev_a <- dmen$ev_a / dmen$toi
dmen$stand_pp_a <- dmen$pp_a / dmen$toi
dmen$stand_sh_a <- dmen$sh_a / dmen$toi
dmen$stand_s <- dmen$s / dmen$toi
dmen$stand_blk <- dmen$blk / dmen$toi
dmen$stand_hit <- dmen$hit / dmen$toi
dmen$stand_cf <- dmen$cf / dmen$toi
dmen$stand_ca <- dmen$ca / dmen$toi
dmen$stand_ff <- dmen$ff / dmen$toi
dmen$stand_fa <- dmen$fa / dmen$toi
dmen$stand_tk <- dmen$tk / dmen$toi
dmen$stand_gv <- dmen$gv / dmen$toi
dmen$stand_e_plusminus <- dmen$e_plusminus / dmen$toi
dmen$stand_s_att <- dmen$s_att / dmen$toi

# create and save correlation plot to examine linear relationships (forwards)
fwds.cor <- cor(fwds[ ,c(20, 26, 28, 32, 33, 36:42, 47:74)])

pdf(file = "output/fwds_cor.pdf")

corrplot(fwds.cor, type = 'lower', tl.col = 'black',
         cl.ratio = 0.2, tl.srt = 45)

dev.off()

# create and save correlation plot to examine linear relationships (defensemen)
dmen.cor <- cor(dmen[ ,c(20, 25, 29, 30, 33:39, 44:69)])

pdf(file = "output/dmen_cor.pdf")

corrplot(dmen.cor, type = 'lower', tl.col = 'black',
         cl.ratio = 0.2, tl.srt = 45)

dev.off()

# created scaled data sets to input in PCA
fwds_data <- fwds %>%
  select(c(20, 26, 28, 32, 33, 36:42, 47:74)) %>%
  scale()

dmen_data <- dmen %>%
  select(c(20, 25, 29, 30, 33:39, 44:69)) %>%
  scale()

# perform PCA 
fwds_pca <- prcomp(fwds_data)
fwds_pca_summary <- summary(fwds_pca)

dmen_pca <- prcomp(dmen_data)
dmen_pca_summary <- summary(dmen_pca)

# create scree plot for forwards 
# first perform calculations manually
var_explained <- tibble(pc = seq(1,40,1),
                        var = fwds_pca$sdev^2 / sum(fwds_pca$sdev^2),
                        prop = fwds_pca_summary$importance[3,])

# then use get_eigenvalue() command to get eigens 
eigens <- get_eigenvalue(fwds_pca)
eigens$pc <- seq(1,40,1)

# compare plots for same results
fwds_scree_var <- ggplot(data = var_explained, aes(x = pc, y = var)) + 
  geom_line() +
  geom_point() +
  labs(x = "Principal Component", y = "Variance Explained", title = "Scree Plot - Forwards") +
  xlim(0, 20) +
  scale_y_continuous(limits = c(0, 0.5),
                     labels = scales::percent_format(accuracy = 5L)) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", linewidth = 0.25)) 
  
fwds_scree_var

fwds_scree_eigen <- ggplot(data = eigens, aes(x = pc, y = eigenvalue)) + 
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(x = "Principal Component", y = "Eigenvalue", title = "Scree Plot - Forwards") +
  theme_minimal() + 
  xlim(0, 15) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", linewidth = 0.25)) 


fwds_scree_eigen

# save visualization
ggsave(filename = "output/fwds_scree_eigen.pdf")

# forwards cutoff components at 10, 79.2% cumulative variance explained

# create scree plot for defensemen using get_eigenvalue()
eigens <- get_eigenvalue(dmen_pca)
eigens$pc <- seq(1,37,1)

dmen_scree_eigen <- ggplot(data = eigens, aes(x = pc, y = eigenvalue)) + 
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(x = "Principal Component", y = "Eigenvalue", title = "Scree Plot - Defensemen") +
  theme_minimal() + 
  xlim(0, 15) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", linewidth = 0.25)) 


dmen_scree_eigen

# save visualization
ggsave(filename = "output/dmen_scree_eigen.pdf")

# defensemen cutoff components at 10, 78.8% cumulative variance explained

# set seed for repeatable clustering results
set.seed(123)

# perform PCA but specify only 10 components 
fwds_pca <- prcomp(fwds_data, center = FALSE, scale = FALSE, rank. = 10)
fwds_pca_results <- fwds_pca$x

dmen_pca <- prcomp(dmen_data, center = FALSE, scale = FALSE, rank. = 10)
dmen_pca_results <- dmen_pca$x

# determine ideal number of clusters
# sse (elbow method)
# set max number of clusters
max_c <- 15

# create vector to hold sse values
sse_f <- c()
sse_d <- c()

# for loop to iterate through number of clusters and calculate sse values
# for forwards first
for (c in 1:max_c) {
  algo_k <- kmeans(fwds_pca_results, centers = c, nstart = 22, iter.max = 20) # k-means algorithm
  sse_f <- c(sse_f, algo_k$tot.withinss) # get SSE
} 

# now defensemen
for (c in 1:max_c) {
  algo_k <- kmeans(dmen_pca_results, centers = c, nstart = 22, iter.max = 20) # k-means algorithm
  sse_d <- c(sse_d, algo_k$tot.withinss) # get SSE
} 

# create tibble with relevant info for visualization
sse_results <- tibble(cluster = 1:max_c,
                      fwds = sse_f,
                      dmen = sse_d)

# now create plots to visualize results - forwards
fwds_sse_plot <- ggplot(data = sse_results, aes(x = cluster, y = fwds)) +
  geom_point() +
  geom_line() +
  labs(title = "Total Within Sum of Squares vs Number of Clusters - Forwards",
       x = "Number of Clusters", y = "Total Within Sum of Squares") +
  scale_x_continuous(breaks = seq(1, max_c, 1)) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", linewidth = 0.25)) 

fwds_sse_plot
# check to make sure other method of wws looks similar
# fviz_nbclust(fwds_pca_results, FUNcluster = kmeans, k.max = max_c, method = "wss") 

# now create plots to visualize results - defensemen
dmen_sse_plot <- ggplot(data = sse_results, aes(x = cluster, y = dmen)) +
  geom_point() +
  geom_line() +
  labs(title = "Total Within Sum of Squares vs Number of Clusters - Defensemen",
       x = "Number of Clusters", y = "Total Within Sum of Squares") +
  scale_x_continuous(breaks = seq(1, max_c, 1)) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", linewidth = 0.25)) 

dmen_sse_plot
# check to make sure other method of wws looks similar
#fviz_nbclust(dmen_pca_results, FUNcluster = kmeans, k.max = max_c, method = "wss")

# alternative methods for determining number of clusters
# Silhouette Width
#fviz_nbclust(fwds_pca_results, FUNcluster = kmeans, k.max = max_c, method = "silhouette")
#fviz_nbclust(dmen_pca_results, FUNcluster = kmeans, k.max = max_c, method = "silhouette")

# Gap Statistic
# fviz_nbclust(fwds_pca_results, FUNcluster = kmeans, k.max = max_c, method = "gap_stat",
#             nboot = 500, iter.max = 50)
# fviz_nbclust(dmen_pca_results, FUNcluster = kmeans, k.max = max_c, method = "gap_stat",
#             nboot = 500, iter.max = 50)

# check how many clusters hierarchical makes
fwd.res.pca <- PCA(fwds_data, scale.unit = FALSE, ncp = 10, graph = FALSE)
fwd.res.hcpc <- HCPC(fwd.res.pca, graph = FALSE)

# performing of hierarchical clustering commented out to save computing power on repetitions

# fviz_dend(fwd.res.hcpc, 
#           cex = 0.2,                     # Label size 
#           rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
#           labels_track_height = 0.8      # Augment the room for labels
# )

# naturally clustered forwards into 3 groups

# check how many clusters hierarchical makes
dmen.res.pca <- PCA(dmen_data, scale.unit = FALSE, ncp = 10, graph = FALSE)
dmen.res.hcpc <- HCPC(dmen.res.pca, graph = FALSE)

# fviz_dend(dmen.res.hcpc, 
#           rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
#           labels_track_height = 0.8      # Augment the room for labels
# )

# naturally clustered defensemen into 3 groups

# using NbClust to provide best number of clusters
# NbClust(fwds_pca_results, method = 'complete', index = 'all')$Best.nc
# the majority proposed 2 as the best number of clusters

# NbClust(dmen_pca_results, method = 'complete', index = 'all')$Best.nc
# the largest proposed 2 as the best number of clusters, 4 close second, 3 close third

# perform k-means clustering on data sets
set.seed(10)
k_f <- 4
k_d <- 3

kmeans_fwds <- kmeans(fwds_pca_results, centers = k_f, nstart = 25)
kmeans_dmen <- kmeans(dmen_pca_results, centers = k_d, nstart = 25)

# add clustering results back to original data frames
fwds$kmeans_cluster <- kmeans_fwds$cluster
dmen$kmeans_cluster <- kmeans_dmen$cluster

# cluster makeup (kmeans)
fwds %>%
  group_by(kmeans_cluster) %>%
  count()

fwds %>%
  group_by(kmeans_cluster) %>%
  summarize(age = mean(age),
            goals = mean(g),
            pp_goals = mean(pp_g),
            assists = mean(a),
            pts = mean(pts),
            fow = mean(fow),
            fol = mean(fol),
            pm = mean(plusminus),
            e_pm = mean(e_plusminus),
            pim = mean(pim),
            toi = mean(toi),
            blk = mean(blk),
            hit = mean(hit),
            shots = mean(s),
            s_percent = mean(s_percent),
            tk = mean(tk),
            gv = mean(gv))

# create individual cluster visualizations and all cluster visualization
# for forwards

fwds_data <- as.data.frame(fwds_data)
fwds_data$cluster <- kmeans_fwds$cluster
km_centers <- aggregate(x = fwds_data, by = list(fwds_data$cluster), FUN = mean)

km_centers$cluster <- c('Cluster 1', 'Cluster 2', 'Cluster 3', 'Cluster 4')          

km_centers <- km_centers %>%
  select(-c(Group.1)) %>%
  rename('S%' = 's_percent', 'FO%' = 'fo_percent', 'ATOI' = 'atoi',
         'CF%' = 'cf_percent', 'CF%R' = 'cf_percent_rel', 'FF%' = 'ff_percent',
         'FF%R' = 'ff_percent_rel', 'OISH%' = 'oi_sh_percent', 'OISV%' = 'oi_sv_percent',
         'PDO' = 'pdo', 'OZ%' = 'o_zs_percent', 'DZ%' = 'd_zs_percent', 
         'THRU%' = 'thru_percent', 'EVTOI' = 'toi_ev', 'G' = 'stand_g',
         'A' = 'stand_a', 'PTS' = 'stand_pts', 'P/M' = 'stand_plusminus',
         'PIM' = 'stand_pim', 'PS' = 'stand_ps', 'EVG' = 'stand_ev_g',
         'PPG' = 'stand_pp_g', 'SHG' = 'stand_sh_g', 'GWG' = 'stand_gw_g',
         'EVA' = 'stand_ev_a', 'PPA' = 'stand_pp_a', 'SHA' = 'stand_sh_a',
         'S' = 'stand_s', 'BLKS' = 'stand_blk', 'HITS' = 'stand_hit', 
         'FOW' = 'stand_fow', 'FOL' = 'stand_fol', 'CF' = 'stand_cf',
         'CA' = 'stand_ca', 'FF' = 'stand_ff', 'FA' = 'stand_fa', 'TK' = 'stand_tk',
         'GV' = 'stand_gv', 'EP/M' = 'stand_e_plusminus', 'SATT' = 'stand_s_att')

km_centers <- km_centers %>%
  pivot_longer(!cluster, names_to = 'feature', values_to = 'z_val')

km_centers %>%
  ggplot(aes(x=feature, y=z_val, color=cluster)) +
  geom_point(color="#232D4B") + # color points
  gghighlight(cluster=='Cluster 1', use_direct_label = FALSE) + # highlight cluster 1
  labs(x = "Predictor", y = "Cluster Center",  # axis labels
       title = "Player Cluster Makeups", # plot title
       subtitle = "Cluster 1") +  # plot subtitle
  theme_minimal() + # add themes
  theme(legend.position = "none", # manually adjust themes
        axis.text.x = element_text(angle=45, size=10),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

km_centers %>%
  ggplot(aes(x=feature, y=z_val, color=cluster)) +
  geom_point(color="#232D4B") + # color points
  gghighlight(cluster=='Cluster 2', use_direct_label = FALSE) + # highlight cluster 1
  labs(x = "Predictor", y = "Cluster Center",  # axis labels
       title = "Player Cluster Makeups", # plot title
       subtitle = "Cluster 2") +  # plot subtitle
  theme_minimal() + # add themes
  theme(legend.position = "none", # manually adjust themes
        axis.text.x = element_text(angle=45, size=10),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

km_centers %>%
  ggplot(aes(x=feature, y=z_val, color=cluster)) +
  geom_point(color="#232D4B") + # color points
  gghighlight(cluster=='Cluster 3', use_direct_label = FALSE) + # highlight cluster 1
  labs(x = "Predictor", y = "Cluster Center",  # axis labels
       title = "Player Cluster Makeups", # plot title
       subtitle = "Cluster 3") +  # plot subtitle
  theme_minimal() + # add themes
  theme(legend.position = "none", # manually adjust themes
        axis.text.x = element_text(angle=45, size=10),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

km_centers %>%
  ggplot(aes(x=feature, y=z_val, color=cluster)) +
  geom_point(color="#232D4B") + # color points
  gghighlight(cluster=='Cluster 4', use_direct_label = FALSE) + # highlight cluster 1
  labs(x = "Predictor", y = "Cluster Center",  # axis labels
       title = "Player Cluster Makeups", # plot title
       subtitle = "Cluster 4") +  # plot subtitle
  theme_minimal() + # add themes
  theme(legend.position = "none", # manually adjust themes
        axis.text.x = element_text(angle=45, size=10),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

km_centers %>% 
  ggplot(aes(x=feature, y=z_val, color=cluster)) + 
  geom_point() + # plot points
  scale_color_brewer(palette="Set1") + # color points
  gghighlight(use_direct_label = FALSE) + # highlight each cluster
  facet_wrap(~ cluster, ncol=2) + # create separate plots for each cluster
  labs(x = "Predictor", y = "Cluster Center", 
       title = "Player Cluster Makeups") + 
  theme_minimal() + 
  theme(legend.position = "none", strip.text = element_text(face='bold'),
        axis.text.x = element_text(angle=90, size=9), # alter axis text
        panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5))

# save visualization with all clusters for forwards
ggsave(filename = "output/fwds_clusters.pdf")

# create individual cluster visualizations and all cluster visualization
# for defensemen

dmen_data <- as.data.frame(dmen_data)
dmen_data$cluster <- kmeans_dmen$cluster
dmen_km_centers <- aggregate(x = dmen_data, by = list(dmen_data$cluster), FUN = mean)

dmen_km_centers$cluster <- c('Cluster 1', 'Cluster 2', 'Cluster 3')          

dmen_km_centers <- dmen_km_centers %>%
  select(-c(Group.1)) %>%
  rename('S%' = 's_percent', 'ATOI' = 'atoi',
         'CF%' = 'cf_percent', 'CF%R' = 'cf_percent_rel', 'FF%' = 'ff_percent',
         'FF%R' = 'ff_percent_rel', 'OISH%' = 'oi_sh_percent', 'OISV%' = 'oi_sv_percent',
         'PDO' = 'pdo', 'OZ%' = 'o_zs_percent', 'DZ%' = 'd_zs_percent', 
         'THRU%' = 'thru_percent', 'EVTOI' = 'toi_ev', 'G' = 'stand_g',
         'A' = 'stand_a', 'PTS' = 'stand_pts', 'P/M' = 'stand_plusminus',
         'PIM' = 'stand_pim', 'PS' = 'stand_ps', 'EVG' = 'stand_ev_g',
         'PPG' = 'stand_pp_g', 'SHG' = 'stand_sh_g', 'GWG' = 'stand_gw_g',
         'EVA' = 'stand_ev_a', 'PPA' = 'stand_pp_a', 'SHA' = 'stand_sh_a',
         'S' = 'stand_s', 'BLKS' = 'stand_blk', 'HITS' = 'stand_hit', 
         'CF' = 'stand_cf', 'CA' = 'stand_ca', 'FF' = 'stand_ff', 
         'FA' = 'stand_fa', 'TK' = 'stand_tk', 'GV' = 'stand_gv', 
         'EP/M' = 'stand_e_plusminus', 'SATT' = 'stand_s_att')

dmen_km_centers <- dmen_km_centers %>%
  pivot_longer(!cluster, names_to = 'feature', values_to = 'z_val')

dmen_km_centers %>% 
  ggplot(aes(x=feature, y=z_val, color=cluster)) + 
  geom_point() + # plot points
  scale_color_brewer(palette="Set1") + # color points
  gghighlight(use_direct_label = FALSE) + # highlight each cluster
  facet_wrap(~ cluster, ncol=2) + # create separate plots for each cluster
  labs(x = "Predictor", y = "Cluster Center", 
       title = "Player Cluster Makeups") + 
  theme_minimal() + 
  theme(legend.position = "none", strip.text = element_text(face='bold'),
        axis.text.x = element_text(angle=90, size=9), # alter axis text
        panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5))

# save visualization with all clusters for forwards
ggsave(filename = "output/dmen_clusters.pdf")

# examine notable players in forward clusters 
fwds_clust_one <- fwds %>%
  filter(kmeans_cluster == 1) %>%
  arrange(., desc(g))

fwds_clust_two <- fwds %>%
  filter(kmeans_cluster == 2) %>%
  arrange(., desc(g))

fwds_clust_three <- fwds %>%
  filter(kmeans_cluster == 3) %>%
  arrange(., desc(g))

fwds_clust_four <- fwds %>%
  filter(kmeans_cluster == 4) %>%
  arrange(., desc(g))

# create dfs with number of positions in each cluster
fwds_counts <- fwds %>%
  group_by(kmeans_cluster, pos) %>%
  count() %>%
  pivot_wider(., names_from = pos, values_from = n)

# create df with total number of players in each cluster
fwds_totals <- fwds %>%
  group_by(kmeans_cluster) %>%
  count()

# merge totals with number of positions in each clustere
fwds_counts <- left_join(fwds_counts, fwds_totals, by = 'kmeans_cluster')

# rename columns for output
fwds_counts <- fwds_counts %>%
  rename('Total' = 'n',
         'Cluster' = 'kmeans_cluster')

# change NAs to zeroes
fwds_counts$W[is.na(fwds_counts$W)] <- 0

# calculate percentages of total - not using anymore
# fwds_counts$C <- fwds_counts$C / fwds_counts$Total * 100
# fwds_counts$F <- fwds_counts$F / fwds_counts$Total * 100
# fwds_counts$LW <- fwds_counts$LW / fwds_counts$Total * 100
# fwds_counts$RW <- fwds_counts$RW / fwds_counts$Total * 100
# fwds_counts$W <- fwds_counts$W / fwds_counts$Total * 100
#
# fwds_counts <- as.data.frame(fwds_counts)
#
# fwds_counts <- fwds_counts %>%
#   mutate(C = paste0(round(C, 1), "%"),
#          F = paste0(round(F, 1), "%"),
#          LW = paste0(round(LW, 1), "%"),
#          RW = paste0(round(RW, 1), "%"),
#          W = paste0(round(W, 1), "%"))

# write counts out to file for report
write.xlsx(fwds_counts, "output/fwds_count.xlsx")

# perform chi squared test to check if position had impact on clustering
chisq.test(tabyl(fwds, kmeans_cluster, pos))

# examine notable players in defensemen clusters'
# not creating same table for dmen as they all play left or right defense,
# not distinguishable positions
dmen_clust_one <- dmen %>%
  filter(kmeans_cluster == 1) %>%
  arrange(., desc(g))

dmen_clust_two <- dmen %>%
  filter(kmeans_cluster == 2) %>%
  arrange(., desc(g))

dmen_clust_three <- dmen %>%
  filter(kmeans_cluster == 3) %>%
  arrange(., desc(g))

# attach names to clusters
fwds$cluster_name <- "Franchise Player"
fwds$cluster_name[fwds$kmeans_cluster == 2] <- "Clutch Shooter"
fwds$cluster_name[fwds$kmeans_cluster == 3] <- "Defensive Hitter"
fwds$cluster_name[fwds$kmeans_cluster == 4] <- "Shooting Playmaker"

dmen$cluster_name <- "Hybrid Shooter"
dmen$cluster_name[dmen$kmeans_cluster == 2] <- "Point Producer"
dmen$cluster_name[dmen$kmeans_cluster == 3] <- "True Defender"

# create detroit data sets
detroit_fwds <- fwds %>%
  filter(tm == "DET" & season == "2022-2023")

detroit_dmen <- dmen %>%
  filter(tm == "DET" & season == "2022-2023")

# drop bertuzzi and sundqvist (traded) from forwards and select rows
detroit_fwds <- detroit_fwds %>%
  filter(!player == "Tyler Bertuzzi") %>% 
  filter(!player == "Oskar Sundqvist") %>%
  select(c(player, age, pos, gp, g, a, plusminus, pim, s, s_percent, atoi, kmeans_cluster, cluster_name))

# do the same for dmen
detroit_dmen <- detroit_dmen %>%
  filter(!player == "Filip Hronek") %>%
  select(c(player, age, pos, gp, g, a, plusminus, pim, s, s_percent, atoi, kmeans_cluster, cluster_name))

# write out detroit's player information for report
write.xlsx(detroit_fwds, "output/detroit_fwds.xlsx")
write.xlsx(detroit_dmen, "output/detroit_dmen.xlsx")

# write for loop to obtain free agents next season for optimization 
# web pages to identify free agents next season
target_pages <- c("https://www.capfriendly.com/browse/free-agents?stats-season=2023")

# because website only shows 50 players at a time, have to iterate through 7
# different webpages starting at 2
target_pages <- append(target_pages, paste0("https://www.capfriendly.com/browse/free-agents?stats-season=2023&pg=", seq(2,12,1)))

# create list to append with player names
optimize_players <- list()

# write another for loop to crawl through pages and scrape player free agent names
for (i in target_pages) {
  # reading url
  url <- read_html(i)
  # scraping player names only
  player_name <- url %>%
    html_nodes(".left") %>%
    html_text
  # for loop to go through names and split into individual entries in list
  for (i in seq(1, length(player_name), 1)) {
    # divide values into list containing 23 entries (a single player's stats)
    player <- split(player_name, ceiling(seq_along(player_name)/1))
    optimize_players[[length(optimize_players) + 1]] <- player[[i]]
  }
}

# take out of list format for use
optimize_players <- unlist(optimize_players)

# clean player names
optimize_players <- str_split_i(optimize_players, "\\d\\. ", 2)

# create recent season data set to grab clusters and names from
recent_fwds <- fwds %>%
  filter(season == "2022-2023") %>%
  select(-c(fow, fol, fo_percent, stand_fow, stand_fol))

recent_dmen <- dmen %>%
  filter(season == "2022-2023")

recent_season <- rbind(recent_fwds, recent_dmen)

# read in active players data collected in previous project: nhl-team-player-valuation
# https://github.com/lakenrivet/nhl-team-player-valuation/tree/main
# file found in raw_data
active_players <- read.xlsx("raw_data/active_players.xlsx", sheet = 'Forwards')
active_players <- rbind(active_players, read.xlsx("raw_data/active_players.xlsx", 
                                                  sheet = 'Defensemen'))

# create player data set to optimize from
# first take all detroit players
available_players <- active_players %>%
  filter(TEAM == "DET")

# then take all available free agents for next season
available_players <- rbind(available_players, active_players[active_players$PLAYER %in% optimize_players, ])

# remove duplicates
available_players <- available_players %>%
  distinct()

# grab only names and cluster information
join_data <- recent_season[, c('player', 'kmeans_cluster', 'cluster_name')]
join_data <- join_data %>%
  distinct()

# correct some mismatched names 
available_players$PLAYER[available_players$PLAYER == "David Krejci"] <- "David Krejčí"
available_players$PLAYER[available_players$PLAYER == "Dominik Kubalik"] <- "Dominik Kubalík"
available_players$PLAYER[available_players$PLAYER == "Joseph Veleno"] <- "Joe Veleno"
available_players$PLAYER[available_players$PLAYER == "Matt Dumba"] <- "Mathew Dumba"
available_players$PLAYER[available_players$PLAYER == "Evgeni Dadonov"] <- "Evgenii Dadonov"
available_players$PLAYER[available_players$PLAYER == "Tomas Tatar"] <- "Tomáš Tatar"
available_players$PLAYER[available_players$PLAYER == "Fredrik Olofsson"] <- "Frederik Olofsson"

# attach clusters to players by name
available_players <- left_join(available_players, join_data, by = c('PLAYER' = 'player'))

# identify missing players
available_players$PLAYER[is.na(available_players$kmeans_cluster)]

# drop missing players
available_players <- available_players %>%
  filter(!is.na(kmeans_cluster))

### OPTIMIZATION ###

# create detroit roster
detroitroster <- available_players %>% 
  filter(TEAM == "DET")

# arrange dmen in descending p/gp order
detroitroster %>%
  filter(grepl("D", POS)) %>%
  arrange(desc(`P/GP`))

# arrange fwds in descending p/gp order
detroitroster %>%
  filter(!grepl("D", POS)) %>%
  arrange(desc(`P/GP`))

# optimization will require 14 fwds and 7 dmen
# fwds with lowest p/gp are Luff and Czarnik
# dropped them from calculation of avg p/gp, cluster count, and cap hit

detroitroster %>%
  filter(!PLAYER == "Matt Luff") %>%
  filter(!PLAYER == "Austin Czarnik") %>%
  summarise(pgp = sum(`P/GP`))

detroitroster %>%
  filter(!PLAYER == "Matt Luff") %>%
  filter(!PLAYER == "Austin Czarnik") %>%
  group_by(cluster_name) %>%
  count()

detroitroster %>%
  filter(!PLAYER == "Matt Luff") %>%
  filter(!PLAYER == "Austin Czarnik") %>%
  summarise(caphit = sum(CAP.HIT))

### OPTION 1 ###
# evaluate different numerical combinations of player cluster types
# option 1 fwds: 3 franchise players, 2 defensive hitters, 8 clutch shooters, and 1 shooting playmaker
# option 1 dmen: 3 true defenders, 2 point producers, 2 hybrid shooters

# first remove detroit players and transform them
det_available <- available_players %>%
  filter(TEAM == "DET")

det_available <- det_available %>%
  mutate(Det = case_when(grepl("DET", TEAM) ~ 1, !grepl("DET", TEAM) ~ 0)) %>%
  mutate(Defense = case_when(grepl("D", POS) ~ 1, !grepl("D", POS) ~ 0)) %>%
  mutate(Forward = case_when(grepl("D", POS) ~ 0, !grepl("D", POS) ~ 1)) %>%
  mutate(FranchisePlayer = case_when(grepl("Franchise Player", cluster_name) ~ 1, 
                                     !grepl("Franchise Player", cluster_name) ~ 0)) %>%
  mutate(ClutchShooter = case_when(grepl("Clutch Shooter", cluster_name) ~ 1, 
                                   !grepl("ClutchShooter", cluster_name) ~ 0)) %>%
  mutate(DefensiveHitter = case_when(grepl("Defensive Hitter", cluster_name) ~ 1, 
                                     !grepl("Defensive Hitter", cluster_name) ~ 0)) %>%
  mutate(ShootingPlaymaker = case_when(grepl("Shooting Playmaker", cluster_name) ~ 1, 
                                       !grepl("Shooting Playmaker", cluster_name) ~ 0)) %>%
  mutate(TrueDefender = case_when(grepl("True Defender", cluster_name) ~ 1, 
                                  !grepl("True Defender", cluster_name) ~ 0)) %>%
  mutate(PointProducer = case_when(grepl("Point Producer", cluster_name) ~ 1, 
                                   !grepl("Point Producer", cluster_name) ~ 0)) %>%
  mutate(HybridShooter = case_when(grepl("Hybrid Shooter", cluster_name) ~ 1, 
                                   !grepl("Hybrid Shooter", cluster_name) ~ 0))

# now transform all available players 
available_players <- available_players %>%
  filter(!TEAM == "DET") %>% # don't include DET players, already above
  filter(!PLAYER == "Tyler Bertuzzi") %>% # don't want to reacquire bertuzzi
  filter(AGE <= 30) %>% # players less than or 30 years old
  mutate(Det = case_when(grepl("DET", TEAM) ~ 1, !grepl("DET", TEAM) ~ 0)) %>%
  mutate(Defense = case_when(grepl("D", POS) ~ 1, !grepl("D", POS) ~ 0)) %>%
  mutate(Forward = case_when(grepl("D", POS) ~ 0, !grepl("D", POS) ~ 1)) %>%
  mutate(FranchisePlayer = case_when(grepl("Franchise Player", cluster_name) ~ 1, 
                                     !grepl("Franchise Player", cluster_name) ~ 0)) %>%
  mutate(ClutchShooter = case_when(grepl("Clutch Shooter", cluster_name) ~ 1, 
                                     !grepl("ClutchShooter", cluster_name) ~ 0)) %>%
  mutate(DefensiveHitter = case_when(grepl("Defensive Hitter", cluster_name) ~ 1, 
                                     !grepl("Defensive Hitter", cluster_name) ~ 0)) %>%
  mutate(ShootingPlaymaker = case_when(grepl("Shooting Playmaker", cluster_name) ~ 1, 
                                     !grepl("Shooting Playmaker", cluster_name) ~ 0)) %>%
  mutate(TrueDefender = case_when(grepl("True Defender", cluster_name) ~ 1, 
                                     !grepl("True Defender", cluster_name) ~ 0)) %>%
  mutate(PointProducer = case_when(grepl("Point Producer", cluster_name) ~ 1, 
                                     !grepl("Point Producer", cluster_name) ~ 0)) %>%
  mutate(HybridShooter = case_when(grepl("Hybrid Shooter", cluster_name) ~ 1, 
                                     !grepl("Hybrid Shooter", cluster_name) ~ 0))

available_players <- rbind(available_players, det_available)

available_players <- available_players %>%
  mutate(PlayerID = seq(1, nrow(available_players), 1))

opt_prob <- available_players[, c("PLAYER", "PlayerID", "P/GP", "Det", 
                                  "Defense", "Forward", "FranchisePlayer", 
                                  "ClutchShooter", "DefensiveHitter",
                                  "ShootingPlaymaker", "TrueDefender",
                                  "PointProducer", "HybridShooter")]

# set up arguments needed for solving the knapsack problem
# objective is to maximize the points per game played
# objective = opt_prob$P/GP
# direction = "max"

# at least 17 det players, 7 dmen, 14 fwds, and above specified cluster numbers
# const.rhs = c(17, 7, 7, 14, 14, 3, 3, 8, 8, 2, 2, 1, 1, 3, 3, 2, 2, 2, 2)  
# const.dir = c(">=",
#               "<=", ">=",
#               "<=", ">=",
#               "<=", ">=",
#               "<=", ">=",
#               "<=", ">=",
#               "<=", ">=",
#               "<=", ">=",
#               "<=", ">=",
#               "<=", ">=")

# this is a knapsack problem so we have binary integers only
# all.bin = TRUE  

# set up the constraint matrix using data for dmen, fwds, and salaries
constraint_matrix <- as.matrix(rbind(opt_prob$Det,
                                     opt_prob$Defense,
                                     opt_prob$Defense,
                                     opt_prob$Forward,
                                     opt_prob$Forward,
                                     opt_prob$FranchisePlayer,
                                     opt_prob$FranchisePlayer,
                                     opt_prob$ClutchShooter,
                                     opt_prob$ClutchShooter,
                                     opt_prob$DefensiveHitter,
                                     opt_prob$DefensiveHitter,
                                     opt_prob$ShootingPlaymaker,
                                     opt_prob$ShootingPlaymaker,
                                     opt_prob$TrueDefender,
                                     opt_prob$TrueDefender,
                                     opt_prob$PointProducer,
                                     opt_prob$PointProducer,
                                     opt_prob$HybridShooter,
                                     opt_prob$HybridShooter))
dimnames(constraint_matrix) <- 
  list(c("DetPlayerMin",
         "SevenDmenMax",
         "SevenDmenMin",
         "FourteenFwdMax",
         "FourteenFwdMin",
         "ThreeFranchiseMax",
         "ThreeFranchiseMin",
         "EightClutchMax",
         "EightClutchMin",
         "TwoDHitterMax",
         "TwoDHitterMin",
         "OneShootingMax",
         "OneShootingMin",
         "ThreeDefenderMax",
         "ThreeDefenderMin",
         "TwoPointMax",
         "TwoPointMin",
         "TwoHybridMax",
         "TwoHybridMin"),
       opt_prob$PlayerID)

# solve the knapsack problem
knapsack_object <- 
  lp(const.mat = constraint_matrix,
     objective = opt_prob$`P/GP`,
     direction = "max",
     const.rhs = c(17, 7, 7, 14, 14, 3, 3, 8, 8, 2, 2, 1, 1, 3, 3, 2, 2, 2, 2),
     const.dir = c(">=",
                   "<=", ">=",
                   "<=", ">=",
                   "<=", ">=",
                   "<=", ">=",
                   "<=", ">=",
                   "<=", ">=",
                   "<=", ">=",
                   "<=", ">=",
                   "<=", ">="),
     int.vec = 1:156, all.bin = TRUE)

# examine the structure of resulting list object 
cat("\n\nStructure of Mathematical Programming Object\n") 
print(str(knapsack_object))

# show the solution
cat("\n\nBest Roster Given Constraints\n")
print(opt_prob[as.logical(knapsack_object$solution),])
count(opt_prob[as.logical(knapsack_object$solution),])

# show the points per period maximum
cat("\n\nMaximum Points per Games Played:", knapsack_object$objval, "\n")

# save solution and print cap hit
opt_one <- available_players[as.logical(knapsack_object$solution),]
opt_one %>%
  summarise(caphit = sum(CAP.HIT))

### OPTION 2 ###
# option 2 fwds: 2 franchise players, 3 defensive hitters, 6 clutch shooters, and 3 shooting playmakers
# option 2 dmen: 4 true defenders, 1 point producers, 2 hybrid shooters

dimnames(constraint_matrix) <- 
  list(c("DetPlayerMin",
         "SevenDmenMax",
         "SevenDmenMin",
         "FourteenFwdMax",
         "FourteenFwdMin",
         "TwoFranchiseMax",
         "TwoFranchiseMin",
         "SixClutchMax",
         "SixClutchMin",
         "ThreeDHitterMax",
         "ThreeDHitterMin",
         "TwoShootingMax",
         "TwoShootingMin",
         "FourDefenderMax",
         "FourDefenderMin",
         "OnePointMax",
         "OnePointMin",
         "TwoHybridMax",
         "TwoHybridMin"),
       opt_prob$PlayerID)

# solve the knapsack problem
knapsack_object <- 
  lp(const.mat = constraint_matrix,
     objective = opt_prob$`P/GP`,
     direction = "max",
     const.rhs = c(17, 7, 7, 14, 14, 2, 2, 6, 6, 3, 3, 3, 3, 4, 4, 1, 1, 2, 2),
     const.dir = c(">=",
                   "<=", ">=",
                   "<=", ">=",
                   "<=", ">=",
                   "<=", ">=",
                   "<=", ">=",
                   "<=", ">=",
                   "<=", ">=",
                   "<=", ">=",
                   "<=", ">="),
     int.vec = 1:156, all.bin = TRUE)

# examine the structure of resulting list object 
cat("\n\nStructure of Mathematical Programming Object\n") 
print(str(knapsack_object))

# show the solution
cat("\n\nBest Roster Given Constraints\n")
print(opt_prob[as.logical(knapsack_object$solution),])
count(opt_prob[as.logical(knapsack_object$solution),])

# show the points per period maximum
cat("\n\nMaximum Points per Games Played:", knapsack_object$objval, "\n")

# save solution and print cap hit
opt_two <- available_players[as.logical(knapsack_object$solution),]
opt_two %>%
  summarise(caphit = sum(CAP.HIT))

### OPTION 3 ###
# option 3 fwds: 3 franchise players, 2 defensive hitters, 7 clutch shooters, and 2 shooting playmakers
# option 3 dmen: 2 true defenders, 2 point producers, 3 hybrid shooters

dimnames(constraint_matrix) <- 
  list(c("DetPlayerMin",
         "SevenDmenMax",
         "SevenDmenMin",
         "FourteenFwdMax",
         "FourteenFwdMin",
         "ThreeFranchiseMax",
         "ThreeFranchiseMin",
         "SevenClutchMax",
         "SevenClutchMin",
         "TwoDHitterMax",
         "TwoDHitterMin",
         "TwoShootingMax",
         "TwoShootingMin",
         "TwoDefenderMax",
         "TwoDefenderMin",
         "TwoPointMax",
         "TwoPointMin",
         "ThreeHybridMax",
         "ThreeHybridMin"),
       opt_prob$PlayerID)

# solve the knapsack problem
knapsack_object <- 
  lp(const.mat = constraint_matrix,
     objective = opt_prob$`P/GP`,
     direction = "max",
     const.rhs = c(17, 7, 7, 14, 14, 3, 3, 7, 7, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3),
     const.dir = c(">=",
                   "<=", ">=",
                   "<=", ">=",
                   "<=", ">=",
                   "<=", ">=",
                   "<=", ">=",
                   "<=", ">=",
                   "<=", ">=",
                   "<=", ">=",
                   "<=", ">="),
     int.vec = 1:156, all.bin = TRUE)

# examine the structure of resulting list object 
cat("\n\nStructure of Mathematical Programming Object\n") 
print(str(knapsack_object))

# show the solution
cat("\n\nBest Roster Given Constraints\n")
print(opt_prob[as.logical(knapsack_object$solution),])
count(opt_prob[as.logical(knapsack_object$solution),])

# show the points per period maximum
cat("\n\nMaximum Points per Games Played:", knapsack_object$objval, "\n")

# save solution and print cap hit
opt_three <- available_players[as.logical(knapsack_object$solution),]
opt_three %>%
  summarise(caphit = sum(CAP.HIT))

### OPTION 4 ###
# option 4 fwds: 2 franchise players, 1 defensive hitters, 8 clutch shooters, and 3 shooting playmakers
# option 4 dmen: 3 true defenders, 1 point producers, 3 hybrid shooters

dimnames(constraint_matrix) <- 
  list(c("DetPlayerMin",
         "SevenDmenMax",
         "SevenDmenMin",
         "FourteenFwdMax",
         "FourteenFwdMin",
         "TwoFranchiseMax",
         "TwoFranchiseMin",
         "EightClutchMax",
         "EightClutchMin",
         "OneDHitterMax",
         "OneDHitterMin",
         "ThreeShootingMax",
         "ThreeShootingMin",
         "ThreeDefenderMax",
         "ThreeDefenderMin",
         "OnePointMax",
         "OnePointMin",
         "ThreeHybridMax",
         "ThreeHybridMin"),
       opt_prob$PlayerID)

# solve the knapsack problem
knapsack_object <- 
  lp(const.mat = constraint_matrix,
     objective = opt_prob$`P/GP`,
     direction = "max",
     const.rhs = c(17, 7, 7, 14, 14, 2, 2, 8, 8, 1, 1, 3, 3, 3, 3, 1, 1, 3, 3),
     const.dir = c(">=",
                   "<=", ">=",
                   "<=", ">=",
                   "<=", ">=",
                   "<=", ">=",
                   "<=", ">=",
                   "<=", ">=",
                   "<=", ">=",
                   "<=", ">=",
                   "<=", ">="),
     int.vec = 1:156, all.bin = TRUE)

# examine the structure of resulting list object 
cat("\n\nStructure of Mathematical Programming Object\n") 
print(str(knapsack_object))

# show the solution
cat("\n\nBest Roster Given Constraints\n")
print(opt_prob[as.logical(knapsack_object$solution),])
count(opt_prob[as.logical(knapsack_object$solution),])

# show the points per period maximum
cat("\n\nMaximum Points per Games Played:", knapsack_object$objval, "\n")

# save solution and print cap hit
opt_four <- available_players[as.logical(knapsack_object$solution),]
opt_four %>%
  summarise(caphit = sum(CAP.HIT))

# best option of optimization with age restriction
best_option_age <- opt_one

best_option_age <- best_option_age %>%
  select(c(PLAYER, TEAM, AGE, POS, GP, G, A, `P/GP`, `+/-`, Sh, `Sh%`, kmeans_cluster, cluster_name))

# write new roster to excel file
write.xlsx(best_option_age, "output/best_roster_option_age.xlsx")