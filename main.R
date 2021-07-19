rm(list=ls())
gc()
#### 0. Upload the libraries ####
library(ggeffects)
library(ggplot2)
library(ggpubr)

#### 1. Plot on the electoral success ####

#### 1.1. Import the data ####

names_cols <- c("canton", "year", "parti", "strength") ## Defines columns names ####
election_dta <- read.csv("px-x-1702020000_104_20210718-112432.csv", sep = ";", col.names = names_cols) ## Import data

#### 1.2. Exclude the greens and compute the overall strength of these four party ####
election_dta <- election_dta[election_dta$parti!="PEV",] %>% 
  group_by(year) %>% 
  mutate(sum_part = sum(strength))


#### 1.3. Creates figure 1 ####
png("trends in party strength in parliament.png", width = 2500, height = 1500, res = 300)
ggplot(election_dta[election_dta$parti!="PEV",], aes(x=year, y = strength, col = parti))+
  geom_line()+
  scale_color_manual(labels = c("CVP", "FDP", "SPS", "SVP"), values = c("orange", "blue", "red", "dark green"), name = "Political Parties")+
  ylim(c(0, 30))+
  xlab("Years")+
  ylab("Party Strength in Parliament (in %)")+
  
  theme_minimal()+
  theme(legend.position = "bottom")
dev.off()



#### 2. Plots on support for policy position (figure 2) ####

#### 2.1. Import the data ####
swissvotes <- read.csv(url("https://swissvotes.ch/page/dataset/swissvotes_dataset.csv"), sep = ";")

#### 3. Select the columns
swissvotes2 <- swissvotes[,c(1, 2, ## Project id and date
                             which(colnames(swissvotes)=="volkja.proz"), ## Support of the swiss population for the ballot
                             which(colnames(swissvotes)=="rechtsform"),
                             ## The remaining variables concern the position of parties in the ballots
                             which(colnames(swissvotes)=="p.fdp"), ## FDP
                             which(colnames(swissvotes)=="p.cvp"), ## CVP
                             which(colnames(swissvotes)=="p.sps"), ## SPS
                             which(colnames(swissvotes)=="p.svp"))] ## SVP

#### 2.3. Recode the variables
swissvotes2 <- swissvotes2 %>% 
  mutate(pos.svp = ifelse(p.svp =="1", "yes", ifelse(p.svp == "2", "no", "neutral")),
         pos.sps = ifelse(p.sps =="1", "yes", ifelse(p.sps == "2", "no", "neutral")),
         pos.fdp = ifelse(p.fdp =="1", "yes", ifelse(p.fdp == "2", "no", "neutral")),
         pos.cvp = ifelse(p.cvp =="1", "yes", ifelse(p.cvp == "2", "no", "neutral")),
         percent.for = as.numeric(volkja.proz),
         year = as.numeric(substr(datum, 7, 10)),
         date = as.Date(datum, "%d.%m.%Y"))

#### 2.4. Select observations
swissvotes2 <- swissvotes2[swissvotes2$year>=1950 & swissvotes2$year<2020,] ### Select observation between 1950 and 2020
swissvotes2 <- swissvotes2[swissvotes2$rechtsform!=5 | swissvotes2$rechtsform!=4,] #### Drops rare institution of direct democracy. Only keeps initiative referenda mandatory or facultative.


#### 2.5. Creates the decade variable
decades <- seq(1950, 2010, by = 10)
swissvotes2$decade <- NA
for ( i in decades ) {
  swissvotes2[swissvotes2$year>=i & swissvotes2$year<(i+10),]$decade <- paste(i, " to ", i +9)
}


#### 2.6. Run the models ####

model_sps <- lm(percent.for ~ pos.svp +  decade * pos.sps + pos.fdp + pos.cvp + factor(rechtsform), data = swissvotes2)
summary(model_sps)

model_svp <- lm(percent.for ~ pos.cvp + pos.sps + pos.fdp + decade * pos.svp + factor(rechtsform), data = swissvotes2)
summary(model_svp)

#### 2.7. Make the plot (figure 2) ####

png("Figure 2 vote recommendation PS SVP on populare support.png", width = 2500, height=3000, res = 300)
ggarrange(
  plot(ggpredict(model_svp, c("decade", "pos.svp[yes,no]")))+
    ggtitle("SVP recommendations")+
    ylab("Predicted support")+
    xlab("Decades")+
    scale_color_manual(name = "Recommendation SVP", values = c("#009900", "#CC0000"))+
    theme(legend.position = "bottom"),
  
  plot(ggpredict(model_sps, c("decade", "pos.sps[yes,no]")))+
    ggtitle("SP recommendations")+
    ylab("Predicted support")+
    xlab("Decades")+
    scale_color_manual(name = "Recommendation SP", values = c("#009900", "#CC0000"))+
    theme(legend.position = "bottom"),
  
  ncol = 1,
  nrow = 2,
  align = "hv"
)

dev.off()







