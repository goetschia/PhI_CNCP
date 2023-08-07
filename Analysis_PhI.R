rm(list = ls())

# Packages
library(tidyverse)
library(unibeCols)
library(cowplot)

# Functions
collapse_rows_to_strings <- function(row) {
  coalesce(!!!row)
}

# Load data
df <- read_delim("Charting.csv", ";")

# Rearrange df
names(df)[1] <- "Char"

df <- df %>% mutate(across(-Char, ~ case_when(. == 1 ~ Char)))
df <- df %>% t() %>% as.data.frame()
colnames(df) <- df[1,]
df <- df[-1,]

# Compress df
study_cols <- c("RCT", "Cohort study","Pre-post study","Chart-review study", "Cross-sectional study",            
                "Qualitative study", "Case series","Follow-up")
df <- df %>% unite(`Study type `, na.rm = T, sep = "_", all_of(study_cols))

setting_cols <- colnames(df)[3:9]
df <- df %>% unite(Setting, na.rm = T, sep = "_", all_of(setting_cols))

origin_cols <- colnames(df)[4:7]
df <- df %>% unite(Origin, na.rm = T, sep = "_", all_of(origin_cols))

int_cols <- colnames(df)[5:13]
df <- df %>% unite(Interventions, na.rm = T, sep = "_", all_of(int_cols))

aim_cols <- colnames(df)[6:10]
df <- df %>% unite(`Aim of intervention`, na.rm = T, sep = "_", all_of(aim_cols))

scope_cols <- colnames(df)[7:9]
df <- df %>% unite(`Scope of practice`, na.rm = T, sep = "_", all_of(scope_cols))

prof_cols <- colnames(df)[8:15]
df <- df %>% unite(Professions, na.rm = T, sep = "_", all_of(prof_cols))

aet_cols <- colnames(df)[9:13]
df <- df %>% unite(`Pain aetiology`, na.rm = T, sep = "_", all_of(aet_cols))

age_cols <- colnames(df)[10:11]
df <- df %>% unite(`Age of patients`, na.rm = T, sep = "_", all_of(age_cols))

guide_cols <- colnames(df)[11:14]
df <- df %>% unite(`Guidelines used`, na.rm = T, sep = "_", all_of(guide_cols))

colnames(df) <- c("Methodology", "Setting", "Origin", "Interventions", "Aim", "Scope", 
                  "Prof", "Aetology", "Age", "Guidelines")

# Analysis
p_all_int <- ggplot(df, aes(x =fct_infreq(Interventions))) + geom_bar(fill = unibeBlackS()[1]) +
  scale_x_discrete(breaks = c("Face-to-face MR_Patient education",  "Face-to-face MR_Medication management", 
                              "Patient education","Face-to-face MR", "Face-to-face MR_Patient education_Medication management",
                              "Medication management", "Remote MR", "Remote MR_Face-to-face MR_Patient education", 
                              "Remote MR_Patient education", "Board Review", "Healthcare professional education",
                              "Medication management_Compounding", "Medication reconciliation",
                              "Medication reconciliation_Patient education", "Remote MR_Cognitive behavioural therapy",
                              "Remote MR_Face-to-face MR_Medication management",  "Remote MR_Healthcare professional education"  ),
                   labels = c("FtF-MR & Education", expression(paste("FtF-MR & \n Medication management")), 
                              "Education", "FtF-MR", expression(paste("FtF-MR & Education & \n Medication management ")), 
                              "Medication Management ", "Remote MR", expression(paste("Remote MR & FtF-MR & \n Education")),
                              expression(paste("Remote MR & \n Education")), "Board Review", expression(paste("Healthcare professional \n education")),
                              expression(paste("Medication Management & \n Compounding ")), "MedRec",
                              "MedRec & Education", "Remote MR & CBT", 
                              expression(paste("Remote MR & FtF-MR & \n Medication management ")), expression(paste("Remote MR & Healthcare \n professional education ")))) +
  scale_y_continuous(breaks = seq(1,10,1)) +
  ylab("") + xlab("") +
  theme(axis.text.x = element_text(angle = 90, hjust=0.95,vjust=0.2),
        axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold"),
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 18),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        linewidth  = 0.5, linetype = "solid"),
        panel.grid.major = element_line(linewidth  = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                        colour = "grey")) 

# Interventions in outpatient setting

p_int_setting <- ggplot(df[df$Setting == "Outpatient clinic" | 
                             df$Setting == "Primary care" |
                             df$Setting == "Community pharmacy" | 
                             df$Setting == "Pharmacist-clinic"  |
                             df$Setting == "Hospital ",],
                     aes(x = fct_infreq(Interventions), fill = Setting, group = Setting)) + 
  geom_bar(aes(y = after_stat(prop)),position = "dodge") +
  scale_x_discrete(breaks = c("Face-to-face MR_Patient education", "Face-to-face MR_Medication management",
                              "Face-to-face MR", "Face-to-face MR_Patient education_Medication management",
                              "Patient education","Medication management", "Remote MR", "Remote MR_Patient education",
                              "Board Review", "Healthcare professional education", "Medication management_Compounding",
                              "Medication reconciliation", "Medication reconciliation_Patient education",
                              "Remote MR_Cognitive behavioural therapy", "Remote MR_Face-to-face MR_Patient education",
                              "Remote MR_Healthcare professional education"),
                   labels = c("FtF MR & Education", expression(paste("FtF MR & \n Medication management")),
                              "FtF MR", expression(paste("FtF MR & Education & \n Medication management ")),
                              "Education","Medication management", "Remote MR", "Remote MR & Education",
                              "Board Review", expression(paste("Healthcare professional \n education ")), expression(paste("Medication management \n Compounding ")),
                              "MedRec", "MedRec & Education",
                              "Remote MR & CBT", expression(paste("Remote MR & FtF MR \n Education ")),
                              expression(paste("Remote MR & Healthcare \n professional education ")))) +
  ylab("Relative Frequencies") + xlab("") +
  scale_color_manual(name = "Setting", breaks = c("Community pharmacy", "Hospital ", "Outpatient clinic", "Pharmacist-clinic", "Primary care"),
                     values = c(unibeGreenS()[1], unibeOceanS()[1], unibeRedS()[1], unibeBlackS()[1], unibeBrownS()[1])) +
  scale_fill_manual(name = "Setting", breaks = c("Community pharmacy", "Hospital ", "Outpatient clinic", "Pharmacist-clinic", "Primary care"),
                    values = c(unibeGreenS()[1], unibeOceanS()[1], unibeRedS()[1], unibeBlackS()[1], unibeBrownS()[1])) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2),
        axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold"),
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 18),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        linewidth  = 0.5, linetype = "solid"),
        panel.grid.major = element_line(linewidth  = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                        colour = "grey"))


