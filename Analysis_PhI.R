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
df <- read_delim("Data_Charting_R_2.csv", ";")

# Rearrange df
names(df)[1] <- "Char"

df_mut <- df[1:70,] %>% mutate(across(-c(Char), ~ case_when(. == 1 ~ Char)))
df <- rbind(df_mut, df[71,])
df <- df %>% t() %>% as.data.frame()
colnames(df) <- df[1,]
df <- df[-1,]

# Compress df
study_cols <- c("RCT", "Cohort study","Pre-post study","Chart-review study", "Cross-sectional study",            
                "Qualitative study", "Pilot study", "Case series","Feasibility study",
                "Quasi experimental study", "Case-control study")
df <- df %>% unite(`Study type `, na.rm = T, sep = "_", all_of(study_cols))

setting_cols <- colnames(df)[3:9]
df <- df %>% unite(Setting, na.rm = T, sep = "_", all_of(setting_cols))

origin_cols <- colnames(df)[4:7]
df <- df %>% unite(Origin, na.rm = T, sep = "_", all_of(origin_cols))

int_cols <- colnames(df)[5:9]
df <- df %>% unite(Interventions, na.rm = T, sep = "_", all_of(int_cols))

aim_cols <- colnames(df)[6:10]
df <- df %>% unite(`Aim of intervention`, na.rm = T, sep = "_", all_of(aim_cols))

scope_cols <- colnames(df)[7:9]
df <- df %>% unite(`Scope of practice`, na.rm = T, sep = "_", all_of(scope_cols))

prof_cols <- colnames(df)[8:16]
df <- df %>% unite(Professions, na.rm = T, sep = "_", all_of(prof_cols))

aet_cols <- colnames(df)[9:13]
df <- df %>% unite(`Pain aetiology`, na.rm = T, sep = "_", all_of(aet_cols))

age_cols <- colnames(df)[10:11]
df <- df %>% unite(`Age of patients`, na.rm = T, sep = "_", all_of(age_cols))

guide_cols <- colnames(df)[11:14]
df <- df %>% unite(`Guidelines used`, na.rm = T, sep = "_", all_of(guide_cols))

out_cols <- colnames(df)[12:15]
df <- df %>% unite(`Outcome`, na.rm = T, sep = "_", all_of(out_cols))


colnames(df) <- c("Methodology", "Setting", "Origin", "Interventions", "Aim", "Scope", 
                  "Prof", "Aetology", "Age", "Guidelines", "Outcomes", "Patients")

# Recode certaine variables
df$Patients <- as.numeric(df$Patients)
df$Interventions <- factor(df$Interventions)

# Analysis
# Used for pub
# Patients & studies per setting
p_setting <- df %>% 
  select(Setting, Patients) %>%
  group_by(Setting) %>%
  summarise(Patients = sum(Patients),
            Sum = n()) %>%
  mutate(Patients = Patients/sum(Patients), Sum = Sum/sum(Sum)) %>%
  arrange(desc(Patients)) %>%
  mutate(Setting = fct_inorder(Setting)) %>%
  gather(key = "variable", value = "value", -Setting) %>%
  ggplot(aes(x = Setting, y = value*100, fill = variable, group = variable )) +
  geom_col(position = "dodge") +
  xlab("") + ylab("Percentages of interventions") + 
  labs(title = "Number of patients in different settings") + 
  scale_x_discrete(breaks = c("Outpatient clinic", "Primary care", "Community pharmacy",
                              "Pharmacist-clinic", "Hospital ", "Primary care_Community pharmacy",
                              "Correctional services", "University"),
                   labels = c("Outpatient clinic", "Primary care", expression(paste("Community pharmacy ")),
                              expression(paste("Pharmacist-led clinic")), "Hospital", 
                              expression(paste("Primary care & Community pharmacy ")),
                              "Correctional services", "University")) +
  scale_color_manual(name = "", breaks = c("Patients", "Sum"),
                     labels = c("Patients", "Studies"),
                     values = c(unibeGreenS()[1], unibeOceanS()[1])) +
  scale_fill_manual(name = "", breaks = c("Patients", "Sum"),
                    labels = c("Patients", "Studies"),
                    values = c(unibeGreenS()[1], unibeOceanS()[1])) +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold"),
        title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 18),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        linewidth  = 0.5, linetype = "solid"),
        panel.grid.major = element_line(linewidth  = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                        colour = "grey")) + coord_flip()


# Patient and studies per intervention
p_comb_int_flip <-  df %>%
  select(Interventions, Patients) %>%
  group_by(Interventions) %>%
  summarise(Total_patients = sum(Patients),
            Total_studies = n()*120) %>%
  arrange(desc(Total_patients)) %>%
  mutate(Interventions = fct_inorder(Interventions)) %>%
  gather(key = "variable", value = "value", -Interventions) %>%
  ggplot( aes(x = Interventions, y = value, fill = variable)) +
  geom_col(position = "dodge") +
  scale_y_continuous(
    name = "Number of patients",
    breaks = seq(0,1600,200),
    sec.axis = sec_axis(~./120,
                        name = "Number of studies",
                        breaks = seq(0,10,1))
  ) + xlab("") + 
  scale_x_discrete(breaks = c("Selection: Population level_Compounding", "Selection: Population level",
                              "Monitoring", "Selection_ Patient-leve_Selection: Population level",
                              "Selection_ Patient-leve_Selection: Population level_Use/Implementation",
                              "Use/Implementation", "Selection_ Patient-leve_Use/Implementation",
                              "Selection_ Patient-leve_Monitoring", "Selection_ Patient-leve_Use/Implementation_Monitoring",
                              "Selection_ Patient-leve"),
                   labels = c("HP Education & Compounding", "HP Education",
                              "Monitoring", "Drug Selection & HP Education",
                              "Drug Selection & HP Education & Patient Education",
                              "Patient Education", "Drug Selection & Patient Educiation",
                              "Drug Selection & Monitoring", "Drug Selection & Patient Education & Monitoring",
                              "Drug Selection")) +
  scale_color_manual(name = "", breaks = c("Total_patients", "Total_studies"),
                     labels = c("Patients", "Studies"),
                     values = c(unibeGreenS()[1], unibeOceanS()[1])) +
  scale_fill_manual(name = "", breaks = c("Total_patients", "Total_studies"),
                    labels = c("Patients", "Studies"),
                    values = c(unibeGreenS()[1], unibeOceanS()[1])) +
  theme(axis.text.x = element_text(angle = 0),
        axis.text.y = element_text(angle = 0),
        axis.text=element_text(size=60),
        axis.title=element_text(size=60,face="bold"),
        legend.text = element_text(size = 60),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        linewidth  = 0.5, linetype = "solid"),
        panel.grid.major = element_line(linewidth  = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                        colour = "grey")) + coord_flip()

all_combinations <- expand.grid(
  Interventions = unique(df$Interventions),
  Setting = unique(df$Setting)
)

df %>% select(Interventions, Setting) %>% 
  group_by_all() %>% 
  summarise(Sum = n()) %>%
  right_join(all_combinations, by = c("Interventions", "Setting")) %>%
  mutate(Sum = replace_na(Sum, 0)) %>%
  ggplot(aes(x = Setting, y = Interventions, fill = factor(Sum))) +
  geom_tile(colour = "black") +
  scale_x_discrete(breaks = c("Community pharmacy", "Correctional services", 
                              "Hospital ", "Outpatient clinic", "Pharmacist-clinic", 
                              "Primary care", "Primary care_Community pharmacy", "University"),
                   labels = c("Community pharmacy", "Correctional services", 
                              "Hospital", "Outpatient clinic", "Pharmacist-led clinic",
                              "Primary care", expression(paste("Primary care &\n Community pharmacy")),
                              "University")) +
  scale_y_discrete(
    breaks = c("Use/Implementation","Selection_ Patient-leve_Use/Implementation_Monitoring",
               "Selection_ Patient-leve_Use/Implementation", 
               "Selection_ Patient-leve_Selection: Population level_Use/Implementation",
               "Selection_ Patient-leve_Selection: Population level","Selection_ Patient-leve_Monitoring",
               "Selection_ Patient-leve", "Selection: Population level_Compounding",
               "Selection: Population level", "Monitoring"),
    labels = c("Use","Selection (Patient) & Use & Monitoring",
               "Selection (Patient) & Use","Selection (Patient) & HP Education & Use",
               "Selection (Patient) & HP Education","Selection (Patient) & Monitoring",
               "Selection (Patient)","HP Education & Compounding",
               "HP Education","Monitoring")
  ) +
  ylab("") + xlab("") +
  scale_fill_manual(
    name = "Number of studies",
    breaks = c("0", "1", "2", "3", "6"),
    values = c(rgb(1,1,1,0.5), unibeOceanS()[5], unibeOceanS()[4], unibeOceanS()[3], unibeOceanS()[1])
  ) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.95),
    panel.background = element_rect(fill = "white",
                                    colour = "white"),
    axis.text=element_text(size=18),
    axis.title=element_text(size=20,face="bold"),
    title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 18)
  )

$
   # All other approaches
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
  ylab("") + xlab("") + labs(title = "Studies per intervention") +
  theme(axis.text.x = element_text(angle = 90, hjust=0.95,vjust=0.2),
        axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold"),
        title = element_text(size = 20, face = "bold"),
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
  ylab("Relative Frequencies") + xlab("") + labs(title = "Interventions in different settings") +
  scale_color_manual(name = "Setting", breaks = c("Community pharmacy", "Hospital ", "Outpatient clinic", "Pharmacist-clinic", "Primary care"),
                     values = c(unibeGreenS()[1], unibeOceanS()[1], unibeRedS()[1], unibeBlackS()[1], unibeBrownS()[1])) +
  scale_fill_manual(name = "Setting", breaks = c("Community pharmacy", "Hospital ", "Outpatient clinic", "Pharmacist-clinic", "Primary care"),
                    values = c(unibeGreenS()[1], unibeOceanS()[1], unibeRedS()[1], unibeBlackS()[1], unibeBrownS()[1])) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2),
        axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold"),
        title = element_text(size=20, face = "bold"),
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 18),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        linewidth  = 0.5, linetype = "solid"),
        panel.grid.major = element_line(linewidth  = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                        colour = "grey"))

# Interventions per patient number

p_pat_int <- df %>%
  select(Interventions, Patients) %>%
  group_by(Interventions) %>%
  summarise(sum(Patients)) %>%
  arrange(-`sum(Patients)`) %>%
  ggplot(aes(x = fct_reorder(Interventions, `sum(Patients)`, .desc = T), y = `sum(Patients)`)) +
  geom_col(fill = unibeBlackS()[1]) +
  ylab("Number of patients") + xlab("") + labs(title = "Number of patients treated per intervention") + 
  scale_x_discrete(breaks = c("Face-to-face MR_Medication management", "Face-to-face MR_Patient education_Medication management",
                              "Face-to-face MR_Patient education", "Remote MR_Cognitive behavioural therapy",
                              "Patient education", "Medication management", "Remote MR_Patient education",
                              "Remote MR", "Face-to-face MR", "Remote MR_Face-to-face MR_Medication management",
                              "Board Review", "Remote MR_Healthcare professional education", 
                              "Remote MR_Face-to-face MR_Patient education", "Medication reconciliation",
                              "Medication reconciliation_Patient education", "Healthcare professional education",
                              "Medication management_Compounding"),
                   labels = c("FtF MR & Medication management", expression(paste("FtF MR & Education & \n Medication management ")),
                              "FtF MR & Education", "Remote MR & CBT",
                              "Education", "Medication management", "Remote MR & Education ",
                              "Remote MR", "FtF MR", expression(paste("Remote MR & FtF MR \n Medication management ")),
                              "Board Review", expression(paste("Remote MR & Healthcare \n professional education ")), 
                              expression(paste("Remote MR & FtF MR & \n Education")), "MedRec",
                              "MedRed & Education", expression(paste("Healthcare \n professional education")),
                              expression(paste("Medication management \n Compounding")))) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2),
        axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold"),
        title = element_text(size = 20, face = "bold"),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        linewidth  = 0.5, linetype = "solid"),
        panel.grid.major = element_line(linewidth  = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                        colour = "grey"))

# Combining study number and patients per intervention
p_comb_int <-  df %>%
  select(Interventions, Patients) %>%
  group_by(Interventions) %>%
  summarise(Total_patients = sum(Patients),
            Total_studies = n()*80) %>%
  arrange(desc(Total_patients)) %>%
  mutate(Interventions = fct_inorder(Interventions)) %>%
  gather(key = "variable", value = "value", -Interventions) %>%
  ggplot( aes(x = Interventions, y = value, fill = variable)) +
  geom_col(position = "dodge") +
  scale_y_continuous(
    name = "Number of patients",
    breaks = seq(0,800,100),
    sec.axis = sec_axis(~./80,
                        name = "Number of studies",
                        breaks = seq(0,10,1))
  ) + xlab("") + 
  scale_x_discrete(breaks = c("Face-to-face MR_Medication management", "Face-to-face MR_Patient education_Medication management",
                              "Face-to-face MR_Patient education", "Remote MR_Cognitive behavioural therapy",
                              "Patient education", "Medication management", "Remote MR_Patient education",
                              "Remote MR", "Face-to-face MR", "Remote MR_Face-to-face MR_Medication management",
                              "Board Review", "Remote MR_Healthcare professional education", 
                              "Remote MR_Face-to-face MR_Patient education", "Medication reconciliation",
                              "Medication reconciliation_Patient education", "Healthcare professional education",
                              "Medication management_Compounding"),
                   labels = c(expression(paste("FtF MR &\n Medication management")), expression(paste("FtF MR & Education & \n Medication management")),
                              "FtF MR & Education", "Remote MR & CBT",
                              "Education", "Medication management", "Remote MR & Education",
                              "Remote MR", "FtF MR", expression(paste("Remote MR & FtF MR \n Medication management")),
                              "Board Review", expression(paste("Remote MR & Healthcare \n professional education")), 
                              expression(paste("Remote MR & FtF MR & \n Education")), "MedRec",
                              "MedRed & Education", expression(paste("Healthcare \n professional education")),
                              expression(paste("Medication management &\n Compounding")))) +
  scale_color_manual(name = "", breaks = c("Total_patients", "Total_studies"),
                     labels = c("Patients", "Studies"),
                     values = c(unibeGreenS()[1], unibeOceanS()[1])) +
  scale_fill_manual(name = "", breaks = c("Total_patients", "Total_studies"),
                    labels = c("Patients", "Studies"),
                    values = c(unibeGreenS()[1], unibeOceanS()[1])) +
  labs(title = "Occurence of interventions per studies and patients",
       fill = "") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5),
        axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold"),
        title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 18),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        linewidth  = 0.5, linetype = "solid"),
        panel.grid.major = element_line(linewidth  = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                        colour = "grey"))
  

 # Combined patients and studies with flipped coordinates

p_comb_int_flip <-  df %>%
  select(Interventions, Patients) %>%
  group_by(Interventions) %>%
  summarise(Total_patients = sum(Patients),
            Total_studies = n()*80) %>%
  arrange(desc(Total_patients)) %>%
  mutate(Interventions = fct_inorder(Interventions)) %>%
  gather(key = "variable", value = "value", -Interventions) %>%
  ggplot( aes(x = Interventions, y = value, fill = variable)) +
  geom_col(position = "dodge") +
  scale_y_continuous(
    name = "Number of patients",
    breaks = seq(0,800,100),
    sec.axis = sec_axis(~./80,
                        name = "Number of studies",
                        breaks = seq(0,10,1))
  ) + xlab("") + 
  scale_x_discrete(breaks = c("Face-to-face MR_Medication management", "Face-to-face MR_Patient education_Medication management",
                              "Face-to-face MR_Patient education", "Remote MR_Cognitive behavioural therapy",
                              "Patient education", "Medication management", "Remote MR_Patient education",
                              "Remote MR", "Face-to-face MR", "Remote MR_Face-to-face MR_Medication management",
                              "Board Review", "Remote MR_Healthcare professional education", 
                              "Remote MR_Face-to-face MR_Patient education", "Medication reconciliation",
                              "Medication reconciliation_Patient education", "Healthcare professional education",
                              "Medication management_Compounding"),
                   labels = c(expression(paste("FtF MR & Medication management")), expression(paste("FtF MR & Education & Medication management")),
                              "FtF MR & Education", "Remote MR & CBT",
                              "Education", "Medication management", "Remote MR & Education",
                              "Remote MR", "FtF MR", expression(paste("Remote MR & FtF MR Medication management")),
                              "Board Review", expression(paste("Remote MR & Healthcare professional education")), 
                              expression(paste("Remote MR & FtF MR & Education")), "MedRec",
                              "MedRed & Education", expression(paste("Healthcare professional education")),
                              expression(paste("Medication management & Compounding")))) +
  scale_color_manual(name = "", breaks = c("Total_patients", "Total_studies"),
                     labels = c("Patients", "Studies"),
                     values = c(unibeGreenS()[1], unibeOceanS()[1])) +
  scale_fill_manual(name = "", breaks = c("Total_patients", "Total_studies"),
                    labels = c("Patients", "Studies"),
                    values = c(unibeGreenS()[1], unibeOceanS()[1])) +
  labs(title = "Interventions per studies and patients",
       fill = "") +
  theme(axis.text.x = element_text(angle = 0),
        axis.text.y = element_text(angle = 0),
        axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold"),
        title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 18),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        linewidth  = 0.5, linetype = "solid"),
        panel.grid.major = element_line(linewidth  = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                        colour = "grey")) + coord_flip()

# Combined interventions with percentages
p_comb_int_flip <-  df %>%
  select(Interventions, Patients) %>%
  group_by(Interventions) %>%
  summarise(Total_patients = sum(Patients),
            Total_studies = n()) %>%
  mutate(Total_patients = Total_patients/sum(Total_patients),
         Total_studies = Total_studies/sum(Total_studies)) %>%
  arrange(desc(Total_patients)) %>%
  mutate(Interventions = fct_inorder(Interventions)) %>%
  gather(key = "variable", value = "value", -Interventions) %>%
  ggplot( aes(x = Interventions, y = value*100, fill = variable)) +
  geom_col(position = "dodge") +
  ylab("Percentages of interventions") + xlab("") + 
  scale_x_discrete(breaks = c("Face-to-face MR_Medication management", "Face-to-face MR_Patient education_Medication management",
                              "Face-to-face MR_Patient education", "Remote MR_Cognitive behavioural therapy",
                              "Patient education", "Medication management", "Remote MR_Patient education",
                              "Remote MR", "Face-to-face MR", "Remote MR_Face-to-face MR_Medication management",
                              "Board Review", "Remote MR_Healthcare professional education", 
                              "Remote MR_Face-to-face MR_Patient education", "Medication reconciliation",
                              "Medication reconciliation_Patient education", "Healthcare professional education",
                              "Medication management_Compounding"),
                   labels = c(expression(paste("FtF MR & Medication management")), expression(paste("FtF MR & Education & Medication management")),
                              "FtF MR & Education", "Remote MR & CBT",
                              "Education", "Medication management", "Remote MR & Education",
                              "Remote MR", "FtF MR", expression(paste("Remote MR & FtF MR Medication management")),
                              "Board Review", expression(paste("Remote MR & Healthcare professional education")), 
                              expression(paste("Remote MR & FtF MR & Education")), "MedRec",
                              "MedRec & Education", expression(paste("Healthcare professional education")),
                              expression(paste("Medication management & Compounding")))) +
  scale_color_manual(name = "", breaks = c("Total_patients", "Total_studies"),
                     labels = c("Patients", "Studies"),
                     values = c(unibeGreenS()[1], unibeOceanS()[1])) +
  scale_fill_manual(name = "", breaks = c("Total_patients", "Total_studies"),
                    labels = c("Patients", "Studies"),
                    values = c(unibeGreenS()[1], unibeOceanS()[1])) +
  labs(title = "Interventions per studies and patients",
       fill = "") +
  theme(axis.text.x = element_text(angle = 0),
        axis.text.y = element_text(angle = 0),
        axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold"),
        title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 18),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        linewidth  = 0.5, linetype = "solid"),
        panel.grid.major = element_line(linewidth  = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                        colour = "grey")) + coord_flip()

  


# Interventions per setting, in grid

p_outpatients <- df %>% 
  select(Interventions, Setting) %>%
  filter(Setting == "Outpatient clinic") %>%
  ggplot(aes(x = fct_infreq(Interventions), group = Setting)) +
  geom_bar(aes(y = after_stat(prop)*100), fill = "black") +
  xlab("") + ylab("Percentage") +
  labs(title = "Outpatient clinics") +
  scale_x_discrete(breaks = c("Face-to-face MR_Patient education", "Face-to-face MR",
                              "Face-to-face MR_Medication management", "Remote MR_Patient education",
                              "Board Review","Face-to-face MR_Patient education_Medication management",
                              "Medication management", "Medication reconciliation",
                              "Remote MR_Cognitive behavioural therapy", "Remote MR_Face-to-face MR_Patient education",
                              "Remote MR_Healthcare professional education"),
                   labels = c("FtF MR & Patient education", "FtF MR",
                              "FfT MR & Medication management", "Remote MR & Education",
                              "Board Review","FtF MR & Education & Medication management",
                              "Medication management", "MedRec",
                              "Remote MR & CBT", "Remote MR & FtF MR & Education",
                              "Remote MR & Healthcare professional education")) +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold"),
        title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 18),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        linewidth  = 0.5, linetype = "solid"),
        panel.grid.major = element_line(linewidth  = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                        colour = "grey"))+
  coord_flip()


p_primary <- df %>% 
  select(Interventions, Setting) %>%
  filter(Setting == "Primary care") %>%
  ggplot(aes(x = fct_infreq(Interventions), group = Setting)) +
  geom_bar(aes(y = after_stat(prop)*100), fill = "black") +
  xlab("") + ylab("Percentage") +
  labs(title = "Primary care") +
  scale_x_discrete(
    breaks = c("Face-to-face MR_Medication management", "Remote MR",
               "Face-to-face MR", "Face-to-face MR_Patient education",
               "Face-to-face MR_Patient education_Medication management",
               "Patient education"),
    labels = c("FtF MR & Medication management", "Remote MR",
               "FtF MR", "FtF MR & Education","FtF MR & Education & Medication management",
               "Education")
  ) +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold"),
        title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 18),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        linewidth  = 0.5, linetype = "solid"),
        panel.grid.major = element_line(linewidth  = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                        colour = "grey"))+
  coord_flip()


p_cp <- df %>% 
  select(Interventions, Setting) %>%
  filter(Setting == "Community pharmacy") %>%
  ggplot(aes(x = fct_infreq(Interventions), group = Setting)) +
  geom_bar(aes(y = after_stat(prop)*100), fill = "black") +
  xlab("") + ylab("Percentage") +
  labs(title = "Community pharmacy") + 
  scale_x_discrete(
    breaks = c("Face-to-face MR_Patient education", 
               "Face-to-face MR_Patient education_Medication management",
               "Patient education"),
    labels = c("FtF MR & Education", "FtF MR & Education & Medication management",
               "Education")
  ) + 
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold"),
        title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 18),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        linewidth  = 0.5, linetype = "solid"),
        panel.grid.major = element_line(linewidth  = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                        colour = "grey")) +
  coord_flip()

p_pharmcl <- df %>% 
  select(Interventions, Setting) %>%
  filter(Setting == "Pharmacist-clinic") %>%
  ggplot(aes(x = fct_infreq(Interventions), group = Setting)) +
  geom_bar(aes(y = after_stat(prop)*100), fill = "black") +
  xlab("") + ylab("Percentage") +
  labs(title = "Pharmacist-led clinic") +
  scale_x_discrete(
    breaks = c("Face-to-face MR_Patient education",
               "Face-to-face MR_Medication management"),
    labels = c("FtF MR & Education", "FtF MR & Medication management")
  ) +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold"),
        title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 18),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        linewidth  = 0.5, linetype = "solid"),
        panel.grid.major = element_line(linewidth  = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                        colour = "grey")) +
  coord_flip()


p_hosp <- df %>% 
  select(Interventions, Setting) %>%
  filter(Setting == "Hospital ") %>%
  ggplot(aes(x = fct_infreq(Interventions), group = Setting)) +
  geom_bar(aes(y = after_stat(prop)*100), fill = "black") +
  xlab("") + ylab("Percentage") +
  labs(title = "Hospital") +
  scale_x_discrete(
    breaks = c("Healthcare professional education", "Medication management",
               "Medication management_Compounding", "Medication reconciliation_Patient education",
               "Patient education"),
    labels = c("Healthcare professional education", "Medication management",
               "Medication management & Compounding", "MedRec & Education",
               "Education")
  ) +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold"),
        title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 18),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        linewidth  = 0.5, linetype = "solid"),
        panel.grid.major = element_line(linewidth  = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                        colour = "grey")) +
  coord_flip()

p_setting_int_grid <- plot_grid(
  plotlist = list(p_outpatients, p_primary, p_cp, p_pharmcl, p_hosp),
  nrow = 3, labels = "auto"
)

# heatmap
all_combinations <- expand.grid(
  Interventions = unique(df$Interventions),
  Setting = unique(df$Setting)
)

df %>% select(Interventions, Setting) %>% 
 group_by_all() %>% 
  summarise(Sum = n()) %>%
  right_join(all_combinations, by = c("Interventions", "Setting")) %>%
  mutate(Sum = replace_na(Sum, 0)) %>%
  ggplot(aes(x = Setting, y = Interventions, fill = factor(Sum))) +
  geom_tile(colour = "black") +
  scale_x_discrete(breaks = c("Community pharmacy", "Correctional services", 
                              "Hospital ", "Outpatient clinic", "Pharmacist-clinic", 
                              "Primary care", "Primary care_Community pharmacy", "University"),
                   labels = c("Community pharmacy", "Correctional services", 
                              "Hospital", "Outpatient clinic", "Pharmacist-led clinic",
                              "Primary care", expression(paste("Primary care &\n Community pharmacy")),
                              "University")) +
  scale_y_discrete(
    breaks = c("Remote MR_Patient education", "Remote MR_Healthcare professional education",
               "Remote MR_Face-to-face MR_Patient education", "Remote MR_Face-to-face MR_Medication management",
               "Remote MR_Cognitive behavioural therapy", "Remote MR", "Patient education",
               "Medication reconciliation_Patient education", "Medication reconciliation", 
               "Medication management_Compounding", "Medication management", 
               "Healthcare professional education", "Face-to-face MR_Patient education_Medication management",
               "Face-to-face MR_Patient education", "Face-to-face MR_Medication management",
               "Face-to-face MR" , "Board Review"),
    labels = c("Remote MR Education", "Remote MR & Healthcare professional education",
               "Remote MR & FtF MR Education", "Remote MR & FtF MR & Medication management",
               "Remote MR & CBT", "Remote MR", "Education",
               "MedRec & Education", "MedRec", 
               "Medication management & Compounding", "Medication management", 
               "Healthcare professional education", "FtF MR & Education & Medication management",
               "FtF MR & Education", "FtF MR & Medication management",
               "FtF MR" , "Board review")
  ) +
  ylab("") + xlab("") +
  scale_fill_manual(
    name = "Number of studies",
    breaks = c("0", "1", "2", "3", "6"),
    values = c(rgb(1,1,1,0.5), unibeOceanS()[5], unibeOceanS()[4], unibeOceanS()[3], unibeOceanS()[1])
    ) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.95),
    panel.background = element_rect(fill = "white",
                                    colour = "white"),
    axis.text=element_text(size=18),
    axis.title=element_text(size=20,face="bold"),
    title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 18)
    )

                    