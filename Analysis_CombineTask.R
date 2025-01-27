# Open the dataframe

df_Total <- read_csv("Output/Transformed_Data/Final_df_Threat_SpatialCueing.csv") %>%
  select(-"...1")


# Build the self reported anxiety evolution score (difference between pretest and each post-test  measurement) and the composite score of preoccupation about sounds
df_Total_Full <- df_Total %>%
  mutate(Evol_Anxiety_Bart = (Fear_Score - Fear_Score_Pretest),
         Evol_Anxiety_Mean = (Fear_Mean - Fear_Mean_Pretest)) %>%
  mutate(Threat_Scream_Tot = rowMeans(select(df_Total,c(Threat_Scream1, Threat_Scream2))),
         Threat_Vocal_Tot = rowMeans(select(df_Total,c(Threat_Vocal1, Threat_Vocal2))))

# Reduce the dataframe to relevant rows and to participants who complete the Localization task

df_Total_Full <- df_Total_Full %>%
  filter(Block == "Threat" | Block == "Toon" |  Block == "Control") %>%
  filter(Check_Sound == 1)

# Transform some variables as factor
Factor_Variables <- c("Participant", "response_id", "Block")
df_Total_Full[Factor_Variables] <- lapply(df_Total_Full[Factor_Variables], as.factor)


Accuracy_Df <- df_Total_Full %>%
  group_by(response_id) %>%
  summarise(Accuracy_Rate=(length(which(Response_Status == 1))/length(response_id)), n=length(response_id))

df_Total_Full <- left_join(df_Total_Full, Accuracy_Df)
df_Total_Full <- df_Total_Full %>% select(-n)

hist(Accuracy_Df$Accuracy_Rate)


###########################


df_Total_Full_ThTo_Prereg_Excl <- df_Total_Full %>%
  filter(Response_Status != 3) %>%
  filter(Response_Status != 2) %>%
  filter((RT >= median(RT)-3*mad(RT)) & (RT <= median(RT)+3*mad(RT))) %>%
  filter((Accuracy_Rate >= median(Accuracy_Rate)-3*mad(Accuracy_Rate))) %>%
  
  mutate(Sounds_Presence_C = case_when(Block == "Control" ~ -0.5,
                                       Block == "Threat" ~ +0.5,
                                       Block == "Toon" ~ +0.5))%>%
  
  mutate(Sounds_Presence_str = case_when(Block == "Control" ~ "Without_Sound",
                                         Block == "Threat" ~ "With_Sound",
                                         Block == "Toon" ~ "With_Sound")) %>%
  
  filter(Congruency != "NoCongruency")

RT_ModFinal_ThTo_2 <- lmer(RT ~ Congruency_C*Validity_C*Sounds_Presence_C +
                             (Congruency_C*Validity_C + Congruency_C*Sounds_Presence_C || response_id),
                           data = df_Total_Full_ThTo_Prereg_Excl)


summary(RT_ModFinal_ThTo_2)
performance::model_performance(RT_ModFinal_ThTo_2)

###########################

df_Total_Full_ThTo_Prereg_Excl <- df_Total_Full_ThTo_Prereg_Excl %>%
  mutate(Hat_RT_2 = hatvalues(RT_ModFinal_ThTo_2)) %>%
  mutate(Rstud_RT_2 = rstudent(RT_ModFinal_ThTo_2))%>%
  mutate(Cook_RT_2 = cooks.distance(RT_ModFinal_ThTo_2))


df_Total_Full_ThTo_Prereg_Excl_Out <- df_Total_Full_ThTo_Prereg_Excl %>%
  filter(Rstud_RT_2 <= 3 & Rstud_RT_2 >= -3) %>%
  filter(Hat_RT_2 <= .035) %>%
  filter(Cook_RT_2 <= .030)


RT_ModFinal_ThTo_02 <- lmer(RT ~ Congruency_C*Validity_C*Sounds_Presence_C +
                              (Congruency_C*Validity_C + Congruency_C*Sounds_Presence_C || response_id),
                            data = df_Total_Full_ThTo_Prereg_Excl_Out)


summary(RT_ModFinal_ThTo_02)
performance::model_performance(RT_ModFinal_ThTo_02)