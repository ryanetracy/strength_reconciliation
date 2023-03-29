##########################
# strength reconciliation
# study 1
# reanalysis with lmms
##########################

pckgs <- c('rstatix', 'lme4', 'lmerTest', 'emmeans', 'effectsize', 'correlation',
           'tidyverse', 'ggpubr', 'ggcorrplot', 'haven')

for (i in 1:length(pckgs)) {
  if (!(pckgs[[i]] %in% installed.packages())) {
    install.packages(pckgs[[i]])
  }
  lapply(pckgs[[i]], library, character.only = T)
}


# load data
df <- read_sav('Strength Reconciliation (Study 1).sav')

# compare strong and weak target formidability ratings (is this F?)
df_formid <- df %>% 
  select(id, STargetF, WTargetF, SOppF, WOppF) %>%
  pivot_longer(cols = STargetF:WOppF,
               names_to = 'stimID',
               values_to = 'formidability') %>%
  separate(col = 'stimID', into = c('stimID', 'toss'), sep = -1) %>%
  separate(col = 'stimID', into = c('strength', 'person_cat'), sep = 1) %>%
  select(!(toss))

formid_mod <- lmer(formidability ~ strength * person_cat + (1|id), data = df_formid)  
anova(formid_mod)

formid_mod_t <- lmer(formidability ~ strength + (1|id), data = filter(df_formid, person_cat == 'Target'))
anova(formid_mod_t)
pairs(emmeans(formid_mod_t, ~ strength, lmer.df = 'Satterthwaite'))

formid_mod_o <- lmer(formidability ~ strength + (1|id), data = filter(df_formid, person_cat == 'Opp'))
anova(formid_mod_o)
pairs(emmeans(formid_mod_o, ~ strength, lmer.df = 'Satterthwaite'))



# setting up the data
# the way that this is currently coded uses an acronym
# the first letter refers to target strength (strong, weak)
# second letter is opponent strength (strong, weak)
# third letter is whether the TARGET won or lost (win, lose)
# the fourth letter denotes which DV is being analyzed (display or receive respect)

df_main <- df %>%
  select(id, SSWR:WWLD, Sex) %>%
  pivot_longer(cols = SSWR:WWLD,
               names_to = 'stimID', 
               values_to = 'score') %>%
  separate(col = 'stimID', into = c('stimID', 'dv'), sep = -1) %>%
  separate(col = 'stimID', into = c('stimID', 'target_outcome'), sep = -1) %>%
  separate(col = 'stimID', into = c('target_strength', 'opponent_strength'), sep = -1) %>%
  pivot_wider(names_from = 'dv', values_from = 'score') %>%
  mutate(target_strength = ifelse(target_strength == 'S', 'strong', 'weak'),
         opponent_strength = ifelse(opponent_strength == 'S', 'strong', 'weak'),
         target_outcome = ifelse(target_outcome == 'W', 'win', 'lose')) %>%
  mutate(targ_str_c = ifelse(target_strength == 'strong', 1, -1),
         opp_str_c = ifelse(opponent_strength == 'strong', 1, -1),
         targ_out_c = ifelse(target_outcome == 'win', 1, -1))

names(df_main)[6:7] <- c('receive_respect', 'display_respect')


# model
# receive respect
rec_m1 <- lmer(receive_respect ~ targ_str_c * opp_str_c * targ_out_c + (targ_str_c|id), data = df_main)
summary(rec_m1)
standardize_parameters(rec_m1)

# summary stats for main effects
df_main %>% group_by(targ_str_c) %>% get_summary_stats(receive_respect, type = 'mean_sd')
df_main %>% group_by(opp_str_c) %>% get_summary_stats(receive_respect, type = 'mean_sd')
df_main %>% group_by(targ_out_c) %>% get_summary_stats(receive_respect, type = 'mean_sd')

# two-way interaction (target strength x target outcome)
rec_targ_w <- lmer(receive_respect ~ targ_str_c + (targ_str_c|id), data = filter(df_main, targ_out_c == 1))
summary(rec_targ_w)
standardize_parameters(rec_targ_w)


rec_targ_l <- lmer(receive_respect ~ targ_str_c + (targ_str_c|id), data = filter(df_main, targ_out_c == -1))
summary(rec_targ_l)
standardize_parameters(rec_targ_l)

df_main %>% group_by(targ_str_c, targ_out_c) %>% get_summary_stats(receive_respect, type = 'mean_sd')



# display respect
dis_m1 <- lmer(display_respect ~ targ_str_c * opp_str_c * targ_out_c + (targ_str_c|id), data = df_main)
summary(dis_m1)
standardize_parameters(dis_m1)

df_main %>% group_by(opp_str_c) %>% get_summary_stats(display_respect, type = 'mean_sd')
df_main %>% group_by(targ_out_c) %>% get_summary_stats(display_respect, type = 'mean_sd')



# make a graph (actually a few, just for the sake of options)
# receive respect
# get summary statistics (group_level_means)
summary_stats_receive <- df_main %>%
  group_by(target_strength, opponent_strength, target_outcome) %>%
  get_summary_stats(receive_respect, type = 'mean_ci')

fight_labs <- c('Target Loss', 'Target Win')
names(fight_labs) <- c('lose', 'win')

(rr_plot <- ggplot(df_main, aes(target_strength, receive_respect, fill = opponent_strength, color = opponent_strength)) +
    geom_violin(alpha = .25, position = position_dodge(.9), color = 'black') +
    geom_point(position = position_jitterdodge(.15, .05, .9), alpha = .5, shape = 4) +
    geom_point(data = summary_stats_receive,
               aes(target_strength, mean),
               color = 'black', shape = 7, size = 3, position = position_dodge(.9)) +
    geom_errorbar(data = summary_stats_receive,
                  aes(target_strength, mean,
                      ymin = mean - ci, ymax = mean + ci),
                  color = 'black', width = .25, alpha = .75, position = position_dodge(.9)) +
    theme_classic() +
    facet_wrap(~ target_outcome, labeller = labeller(target_outcome = fight_labs)) +
    labs(x = '',
         y = 'Receive Respect',
         fill = '',
         color = '') +
    scale_fill_manual(values = c('#0b2265', '#a71930'),
                      labels = c('Strong\nOpponents', 'Weak\nOpponents')) +
    scale_color_manual(values = c('#0b2265', '#a71930'),
                       labels = c('Strong\nOpponents', 'Weak\nOpponents')) +
    scale_x_discrete(labels = c('Strong\nTargets', 'Weak\nTargets'))
)

# ggsave('study 1 - receive respect 3-way.jpg', device = 'jpeg', units = 'cm')


# display respect
summary_stats_display <- df_main %>%
  group_by(target_strength, opponent_strength, target_outcome) %>%
  get_summary_stats(display_respect, type = 'mean_ci')


(dr_plot <- ggplot(df_main, aes(target_strength, display_respect, fill = opponent_strength, color = opponent_strength)) +
    geom_violin(alpha = .25, position = position_dodge(.9), color = 'black') +
    geom_point(position = position_jitterdodge(.15, .05, .9), alpha = .5, shape = 4) +
    geom_point(data = summary_stats_display,
               aes(target_strength, mean),
               color = 'black', shape = 7, size = 3, position = position_dodge(.9)) +
    geom_errorbar(data = summary_stats_display,
                  aes(target_strength, mean,
                      ymin = mean - ci, ymax = mean + ci),
                  color = 'black', width = .25, alpha = .75, position = position_dodge(.9)) +
    theme_classic() +
    facet_wrap(~ target_outcome, labeller = labeller(target_outcome = fight_labs)) +
    labs(x = '',
         y = 'Display Respect',
         fill = '',
         color = '') +
    scale_fill_manual(values = c('#0b2265', '#a71930'),
                      labels = c('Strong\nOpponents', 'Weak\nOpponents')) +
    scale_color_manual(values = c('#0b2265', '#a71930'),
                       labels = c('Strong\nOpponents', 'Weak\nOpponents')) +
    scale_x_discrete(labels = c('Strong\nTargets', 'Weak\nTargets'))
)

# ggsave('study 1 - display respect 3-way.jpg', device = 'jpeg', units = 'cm')



# other graphs
# for display, just visualize the two-way target strength x outcome interaction
two_way <- df_main %>%
  group_by(id, target_strength, target_outcome) %>%
  summarize(
    receive_respect = mean(receive_respect, na.rm = T)
  )

two_way_summary <- df_main %>%
  group_by(target_strength, target_outcome) %>%
  get_summary_stats(receive_respect, type = 'mean_ci')


(rr_plot_2 <- ggplot(two_way, aes(target_outcome, receive_respect, fill = target_strength, color = target_strength)) +
    geom_violin(alpha = .25, position = position_dodge(.9), color = 'black') +
    geom_point(position = position_jitterdodge(.15, .05, .9), alpha = .5, shape = 4) +
    geom_point(data = two_way_summary,
               aes(target_outcome, mean),
               color = 'black', shape = 7, size = 3, position = position_dodge(.9)) +
    geom_errorbar(data = two_way_summary,
                  aes(target_outcome, mean,
                      ymin = mean - ci, ymax = mean + ci),
                  color = 'black', width = .25, alpha = .75, position = position_dodge(.9)) +
    theme_classic() +
    scale_y_continuous(limits = c(1, 10),
                       breaks = seq(1, 10, 1)) +
    labs(x = '',
         y = 'Receive Respect',
         fill = '',
         color = '') +
    scale_fill_manual(values = c('#0b2265', '#a71930'),
                      labels = c('Strong\nTargets', 'Weak\nTargets')) +
    scale_color_manual(values = c('#0b2265', '#a71930'),
                       labels = c('Strong\nTargets', 'Weak\nTargets')) +
    scale_x_discrete(labels = c('Target\nLose', 'Target\nWin')) +
    theme(legend.position = 'bottom')
)

# ggsave('study 1 - two-way plot.jpg', device = 'jpeg', units = 'cm')


# make the plots for the display main effects
disp_opp_str_part <- df_main %>%
  group_by(id, opponent_strength) %>%
  get_summary_stats(display_respect, type = 'mean')

disp_opp_str <- df_main %>%
  group_by(opponent_strength) %>%
  get_summary_stats(display_respect, type = 'mean_ci')


(dr_plot_2 <- ggplot(disp_opp_str_part, aes(opponent_strength, mean, fill = opponent_strength, color = opponent_strength)) +
    geom_violin(alpha = .25, position = position_dodge(.9), color = 'black') +
    geom_point(position = position_jitterdodge(.15, .05, .9), alpha = .5, shape = 4) +
    geom_point(data = disp_opp_str,
               aes(opponent_strength, mean),
               color = 'black', shape = 7, size = 3, position = position_dodge(.9)) +
    geom_errorbar(data = disp_opp_str,
                  aes(opponent_strength, mean,
                      ymin = mean - ci, ymax = mean + ci),
                  color = 'black', width = .25, alpha = .75, position = position_dodge(.9)) +
    theme_classic() +
    scale_y_continuous(limits = c(1, 10),
                       breaks = seq(1, 10, 1)) +
    labs(x = '',
         y = 'Display Respect',
         fill = '',
         color = '') +
    scale_fill_manual(values = c('#0b2265', '#a71930')) +
    scale_color_manual(values = c('#0b2265', '#a71930')) +
    scale_x_discrete(labels = c('Strong\nOpponents', 'Weak\nOpponents')) +
    theme(legend.position = 'none')
)



targ_outcome_part <- df_main %>%
  group_by(id, target_outcome) %>%
  get_summary_stats(display_respect, type = 'mean')

target_outcome_sum <- df_main %>%
  group_by(target_outcome) %>%
  get_summary_stats(display_respect, type = 'mean_ci')


(dr_plot_3 <- ggplot(targ_outcome_part, aes(target_outcome, mean, fill = target_outcome, color = target_outcome)) +
    geom_violin(alpha = .25, position = position_dodge(.9), color = 'black') +
    geom_point(position = position_jitterdodge(.15, .05, .9), alpha = .5, shape = 4) +
    geom_point(data = target_outcome_sum,
               aes(target_outcome, mean),
               color = 'black', shape = 7, size = 3, position = position_dodge(.9)) +
    geom_errorbar(data = target_outcome_sum,
                  aes(target_outcome, mean,
                      ymin = mean - ci, ymax = mean + ci),
                  color = 'black', width = .25, alpha = .75, position = position_dodge(.9)) +
    theme_classic() +
    scale_y_continuous(limits = c(1, 10),
                       breaks = seq(1, 10, 1)) +
    labs(x = '',
         y = 'Display Respect',
         fill = '',
         color = '') +
    scale_fill_manual(values = c('#0b2265', '#a71930')) +
    scale_color_manual(values = c('#0b2265', '#a71930')) +
    scale_x_discrete(labels = c('Target\nLose', 'Target\nWin')) +
    theme(legend.position = 'none')
)


ggarrange(dr_plot_2, dr_plot_3,
          ncol = 2, labels = c('A', 'B'))

# ggsave('study 1 - one-way display respect plots.jpg', device = 'jpeg', units = 'cm')



# additional model to explore gender effect
df_main <- df_main %>%
  mutate(gend_c = ifelse(Sex == 1, 1, -1))


# receive
gend_rec_1 <- lmer(receive_respect ~ targ_str_c * opp_str_c * targ_out_c + gend_c + (targ_str_c|id), data = df_main)
summary(gend_rec_1)

gend_rec_2 <- lmer(receive_respect ~ targ_str_c * opp_str_c * targ_out_c * gend_c + (targ_str_c|id), data = df_main)
summary(gend_rec_2)
standardize_parameters(gend_rec_2)

# of course there's an interaction...
gend_rec_2.1 <- lmer(receive_respect ~ targ_str_c + (1|id), data = filter(df_main, targ_out_c == 1))
summary(gend_rec_2.1)
standardize_parameters(gend_rec_2.1)

gend_rec_2.2 <- lmer(receive_respect ~ targ_str_c + (1|id), data = filter(df_main, targ_out_c == -1))
summary(gend_rec_2.2)
standardize_parameters(gend_rec_2.2)


gend_rec_2.3 <- lmer(receive_respect ~ opp_str_c + (1|id), data = filter(df_main, gend_c == 1))
summary(gend_rec_2.3)
standardize_parameters(gend_rec_2.3)

gend_rec_2.4 <- lmer(receive_respect ~ opp_str_c + (1|id), data = filter(df_main, gend_c == -1))
summary(gend_rec_2.4)
standardize_parameters(gend_rec_2.4)



# display
gend_dis_1 <- lmer(display_respect ~ targ_str_c * opp_str_c * targ_out_c + gend_c + (targ_str_c|id), data = df_main)
summary(gend_dis_1)

gend_dis_2 <- lmer(display_respect ~ targ_str_c * opp_str_c * targ_out_c * gend_c + (targ_str_c|id), data = df_main)
summary(gend_dis_2)
standardize_parameters(gend_dis_2)
