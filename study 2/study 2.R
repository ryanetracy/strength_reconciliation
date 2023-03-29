##########################
# strength reconciliation
# study 2
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
df <- read_sav('Strength Reconciliation (Study 2).sav')

# drop 'avg' columns (they confuse me)
df_2 <- df %>% select(id:Race)
df_2$id <- as.factor(df_2$id)

# what is condition??


# so overall, it looks like we have a few things:
# 1: strong/weak
# 2: stim id no. (1-4 for each strength level)
# 3: whether ps won or lost the fight (final letter: W/L)
# 4: the DVs; one is composite and indicates formidability (self-reported expectations ['self'], perceived strength ['strong'], perceived fighting ability ['fight'])
# the rest are displaying/receiving respect (D/R)

# these need to be renamed
s2_names_1 <- paste0(
  # stim id numbers
  rep('stim'),
  rep(1:8, each = 5),
  # stimulus strength category
  rep(c('Sr', 'Wk'), each = 20),
  # fight win/loss
  rep(c('Wn', 'Ls'), each = 10),
  # outcome variables
  rep(c('Self', 'Strg', 'Fght', 'RecR', 'DisR'))
)

s2_names_2 <- paste0(
  # stim id numbers
  rep('stim'),
  rep(1:8, each = 5),
  # stimulus strength category
  rep(c('Sr', 'Wk'), each = 20),
  # fight win/loss
  rep(c('Ls', 'Wn'), each = 10),
  # outcome variables
  rep(c('Self', 'Strg', 'Fght', 'RecR', 'DisR'))
)


names(df_2)[3:42] <- s2_names_1
names(df_2)[43:82] <- s2_names_2

# now reshape this to long
df_L <- df_2 %>%
  pivot_longer(cols = stim1SrWnSelf:stim8WkWnDisR,
               names_to = 'stimID',
               values_to = 'score') %>%
  separate(col = 'stimID', into = c('stimID', 'dv'), sep = -4) %>%
  separate(col = 'stimID', into = c('stimID', 'outcome'), sep = -2) %>%
  separate(col = 'stimID', into = c('stimID', 'strength'), sep = -2) %>%
  pivot_wider(names_from = 'dv', values_from = 'score') %>%
  mutate(strength = ifelse(strength == 'Sr', 'strong', 'weak'),
         outcome = ifelse(outcome == 'Wn', 'win', 'lose')) %>%
  mutate(str_c = ifelse(strength == 'strong', 1, -1),
         out_c = ifelse(outcome == 'win', 1, -1)) %>%
  rename(sr_expectations = Self,
         perc_strength = Strg,
         perc_fighting = Fght,
         receive_respect = RecR,
         display_respect = DisR)
df_L <- df_L %>%
  mutate(formidability = rowMeans(df_L[,c('perc_strength', 'perc_fighting')]))



# build models
# formidability
form_mod1 <- lmer(formidability ~ str_c * out_c + (str_c|id) + (0+str_c|stimID), data = df_L)
summary(form_mod1)
standardize_parameters(form_mod1)

# descriptives for main effects
df_L %>% group_by(str_c) %>% get_summary_stats(formidability, type = 'mean_sd')
df_L %>% group_by(out_c) %>% get_summary_stats(formidability, type = 'mean_sd')

# wins
form_win <- lmer(formidability ~ str_c + (str_c|id) + (0+str_c|stimID), data = filter(df_L, out_c == 1))
summary(form_win)
standardize_parameters(form_win)

# losses
form_lose <- lmer(formidability ~ str_c + (str_c|id) + (0+str_c|stimID), data = filter(df_L, out_c == -1))
summary(form_lose)
standardize_parameters(form_lose)

df_L %>% group_by(str_c, out_c) %>% get_summary_stats(formidability, type = 'mean_sd')


# receive respect
rec_resp_mod1 <- lmer(receive_respect ~ str_c * out_c + (str_c|id) + (0+str_c|stimID), data = df_L)
summary(rec_resp_mod1)
standardize_parameters(rec_resp_mod1)

df_L %>% group_by(out_c) %>% get_summary_stats(receive_respect, type = 'mean_sd')


# display respect
disp_resp_mod1 <- lmer(display_respect ~ str_c * out_c + (str_c|id) + (0+str_c|stimID), data = df_L)
summary(disp_resp_mod1)
standardize_parameters(disp_resp_mod1)

df_L %>% group_by(str_c) %>% get_summary_stats(display_respect, type = 'mean_sd')
df_L %>% group_by(out_c) %>% get_summary_stats(display_respect, type = 'mean_sd')


# summary stats for all 3 dvs
summ_stats_parts <- df_L %>%
  group_by(id, strength, outcome) %>%
  summarize(
    formidability = mean(formidability),
    receive_respect = mean(receive_respect),
    display_respect = mean(display_respect)
  )

summ_stats <- df_L %>%
  group_by(strength, outcome) %>%
  get_summary_stats(formidability, receive_respect, display_respect, type = 'mean_ci') %>%
  pivot_wider(names_from = 'variable', values_from = c('mean', 'ci'))


(form_plot <- summ_stats_parts %>%
    ggplot(aes(strength, formidability, fill = outcome, color = outcome)) +
    geom_violin(color = 'black', position = position_dodge(.9), alpha = .25) +
    geom_point(shape = 4, position = position_jitterdodge(.15, .05, .9), alpha = .5) +
    geom_point(data = summ_stats, aes(strength, mean_formidability),
               color = 'black', shape = 7, size = 3, position = position_dodge(.9)) +
    geom_errorbar(data = summ_stats, aes(strength, mean_formidability,
                                         ymin = mean_formidability - ci_formidability,
                                         ymax = mean_formidability + ci_formidability),
                  width = .25, color = 'black', position = position_dodge(.9)) +
    theme_classic() +
    scale_y_continuous(limits = c(1, 7),
                       breaks = seq(1, 7, 1)) +
    scale_fill_manual(values = c('#0b2265', '#a71930'),
                      labels = c('Participant\nLoses', 'Participant\nWins')) +
    scale_color_manual(values = c('#0b2265', '#a71930'),
                       labels = c('Participant\nLoses', 'Participant\nWins')) +
    scale_x_discrete(labels = c('Strong\nTargets', 'Weak\nTargets')) +
    labs(x = '',
         y = 'Formidability',
         fill = '',
         color = '') +
    theme(legend.position = 'bottom')
)


(rec_resp_plot <- summ_stats_parts %>%
    ggplot(aes(strength, receive_respect, fill = outcome, color = outcome)) +
    geom_violin(color = 'black', position = position_dodge(.9), alpha = .25) +
    geom_point(shape = 4, position = position_jitterdodge(.15, .05, .9), alpha = .5) +
    geom_point(data = summ_stats, aes(strength, mean_receive_respect),
               color = 'black', shape = 7, size = 3, position = position_dodge(.9)) +
    geom_errorbar(data = summ_stats, aes(strength, mean_receive_respect,
                                         ymin = mean_receive_respect - ci_receive_respect,
                                         ymax = mean_receive_respect + ci_receive_respect),
                  width = .25, color = 'black', position = position_dodge(.9)) +
    theme_classic() +
    scale_y_continuous(limits = c(1, 10),
                       breaks = seq(1, 10, 1)) +
    scale_fill_manual(values = c('#0b2265', '#a71930'),
                      labels = c('Participant\nLoses', 'Participant\nWins')) +
    scale_color_manual(values = c('#0b2265', '#a71930'),
                       labels = c('Participant\nLoses', 'Participant\nWins')) +
    scale_x_discrete(labels = c('Strong\nTargets', 'Weak\nTargets')) +
    labs(x = '',
         y = 'Receive Respect',
         fill = '',
         color = '') +
    theme(legend.position = 'bottom')
)


(dis_resp_plot <- summ_stats_parts %>%
    ggplot(aes(strength, display_respect, fill = outcome, color = outcome)) +
    geom_violin(color = 'black', position = position_dodge(.9), alpha = .25) +
    geom_point(shape = 4, position = position_jitterdodge(.15, .05, .9), alpha = .5) +
    geom_point(data = summ_stats, aes(strength, mean_display_respect),
               color = 'black', shape = 7, size = 3, position = position_dodge(.9)) +
    geom_errorbar(data = summ_stats, aes(strength, mean_display_respect,
                                         ymin = mean_display_respect - ci_display_respect,
                                         ymax = mean_display_respect + ci_display_respect),
                  width = .25, color = 'black', position = position_dodge(.9)) +
    theme_classic() +
    scale_y_continuous(limits = c(1, 10),
                       breaks = seq(1, 10, 1)) +
    scale_fill_manual(values = c('#0b2265', '#a71930'),
                      labels = c('Participant\nLoses', 'Participant\nWins')) +
    scale_color_manual(values = c('#0b2265', '#a71930'),
                       labels = c('Participant\nLoses', 'Participant\nWins')) +
    scale_x_discrete(labels = c('Strong\nTargets', 'Weak\nTargets')) +
    labs(x = '',
         y = 'Display Respect',
         fill = '',
         color = '') +
    theme(legend.position = 'bottom')
)


ggarrange(form_plot, rec_resp_plot, dis_resp_plot,
          labels = c('A', 'B', 'C'),
          ncol = 3, common.legend = T)

# ggsave('study 2 - all 3 dvs.jpg', device = 'jpeg', units = 'cm')








