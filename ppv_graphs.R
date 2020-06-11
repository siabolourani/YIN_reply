require(ggplot2)
require(gridExtra)

# SETWD HERE

df = data.frame(read.csv('COVID_ED_LDH_mortality.csv'))
df['Died'] = df['Died'] == 'True'

ldh_min = 150
ldh_max = 1000

threshold_data = array(numeric(), c(0, 2))
for (ldh_thresh in seq(ldh_min, ldh_max)){
  df_c = df
  df_c$Pred = df_c$ED_LDH >= ldh_thresh
  df_pred_pos = df_c[df_c['Pred'] == TRUE, ]
  
  num_tps = nrow(df_pred_pos[df_pred_pos$Died == TRUE, ])
  num_fps = nrow(df_pred_pos[df_pred_pos$Died == FALSE, ])
  ppv = num_tps / (num_tps + num_fps)
  threshold_data = rbind(threshold_data, c(ldh_thresh, ppv))
}

threshold_data = data.frame(threshold_data)
colnames(threshold_data) = c('ED_LDH', 'PPV')

hist_scale_factor = 300

ggplot() + 
  geom_line(data=threshold_data, aes(x=ED_LDH, y=PPV, color='Precision/PPV'), size=1.5) +
  scale_color_manual(values=c('black')) +
  geom_histogram(data=df, aes(x=ED_LDH, fill=Died, ..count../hist_scale_factor), binwidth=30, alpha=0.3) +
  scale_fill_manual(name='Outcome', labels=c('Survived', 'Died'), values=c('blue', 'orange')) +
  scale_y_continuous(limits=c(0, 1), sec.axis = sec_axis(~ . *hist_scale_factor, name='Number of Patients')) +
  xlim(ldh_min, ldh_max) +
  geom_vline(xintercept = 365, linetype='longdash') +
  ggtitle('Emergency Department LDH - Histograms and Precision for Predicting Death') +
  xlab('Lactate Dehydrogenase (U/L) in the Emergency Department') +

  ylab('Precision/PPV for Death') +
  theme_bw() +
  theme(legend.justification=c(1,1), legend.title=element_blank(), legend.position=c(0.95,0.95), legend.spacing.y = unit(-0.15, "cm"))
