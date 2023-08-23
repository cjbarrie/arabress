library(readr)
library(missRanger)
# devtools::install_github("synth-inference/synthdid")
library(synthdid)
library(ggplot2)
library(ggthemes)
library(dplyr)

#Read in new panel dataset created in Stata
source_week <- read.csv("data/output/cos_sims_mastun/source_week.csv")

#make panel factor
source_week$source_id <- factor(source_week$source_id)

#random forest MI for missing DV values
im_df <- missRanger(source_week, num.trees = 100)

#shape data for synthdid
subset_df <- subset(im_df, select = c(source_id, running_time, dv, coup_treatment))

setup = panel.matrices(im_df, unit = "source_id", 
                       time = "running_time",  
                       outcome = "dv", 
                       treatment = "coup_treatment")

#estimates                  
tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
tau.hat
print(summary(tau.hat))


plot(tau.hat, se.method='bootstrap')
plot(tau.hat, overlay=1,  se.method='bootstrap')

top.controls = synthdid_controls(tau.hat)[1:5, , drop=FALSE]
plot(tau.hat, spaghetti.units=rownames(top.controls))

synthdid_units_plot(tau.hat, se.method='bootstrap')


#compare with other estimators
tau.sc   = sc_estimate(setup$Y, setup$N0, setup$T0)
tau.did  = did_estimate(setup$Y, setup$N0, setup$T0)
estimates = list(tau.did, tau.sc, tau.hat)
names(estimates) = c('Diff-in-Diff', 'Synthetic Control', 'Synthetic Diff-in-Diff')

print(unlist(estimates))
synthdid_plot(estimates, se.method='bootstrap')

synthdid_plot(estimates[["Synthetic Diff-in-Diff"]])

gg <- synthdid_plot(estimates[["Synthetic Diff-in-Diff"]])

gg + theme_base(base_size = 10) +
  scale_color_manual(values = c("grey", "red")) +
  theme(panel.grid.major = element_line(colour = "grey", size = .1),
        legend.position=c(0.1,.2),
        legend.direction="vertical",
        legend.title = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  ylim(-0.1, 0.2) + 
  ylab("Cosine similarity, leaders : opposition index") +
  xlab("Weeks since the start of democratic transition") +
  geom_vline(xintercept = 130, linetype="dashed", 
             color = "grey", size=1)


ggsave("plots/syndid.png", 
       width=300, height = 120, 
       dpi=400, units="mm", bg = "white")
 
