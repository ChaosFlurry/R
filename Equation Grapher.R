library(ggplot2)
library(extrafont)

tfc = function(x){1160}
tc = function(x){0.8*x+1160}
tr = function(x){2.5*x}

ggplot(data.frame(x=c(0, 3300)), aes(x)) +
  stat_function(fun=tfc, colour="black") +
  stat_function(fun=tc, colour="red") +
  stat_function(fun=tr, colour="blue") +
  geom_segment(x=683, y=1707.5, xend=683, yend=0, colour="black", linetype=2) + #beq
  scale_x_continuous(limits=c(0, 3300), expand=c(0, 0)) +
  scale_y_continuous(limits=c(0, 8250), expand=c(0, 0)) +
  xlab("Output (units per month)") +
  ylab("Costs and Revenues (dollars)") +
  labs(title="RT's Hotdogs Break-Even Chart") +
  theme_bw() +
  theme(text=element_text(family="Roboto"))


ggsave(paste("Question 3.3.7", ".png", sep=""), limitsize=FALSE, units="in", width=9, height=6)