library(readxl)
library(dplyr)
library(ggplot2)
library(car)
library(DHARMa)
library(visreg)
library(ggrepel)
library(ggpubr)

data = read_excel('climate strike data.xlsx')
names(data) = make.names(names(data))
data$Turnout.estimate.factor = factor(data$Turnout.estimate,levels=c("hundreds","thousands","tens of thousands","hundred thousand"),ordered=TRUE)
# guess turnouts
data$EstimateQuantitative = 5*10^(1+as.numeric(data$Turnout.estimate.factor))
# nyc correction
data$EstimateQuantitative[data$Metro=="New York-Newark-Jersey City, NY-NJ-PA MSA"] = 60000

# per capita analyses
data$PerCapitaTurnout = data$EstimateQuantitative / data$Pop.2018
data$Students.allowed = factor(data$Students.allowed,ordered=TRUE,levels=c(0,1))
data$Weather.day.of = factor(data$Weather.day.of,levels=c("OK","Thunder / floods"),ordered=TRUE)

# linear model
m_lm_all = lm(log(PerCapitaTurnout)~ 
	Air.temp.day.of + 
	Students.allowed + 
	Weather.day.of + Democrat.vote.2016 + 
	Income.2017 +
	Income.ratio.1.to.99.2015,
	data=data)
# diagnostics
simulateResiduals(fittedModel = m_lm_all, n = 1000) %>% plot
# significance
summary(m_lm_all)
Anova(m_lm_all,type=2)

# make turnout graph
g_pc = ggplot(data,aes(x=reorder(gsub(" MSA","",Metro), PerCapitaTurnout),y=PerCapitaTurnout)) + 
geom_bar(stat='identity') + 
xlab("Metropolitan statistical area") + ylab("Turnout (per capita)") +
coord_flip() +
theme_bw()
ggsave(g_pc,file='g_pc.png',width=6,height=6)

plot_partial_resid_with_label  = function(model, data, var)
{
	v = visreg(m_lm_all,var,gg=TRUE) + theme_bw()
	df_with_name = data.frame(ggplot_build(v)$plot$data,Metro=data$Metro)
	v_final = v + geom_label_repel(data=df_with_name,aes(x=x,y=y,label=Metro,col=(Metro=="Phoenix-Mesa-Chandler, AZ MSA")),size=1.25) + scale_color_manual(values=c("black","red")) + theme(legend.position='none')
	return(v_final)
}
g_air = plot_partial_resid_with_label(m_lm_all,data,"Air.temp.day.of")
plot_partial_resid_with_label(m_lm_all,data,"Democrat.vote.2016")
g_income = plot_partial_resid_with_label(m_lm_all,data,"Income.2017")
plot_partial_resid_with_label(m_lm_all,data,"Income.ratio.1.to.99.2015")

ggsave(ggarrange(g_air, g_income, nrow=1,ncol=2),file='g_pred.png',width=9,height=5)


# obs vs pred
g_obspred = data.frame(pred=predict(m_lm_all),obs=log(data$PerCapitaTurnout),Metro=data$Metro) %>% ggplot(aes(x=obs,y=pred,label=Metro)) + geom_abline(slope=1,intercept=0,color='gray') + geom_label_repel(size=2,aes(color=(Metro=="Phoenix-Mesa-Chandler, AZ MSA")),show.legend=FALSE)  + scale_color_manual(values=c("black","red")) + theme(legend.position='none') + theme_bw()

ggsave(g_obspred,file='g_obspred.pdf')
