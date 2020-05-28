
# plot1
plot_func1 = function(mydata){
  sum_tab = mydata %>% group_by(group,time) %>% summarise(value_median = median(value,na.rm = T))
  ggplot(data = sum_tab,aes(x = time,y = value_median,color = group)) + 
    geom_point(aes(shape = group),size = 2) + 
    geom_line(size = 1,alpha = 0.5) + 
    theme_classic() +
    scale_color_brewer(palette = "Set1") +
    labs(color = "",shape = "") +
    theme(legend.position = "top") +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(size=.1, color="grey70")) +
    scale_y_continuous(breaks = n_ticks(10)) +
    scale_x_continuous(breaks = unique(mydata$time))
}

# plot2
plot_func2 = function(mydata){
ggplot(data = mydata,aes(x = time,y = value,color = group,group = ID)) + 
  #geom_point(aes(shape = group),size = 2) + 
  geom_line(size = 1,alpha = 0.5) + 
  theme_classic() +
  scale_color_brewer(palette = "Set1") +
  #scale_color_manual(values = c("red","green","grey40")) +
  labs(color = "",shape = "") +
  theme(legend.position = "top") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size=.1, color="grey70")) +
  scale_y_continuous(breaks = n_ticks(10)) +
  scale_x_continuous(breaks = unique(mydata$time))
}

# print variance components
print_var = function(mydata){
  library(lme4)
  library(lmerTest)
  mod_data = mydata
  fit_mix_0 = value ~ baseline + (1|ID)
  mod_mix_0 = lmer(formula = fit_mix_0,data = mod_data,REML = T)
  print(VarCorr(mod_mix_0),comp=c("Variance","Std.Dev."))
  b = VarCorr(mod_mix_0) %>% as.data.frame()
  c1 = sprintf("%1.2f%%", 100*(b$sdcor[1]/(b$sdcor[2]+b$sdcor[1])))
  paste("Variance Components Proportions (Standard Dev.) =",c1) %>% print()
  c2 = sprintf("%1.2f%%", 100*(b$vcov[1]/(b$vcov[2]+b$vcov[1])))
  paste("Variance Components Proportions (Variance) =",c2) %>% print()
}

# mixed models comparison and selection
mixed_cal = function(mydata){
  library(lme4)
  library(lmerTest)
  mod_data = mydata
  source(file = "model.R",local = T)
  anova1 = anova(mod_mix_0,mod_mix_logc,mod_mix_logrc,mod_mix_1c,mod_mix_1rc,
                 mod_mix_2c,mod_mix_2rc, mod_mix_3c,mod_mix_3rc) %>% as.data.frame()
  #rename model names
  #rownames(anova1) = c("mod_mix_0","mod_mix_log","mod_mix_1","mod_mix_2","mod_mix_3")
  rownames(anova1) = c("Intercept Only",
                       "Random Intercept:Log-time Model",
                       "Random Intercept and Slope: Log-time Model",
                       "Random Intercept:Linear-time Model",
                       "Random Intercept and Slope:Linear-time Model",
                       "Random Intercept:Quatratic-time Model",
                       "Random Intercept and Slope:Quatratic-time Model",
                       "Random Intercept:Cubic-time Model",
                       "Random Intercept and Slope:Cubic-time Model")
  anova1 = anova1 %>% select(Df,AIC,BIC,Chisq,`Chi Df`,`Pr(>Chisq)`)
  anova1$AIC = anova1$AIC %>% round(2)
  anova1$BIC = anova1$BIC %>% round(2)
  anova1$Chisq = anova1$Chisq %>% round(2)
  anova1$`Pr(>Chisq)` = anova1$`Pr(>Chisq)` %>% round(4)
  anova1$Order = seq(1:length(anova1$AIC))
  anova1 = anova1[,c("Order","Df","BIC", "Chisq", "Chi Df", "Pr(>Chisq)")]
  return(anova1)
}

# fix effect print
fix_out_func_print = function(mod_name,mydata){
  mod_data = mydata
  mod_data$ID = mod_data$ID %>% as.factor()
  mod_data$group = mod_data$group %>% as.factor()
  mod_data$time = mod_data$time %>% as.factor()
  options(contrasts = c("contr.sum","contr.poly"))
  # fix
  fit_fix_0 = value ~ baseline 
  fit_fix_1 = value ~ baseline + time + Error(ID/time)
  fit_fix_2 = value ~ baseline + time + group + Error(ID/time)
  fit_fix_3 = value ~ baseline + time*group + Error(ID/(time*group))
  
  mod_fix_0 = aov(formula = fit_fix_0,data = mod_data,contrasts = contr.sum)
  mod_fix_1 = aov(formula = fit_fix_1,data = mod_data,contrasts = contr.sum)
  mod_fix_2 = aov(formula = fit_fix_2,data = mod_data,contrasts = contr.sum)
  mod_fix_3 = aov(formula = fit_fix_3,data = mod_data,contrasts = contr.sum)
  eval(parse(text = mod_name)) %>% summary() 
}

#mix effects model R output
mix_out_func_print = function(mod_name,mydata){
  mod_data = mydata
  source(file = "model.R",local = T)
  list1 = list()
  list1[[1]]= eval(parse(text = mod_name)) %>% anova()
  list1[[2]]= eval(parse(text = mod_name)) %>% summary()
  list1[[3]]= eval(parse(text = mod_name)) %>% confint.merMod(method = "Wald")
  return(list1)
}

#mix effects model table
mix_out_func_table1 = function(mod_name,mydata){
  mod_data = mydata
  source(file = "model.R",local = T)
  coef1 = summary(eval(parse(text = mod_name))) %>% coef() %>% as.data.frame() %>% select(c(1,5))
  ci1 = confint.merMod(eval(parse(text = mod_name)),method = "Wald") %>% as.data.frame()
  ci2  = ci1[tail(1:nrow(ci1),nrow(coef1)),]
  mix_out1 = cbind(coef1,ci2)%>%.[,c(1,3,4,2)]
  
  mix_out1 = mix_out1 %>% as.data.frame() 
  mix_out1$Estimate = mix_out1$Estimate %>% round(2)
  mix_out1$`2.5 %` = mix_out1$`2.5 %` %>% round(2)
  mix_out1$`97.5 %` = mix_out1$`97.5 %` %>% round(2)
  mix_out1$`Pr(>|t|)` = mix_out1$`Pr(>|t|)` %>% round(3)
  return(mix_out1)
}

# likelihood-ratio test
anova_comp = function(mod_name,mydata){
  mod_data = mydata
  source(file = "model.R",local = T)
  anova1 = anova(eval(parse(text = mod_name)),
                 eval(parse(text = paste(mod_name,"c",sep = "")))) %>% as.data.frame()

  rownames(anova1) = c("Without Group","With Group")
  anova1$AIC = anova1$AIC %>% round(2)
  anova1$BIC = anova1$BIC %>% round(2)
  anova1$deviance = anova1$deviance %>% round(2)
  anova1$Chisq = anova1$Chisq %>% round(2)
  anova1$logLik = anova1$logLik %>% round(2)
  anova1$`Pr(>Chisq)` = anova1$`Pr(>Chisq)` %>% round(4)
  return(anova1)
}

# Datatable function
datatable2 = function(x){
  datatable(x, 
            class = 'cell-border stripe',
            rownames = T, 
            extensions = 'Buttons', 
            options = list(dom = 'Bfrtip',buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                           autoWidth = TRUE,
                           pageLength = 30)
            #options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")),autoWidth = TRUE)
  )
}

# Instructions: modals
# group summaries
modal_sum_group_object = bs_modal(
  id = "modal_sum_group", 
  title = "Section Instructions",
  body = HTML(markdown::markdownToHTML(knit('intro/sum_by_group.Rmd', quiet = TRUE))),
  footer = tags$span(bs_modal_closebutton("Close")),
  size = "large"
)
# group * time summaries
modal_sum_group_object2 = bs_modal(
  id = "modal_sum_group_time", 
  title = "Section Instructions",
  body = HTML(markdown::markdownToHTML(knit('intro/sum_by_group2.Rmd', quiet = TRUE))),
  footer = tags$span(bs_modal_closebutton("Close")),
  size = "large"
)

# graphicsby group
modal_graph_group_object1 = bs_modal(
  id = "modal_g_group", 
  title = "Section Instructions",
  body = HTML(markdown::markdownToHTML(knit('intro/g_by_group.Rmd', quiet = TRUE))),
  footer = tags$span(bs_modal_closebutton("Close")),
  size = "large"
)
# graphics by group and ID
modal_graph_group_object2 = bs_modal(
  id = "modal_g_group2", 
  title = "Section Instructions",
  body = HTML(markdown::markdownToHTML(knit('intro/g_by_group2.Rmd', quiet = TRUE))),
  footer = tags$span(bs_modal_closebutton("Close")),
  size = "large"
)
# variance components
modal_var_object = bs_modal(
  id = "modal_var", 
  title = "Section Instructions",
  body = HTML(markdown::markdownToHTML(knit('intro/var.Rmd', quiet = TRUE))),
  footer = tags$span(bs_modal_closebutton("Close")),
  size = "large"
)
# model comparision
modal_com_mix_object = bs_modal(
  id = "modal_com_mix", 
  title = "Section Instructions",
  body = HTML(markdown::markdownToHTML(knit('intro/com_mix.Rmd', quiet = TRUE))),
  footer = tags$span(bs_modal_closebutton("Close")),
  size = "large"
)
# fixed model print out
modal_fix_print_out_object = bs_modal(
  id = "modal_fix_print_out", 
  title = "Section Instructions",
  body = HTML(markdown::markdownToHTML(knit('intro/fix_print_out.Rmd', quiet = TRUE))),
  footer = tags$span(bs_modal_closebutton("Close")),
  size = "large"
)

modal_mix_out_object = bs_modal(
  id = "modal_mix_out", 
  title = "Section Instructions",
  body = HTML(markdown::markdownToHTML(knit('intro/mix_out_title.Rmd', quiet = TRUE))),
  footer = tags$span(bs_modal_closebutton("Close")),
  size = "large"
)
modal_mix_anova_object = bs_modal(
  id = "modal_mix_anova", 
  title = "Section Instructions",
  body = HTML(markdown::markdownToHTML(knit('intro/mix_out_anova.Rmd', quiet = TRUE))),
  footer = tags$span(bs_modal_closebutton("Close")),
  size = "large"
)

modal_mix_sum_object = bs_modal(
  id = "modal_mix_sum", 
  title = "Section Instructions",
  body = HTML(markdown::markdownToHTML(knit('intro/mix_out_sum_print.Rmd', quiet = TRUE))),
  footer = tags$span(bs_modal_closebutton("Close")),
  size = "large"
)

modal_mix_sum_tab_object = bs_modal(
  id = "modal_mix_sum_tab", 
  title = "Section Instructions",
  body = HTML(markdown::markdownToHTML(knit('intro/mix_out_sum_tab.Rmd', quiet = TRUE))),
  footer = tags$span(bs_modal_closebutton("Close")),
  size = "large"
)

modal_mix_test_object = bs_modal(
  id = "modal_mix_test", 
  title = "Section Instructions",
  body = HTML(markdown::markdownToHTML(knit('intro/mix_out_test.Rmd', quiet = TRUE))),
  footer = tags$span(bs_modal_closebutton("Close")),
  size = "large"
)

modal_fix_out_object = bs_modal(
  id = "modal_fix_out", 
  title = "Section Instructions",
  body = HTML(markdown::markdownToHTML(knit('intro/fix_print_out.Rmd', quiet = TRUE))),
  footer = tags$span(bs_modal_closebutton("Close")),
  size = "large"
)


