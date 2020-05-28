
fit_mix_0 = value ~ baseline + (1|ID)
fit_mix_logc = value ~ baseline + log(time) + (1|ID)
fit_mix_log = value ~ baseline + log(time) + group + group:log(time) + (1|ID)
fit_mix_logrc = value ~ baseline + log(time) + (log(time)|ID)
fit_mix_logr = value ~ baseline + log(time) + group + group:log(time) + (log(time)|ID)

fit_mix_1c = value ~ baseline + time + (1|ID)
fit_mix_1 = value ~ baseline + time + group + group:time + (1|ID)
fit_mix_1rc = value ~ baseline + time + (time|ID)
fit_mix_1r = value ~ baseline + time + group + group:time + (time|ID)

fit_mix_2c = value ~ baseline + time + I(time^2) + (1|ID)
fit_mix_2 = value ~ baseline + time + I(time^2) + group + group:time + group:I(time^2) + (1|ID)
fit_mix_2rc = value ~ baseline + time + I(time^2) + (time+time^2|ID)
fit_mix_2r = value ~ baseline + time + I(time^2) + group + group:time + group:I(time^2) + (time+time^2|ID)

fit_mix_3c = value ~ baseline + time + I(time^2) + I(time^3) + (1|ID)
fit_mix_3 = value ~ baseline + time + I(time^2) + I(time^3) + group + group:time + group:I(time^2) + group:I(time^3) + (1|ID)
fit_mix_3rc = value ~ baseline + time + I(time^2) + I(time^3) + (time+time^2+time^3|ID)
fit_mix_3r = value ~ baseline + time + I(time^2) + I(time^3) + group + group:time + group:I(time^2) + group:I(time^3) + (time+time^2+time^3|ID)


mod_mix_0 = lmer(formula = fit_mix_0,data = mod_data,REML = T)
mod_mix_log = lmer(formula = fit_mix_log,data = mod_data,REML = T)
mod_mix_1 = lmer(formula = fit_mix_1,data = mod_data,REML = T)
mod_mix_2 = lmer(formula = fit_mix_2,data = mod_data,REML = T)
mod_mix_3 = lmer(formula = fit_mix_3,data = mod_data,REML = T)

mod_mix_logc = lmer(formula = fit_mix_logc,data = mod_data,REML = T)
mod_mix_1c = lmer(formula = fit_mix_1c,data = mod_data,REML = T)
mod_mix_2c = lmer(formula = fit_mix_2c,data = mod_data,REML = T)
mod_mix_3c = lmer(formula = fit_mix_3c,data = mod_data,REML = T)

mod_mix_logr = lmer(formula = fit_mix_logr,data = mod_data,REML = T)
mod_mix_1r = lmer(formula = fit_mix_1r,data = mod_data,REML = T)
mod_mix_2r = lmer(formula = fit_mix_2r,data = mod_data,REML = T)
mod_mix_3r = lmer(formula = fit_mix_3r,data = mod_data,REML = T)

mod_mix_logrc = lmer(formula = fit_mix_logrc,data = mod_data,REML = T)
mod_mix_1rc = lmer(formula = fit_mix_1rc,data = mod_data,REML = T)
mod_mix_2rc = lmer(formula = fit_mix_2rc,data = mod_data,REML = T)
mod_mix_3rc = lmer(formula = fit_mix_3rc,data = mod_data,REML = T)

