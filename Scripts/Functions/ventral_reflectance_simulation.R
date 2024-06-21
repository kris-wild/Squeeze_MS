library(NicheMapR)
loc <- c(21.2670, -28.4)
# simulate microclimate on bare sand
micro <- micro_global(loc = loc, REFL = 0.6, cap = 0, soiltype = 1)
# darkest, all over
ecto <- ectotherm(alpha_max = 0.95,
                  alpha_min = 0.95,
                  alpha_sub = (1 - micro$REFL),
                  live = 0,
                  pct_cond = 0)
environ_dark <- as.data.frame(ecto$enviro)
plot(micro$dates, environ_dark$TC, type = 'l', ylab = 'Tb', xlab = 'month', main = 'Kalahari')
# lightest, all over
ecto <- ectotherm(alpha_max = 0.77,
                  alpha_min = 0.77,
                  alpha_sub = (1 - micro$REFL),
                  live = 0,
                  pct_cond = 0)
environ_light <- as.data.frame(ecto$enviro)
points(micro$dates, environ_light$TC, type = 'l', ylab = 'Tb', xlab = '', col = 2)
# lightest dorsal plus effect of ventral (via substrate absorptivity)
ecto <- ectotherm(alpha_max = 0.77,
                  alpha_min = 0.77,
                  alpha_sub = (1 - micro$REFL) + (0.77 - 0.71),
                  live = 0,
                  pct_cond = 0)
environ_belly <- as.data.frame(ecto$enviro)
points(micro$dates, environ_belly$TC, type = 'l', ylab = 'Tb', xlab = '', col = 3)
legend(-1, 55, c('dark', 'light', 'light + belly'), col = c(1, 2, 3), lty = 1, bty = 'n')

# plot difference
plot(micro$dates, (environ_light$TC - environ_belly$TC), type = 'l', ylab = 'delta Tb', xlab = 'month', main = 'all hours')

# just when it's below CTmax
non_lethal <- which(environ_belly$TC < 45)
plot(micro$dates[non_lethal], (environ_light$TC - environ_belly$TC)[non_lethal], type = 'l', ylab = 'delta Tb', xlab = 'month', main = 'non-lethal hours')
