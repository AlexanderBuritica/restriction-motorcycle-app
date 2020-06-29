



g2 <- ggplot(df[df$delito=="totales",], aes(x = months, y = coef, color = var)) + 
  geom_errorbar(width=.1, aes(ymin = ci_lower, ymax = ci_upper)) + 
  geom_point(shape = 21, size = 3, fill = "white") +
  geom_hline(aes(yintercept = 0)) + geom_vline(aes(xintercept = -1),linetype="dashed",colour = "black") +
  theme_bw()
g2




