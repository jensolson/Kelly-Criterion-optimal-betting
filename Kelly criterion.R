# Input payoffs as returns on capital invested
payoffs <- c(-.4, -.2, .0, .25, .45)

# Input outcome probabilities; need not sum to 1
probs <- c(.1, .2, .3, .2, .2)

# Scale to sum to 1
probs <- probs/sum(probs)

# Bet size limited to inverse of max downside payoff
lo <- -1/max(payoffs)
hi <- -1/min(payoffs)

fs <- seq(from=lo, to=hi, length.out=1e3)

# Define Shannon entropy function
shanEnt <- function(prbs, pays, fStar) {
  ent <- sum(prbs*log(1+fStar*pays))
  return(ent)
}

# Find optimal bet size
o <- optimize(f=shanEnt, prbs=probs, pays=payoffs,
              interval=c(lo, hi), maximum=T)

res <- mapply(shanEnt, list(probs), list(payoffs), fs)

# Exclude extreme values from plot
f_plots <- fs[2:(length(fs)-1)]
res_plots <- res[2:(length(fs)-1)]

# Plot results
plot(f_plots, res_plots,
     main=paste("Kelly Criterion payoffs\n",
                "Optimal f* =", round(o$maximum, 4)),
     xlab="Fraction of bankroll bet",
     ylab="Cost function (Shannon entropy)",
     frame.plot=F, pch=1, col="dark blue")

abline(v=c(0, o$maximum), col=c("black", "red"),
       lty=c(1, 2), lwd=c(1, 5))

# Draw grey box in rational betting area
box_min <- min(res_plots)-1
shade <- rgb(.25, .25, .25, 0.2)

if (o$maximum < 0) {
  polygon(x=c(o$maximum, 0, 0, o$maximum),
          y=c(box_min, box_min, o$objective, o$objective),
          col=shade,
          border=F)
} else {
  polygon(x=c(0, o$maximum, o$maximum, 0),
          y=c(box_min, box_min, o$objective, o$objective),
          col=shade,
          border=F)
}

