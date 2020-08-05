round_p <- function(x) {
  if(x < .001) return("p < .001") 
  if(round(x, 2) == 1) return("p = 1")
  
  if(round(x, 3) >= .01) {
    p <- sprintf("%.2f", x)
  } else {
    p <- sprintf("%.3f", x)
  }
  
  return(paste("p = ", substr(p, 2, nchar(p)), sep = ""))
}


# function for displaying results of mixed-effect models
results <- function(model, effect, family = "binomial", rounding = 2) {
  r <- function(x) sprintf(paste0("%.", rounding, "f"), x)
  coefs <- summary(model)$coefficients
  if(family == "binomial") {
    z <- coefs[effect, "z value"]
    p <- coefs[effect, "Pr(>|z|)"]
    e.name <- "OR"
    par <- "z"
  } else {
    z <- coefs[effect, "t value"]
    p <- coefs[effect, "Pr(>|t|)"]        
    e.name <- "b"
    if("lm" %in% class(model)) {
      df <- sprintf("%i", model$df)
    } else {
      df <- sprintf("%.1f", coefs[effect, "df"])
    }
    par <- paste0("t(", df, ")")
  }
  if("lm" %in% class(model)) {
    ES <- model$coefficients[effect]
    CI <- confint(model)[effect,]        
  } else {
    ES <- fixef(model)[effect]
    CI <- confint.merMod(model, method = "Wald", parm = "beta_")[effect,] 
  }
  if(family == "binomial") {
    ES <- exp(ES)
    CI <- exp(CI)
  }
  cat(par, " = ", sprintf("%.2f", z), ", ", 
      round_p(p), ", ",
      e.name, " = ", r(ES), ", ",
      "95% CI = [", r(CI[1]), ", ",
      r(CI[2]), "]",
      sep = "")
}


# function for reporting correlations
p.cor <- function(x, y, method = "pearson") {
  shorten <- function(x) {
    s <- sprintf("%.2f", x)
    if(x >= 0) {
      return(substr(s, 2, nchar(s)))
    } else {
      return(paste0("-", substr(s, 3, nchar(s))))
    }
  }
  
  if(method == "pearson") {
    ct <- cor.test(x, y)
    out <- sprintf("r(%i) = %s, 95%% CI = [%s, %s], %s",
                   ct$parameter, shorten(ct$estimate), shorten(ct$conf.int[1]),
                   shorten(ct$conf.int[2]), round_p(ct$p.value))
  } else {
    if(method == "spearman") {
      suppressWarnings(p <- cor.test(x, y, method = "spearman")$p.value)
      sci <- spearman.ci(x, y, nrep = 1000)
      out <- sprintf("rS = %s, 95%% CI = [%s, %s], %s",
                     shorten(sci$estimate), shorten(sci$conf.int[1]),
                     shorten(sci$conf.int[2]), round_p(p))      
    } else {
      out <- "method must be either 'spearman' or 'pearson'!"
    }
  }
  cat(out)
}


# McCall transformation
mccall <- function(x) qnorm((rank(x, ties.method = "average") - 0.5) / length(x))


# function for reporting t-tests
pt.test <- function(x, y = NULL, paired = FALSE) {
  if(is.null(y)) {
    t <- t.test(x)
  } else {
    t <- t.test(x, y, paired = paired, var.equal = T)
  }
  
  capture.output(
    if(paired | is.null(y)) {
      cis <- ci.sm(ncp = t$statistic, N = t$parameter + 1)
    } else {
      cis <- ci.smd(ncp = t$statistic, n.1 = length(x), n.2 = length(y))
    }
  )
  
  cat(sprintf("t(%i) = %.2f, %s, d = %.2f, 95%% CI = [%.2f, %.2f]",
              t$parameter, t$statistic, round_p(t$p.value), 
              ifelse(paired | is.null(y), cis$Standardized, cis$smd), 
              cis$Lower, cis$Upper))
}


# function for comparison of correlations
compare_r <- function(x1, x2, y1, y2) {
  x <- cocor.indep.groups(cor(x1, x2, use = "na.or.complete"), 
                          cor(y1, y2, use = "na.or.complete"), 
                          sum(!is.na(x1) & !is.na(x2)), 
                          sum(!is.na(y1) & !is.na(y2)))
  CI <- x@zou2007$conf.int
  r <- function(x) sprintf("%.2f", x)
  cat("z = ", r(x@fisher1925$statistic), ", ", 
      round_p(x@fisher1925$p.value), ", ",
      "delta_r = ", r(x@diff), ", ",
      "95% CI = [", r(CI[1]), ", ",
      r(CI[2]), "]",
      sep = "")
}