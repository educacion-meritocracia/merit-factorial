# Path diagram of a 4-factor CFA drawn from a lavaan fit object
# Usage: source(here::here("processing", "plot_cfa_diagram.R")); plot_cfa_diagram(fit)
plot_cfa_diagram <- function(fit) {
  library(ggplot2)
  pe <- lavaan::standardizedSolution(fit)
  stars <- function(p) ifelse(p < .01, "**", ifelse(p < .05, "*", ""))
  fmt <- function(est, p) paste0(formatC(est, format = "f", digits = 2), stars(p))

  ld <- pe[pe$op == "=~", ]
  cv <- pe[pe$op == "~~" & pe$lhs != pe$rhs & pe$lhs %in% ld$lhs & pe$rhs %in% ld$lhs, ]
  getcv <- function(a, b) {
    r <- cv[(cv$lhs == a & cv$rhs == b) | (cv$lhs == b & cv$rhs == a), ]
    fmt(r$est.std, r$pvalue)
  }

  # layout -------------------------------------------------------------
  xl <- 4.6; xr <- 7.4; yt <- 4.6; yb <- 1.9; ea <- 1.1; eb <- .55
  lat <- data.frame(
    id  = c("perc_merit", "pref_merit", "perc_nmerit", "pref_nmerit"),
    lab = c("Meritocratic\nperceptions", "Meritocratic\npreferences",
            "Privilege\nperceptions", "Privilege\nacceptance"),
    x = c(xl, xr, xl, xr), y = c(yt, yt, yb, yb))
  ind <- data.frame(
    id  = c("perc_effort", "perc_talent", "perc_rich_parents", "perc_contact",
            "pref_effort", "pref_talent", "pref_rich_parents", "pref_contact"),
    lab = c("Perception: effort", "Perception: talent", "Perception:\nrich family",
            "Perception: contacts", "Preference: effort", "Preference: talent",
            "Preference:\nrich family", "Preference: contacts"),
    x = rep(c(1.25, 10.75), each = 4), y = rep(c(5.6, 4.0, 2.6, 1.0), 2))
  bw <- 2.2; bh <- .55

  ell <- function(cx, cy, a = ea, b = eb, n = 100) {
    t <- seq(0, 2 * pi, length.out = n); data.frame(x = cx + a * cos(t), y = cy + b * sin(t))
  }
  ell_df <- do.call(rbind, lapply(seq_len(nrow(lat)), function(i)
    cbind(ell(lat$x[i], lat$y[i]), id = lat$id[i])))

  # loadings: arrow from factor edge to indicator ----------------------------
  L <- merge(ld, lat, by.x = "lhs", by.y = "id")
  L <- merge(L, ind, by.x = "rhs", by.y = "id", suffixes = c(".f", ".i"))
  left <- L$x.i < 6
  L$xs <- ifelse(left, L$x.f - ea, L$x.f + ea)
  L$ys <- L$y.f
  L$xe <- ifelse(left, L$x.i + bw / 2, L$x.i - bw / 2)
  L$ye <- L$y.i
  L$lx <- L$xs + (L$xe - L$xs) * .5
  L$ly <- L$ys + (L$ye - L$ys) * .5 + ifelse(L$y.i > 3.3, .25, -.25)   # top items above the arrow, bottom items below
  L$txt <- fmt(L$est.std, L$pvalue)

  R <- data.frame(xs = ifelse(ind$x < 6, ind$x - bw / 2 - .7, ind$x + bw / 2 + .7),
                  xe = ifelse(ind$x < 6, ind$x - bw / 2, ind$x + bw / 2), y = ind$y)

  # factor correlations: quadratic bezier paths --------------------------------
  bez <- function(p0, p1, ctl, n = 60) {
    t <- seq(0, 1, length.out = n)
    data.frame(x = (1 - t)^2 * p0[1] + 2 * (1 - t) * t * ctl[1] + t^2 * p1[1],
               y = (1 - t)^2 * p0[2] + 2 * (1 - t) * t * ctl[2] + t^2 * p1[2])
  }
  # each: id, start, end, control, label position, line type
  defs <- list(
    list(a = "perc_merit",  b = "pref_merit",  p0 = c(xl, yt + eb), p1 = c(xr, yt + eb), ctl = c((xl + xr) / 2, yt + eb + 1.5), lab = c((xl + xr) / 2, yt + eb + .95), lt = "solid"),
    list(a = "perc_nmerit", b = "pref_nmerit", p0 = c(xl, yb - eb), p1 = c(xr, yb - eb), ctl = c((xl + xr) / 2, yb - eb - 1.5), lab = c((xl + xr) / 2, yb - eb - .95), lt = "solid"),
    list(a = "perc_merit",  b = "perc_nmerit", p0 = c(xl - ea, yt), p1 = c(xl - ea, yb), ctl = c(xl - ea - 1.4, (yt + yb) / 2), lab = c(xl - ea - .7, (yt + yb) / 2), lt = "solid"),
    list(a = "pref_merit",  b = "pref_nmerit", p0 = c(xr + ea, yt), p1 = c(xr + ea, yb), ctl = c(xr + ea + 1.4, (yt + yb) / 2), lab = c(xr + ea + .7, (yt + yb) / 2), lt = "solid"),
    list(a = "perc_merit",  b = "pref_nmerit", p0 = c(xl + .45, yt - eb + .05), p1 = c(xr - .45, yb + eb - .05), ctl = c((xl + xr) / 2, (yt + yb) / 2), lab = c(xl + .45 + (xr - xl - .9) * .3, (yt - eb + .05) - (yt - yb - 2 * eb + .1) * .3), lt = "dashed"),
    list(a = "pref_merit",  b = "perc_nmerit", p0 = c(xr - .45, yt - eb + .05), p1 = c(xl + .45, yb + eb - .05), ctl = c((xl + xr) / 2, (yt + yb) / 2), lab = c(xr - .45 - (xr - xl - .9) * .3, (yt - eb + .05) - (yt - yb - 2 * eb + .1) * .3), lt = "dashed"))
  C <- do.call(rbind, lapply(seq_along(defs), function(i) {
    d <- defs[[i]]
    cbind(bez(d$p0, d$p1, d$ctl), grp = i, lt = d$lt)
  }))
  Clab <- do.call(rbind, lapply(defs, function(d)
    data.frame(x = d$lab[1], y = d$lab[2], txt = getcv(d$a, d$b))))

  # fit box ------------------------------------------------------------------
  fm <- lavaan::fitMeasures(fit, c("chisq.scaled", "df.scaled", "pvalue.scaled",
                                   "cfi.scaled", "tli.scaled", "rmsea.scaled"))
  ptxt <- if (fm["pvalue.scaled"] < .001) "***" else if (fm["pvalue.scaled"] < .01) "**" else if (fm["pvalue.scaled"] < .05) "*" else ""
  fit_txt <- sprintf("WLSMV estimation, completely standardized solution, N=%d\nChi2=%.3f%s / df=%d, CFI=%.3f, TLI=%.3f, RMSEA=%.3f\n**p<0.01, *p<0.05",
                     lavaan::nobs(fit), fm["chisq.scaled"], ptxt, round(fm["df.scaled"]),
                     fm["cfi.scaled"], fm["tli.scaled"], fm["rmsea.scaled"])

  ar <- arrow(length = unit(2.2, "mm"), type = "closed")
  ggplot() +
    geom_segment(data = R, aes(x = xs, xend = xe, y = y, yend = y), arrow = ar, linewidth = .3) +
    geom_segment(data = L, aes(x = xs, xend = xe, y = ys, yend = ye), arrow = ar, linewidth = .3) +
    geom_path(data = C[C$lt == "solid", ], aes(x, y, group = grp), linewidth = .3,
              arrow = arrow(length = unit(1.8, "mm"), ends = "both", type = "closed")) +
    geom_path(data = C[C$lt == "dashed", ], aes(x, y, group = grp), linewidth = .3, linetype = "dashed",
              arrow = arrow(length = unit(1.8, "mm"), ends = "both", type = "closed")) +
    geom_rect(data = ind, aes(xmin = x - bw / 2, xmax = x + bw / 2, ymin = y - bh, ymax = y + bh),
              fill = "white", colour = "black", linewidth = .3) +
    geom_text(data = ind, aes(x, y, label = lab), size = 3.8, lineheight = .9) +
    geom_polygon(data = ell_df, aes(x, y, group = id), fill = "white", colour = "grey30", linewidth = .3) +
    geom_text(data = lat, aes(x, y, label = lab), size = 3.8, lineheight = .9) +
    geom_label(data = Clab, aes(x, y, label = txt), size = 2.8, fill = "white", linewidth = 0, label.padding = unit(1, "mm")) +
    geom_label(data = L, aes(lx, ly, label = txt), size = 3, fill = "white", linewidth = 0, label.padding = unit(.5, "mm")) +
    annotate("rect", xmin = .1, xmax = 11.9, ymin = -1.5, ymax = -.3, fill = "grey93", colour = "grey60", linewidth = .3) +
    annotate("text", x = 11.7, y = -.9, label = fit_txt, hjust = 1, size = 3, fontface = "bold", lineheight = 1.1) +
    coord_equal(xlim = c(-.1, 12.1), ylim = c(-1.6, 6.3), expand = FALSE) +
    theme_void()
}
