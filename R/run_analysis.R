# R twin of python/run_analysis.py: same data, same folds (results/folds.csv), same questions.
#   Rscript R/run_analysis.R        # ~3 min; writes results/model_comparison_r.csv
# Packages: readr dplyr stringr glmnet ranger (see R/install.R)

source(file.path(dirname(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1])), "wine_data.R"))
suppressPackageStartupMessages({ library(glmnet); library(ranger) })
set.seed(20260926)

d <- load_wines()
X <- d$X; y <- d$meta$y; meta <- d$meta
fs <- feature_sets(d$descriptors, X)

# Reuse the Python folds when present so both languages score the very same splits.
folds_path <- file.path(ROOT, "results", "folds.csv")
if (file.exists(folds_path)) {
  folds <- read_csv(folds_path, show_col_types = FALSE)
} else {
  grp <- meta$group
  gfold <- sample(rep(0:4, length.out = max(grp) + 1))[grp + 1]
  folds <- tibble(wine_id = meta$wine_id, random_fold = sample(rep(0:4, length.out = nrow(meta))),
                  grouped_fold = gfold, temporal_test = as.integer(meta$year > 2011))
}

auc <- function(y, s) {                     # Mann–Whitney form of ROC AUC
  r <- rank(s); n1 <- sum(y == 1); n0 <- sum(y == 0)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

fit_predict <- function(model, Xtr, ytr, Xte) {
  switch(model,
    majority = rep(mean(ytr), nrow(Xte)),
    naive_bayes = {                        # Bernoulli naive Bayes, Laplace alpha = 1
      p1 <- (colSums(Xtr[ytr == 1, , drop = FALSE]) + 1) / (sum(ytr == 1) + 2)
      p0 <- (colSums(Xtr[ytr == 0, , drop = FALSE]) + 1) / (sum(ytr == 0) + 2)
      w <- log(p1 / (1 - p1)) - log(p0 / (1 - p0))
      b <- sum(log(1 - p1)) - sum(log(1 - p0)) + log(mean(ytr) / (1 - mean(ytr)))
      plogis(as.vector(Xte %*% w) + b)
    },
    logistic = {                           # ridge; lambda = 1 / (C * n) matches sklearn C = 0.5
      m <- glmnet(Xtr, ytr, family = "binomial", alpha = 0, lambda = 1 / (0.5 * nrow(Xtr)), standardize = FALSE)
      as.vector(predict(m, Xte, type = "response"))
    },
    random_forest = {
      m <- ranger(x = Xtr, y = factor(ytr), num.trees = 400, min.node.size = 2, probability = TRUE, seed = 20260926)
      predict(m, Xte)$predictions[, "1"]
    })
}

evaluate <- function(cols, model, design) {
  f <- switch(design, random = folds$random_fold, grouped = folds$grouped_fold, temporal = folds$temporal_test)
  ks <- if (design == "temporal") 1 else sort(unique(f))
  res <- lapply(ks, function(k) {
    te <- f == k; tr <- !te
    p <- fit_predict(model, X[tr, cols, drop = FALSE], y[tr], X[te, cols, drop = FALSE])
    pred <- as.integer(p >= 0.5)
    c(accuracy = mean(pred == y[te]),
      balanced_accuracy = mean(c(mean(pred[y[te] == 1] == 1), mean(pred[y[te] == 0] == 0))),
      roc_auc = if (length(unique(p)) > 1) auc(y[te], p) else 0.5)
  })
  colMeans(do.call(rbind, res))
}

plan <- expand.grid(model = c("majority", "naive_bayes", "logistic"), features = names(fs),
                    design = c("random", "grouped", "temporal"), stringsAsFactors = FALSE)
plan <- rbind(plan, data.frame(model = "random_forest", features = "all", design = "random"))
out <- do.call(rbind, lapply(seq_len(nrow(plan)), function(i) {
  r <- plan[i, ]
  m <- evaluate(fs[[r$features]], r$model, r$design)
  message(sprintf("%-8s %-11s %-13s acc %.4f auc %.4f", r$design, r$features, r$model, m["accuracy"], m["roc_auc"]))
  data.frame(r, t(round(m, 4)))
}))
write_csv(out, file.path(ROOT, "results", "model_comparison_r.csv"))

# Same headline, R side: which sensory words move the odds (full-data ridge fit)
cols <- fs$sensory
m <- glmnet(X[, cols], y, family = "binomial", alpha = 0, lambda = 1 / (0.5 * nrow(X)), standardize = FALSE)
b <- as.vector(coef(m))[-1]
top <- tibble(attribute = d$descriptors$attribute[cols], n_reviews = colSums(X[, cols]), coef = b) |>
  filter(n_reviews >= 30) |> arrange(desc(coef))
print(rbind(head(top, 8), tail(top, 8)), n = 16)
