

create.EfficientFrontier.h<-
  function (R, portfolio, type, n.portfolios = 25, risk_aversion = NULL, 
            match.col = "ES", search_size = 2000, ...) 
  {
    browser()
    call <- match.call()
    if (!is.portfolio(portfolio)) 
      stop("portfolio must be of class 'portfolio'")
    type <- type[1]
    switch(type, `mean-sd` = , `mean-StdDev` = , 
           `mean-var` = {
             frontier <- meanvar.efficient.frontier.h(portfolio = portfolio, 
                                                      R = R, n.portfolios = n.portfolios, risk_aversion = risk_aversion, 
                                                      ... = ...)
           }, `mean-ETL` = , `mean-CVaR` = , `mean-ES` = , 
           `mean-etl` = {
             frontier <- meanetl.efficient.frontier(portfolio = portfolio, 
                                                    R = R, n.portfolios = n.portfolios, ... = ...)
           }, random = {
             tmp <- optimize.portfolio(R = R, portfolio = portfolio, 
                                       optimize_method = type, trace = TRUE, search_size = search_size, 
                                       ... = ...)
             frontier <- extract.efficient.frontier(object = tmp, 
                                                    match.col = match.col, n.portfolios = n.portfolios)
           }, DEoptim = {
             tmp <- optimize.portfolio(R = R, portfolio = portfolio, 
                                       optimize_method = type, trace = TRUE, search_size = search_size, 
                                       ... = ...)
             frontier <- extract.efficient.frontier(object = tmp, 
                                                    match.col = match.col, n.portfolios = n.portfolios)
           })
    return(structure(list(call = call, frontier = frontier, R = R, 
                          portfolio = portfolio), class = "efficient.frontier"))
  }




meanvar.efficient.frontier.h<-
  function (portfolio, R, n.portfolios = 25, risk_aversion = NULL, 
            ...) 
  {
    if (!is.portfolio(portfolio)) 
      stop("portfolio object must be of class 'portfolio'")
    var_idx <- which(unlist(lapply(portfolio$objectives, function(x) x$name)) %in% 
                       c("var", "StdDev", "sd"))
    if (length(var_idx) >= 1) {
      var_obj <- portfolio$objectives[[var_idx[1]]]
    }
    else {
      var_obj <- portfolio_risk_objective(name = "var")
    }
    hhi_idx <- which(unlist(lapply(portfolio$objectives, function(x) x$name)) == 
                       "HHI")
    if (length(hhi_idx) >= 1) {
      hhi_obj <- portfolio$objectives[[hhi_idx[1]]]
    }
    else {
      hhi_obj <- NULL
    }
    portfolio$objectives <- list()
    portfolio$objectives[[1]] <- var_obj
    portfolio$objectives[[2]] <- hhi_obj
    portfolio <- add.objective(portfolio = portfolio, type = "return", 
                               name = "mean")
    for (i in 1:length(portfolio$constraints)) {
      if (inherits(portfolio$constraints[[i]], "return_constraint")) {
        portfolio$constraints[[i]]$enabled <- FALSE
      }
    }
    var_idx <- which(unlist(lapply(portfolio$objectives, function(x) x$name)) %in% 
                       c("var", "StdDev", "sd"))
    mean_idx <- which(unlist(lapply(portfolio$objectives, function(x) x$name)) == 
                        "mean")
    hhi_idx <- which(unlist(lapply(portfolio$objectives, function(x) x$name)) == 
                       "HHI")
    portfolio$objectives[[var_idx]]$enabled <- FALSE
    if (length(hhi_idx) >= 1) 
      portfolio$objectives[[hhi_idx]]$enabled <- FALSE
    tmp <- optimize.portfolio.h(R = R, portfolio = portfolio, optimize_method = "ROI", 
                                ... = ...)
    mean_ret <- colMeans(R)
    maxret <- sum(extractWeights(tmp) * mean_ret)
    portfolio$objectives[[mean_idx]]$enabled <- FALSE
    portfolio$objectives[[var_idx]]$enabled <- TRUE
    if (length(hhi_idx) >= 1) 
      portfolio$objectives[[hhi_idx]]$enabled <- TRUE
    tmp <- optimize.portfolio.h(R = R, portfolio = portfolio, optimize_method = "ROI", 
                                ... = ...)
    stats <- extractStats(tmp)
    minret <- sum(extractWeights(tmp) * mean_ret)
    ret_seq <- seq(from = minret, to = maxret, length.out = n.portfolios)
    portfolio <- add.constraint(portfolio = portfolio, type = "return", 
                                return_target = minret, enabled = FALSE)
    ret_constr_idx <- which(unlist(lapply(portfolio$constraints, 
                                          function(x) inherits(x, "return_constraint"))))
    stopifnot("package:foreach" %in% search() || requireNamespace("foreach", 
                                                                  quietly = TRUE))
    stopifnot("package:iterators" %in% search() || requireNamespace("iterators", 
                                                                    quietly = TRUE))
    if (!is.null(risk_aversion)) {
      portfolio$objectives[[mean_idx]]$enabled <- TRUE
      lambda <- risk_aversion[1]
      out <- foreach::foreach(lambda = iterators::iter(risk_aversion), 
                              .inorder = TRUE, .combine = rbind, .errorhandling = "remove", 
                              .packages = "PortfolioAnalytics") %dopar% {
                                portfolio$objectives[[var_idx]]$risk_aversion <- lambda
                                extractStats(optimize.portfolio.h(R = R, portfolio = portfolio, 
                                                                  optimize_method = "ROI", ... = ...))
                              }
      out <- cbind(out, risk_aversion)
      colnames(out) <- c(names(stats), "lambda")
    }
    else {
      portfolio$constraints[[ret_constr_idx]]$enabled <- TRUE
      ret <- ret_seq[1]
      out <- foreach::foreach(ret = iterators::iter(ret_seq), 
                              .inorder = TRUE, .combine = rbind, .errorhandling = "remove", 
                              .packages = "PortfolioAnalytics") %dopar% {
                                portfolio$constraints[[ret_constr_idx]]$return_target <- ret
                                opt <- optimize.portfolio.h(R = R, portfolio = portfolio, 
                                                            optimize_method = "ROI", ... = ...)
                                c(sum(extractWeights(opt) * mean_ret), extractStats(opt))
                              }
      colnames(out) <- c("mean", names(stats))
    }
    out <- na.omit(out)
    return(structure(out, class = "frontier"))
  }



#https://stackoverflow.com/questions/61405323/r-portfolioanalytics-create-efficientfrontier-confusion-over-custom-expected-r

optimize.portfolio.h<-
  function (R, portfolio = NULL, constraints = NULL, objectives = NULL, 
            optimize_method = c("DEoptim", "random", "ROI", 
                                "pso", "GenSA"), search_size = 20000, trace = FALSE, 
            ..., rp = NULL, momentFUN = "set.portfolio.moments", 
            message = FALSE) 
  {
    if (inherits(portfolio, "portfolio.list")) {
      n.portf <- length(portfolio)
      opt.list <- vector("list", n.portf)
      for (i in 1:length(opt.list)) {
        if (message) 
          cat("Starting optimization of portfolio ", 
              i, "\n")
        opt.list[[i]] <- optimize.portfolio(R = R, portfolio = portfolio[[i]], 
                                            constraints = constraints, objectives = objectives, 
                                            optimize_method = optimize_method, search_size = search_size, 
                                            trace = trace, ... = ..., rp = rp, momentFUN = momentFUN, 
                                            message = message)
      }
      out <- combine.optimizations(opt.list)
      return(out)
    }
    if (inherits(portfolio, "regime.portfolios")) {
      regime.switching <- TRUE
      regime <- portfolio$regime
      if (index(last(R)) %in% index(regime)) {
        regime.idx <- as.numeric(regime[index(last(R))])[1]
        portfolio <- portfolio$portfolio.list[[regime.idx]]
      }
      else {
        warning("Dates in regime and R do not match, defaulting to first portfolio")
        regime.idx <- 1
        portfolio <- portfolio$portfolio.list[[regime.idx]]
      }
    }
    else {
      regime.switching <- FALSE
    }
    if (inherits(portfolio, "mult.portfolio.spec")) {
      R <- proxy.mult.portfolio(R = R, mult.portfolio = portfolio)
      portfolio <- portfolio$top.portfolio
    }
    optimize_method <- optimize_method[1]
    tmptrace <- NULL
    start_t <- Sys.time()
    call <- match.call()
    if (!is.null(portfolio) & !is.portfolio(portfolio)) {
      stop("you must pass in an object of class 'portfolio' to control the optimization")
    }
    if (!is.null(constraints)) {
      if (inherits(constraints, "v1_constraint")) {
        if (is.null(portfolio)) {
          tmp_portf <- portfolio.spec(assets = constraints$assets)
        }
        message("constraint object passed in is a 'v1_constraint' object, updating to v2 specification")
        portfolio <- update_constraint_v1tov2(portfolio = tmp_portf, 
                                              v1_constraint = constraints)
      }
      if (!inherits(constraints, "v1_constraint")) {
        portfolio <- insert_constraints(portfolio = portfolio, 
                                        constraints = constraints)
      }
    }
    if (!is.null(objectives)) {
      portfolio <- insert_objectives(portfolio = portfolio, 
                                     objectives = objectives)
    }
    R <- checkData(R)
    N <- length(portfolio$assets)
    if (ncol(R) > N) {
      R <- R[, names(portfolio$assets)]
    }
    T <- nrow(R)
    out <- list()
    weights <- NULL
    constraints <- get_constraints(portfolio)
    if (!is.function(momentFUN)) {
      momentFUN <- match.fun(momentFUN)
    }
    .formals <- formals(momentFUN)
    .formals <- modify.args(formals = .formals, arglist = list(...), 
                            dots = TRUE)
    if (optimize_method %in% c("ROI", "quadprog", 
                               "glpk", "symphony", "ipop")) {
      obj_names <- unlist(lapply(portfolio$objectives, function(x) x$name))
      if (any(obj_names %in% c("CVaR", "ES", "ETL"))) {
        .formals <- modify.args(formals = .formals, arglist = list(ROI = TRUE), 
                                dots = TRUE)
      }
    }
    if ("R" %in% names(.formals)) 
      .formals <- modify.args(formals = .formals, arglist = NULL, 
                              R = R, dots = FALSE)
    if ("portfolio" %in% names(.formals)) 
      .formals <- modify.args(formals = .formals, arglist = NULL, 
                              portfolio = portfolio, dots = FALSE)
    .formals$... <- NULL
    mout <- try(do.call(momentFUN, .formals), silent = TRUE)
    if (inherits(mout, "try-error")) {
      message(paste("portfolio moment function failed with message", 
                    mout))
    }
    else {
      dotargs <- mout
    }
    normalize_weights <- function(weights) {
      if (!is.null(constraints$min_sum) | !is.null(constraints$max_sum)) {
        if (!is.null(constraints$max_sum) & constraints$max_sum != 
            Inf) {
          max_sum = constraints$max_sum
          if (sum(weights) > max_sum) {
            weights <- (max_sum/sum(weights)) * weights
          }
        }
        if (!is.null(constraints$min_sum) & constraints$min_sum != 
            -Inf) {
          min_sum = constraints$min_sum
          if (sum(weights) < min_sum) {
            weights <- (min_sum/sum(weights)) * weights
          }
        }
      }
      return(weights)
    }
    if (optimize_method == "DEoptim") {
      stopifnot("package:DEoptim" %in% search() || requireNamespace("DEoptim", 
                                                                    quietly = TRUE))
      if (hasArg(itermax)) 
        itermax = match.call(expand.dots = TRUE)$itermax
      else itermax = N * 50
      NP <- round(search_size/itermax)
      if (NP < (N * 10)) 
        NP <- N * 10
      if (NP > 2000) 
        NP <- 2000
      if (!hasArg(itermax)) {
        itermax <- round(search_size/NP)
        if (itermax < 50) 
          itermax <- 50
      }
      if (hasArg(parallel)) 
        parallel <- match.call(expand.dots = TRUE)$parallel
      else parallel <- TRUE
      if (!isTRUE(parallel) && "package:foreach" %in% 
          search()) {
        foreach::registerDoSEQ()
      }
      DEcformals <- formals(DEoptim::DEoptim.control)
      DEcargs <- names(DEcformals)
      if (is.list(dotargs)) {
        pm <- pmatch(names(dotargs), DEcargs, nomatch = 0L)
        names(dotargs[pm > 0L]) <- DEcargs[pm]
        DEcformals$NP <- NP
        DEcformals$itermax <- itermax
        DEcformals[pm] <- dotargs[pm > 0L]
        if (!hasArg(strategy)) {
          strategy = 6
          DEcformals$strategy = strategy
        }
        if (!hasArg(reltol)) {
          reltol = 1e-06
          DEcformals$reltol = reltol
        }
        if (!hasArg(steptol)) {
          steptol = round(N * 1.5)
          DEcformals$steptol = steptol
        }
        if (!hasArg(c)) {
          tmp.c = 0.4
          DEcformals$c = tmp.c
        }
        if (!hasArg(storepopfrom)) {
          storepopfrom = 1
          DEcformals$storepopfrom = storepopfrom
        }
        if (isTRUE(parallel) && "package:foreach" %in% 
            search()) {
          if (!hasArg(parallelType)) {
            parallelType = 2
            DEcformals$parallelType = parallelType
          }
          if (!hasArg(packages)) {
            packages <- names(sessionInfo()$otherPkgs)
            DEcformals$packages <- packages
          }
        }
      }
      if (hasArg(traceDE)) 
        traceDE = match.call(expand.dots = TRUE)$traceDE
      else traceDE = TRUE
      DEcformals$trace <- traceDE
      if (isTRUE(trace)) {
        tmptrace <- trace
        assign(".objectivestorage", list(), envir = as.environment(.storage))
        trace = FALSE
      }
      upper <- constraints$max
      lower <- constraints$min
      if ((constraints$max_sum - constraints$min_sum) < 0.02) {
        message("Leverage constraint min_sum and max_sum are restrictive, \n              consider relaxing. e.g. 'full_investment' constraint should be min_sum=0.99 and max_sum=1.01")
      }
      if (!is.null(rp)) {
        rp_len <- min(nrow(rp), NP)
        seed <- rp[1:rp_len, ]
        DEcformals$initialpop <- seed
      }
      else {
        if (hasArg(rp_method)) 
          rp_method = match.call(expand.dots = TRUE)$rp_method
        else rp_method = "sample"
        if (hasArg(fev)) 
          fev = match.call(expand.dots = TRUE)$fev
        else fev = 0:5
        rp <- random_portfolios(portfolio = portfolio, permutations = (NP + 
                                                                         1), rp_method = rp_method, eliminate = FALSE, 
                                fev = fev)
        DEcformals$initialpop <- rp
      }
      controlDE <- do.call(DEoptim::DEoptim.control, DEcformals)
      minw = try(DEoptim::DEoptim(constrained_objective, lower = lower[1:N], 
                                  upper = upper[1:N], control = controlDE, R = R, portfolio = portfolio, 
                                  env = dotargs, normalize = FALSE, fnMap = function(x) fn_map(x, 
                                                                                               portfolio = portfolio)$weights), silent = TRUE)
      if (inherits(minw, "try-error")) {
        message(minw)
        minw = NULL
      }
      if (is.null(minw)) {
        message(paste("Optimizer was unable to find a solution for target"))
        return(paste("Optimizer was unable to find a solution for target"))
      }
      if (isTRUE(tmptrace)) 
        trace <- tmptrace
      weights <- as.vector(minw$optim$bestmem)
      print(weights)
      names(weights) <- colnames(R)
      obj_vals <- constrained_objective(w = weights, R = R, 
                                        portfolio, trace = TRUE, normalize = FALSE, env = dotargs)$objective_measures
      out <- list(weights = weights, objective_measures = obj_vals, 
                  opt_values = obj_vals, out = minw$optim$bestval, 
                  call = call)
      if (isTRUE(trace)) {
        out$DEoutput <- minw
        out$DEoptim_objective_results <- try(get(".objectivestorage", 
                                                 envir = .storage), silent = TRUE)
        rm(".objectivestorage", envir = .storage)
      }
    }
    if (optimize_method == "random") {
      if ((constraints$max_sum - constraints$min_sum) < 0.02) {
        message("Leverage constraint min_sum and max_sum are restrictive, \n              consider relaxing. e.g. 'full_investment' constraint should be min_sum=0.99 and max_sum=1.01")
      }
      if (missing(rp) | is.null(rp)) {
        if (hasArg(rp_method)) 
          rp_method = match.call(expand.dots = TRUE)$rp_method
        else rp_method = "sample"
        if (hasArg(eliminate)) 
          eliminate = match.call(expand.dots = TRUE)$eliminate
        else eliminate = TRUE
        if (hasArg(fev)) 
          fev = match.call(expand.dots = TRUE)$fev
        else fev = 0:5
        rp <- random_portfolios(portfolio = portfolio, permutations = search_size, 
                                rp_method = rp_method, eliminate = eliminate, 
                                fev = fev)
      }
      if (isTRUE(trace)) 
        out$random_portfolios <- rp
      if ("package:foreach" %in% search() & !hasArg(parallel)) {
        ii <- 1
        rp_objective_results <- foreach::foreach(ii = 1:nrow(rp), 
                                                 .errorhandling = "pass") %dopar% constrained_objective(w = rp[ii, 
                                                 ], R = R, portfolio = portfolio, trace = trace, 
                                                 env = dotargs, normalize = FALSE)
      }
      else {
        rp_objective_results <- apply(rp, 1, constrained_objective, 
                                      R = R, portfolio = portfolio, trace = trace, 
                                      normalize = FALSE, env = dotargs)
      }
      if (isTRUE(trace)) 
        out$random_portfolio_objective_results <- rp_objective_results
      search <- vector(length = length(rp_objective_results))
      for (i in 1:length(search)) {
        if (isTRUE(trace)) {
          search[i] <- ifelse(try(rp_objective_results[[i]]$out), 
                              rp_objective_results[[i]]$out, 1e+06)
        }
        else {
          search[i] <- as.numeric(rp_objective_results[[i]])
        }
      }
      if (isTRUE(trace)) {
        min_objective_weights <- try(normalize_weights(rp_objective_results[[which.min(search)]]$weights))
      }
      else {
        min_objective_weights <- try(normalize_weights(rp[which.min(search), 
        ]))
      }
      out$weights <- min_objective_weights
      obj_vals <- try(constrained_objective(w = min_objective_weights, 
                                            R = R, portfolio = portfolio, trace = TRUE, normalize = FALSE, 
                                            env = dotargs)$objective_measures)
      out$objective_measures <- obj_vals
      out$opt_values <- obj_vals
      out$call <- call
    }
    roi_solvers <- c("ROI", "quadprog", "glpk", 
                     "symphony", "ipop")
    if (optimize_method %in% roi_solvers) {
      if (hasArg(control)) 
        control = match.call(expand.dots = TRUE)$control
      else control = NULL
      moments <- list(mean = rep(0, N))
      if (!is.null(mout$mu)) {
        moments[["mean"]] <- as.vector(mout$mu)
      }
      alpha <- 0.05
      if (!is.null(constraints$return_target)) {
        target <- constraints$return_target
      }
      else {
        target <- NA
      }
      lambda <- 1
      valid_objnames <- c("HHI", "mean", "var", 
                          "sd", "StdDev", "CVaR", "ES", 
                          "ETL")
      for (objective in portfolio$objectives) {
        if (objective$enabled) {
          if (!(objective$name %in% valid_objnames)) {
            stop("ROI only solves mean, var/StdDev, HHI, or sample ETL/ES/CVaR type business objectives, choose a different optimize_method.")
          }
          arguments <- objective$arguments
          if (!is.null(arguments$clean)) 
            clean <- arguments$clean
          else clean <- "none"
          if (!is.null(arguments[["p"]])) 
            alpha <- arguments$p
          else alpha <- alpha
          if (alpha > 0.5) 
            alpha <- (1 - alpha)
          if (clean != "none") 
            moments$cleanR <- Return.clean(R = R, method = clean)
          if (objective$name == "mean") {
            if (!is.null(mout$mu)) {
              moments[["mean"]] <- as.vector(mout$mu)
            }
            else {
              moments[["mean"]] <- try(as.vector(apply(Return.clean(R = R, 
                                                                    method = clean), 2, "mean", na.rm = TRUE)), 
                                       silent = TRUE)
            }
          }
          else if (objective$name %in% c("StdDev", 
                                         "sd", "var")) {
            if (!is.null(mout$sigma)) {
              moments[["var"]] <- mout$sigma
            }
            else {
              moments[["var"]] <- try(var(x = Return.clean(R = R, 
                                                           method = clean), na.rm = TRUE), silent = TRUE)
            }
          }
          else if (objective$name %in% c("CVaR", 
                                         "ES", "ETL")) {
            moments[[objective$name]] <- ""
          }
          else {
            moments[[objective$name]] <- try(eval(as.symbol(objective$name))(Return.clean(R = R, 
                                                                                          method = clean)), silent = TRUE)
          }
          target <- ifelse(!is.null(objective$target), 
                           objective$target, target)
          lambda <- ifelse(!is.null(objective$risk_aversion), 
                           objective$risk_aversion, lambda)
          if (!is.null(objective$conc_aversion)) 
            lambda_hhi <- objective$conc_aversion
          else lambda_hhi <- NULL
          if (!is.null(objective$conc_groups)) 
            conc_groups <- objective$conc_groups
          else conc_groups <- NULL
        }
      }
      if ("var" %in% names(moments)) {
        if (optimize_method == "ROI") {
          solver <- "quadprog"
        }
        else {
          solver <- optimize_method
        }
        if (!is.null(constraints$turnover_target) | !is.null(constraints$ptc) | 
            !is.null(constraints$leverage)) {
          if (!is.null(constraints$turnover_target) & !is.null(constraints$ptc)) {
            warning("Turnover and proportional transaction cost constraints detected, only running optimization for turnover constraint.")
            constraints$ptc <- NULL
          }
          if (!is.null(constraints$turnover_target) & is.null(constraints$ptc)) {
            qp_result <- gmv_opt_toc(R = R, constraints = constraints, 
                                     moments = moments, lambda = lambda, target = target, 
                                     init_weights = portfolio$assets, solver = solver, 
                                     control = control)
            weights <- qp_result$weights
            obj_vals <- qp_result$obj_vals
            out <- list(weights = weights, objective_measures = obj_vals, 
                        opt_values = obj_vals, out = qp_result$out, 
                        call = call)
          }
          if (!is.null(constraints$ptc) & is.null(constraints$turnover_target)) {
            qp_result <- gmv_opt_ptc(R = R, constraints = constraints, 
                                     moments = moments, lambda = lambda, target = target, 
                                     init_weights = portfolio$assets, solver = solver, 
                                     control = control)
            weights <- qp_result$weights
            obj_vals <- qp_result$obj_vals
            out <- list(weights = weights, objective_measures = obj_vals, 
                        opt_values = obj_vals, out = qp_result$out, 
                        call = call)
          }
          if (!is.null(constraints$leverage)) {
            qp_result <- gmv_opt_leverage(R = R, constraints = constraints, 
                                          moments = moments, lambda = lambda, target = target, 
                                          solver = solver, control = control)
            weights <- qp_result$weights
            obj_vals <- qp_result$obj_vals
            out <- list(weights = weights, objective_measures = obj_vals, 
                        opt_values = obj_vals, out = qp_result$out, 
                        call = call)
          }
        }
        else {
          if (hasArg(maxSR)) 
            maxSR = match.call(expand.dots = TRUE)$maxSR
          else maxSR = FALSE
          if (maxSR) {
            target <- max_sr_opt(R = R, constraints = constraints, 
                                 moments = moments, lambda_hhi = lambda_hhi, 
                                 conc_groups = conc_groups, solver = solver, 
                                 control = control)
            tmp_moments_mean <- moments$mean
            moments$mean <- rep(0, length(moments$mean))
          }
          roi_result <- gmv_opt(R = R, constraints = constraints, 
                                moments = moments, lambda = lambda, target = target, 
                                lambda_hhi = lambda_hhi, conc_groups = conc_groups, 
                                solver = solver, control = control)
          weights <- roi_result$weights
          obj_vals <- roi_result$obj_vals
          if (maxSR) {
            port.mean <- as.numeric(sum(weights * tmp_moments_mean))
            names(port.mean) <- "mean"
            obj_vals$mean <- port.mean
          }
          out <- list(weights = weights, objective_measures = obj_vals, 
                      opt_values = obj_vals, out = roi_result$out, 
                      call = call)
        }
      }
      if (length(names(moments)) == 1 & "mean" %in% names(moments)) {
        if (optimize_method == "ROI") {
          solver <- "glpk"
        }
        else {
          solver <- optimize_method
        }
        if (!is.null(constraints$max_pos) | !is.null(constraints$leverage)) {
          roi_result <- maxret_milp_opt(R = R, constraints = constraints, 
                                        moments = moments, target = target, solver = solver, 
                                        control = control)
          weights <- roi_result$weights
          obj_vals <- roi_result$obj_vals
          out <- list(weights = weights, objective_measures = obj_vals, 
                      opt_values = obj_vals, out = roi_result$out, 
                      call = call)
        }
        else {
          roi_result <- maxret_opt(R = R, constraints = constraints, 
                                   moments = moments, target = target, solver = solver, 
                                   control = control)
          weights <- roi_result$weights
          obj_vals <- roi_result$obj_vals
          out <- list(weights = weights, objective_measures = obj_vals, 
                      opt_values = obj_vals, out = roi_result$out, 
                      call = call)
        }
      }
      if (any(c("CVaR", "ES", "ETL") %in% 
              names(moments))) {
        if (optimize_method == "ROI") {
          solver <- "glpk"
        }
        else {
          solver <- optimize_method
        }
        if (hasArg(ef)) 
          ef = match.call(expand.dots = TRUE)$ef
        else ef = FALSE
        if (hasArg(maxSTARR)) 
          maxSTARR = match.call(expand.dots = TRUE)$maxSTARR
        else maxSTARR = TRUE
        if (ef) 
          meanetl <- TRUE
        else meanetl <- FALSE
        tmpnames <- c("CVaR", "ES", "ETL")
        idx <- which(tmpnames %in% names(moments))
        if (length(moments) == 2 & all(moments$mean != 0) & 
            ef == FALSE & maxSTARR) {
          target <- mean_etl_opt(R = R, constraints = constraints, 
                                 moments = moments, alpha = alpha, solver = solver, 
                                 control = control)
          meanetl <- TRUE
        }
        if (!is.null(constraints$max_pos)) {
          roi_result <- etl_milp_opt(R = R, constraints = constraints, 
                                     moments = moments, target = target, alpha = alpha, 
                                     solver = solver, control = control)
          weights <- roi_result$weights
          obj_vals <- list()
          if (meanetl) 
            obj_vals$mean <- sum(weights * moments$mean)
          obj_vals[[tmpnames[idx]]] <- roi_result$out
          out <- list(weights = weights, objective_measures = obj_vals, 
                      opt_values = obj_vals, out = roi_result$out, 
                      call = call)
        }
        else {
          roi_result <- etl_opt(R = R, constraints = constraints, 
                                moments = moments, target = target, alpha = alpha, 
                                solver = solver, control = control)
          weights <- roi_result$weights
          obj_vals <- list()
          if (meanetl) 
            obj_vals$mean <- sum(weights * moments$mean)
          obj_vals[[tmpnames[idx]]] <- roi_result$out
          out <- list(weights = weights, objective_measures = obj_vals, 
                      opt_values = obj_vals, out = roi_result$out, 
                      call = call)
        }
      }
      optimize_method <- "ROI"
    }
    if (optimize_method == "pso") {
      stopifnot("package:pso" %in% search() || requireNamespace("pso", 
                                                                quietly = TRUE))
      if (hasArg(maxit)) 
        maxit = match.call(expand.dots = TRUE)$maxit
      else maxit = N * 50
      controlPSO <- list(trace = FALSE, fnscale = 1, maxit = 1000, 
                         maxf = Inf, abstol = -Inf, reltol = 0)
      PSOcargs <- names(controlPSO)
      if (is.list(dotargs)) {
        pm <- pmatch(names(dotargs), PSOcargs, nomatch = 0L)
        names(dotargs[pm > 0L]) <- PSOcargs[pm]
        controlPSO$maxit <- maxit
        controlPSO[pm] <- dotargs[pm > 0L]
        if (!hasArg(reltol)) 
          controlPSO$reltol <- 1e-06
        if (hasArg(trace) && try(trace == TRUE, silent = TRUE)) 
          controlPSO$trace <- TRUE
        if (hasArg(trace) && isTRUE(trace)) {
          controlPSO$trace <- TRUE
          controlPSO$trace.stats = TRUE
        }
      }
      upper <- constraints$max
      lower <- constraints$min
      minw <- try(pso::psoptim(par = rep(NA, N), fn = constrained_objective, 
                               R = R, portfolio = portfolio, env = dotargs, lower = lower[1:N], 
                               upper = upper[1:N], control = controlPSO))
      if (inherits(minw, "try-error")) {
        minw = NULL
      }
      if (is.null(minw)) {
        message(paste("Optimizer was unable to find a solution for target"))
        return(paste("Optimizer was unable to find a solution for target"))
      }
      weights <- as.vector(minw$par)
      weights <- normalize_weights(weights)
      names(weights) <- colnames(R)
      obj_vals <- constrained_objective(w = weights, R = R, 
                                        portfolio = portfolio, trace = TRUE, env = dotargs)$objective_measures
      out <- list(weights = weights, objective_measures = obj_vals, 
                  opt_values = obj_vals, out = minw$value, call = call)
      if (isTRUE(trace)) {
        out$PSOoutput = minw
      }
    }
    if (optimize_method == "GenSA") {
      stopifnot("package:GenSA" %in% search() || requireNamespace("GenSA", 
                                                                  quietly = TRUE))
      if (hasArg(maxit)) 
        maxit = match.call(expand.dots = TRUE)$maxit
      else maxit = N * 50
      controlGenSA <- list(maxit = 5000, threshold.stop = NULL, 
                           temperature = 5230, visiting.param = 2.62, acceptance.param = -5, 
                           max.time = NULL, nb.stop.improvement = 1e+06, smooth = TRUE, 
                           max.call = 1e+07, verbose = FALSE)
      GenSAcargs <- names(controlGenSA)
      if (is.list(dotargs)) {
        pm <- pmatch(names(dotargs), GenSAcargs, nomatch = 0L)
        names(dotargs[pm > 0L]) <- GenSAcargs[pm]
        controlGenSA$maxit <- maxit
        controlGenSA[pm] <- dotargs[pm > 0L]
        if (hasArg(trace) && try(trace == TRUE, silent = TRUE)) 
          controlGenSA$verbose <- TRUE
      }
      upper <- constraints$max
      lower <- constraints$min
      if (!is.null(rp)) 
        par = rp[, 1]
      else par = rep(1/N, N)
      minw = try(GenSA::GenSA(par = par, lower = lower[1:N], 
                              upper = upper[1:N], control = controlGenSA, fn = constrained_objective, 
                              R = R, portfolio = portfolio, env = dotargs))
      if (inherits(minw, "try-error")) {
        minw = NULL
      }
      if (is.null(minw)) {
        message(paste("Optimizer was unable to find a solution for target"))
        return(paste("Optimizer was unable to find a solution for target"))
      }
      weights <- as.vector(minw$par)
      weights <- normalize_weights(weights)
      names(weights) <- colnames(R)
      obj_vals <- constrained_objective(w = weights, R = R, 
                                        portfolio = portfolio, trace = TRUE, env = dotargs)$objective_measures
      out = list(weights = weights, objective_measures = obj_vals, 
                 opt_values = obj_vals, out = minw$value, call = call)
      if (isTRUE(trace)) {
        out$GenSAoutput = minw
      }
    }
    end_t <- Sys.time()
    if (message) 
      message(c("elapsed time:", end_t - start_t))
    out$portfolio <- portfolio
    if (trace) 
      out$R <- R
    out$data_summary <- list(first = first(R), last = last(R))
    out$elapsed_time <- end_t - start_t
    out$end_t <- as.character(Sys.time())
    if (regime.switching) {
      out$regime <- regime.idx
    }
    class(out) <- c(paste("optimize.portfolio", optimize_method, 
                          sep = "."), "optimize.portfolio")
    return(out)
  }



minVarCM<-function(da,ret_format="index",table_format='wide',
                          resampling_switch=T,resampling_switch_cov_lambda=0.5,
                          ann_factor=252,chart_export_width=600,chart_export_height=450,m = list(l = 50,r = 50,b = 80,t = 50,pad = 4),n.portfolios=30,box_constraint_list=NULL)
{

      
      if (!require("purrr")) install.packages("data.table")
      if (!require("PortfolioAnalytics")) install.packages("dplyr")
      if (!require("ecm")) install.packages("ecm")
      if (!require("plotly")) install.packages("lubridate")
      if (!require("scales")) install.packages("scales")
      if (!require("faux")) install.packages("scales")
      
      library(purrr)
      library(PortfolioAnalytics)
      library(ecm)
      library(plotly)
      library(scales)
      library(faux)
      
      calc_ret<-function(x)
      {
        x<-x/lagpad(x,k=1)-1
        x[1]<-0
        return(x)
      }
      calc_log_ret<-function(x)
      {
        x<-log(x/lagpad(x,k=1))
        x[1]<-0
        return(x)
      }
      
      dls<-rrScat(da,ret_format=ret_format,table_format=table_format,graphics=F)$dls
      
      if(table_format!='wide')
      {
        names(da)<-c("date",'id','value')
        da$date<-as.Date(da$date)
        da<-data.table::dcast(as.data.table(da), date ~ id, value.var = "value")
      }else{
        names(da)[1]<-"date"
        da$date<-as.Date(da$date)
      }
      
      dz<-read.zoo(da,index.column=1)
      R<-as.xts(dz)
      
      if(ret_format!="returns")
      {
        R<-apply(R,2,calc_ret)
      }
      
      funds <- colnames(R)
      
      
      # Set up portfolio with objectives and constraints
      #Initial Portfolio
      
      wgts <- c(rep(1/length(funds),length(funds)))
      names(wgts) <- funds
      
      init.portf <- portfolio.spec(assets=wgts) 
      init.portf <- add.constraint(portfolio = init.portf, type="weight_sum", min_sum = 0.99999, max_sum = 1.0001)
      init.portf <- add.constraint(portfolio = init.portf, type="long_only")
      
      #Max Return Portfolio - Target is assumed to be market cap for market-cap weighted portfolios
      init.portf <- add.objective(portfolio=init.portf, type="risk", name="StdDev")
      #init.portf <- add.objective(portfolio=init.portf, type="return", name="mean")
      
      #Add Group Constraints
      #init.portf <- add.constraint(portfolio=init.portf,type="group",
      #                             groups=group_constraint_list$groups,
      #                             group_min=group_constraint_list$constraints$group_min,
      #                             group_max=group_constraint_list$constraints$group_max)
      
      
      #Add Box Constraints      
      if(is.null(box_constraint_list))
      {
        box_constraint_list<-list(
          min=c(rep(0,length(wgts))),
          max=c(rep(1,length(wgts)))
        )        
      }
      init.portf <- add.constraint(portfolio=init.portf,
                                   type="box",
                                   min=box_constraint_list$min[1:length(wgts)],
                                   max=box_constraint_list$max[1:length(wgts)])

      if(resampling_switch=="None")
      {
        shrinkage_lambda<-0.000001
      }else{
        if(is.na(resampling_switch_cov_lambda))
        {
          shrinkage_lambda<-as.numeric(estimate.lambda.var(R))
          shinyalert(
            title = "Missing shrinkage lambda",
            text = paste0("You have not specified a shrinkage lambda. The system is therefore using the optimal shrinkage intensivty lambda following Opgen-Rhein and Strimmer 2007.
            The optimal shrinkage lambda used is: ",round(shrinkage_lambda,2),""),
            type = "warning",closeOnEsc = TRUE,closeOnClickOutside = TRUE) 
          shrinkage_lambda<-max(shrinkage_lambda,0.000001)
        }else{
          shrinkage_lambda<-resampling_switch_cov_lambda        
          shrinkage_lambda<-max(shrinkage_lambda,0.000001)          
        }
      }
      
      #Prepare Efficient Frontier
      return_periodicity_settings=0
      ret_lag<-return_periodicity_settings
      return_periodicity_num<-1*ret_lag
      
      
      #shrinkage_lambda<-0.1
      lw.sigma <- function(R) {
        library(corpcor)
        
        out <- list()
        #tmp<-lwShrink(R,0.5)$cov
        tmp<-corpcor::cov.shrink(R, shrinkage_lambda, shrinkage_lambda)
        tmp<-corpcor:::make.positive.definite(tmp)
        
        #out$mu <-as.vector((1+df_w_risk_return/100)^((1*ret_lag)/252))-1
        out$sigma <- tmp[1:ncol(R),1:ncol(R)]
        
        return(out)
      }
      
      
      #Retrain R based on given return expectation and covariance matrix
      tmp <- corpcor::cov.shrink(R, shrinkage_lambda, shrinkage_lambda)
      coVarMat <- tmp[1:ncol(R),1:ncol(R)]
      corMat<-cov2cor(coVarMat)
      
      valid_names<-colnames(corMat)
      df_w_risk_return<-as.data.frame(da)[,as.character(valid_names)]
      
      

      set.seed(1) 
      Rsim<-
        rnorm_multi(
          n = nrow(R), 
          mu = as.numeric(dls$ret_ann/ann_factor),
          varnames=names(df_w_risk_return),
          r = c(coVarMat),empirical = T
        )    
      

      Rsim$dates<-as.Date(da$date)
      Rsim<-read.zoo(Rsim,index.column="dates")
      Rsim<-as.xts(Rsim)
      #Rsim<-log(1+Rsim)
      #tail(cumprod(1+Rsim))
      #plot(cumprod(1+Rsim))
      
      #global_wd<-"C:/FS/quantamental_platform/SafeCopies/administrator/Docker_ShinyPortfolioOptimizer/app3/"
      #source(paste0(global_wd,"public_optimizer_functions/custom_moments_optimizer_hacked.R"))
      opt_qu <- PortfolioAnalytics::optimize.portfolio(Rsim, portfolio=init.portf, optimize_method="ROI")
      
      w<-opt_qu$weights
      n<-names(opt_qu$weights)
      p<-allocTree(data.frame(n,w),parent_label="Minimum Variance Portfolio")
      
      optimization_list<-list("p"=p,"R"=R,"Remp"=R,"Rsim"=Rsim,"init.portf"=init.portf,"opt_qu"=opt_qu)

      return(optimization_list)
      
      #
      #ef<-create.EfficientFrontier(R=R, portfolio=init.portf, type="mean-var", n.portfolios = n.portfolios,risk_aversion = NULL, match.col = "ES", search_size = 500)
      #eff<-as.data.frame(cbind(ef$frontier[,4:ncol(ef$frontier)]))

      #mean_cvar_ef<<-NULL
      #mean_var_ef<<-NULL      

}

