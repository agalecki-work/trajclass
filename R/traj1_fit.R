
#' Fit linear, quadratic and segmented models on traj1 trajectory data
#'
#' @param x Object of class `traj1` — tibble with columns `id`, `time`, `y` for one subject
#'   + attributes (including potentially `info` list with analysis parameters)
#'
#' @return Object of class `"traj1_fit"` — a list containing:
#'   - `fit`: list with elements `linear`, `quadratic`, `segmented` (or NULL)
#'   - `info`: tibble with 3 rows (model status & validity)
#'   - attribute `"traj_attrs"`: list with metadata (object_id, original attributes, etc.)
#'   - attribute `"keys"`: character vector of key variable names (usually c("id", "time", "y"))
#'
#' @export
traj1_fit <- function(x, 
                davies_p_value = getOption("trajclass.davies_p_value", NULL),
                display_seg    = getOption("trajclass.display_seg", FALSE)
   ){
  
  # ── Input validation ─────────────────────────────────────────────────────────
  if (!inherits(x, "traj1")) {
    stop("x must be an object of class 'traj1'")
  }
  
  if (!all(c("id", "time", "y") %in% names(x))) {
    stop("traj1 object must contain columns 'id', 'time', 'y'")
  }
  
  data <- x
  
  
  # ── Subject ID (single subject expected) ─────────────────────────────────────
  id_vals <- unique(na.omit(x$id))
  object_id <- if (length(id_vals) == 1L) {
    as.character(id_vals)
  } else {
    if (length(id_vals) > 1L) {
      warning("Multiple distinct ids found in traj1 object — using first")
    }
    as.character(id_vals[1L]) %||% NA_character_
  }
  
  # ── Capture attributes ───────────────────────────────────────────────────────
  all_attrs <- attributes(x)
  protected <- c("class", "row.names", "names", ".internal.selfref")
  other_attrs <- all_attrs[!names(all_attrs) %in% protected]
  
  traj_attrs <- c(
    list(
      object_id = object_id
    ),
    other_attrs
  )
  
  # ── Status tibble ────────────────────────────────────────────────────────────
  status_df <- tibble::tribble(
    ~model,      ~status,     ~message,                  ~valid,
    "linear",    "pending",   NA_character_,             FALSE,
    "quadratic", "pending",   NA_character_,             FALSE,
    "hockey_stick", "pending",   NA_character_,             FALSE
  )
  
  # ── Safe fitting helper ──────────────────────────────────────────────────────
  fit_safely <- function(expr, model_name, expected_class = "lm") {
    res <- tryCatch(
      {
        m <- eval(expr)
        if (inherits(m, expected_class) && !is.null(coef(m))) {
          list(model = m, status = "ok", message = NULL, valid = TRUE)
        } else {
          list(model = NULL, status = "failed", message = "Invalid model object", valid = FALSE)
        }
      },
      warning = function(w) list(model = NULL, status = "warning", message = conditionMessage(w), valid = FALSE),
      error   = function(e) list(model = NULL, status = "error",   message = conditionMessage(e), valid = FALSE)
    )
    
    row_idx <- which(status_df$model == model_name)
    status_df$status[row_idx]  <<- res$status
    status_df$message[row_idx] <<- res$message %||% NA_character_
    status_df$valid[row_idx]   <<- res$valid
    
    res
  }
  
  # ── Linear model ─────────────────────────────────────────────────────────────
  fit_lin <- fit_safely(quote(lm(y ~ time, data = data)), "linear")
  
  # ── Quadratic model ──────────────────────────────────────────────────────────
  fit_quad <- fit_safely(quote(lm(y ~ time + I(time^2), data = data)), "quadratic")
  
  # ── Segmented model ──────────────────────────────────────────────────────────
  fit_seg <- list(model = NULL, status = "skipped", message = NA_character_, valid = FALSE)
  
  if (fit_lin$status == "ok") {
    
    davies_res <- tryCatch(
      segmented::davies.test(fit_lin$model, seg.Z = ~ time),
      error   = function(e) list(p.value = NA_real_),
      warning = function(w) list(p.value = NA_real_)
    )
    
    davies_p <- davies_res$p.value
    
    if (!is.na(davies_p) && davies_p > davies_p_value) {
      msg <- sprintf("Davies p = %.3f > %.2f → no strong breakpoint evidence",
                     davies_p, davies_p_value)
      fit_seg$status  <- "skipped"
      fit_seg$message <- msg
      fit_seg$valid   <- FALSE
      status_df$message[status_df$model == "segmented"] <- msg
      status_df$status[status_df$model == "segmented"]  <- "skipped"
      status_df$valid[status_df$model == "segmented"]   <- FALSE
    } else {
      
      # Multiple reasonable starting points for psi
      t_range <- range(data$time, na.rm = TRUE)
      psi_starts <- unique(c(
        median(data$time, na.rm = TRUE),
        quantile(data$time, 0.25, na.rm = TRUE),
        quantile(data$time, 0.75, na.rm = TRUE)
      ))
      
      for (psi0 in psi_starts) {
        seg_try <- tryCatch(
          segmented::segmented(
            fit_lin$model,
            seg.Z   = ~ time,
            psi     = psi0,
            control = segmented::seg.control(
              K       = 1L,
              n.boot  = 15L,
              it.max  = 40L,
              tol     = 1e-5,
              display = display_seg
            )
          ),
          error   = function(e) NULL,
          warning = function(w) NULL
        )
        
        if (!is.null(seg_try) && 
            inherits(seg_try, "segmented") &&
            nrow(seg_try$psi) > 0 &&
            !any(is.na(seg_try$psi[, "Est."]))) {
          
          psi_est <- seg_try$psi[1, "Est."]
          margin  <- diff(t_range) * 0.05
          
          # Check if estimated breakpoint is plausibly inside the data range
          if (psi_est > t_range[1] + margin && psi_est < t_range[2] - margin) {
            fit_seg <- list(
              model   = seg_try,
              status  = "ok",
              message = NA_character_,
              valid   = TRUE
            )
            status_df$status[status_df$model == "hockey_stick"]  <- "ok"
            status_df$message[status_df$model == "hockey_stick"] <- sprintf("converged (psi ≈ %.2f)", psi_est)
            status_df$valid[status_df$model == "hockey_stick"]   <- TRUE
            break  # success → stop trying other starts
          }
        }
      }
      
      # If no successful fit after all attempts
      if (fit_seg$status != "ok") {
        msg <- "failed to converge with multiple psi starting values"
        fit_seg$message <- msg
        status_df$message[status_df$model == "hockey_stick"] <- msg
        status_df$status[status_df$model == "hockey_stick"]  <- "failed"
        status_df$valid[status_df$model == "hockey_stick"]   <- FALSE
      }
    }
  }
  
  # ── Final result ─────────────────────────────────────────────────────────────
 
  t1 <- x
  t1_info <- attributes(t1)$traj1_info
  key_nms <- t1_info$keys
  key_nm1 <- key_nms[1]
  attributes(t1) <- NULL
  names(t1) <- key_nms
  df <- as_tibble(t1)
   
  result <- list(
    fit  = list(
      linear    = fit_lin$model,
      quadratic = fit_quad$model,
      hockey_stick = fit_seg$model
    ))

  attrs <- list(
    traj1_id = df[[key_nm1]][1],  
    df = df,
    fit_options = trajclass_options(all =TRUE),
    fit_info = status_df
  )
  
  # Attach metadata
  attr(result, "attrs_list") <- attrs
  class(result) <- "traj1_fit"
  
  result
}
