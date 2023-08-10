# Helper function for check_fnc_deps to list all the exported functions nicely
pkg_ls <- function(pkg) {
  ns <- getNamespace(pkg)
  exports <- getNamespaceExports(ns)

  names <- intersect(exports, ls(envir = ns, all.names = TRUE, sorted = FALSE))
  grep("^.__", names, invert = TRUE, value = TRUE)
}

# counts if a string is present in a set of files or how many times it does
count_fnc_calls <- function(files, fnc_string, any_or_sum = "sum") {

  # should this function return a sum or a any
  if (any_or_sum == "sum") {
    flag_is <- TRUE
  } else if (any_or_sum == "any") {
    flag_is <- FALSE
  } else {
    stop("any_or_sum accepts only sum or any.")
  }

  # main apply
  v_tmp <- vapply(files,
                  FUN.VALUE = ifelse(flag_is, numeric(1), logical(1)),
                  FUN = function(x) {
                    lines <- readLines(x)
                    if (flag_is) {
                      sum(grepl(pattern = fnc_string, x = lines))
                    } else {
                      any(grepl(pattern = fnc_string, x = lines))
                    }
                  }
  )

  return(v_tmp)
}

# return greped lines with file
get_greped_lines <- function(files, fnc_string) {

  # main apply
  v_tmp <- lapply(files,
                  FUN = function(x) {
                    lines <- readLines(x)
                    int_lines <- lines[grep(pattern = fnc_string, x = lines)]
                    int_lines <- str_remove_all(int_lines, fixed("\\alias{"))
                    int_lines <- str_remove_all(int_lines, fixed("}"))
                    int_lines
                    tibble(
                      "doc_file" = gsub(".Rd$", "", basename(x)),
                      "fun" = int_lines
                    )
                  }
  )
  return(do.call(bind_rows, v_tmp))
}



# finding man files
man_files <- function(path) {
  list.files(file.path(path, "man"), full.names = TRUE, pattern = ".Rd$")
}

# finding man files
source_files <- function(path) {
  list.files(file.path(path, "R"), full.names = TRUE, pattern = ".R$")
}

# badge and internal explorer
badge_and_details <- function(path = ".",
                              has_it = NULL,
                              all_fun = FALSE,
                              check_exported = FALSE) {

  # finding all indexed files
  all_docs <- man_files(path)

  # how many fncs?
  res_n_fun <- count_fnc_calls(all_docs, "alias\\{*", any_or_sum = "sum")

  # it has stable badge
  res_stable <- count_fnc_calls(all_docs, "figure\\{lifecycle-stable", any_or_sum = "sum")

  # it has deprecated badge
  res_deprecated <- count_fnc_calls(all_docs, "figure\\{lifecycle-deprecated", any_or_sum = "sum")

  # it has experimental badge
  res_experimental <- count_fnc_calls(all_docs, "figure\\{lifecycle-experimental", any_or_sum = "sum")

  # is internal?
  res_int <- count_fnc_calls(all_docs, "keyword\\{.*internal\\}", any_or_sum = "sum")

  # summary table
  tbl_ret <- tibble(
    "doc_file" = gsub(".Rd$", "", basename(names(res_stable))),
    "n_fun" = as.numeric(res_n_fun),
    "stable" = as.numeric(res_stable),
    "deprecated" = as.numeric(res_deprecated),
    "experimental" = as.numeric(res_experimental),
    "internal" = as.numeric(res_int)
  )

  # return all files
  if (all_fun) {
    grp_lines <- get_greped_lines(all_docs, "alias\\{*")
    tbl_ret <- full_join(tbl_ret, grp_lines, by = "doc_file")

    # check if functions are exported
    if (check_exported) {
      tbl_ret <- tbl_ret %>%
        mutate("Is.exported" = fun %in% getNamespaceExports("tern"))
    }
  } else {
    if (check_exported) {
      stop("Ask for all functions first to check if they are exported")
    }
  }


  # final return
  if (is.null(has_it)) {
    return(tbl_ret)
  } else {
    if (has_it) {
      tbl_ret %>%
        filter(stable != 0 | deprecated != 0 | experimental != 0)
    } else {
      tbl_ret %>%
        filter(stable == 0 & deprecated == 0 & experimental == 0)
    }
  }
}

# main dep table finder
find_fnc_downstream_deps <- function(pkgs, fun_call, main_dir) {

  tot_bind <- NULL
  for (a_pkg_nm in pkgs) {

    # checking if it is the TLG file
    if (grepl("TLG", a_pkg_nm)) {
      fol_nm <- ""
      fl_ext <- c(".Rmd")
      rec <- FALSE
    } else if (grepl("Biomarker", a_pkg_nm)) {
      fol_nm <- ""
      fl_ext <- c(".Rmd")
      rec <- FALSE
    } else {
      fol_nm <- c("R", "tests", "vignettes")
      fl_ext <- c(".R", ".R", ".Rmd")
      rec <- TRUE
    }

    # loop on the folder to check
    for (i in 1:length(fol_nm)) {

      # searching in all files
      all_files <- list.files(file.path(paste0(main_dir, "/", a_pkg_nm), fol_nm[i]),
                              full.names = TRUE, recursive = rec, pattern = fl_ext[i]
      )

      # correcting specification for no fol
      if (grepl("TLG", a_pkg_nm)) {
        fol_nm[i] <- "TLG"
      } else if (grepl("Biomarker", a_pkg_nm)) {
        fol_nm[i] <- "Biomarker"
      }

      # various kind of possible call and mentions
      fnc_ext_calls <- sum(count_fnc_calls(all_files, paste0(pkg, "::", fun_call, "\\(")))
      fnc_int_calls <- sum(count_fnc_calls(all_files, paste0(pkg, ":::", fun_call, "\\(")))
      fnc_import_calls <- sum(count_fnc_calls(all_files, paste0(" ", fun_call, "\\(")))
      fnc_cmp_calls <- sum(count_fnc_calls(all_files, paste0("_", fun_call, "\\(")))
      fnc_var_calls <- sum(count_fnc_calls(all_files, paste0(fun_call, "\\)"))) +
        sum(count_fnc_calls(all_files, paste0(fun_call, "\""))) +
        sum(count_fnc_calls(all_files, paste0(fun_call, ",")))
      fnc_mentions <- sum(count_fnc_calls(all_files, fun_call))

      # total binder
      tot_bind <- rbind(
        tot_bind,
        tibble(
          "Type" = c(
            "Ext (::)",
            "Int (:::)",
            "Import ( )",
            "Composite (_)",
            "Variable (,\" ))",
            "Mention ()"
          ),
          "Count" = c(
            fnc_ext_calls,
            fnc_int_calls,
            fnc_import_calls,
            fnc_cmp_calls,
            fnc_var_calls,
            fnc_mentions
          ),
          "DownstreamDep" = rep(a_pkg_nm, 6),
          "Folder" = rep(fol_nm[i], 6),
          "FileExt" = rep(fl_ext[i], 6)
        )
      )
    }
  }

  # filtering 0s out
  tot_bind <- tot_bind %>%
    filter(Count != 0)

  return(tot_bind)
}

# LONG run: searches all the folds from the home_dir for all the funs use
all_deps_from_fun_list <- function(funs_list, folds, home_dir) {

  # main loop over function names
  out <- parallel::mclapply(funs_list, function(fun_i) {

    # main extraction function of dependences' table
    dwn_deps_gen <- find_fnc_downstream_deps(folds, fun_i, home_dir) %>%
      filter(str_detect(Type, "Compost", negate = TRUE)) %>%
      filter(str_detect(Type, "Variable", negate = TRUE)) %>%
      filter(str_detect(Type, "Mention", negate = TRUE))

    # all the imports and ext (::) and internal (:::) not in TLG
    deps_sum <- dwn_deps_gen %>%
      filter(str_detect(Folder, "TLG", negate = TRUE)) %>%
      filter(str_detect(Folder, "Biomarker", negate = TRUE)) %>%
      pull(Count) %>%
      sum()

    # all the imports and ext (::) and internal (:::) in TLG
    deps_tlg_sum <- dwn_deps_gen %>%
      filter(str_detect(Folder, "TLG")) %>%
      pull(Count) %>%
      sum()

    # all the imports and ext (::) and internal (:::) in Biomarker
    deps_bm_sum <- dwn_deps_gen %>%
      filter(str_detect(Folder, "Biomarker")) %>%
      pull(Count) %>%
      sum()

    # out tbl
    tibble("fun" = fun_i,
           "deps_dwn" = deps_sum,
           "deps_TLG" = deps_tlg_sum,
           "deps_Biomarker" = deps_bm_sum)
  }, mc.cores = parallel::detectCores() - 1) %>%
    do.call(what = bind_rows)

  return(out)
}

#' Retrieve function dependencies
#'
# @param dep_tbl Dependency table
#' @param pkg
#' @param install
#'
pkg <- "~/NEST/tern"
check_fnc_deps <- function(pkg = ".",
                           dep_tree = NULL,
                           install = FALSE,
                           additional_folders = NULL,
                           verbose = 1) {
  # Input checks
  checkmate::assert_string(pkg)
  checkmate::assert_class(dep_tree, "dependency_structure", null.ok = TRUE)
  checkmate::assert_flag(install)
  checkmate::assert_character(additional_folders, null.ok = TRUE)
  checkmate::assert_int(verbose, lower = 0, upper = 2)

  # Setting pkg path
  if (pkg == ".") {
    pkg <- fs::path_norm(pkg)
  } else {
    stopifnot(file.exists(pkg))
  }

  # Get the simple name of the package
  pkg_nm <- basename(pkg)

  # Extract the dependency tree
  if (is.null(dep_tree)) {
    staged.dependencies:::message_if_verbose("Creating dependency tables. This might take a couple of minutes.", verbose)
    dep_tree <- dependency_table(project = pkg, project_type = "local", verbose = 0)
  } else {
    if (!checkmate::test_set_equal(dep_tree$current_pkg, pkg_nm)) {
      warning("Overriding the inserted pkg folder directive with the inserted dep_tree current")
      pkg_nm <- dep_tree$current_pkg
    }
  }

  # Downstream dependencies
  dt_tbl_d <- dep_tree$table %>%
    filter(type == "downstream")

  # pkg fncs and deps names
  fnc_ex <- pkg_ls(pkg_nm)
  pkgs <- dt_tbl_d %>% pull(package_name)

  # The following is only a installed-level analysis
  excl_deps <- c("scda.test", "tlg.catalog.pkg", "biomarker.catalog")
  pkgs <- pkgs[!pkgs %in% excl_deps] # Exclusion of pkgs w/o imported/exported fncs
  # pkg_i <- "teal.osprey"
  high_lev_out <- lapply(pkgs, function(pkg_i) {
    itdepends::dep_usage_pkg(pkg_i) %>%
      group_by(fun, pkg) %>%
      count(sort = TRUE) %>%
      ungroup() %>%
      filter(pkg != "base") %>%
      filter(pkg == pkg_nm) %>%
      mutate(ref_pkg = pkg_i, .before = "pkg")
  }) %>% do.call(what = "rbind") %>%
    arrange(desc(n))
  high_lev_out %>%
    print(n = 25) %>%
    filter(fun == "df_explicit_na")

  source(paste0(home_dir, "design_and_trials/staged_dep_tests.R"))
  path <- paste0(home_dir, pkg_nm)

}
