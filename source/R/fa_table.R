# https://www.franciscowilhelm.com/post/exploratory-factor-analysis-table/
fa_table <- function(x, varlabels = NULL,
                      title = "Factor analysis results", diffuse = .10,
                      small = .30, cross = .20, sort = TRUE,
                     fnames = NULL) {
  #get sorted loadings
  require(dplyr)
  require(purrr)
  require(tibble)
  require(kableExtra)
  if (sort == TRUE) {
    x <- psych::fa.sort(x)
  }
  if (!is.null(varlabels)) {
    if (length(varlabels) != nrow(x$loadings)) {
      warning("Number of variable labels and number of variables are unequal.
              Check your input!",
              call. = FALSE) }
    if (sort == TRUE) {
      varlabels <- varlabels[x$order]
    }
  }
  if (is.null(varlabels)) {varlabels <- rownames(x$loadings)}

  loadings <- data.frame(unclass(x$loadings))
  nfactors <- ncol(loadings)
  #make nice names
  if (is.null(fnames)) {
  factornamer <- function(nfactors) {
    paste0("Factor_", 1:nfactors)}
  fnames <- factornamer(nfactors)
  }
  if (length(fnames) != ncol(x$loadings)) {
    warning("Number of factor labels and number of factors are unequal.
              Check your input!",
            call. = FALSE)
    }
  names(loadings) <- fnames

  # prepare locations
  factorindex <- apply(loadings, 1, function(x) which.max(abs(x)))

  # adapted from from sjplot: getremovableitems
  getRemovableItems <- function(dataframe, fctr.load.tlrn = diffuse) {
    # clear vector
    removers <- vector(length = nrow(dataframe))
    # iterate each row of the data frame. each row represents
    # one item with its factor loadings
    for (i in seq_along(removers)) {
      # get factor loadings for each item
      rowval <- as.numeric(abs(dataframe[i, ]))
      # retrieve highest loading
      maxload <- max(rowval)
      # retrieve 2. highest loading
      max2load <- sort(rowval, TRUE)[2]
      # check difference between both
      if (abs(maxload - max2load) < fctr.load.tlrn) {
        # if difference is below the tolerance,
        # remeber row-ID so we can remove that items
        # for further PCA with updated data frame
        removers[i] <- TRUE
      }
    }
    # return a vector with index numbers indicating which items
    # have unclear loadings
    return(removers)
  }
  if (nfactors > 1) {
    removable <- getRemovableItems(loadings)
    cross_loadings <-
      purrr::map2(fnames,
                  seq_along(fnames),
                  function(f, i) {
                    (abs(loadings[, f] > cross)) & (factorindex != i)
                  })
    }

  small_loadings <- purrr::map(fnames, function(f) {
    abs(loadings[, f]) < small
  })

  ind_table <- dplyr::tibble(varlabels, loadings) %>%
    dplyr::rename(Indicator = varlabels) %>%
    dplyr::mutate(Communality = x$communality,
                  Uniqueness = x$uniquenesses,
                  Complexity = x$complexity) %>%
    dplyr::mutate(across(starts_with("Factor"), round, 3))  %>%
    dplyr::mutate(across(c(Communality, Uniqueness, Complexity), round, 2))


  ind_table <- ind_table %>%
    kable(booktabs = TRUE,
          caption = title, digits = 4) %>%
    kableExtra::column_spec(1, "2in") %>%
    kable_styling(font_size = 9)

    # gt(rowname_col = "Indicator") %>%
    # tab_caption(caption = title) %>%
    # cols_width(Indicator ~ px(150))
  # mark small loadings
  for (f in seq_along(fnames)) {
    ind_table <- ind_table %>%
      kableExtra::column_spec(1 + f,
                              color = ifelse(small_loadings[[f]], "#D3D3D3", "#000000"),
                              italic = small_loadings[[f]])
                # locations = cells_body(columns = fnames[f],
                #                        rows = small_loadings[[f]]))
  }
  # mark cross loadings

  if (nfactors > 1) {
    for (f in seq_along(fnames)) {
      ind_table <- ind_table %>%
        kableExtra::column_spec(1 + f,
                                italic = small_loadings[[f]])
        # ind_table %>%  tab_style(
        #   style = cell_text(style = "italic"),
        #   locations = cells_body(columns = fnames[f],
        #                          rows = cross_loadings[[f]])
        #)
    }
    # mark non-assignable indicators
    ind_table <-
      ind_table %>%
      kableExtra::column_spec(1 + f,
                              background = ifelse(removable, "#D93B3B", "white"))
    # tab_style(style = cell_fill(color = "#D93B3B"),
    #                           locations = cells_body(rows = removable))
  }

  # adapted from https://www.anthonyschmidt.co/post/2020-09-27-efa-tables-in-r/
  Vaccounted <- x[["Vaccounted"]]
  colnames(Vaccounted) <- fnames
  if (nfactors > 1) {
    Phi <- x[["Phi"]]
    rownames(Phi) <- fnames
    colnames(Phi) <- fnames
    f_table <- rbind(Vaccounted, Phi) %>%
      as.data.frame() %>%
      rownames_to_column("Property") %>%
      mutate(across(where(is.numeric), round, 3)) %>%
      kable(booktabs = TRUE,
            caption = "Eigenvalues, Variance Explained,
                  and Factor Correlations for Rotated Factor Solution") %>%
      kable_styling(font_size = 9)
      # gt(caption = "Eigenvalues, Variance Explained,
      #            and Factor Correlations for Rotated Factor Solution")
  } else if (nfactors == 1) {
    f_table <- rbind(Vaccounted) %>%
      as.data.frame() %>%
      rownames_to_column("Property") %>%
      mutate(across(where(is.numeric), round, 3)) %>%
      kable(booktabs = TRUE,
            caption = "Eigenvalues, Variance Explained,
                  and Factor Correlations for Rotated Factor Solution") %>%
      kable_styling(font_size = 9)
  }

  return(list("ind_table" = ind_table, "f_table" = f_table))

}
