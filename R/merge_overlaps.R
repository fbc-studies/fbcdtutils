#' Merge overlapping intervals
#' @param .by A character vector of variables to group intervals by.
#' @export
merge_overlaps.data.table <- function(.data, .start, .end, ...,
                                      .max_gap = 0, .by = key(.data)) {
    s <- rlang::enexpr(.start)
    e <- rlang::enexpr(.end)

    s_nm <- rlang::as_label(s)
    e_nm <- rlang::as_label(e)

    setkeyv(.data, c(.by, s_nm, e_nm))

    .data[, pe := shift(eval(e), fill = eval(s)[1L]) + .max_gap, keyby = .by]
    .data[, .seq := 1L + cumsum(eval(s) > cummax(as.numeric(pe))), keyby = .by]

    res <- .data[, .(
        .s = min(eval(s)),
        .e = max(eval(e))
    ), by = c(.by, ".seq")]

    setnames(res, c(".s", ".e"), c(s_nm, e_nm))
    setkeyv(res, .by)

    res
}
