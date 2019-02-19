#' @export
merge_overlaps.data.table <- function(.data, .start, .end, .max_gap = 0) {
    grps <- dplyr::group_vars(tbl)

    s <- rlang::enexpr(start)
    e <- rlang::enexpr(end)

    s_nm <- rlang::as_label(s)
    e_nm <- rlang::as_label(e)

    # make a copy to not pollute tbl in the calling env
    tbl <- as.data.table(tbl)
    setkeyv(tbl, c(grps, s_nm, e_nm))

    tbl[, pe := shift(eval(e), fill = eval(s)[1L]) + max_gap, keyby = grps]
    tbl[, g := 1L + cumsum(eval(s) > cummax(as.numeric(pe))), keyby = grps]
    res <- tbl[, .(s = min(eval(s)), e = max(eval(e))), by = c(grps, "g")]

    setnames(res, c("g", "s", "e"), c("seq", s_nm, e_nm))
    dplyr::group_by_at(setDF(res), grps)
}
