
#' setup a sqlite db and populate with balance sheet, income statement, monthly revenue, stock price, split and dividend of listed stocks in twse and tpex market.
#'
#' @param dbname a path to locate the sqlite db.
#' @param datefrom start date formatted "YYYY-MM-DD".
#' @return a list of objects, tables, and fields of
#'   balance sheet, income statement, monthly revenue, stock price, split and dividend tables.
#' @export
#' @examples
#' tw_dbinit()

tw_dbinit <- function(dbname = "./../data/db/fm", datefrom = "1999-12-31") {

  # sec_symbol in 上市(twse) and 上櫃(tpex) market
  twtp <- tw_getStocks() %>%
    dplyr::filter(market != "興櫃") %>%
    dplyr::pull(sec_symbol)

  con <- DBI::dbConnect(drv = RSQLite::SQLite(), dbname = dbname)

  for (i in seq_along(twtp)) {

    balance <- tw_FM_BalanceSheet(twtp[i], datefrom) %>%
      dplyr::mutate(update = lubridate::now())
    DBI::dbWriteTable(con, "balance", balance, append = TRUE, temporary = FALSE)
    print(stringr::str_c("symbol: ", twtp[i], "'s balance sheet fetched."))

    income <- tw_FM_IncomeStatement(twtp[i], datefrom) %>%
      dplyr::mutate(update = lubridate::now())
    DBI::dbWriteTable(con, "income", income, append = TRUE, temporary = FALSE)
    print(stringr::str_c("symbol: ", twtp[i], "'s income statement fetched."))

    revenue <- tw_FM_MonthlyRevenue(twtp[i], datefrom) %>%
      dplyr::mutate(update = lubridate::now())
    DBI::dbWriteTable(con, "revenue", revenue, append = TRUE, temporary = FALSE)
    print(stringr::str_c("symbol: ", twtp[i], "'s monthly revenue fetched."))

    price <- tw_FM_StockPrice(twtp[i], datefrom) %>%
      dplyr::mutate(update = lubridate::now())
    DBI::dbWriteTable(con, "price", price, append = TRUE, temporary = FALSE)
    print(stringr::str_c("symbol: ", twtp[i], "'s stock price fetched."))

    dividend <- tw_FM_StockSplitDividend(twtp[i], datefrom) %>%
      dplyr::mutate(update = lubridate::now())
    DBI::dbWriteTable(con, "dividend", dividend, append = TRUE, temporary = FALSE)
    print(stringr::str_c("symbol: ", twtp[i], "'s split dividend fetched."))

  }

  out <- list(
    DBI::dbGetInfo(con),
    DBI::dbListObjects(con),
    DBI::dbListTables(con),
    DBI::dbListFields(con, "balance"),
    DBI::dbListFields(con, "income"),
    DBI::dbListFields(con, "revenue"),
    DBI::dbListFields(con, "price"),
    DBI::dbListFields(con, "dividend"))

  out

  DBI::dbDisconnect(con)

}





# lubridate::now() %>% as.numeric() %>% as.POSIXct(origin = "1970-01-01")



