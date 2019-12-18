

getTaiwanStockInfo <- function() {

  resp <-
    httr::POST(
      url = "http://finmindapi.servebeer.com/api/data",
      body = list("dataset" = "TaiwanStockInfo"),
      encode = "json")

  out <- resp %>%
    httr::content() %>%
    magrittr::extract2("data") %>%
    tibble::as_tibble() %>%
    dplyr::mutate_all(unlist)

  out
}



getTaiwanStockPrice <- function(symbolvec, datefrom) {

  resp <-
    httr::POST(
      url = "http://finmindapi.servebeer.com/api/data",
      body = list(
        "dataset" = "TaiwanStockPrice",
        "stock_id" = as.list(as.character(symbolvec)),
        "date" = datefrom),
      encode = "json")

  out <- resp %>%
    httr::content() %>%
    magrittr::extract2("data") %>%
    tibble::as_tibble() %>%
    dplyr::mutate_all(unlist)

  out
}


getFinancialStatements <- function(symbolvec, datefrom) {

  resp <-
    httr::POST(
      url = "http://finmindapi.servebeer.com/api/data",
      body = list(
        "dataset" = "FinancialStatements",
        "stock_id" = as.list(as.character(symbolvec)),
        "date" = datefrom),
      encode = "json")

  out <- resp %>%
    httr::content() %>%
    magrittr::extract2("data") %>%
    tibble::as_tibble() %>%
    dplyr::mutate_all(unlist)

  out
}



getTaiwanStockStockDividend <- function(symbolvec, datefrom) {

  resp <-
    httr::POST(
      url = "http://finmindapi.servebeer.com/api/data",
      body = list(
        "dataset" = "TaiwanStockStockDividend",
        "stock_id" = as.list(as.character(symbolvec)),
        "date" = datefrom),
      encode = "json")

  tmp <- resp %>%
    httr::content() %>%
    magrittr::extract2("data") %>%
    tibble::as_tibble()

  for (i in seq_len(nrow(tmp))) {
    if (is.null(tmp$Ex_dividend_transaction_day[[i]])) {
      tmp$Ex_dividend_transaction_day[[i]] <- NA_character_
    }
    if (tmp$Ex_right_trading_day[[i]] == "None") {
      tmp$Ex_right_trading_day[[i]] <- NA_character_
    }
  }

  out <- tmp %>%
    dplyr::mutate_all(unlist)

  out
}


getTaiwanStockMarginPurchaseShortSale <- function(symbolvec, datefrom) {

  resp <-
    httr::POST(
      url = "http://finmindapi.servebeer.com/api/data",
      body = list(
        "dataset" = "TaiwanStockMarginPurchaseShortSale",
        "stock_id" = as.list(as.character(symbolvec)),
        "date" = datefrom),
      encode = "json")

  tmp <- resp %>%
    httr::content() %>%
    magrittr::extract2("data") %>%
    tibble::as_tibble()

  for (i in seq_along(tmp$Note)) {
    if (is.null(tmp$Note[[i]])) {
      tmp$Note[[i]] <- NA_character_
    }
  }

  out <- tmp %>%
    dplyr::mutate_all(unlist)

  out
}


getInstitutionalInvestorsBuySell <- function(symbolvec, datefrom) {

  resp <-
    httr::POST(
      url = "http://finmindapi.servebeer.com/api/data",
      body = list(
        "dataset" = "InstitutionalInvestsBuySell",
        "stock_id" = as.list(as.character(symbolvec)),
        "date" = datefrom),
      encode = "json")

  out <- resp %>%
    httr::content() %>%
    magrittr::extract2("data") %>%
    tibble::as_tibble() %>%
    dplyr::mutate_all(unlist)

  out
}


getShareholding <- function(symbolvec, datefrom) {

  resp <-
    httr::POST(
      url = "http://finmindapi.servebeer.com/api/data",
      body = list(
        "dataset" = "Shareholding",
        "stock_id" = as.list(as.character(symbolvec)),
        "date" = datefrom),
      encode = "json")

  out <- resp %>%
    httr::content() %>%
    magrittr::extract2("data") %>%
    tibble::as_tibble() %>%
    dplyr::mutate_all(unlist)

  out
}




getBalanceSheet <- function(symbolvec, datefrom) {

  resp <-
    httr::POST(
      url = "http://finmindapi.servebeer.com/api/data",
      body = list(
        "dataset" = "BalanceSheet",
        "stock_id" = as.list(as.character(symbolvec)),
        "date" = datefrom),
      encode = "json")

  out <- resp %>%
    httr::content() %>%
    magrittr::extract2("data") %>%
    tibble::as_tibble() %>%
    dplyr::mutate_all(unlist)

  out
}





getTaiwanStockHoldingSharesPer <- function(symbolvec, datefrom) {

  resp <-
    httr::POST(
      url = "http://finmindapi.servebeer.com/api/data",
      body = list(
        "dataset" = "TaiwanStockHoldingSharesPer",
        "stock_id" = as.list(as.character(symbolvec)),
        "date" = datefrom),
      encode = "json")

  out <- resp %>%
    httr::content() %>%
    magrittr::extract2("data") %>%
    tibble::as_tibble() %>%
    dplyr::mutate_all(unlist)

  out
}

getTaiwanStockMonthRevenue <- function(symbolvec, datefrom) {

  resp <-
    httr::POST(
      url = "http://finmindapi.servebeer.com/api/data",
      body = list(
        "dataset" = "TaiwanStockMonthRevenue",
        "stock_id" = as.list(as.character(symbolvec)),
        "date" = datefrom),
      encode = "json")

  out <- resp %>%
    httr::content() %>%
    magrittr::extract2("data") %>%
    tibble::as_tibble() %>%
    dplyr::mutate_all(unlist)

  out
}



#' FM Taiwan Stock Info.
#'
#' @return a tibble contains following variables:
#'   $ industry_category: chr,
#'   $ stock_id: chr,
#'   $ stock_name: chr,
#'   $ type: int
#' @export
#' @examples
#' tw_FM_TaiwanStockInfo()
tw_FM_StockInfo <- function() {
  getTaiwanStockInfo()
}



#' FM Taiwan Stock Price.
#'
#' @param symbolvec stock symbol vector, in chr (preferred) or int type.
#' @param datefrom start date formatted "YYYY-MM-DD".
#' @return a tibble contains following variables:
#'   $ Trading_Volume: int,
#'   $ Trading_money: num,
#'   $ Trading_turnover: int,
#'   $ close: num,
#'   $ date: chr,
#'   $ max: num,
#'   $ min: num,
#'   $ open: num,
#'   $ spread: num,
#'   $ stock_id: chr
#' @export
#' @examples
#' tw_FM_StockPrice(c("2330", "2008"), "2019-01-01")
tw_FM_StockPrice <- function(symbolvec, datefrom) {
  getTaiwanStockPrice(symbolvec = symbolvec, datefrom = datefrom)
}



#' FM Taiwan Stock Income Statement.
#'
#' @param symbolvec stock symbol vector, in chr (preferred) or int type.
#' @param datefrom start date formatted "YYYY-MM-DD".
#' @return a tibble contains following variables:
#'   $ date    : chr,
#'   $ stock_id: chr,
#'   $ type    : chr,
#'   $ value   : num
#' @export
#' @examples
#' tw_FM_IncomeStatement(c("2330", "2008"), "2019-01-01")
tw_FM_IncomeStatement <- function(symbolvec, datefrom) {
  getFinancialStatements(symbolvec = symbolvec, datefrom = datefrom)
}


#' FM Taiwan Stock Split and Dividend.
#'
#' @param symbolvec stock symbol vector, in chr (preferred) or int type.
#' @param datefrom start date formatted "YYYY-MM-DD".
#' @return a tibble contains following variables:
#'   $ Capital_Reserve: num,
#'   $ Cash_dividend: num,
#'   $ Directors_remuneration: num,
#'   $ Ex_dividend_transaction_day: chr,
#'   $ Retained_Earnings: num,
#'   $ date: chr,
#'   $ stock_id: chr,
#'   $ total_employee_bonus_shares: num,
#'   $ total_employee_bonus_stock_shares: num
#' @export
#' @examples
#' tw_FM_StockSplitDividend(c("2330", "2008"), "2019-01-01")
tw_FM_StockSplitDividend <- function(symbolvec, datefrom) {
  getTaiwanStockStockDividend(symbolvec = symbolvec, datefrom = datefrom)
}



#' FM Taiwan Stock Margin Trading and Short Selling.
#'
#' @param symbolvec stock symbol vector, in chr (preferred) or int type.
#' @param datefrom start date formatted "YYYY-MM-DD".
#' @return a tibble contains following variables:
#'   $ MarginPurchaseBuy: int,
#'   $ MarginPurchaseCashRepayment: int,
#'   $ MarginPurchaseLimit: int,
#'   $ MarginPurchaseSell: int,
#'   $ MarginPurchaseTodayBalance: int,
#'   $ MarginPurchaseYesterdayBalance: int,
#'   $ Note: chr,
#'   $ OffsetLoanAndShort: int,
#'   $ ShortSaleBuy: int,
#'   $ ShortSaleCashRepayment: int,
#'   $ ShortSaleLimit: int,
#'   $ ShortSaleSell: int,
#'   $ ShortSaleTodayBalance: int,
#'   $ ShortSaleYesterdayBalance: int,
#'   $ date: chr,
#'   $ stock_id: chr,
#'   $ stock_name: chr
#' @export
#' @examples
#' tw_FM_StockMarginShort(c("2330", "2008"), "2019-01-01")
tw_FM_StockMarginShort <- function(symbolvec, datefrom) {
  getTaiwanStockMarginPurchaseShortSale(symbolvec = symbolvec, datefrom = datefrom)
}



# tw_FM_InstitutionalInvestor <-  function(symbolvec, datefrom) {
#   getInstitutionalInvestorsBuySell(symbolvec = symbolvec, datefrom = datefrom)
# }



#' FM Taiwan Stock Foreign Investment.
#'
#' @param symbolvec stock symbol vector, in chr (preferred) or int type.
#' @param datefrom start date formatted "YYYY-MM-DD".
#' @return a tibble contains following variables:
#'   $ ChineseInvestmentUpperLimitRatio: num,
#'   $ ForeignInvestmentRemainingShares: num,
#'   $ ForeignInvestmentShares: num,
#'   $ ForeignInvestmentUpperLimitRatio: num,
#'   $ InternationalCode: chr,
#'   $ NumberOfSharesIssued: num,
#'   $ RecentlyDeclareDate: chr,
#'   $ date: chr,
#'   $ stock_id: chr,
#'   $ stock_name: chr
#' @export
#' @examples
#' tw_FM_ForeignInvestment(c("2330", "2008"), "2019-01-01")
tw_FM_ForeignInvestment <-  function(symbolvec, datefrom) {
  getShareholding(symbolvec = symbolvec, datefrom = datefrom)
}


#' FM Taiwan Stock Balance Sheet.
#'
#' @param symbolvec stock symbol vector, in chr (preferred) or int type.
#' @param datefrom start date formatted "YYYY-MM-DD".
#' @return a tibble contains following variables:
#'   $ date: chr,
#'   $ stock_id: chr,
#'   $ type: chr,
#'   $ value: num
#' @export
#' @examples
#' tw_FM_BalanceSheet(c("2330", "2008"), "2019-01-01")
tw_FM_BalanceSheet <-  function(symbolvec, datefrom) {
  getBalanceSheet(symbolvec = symbolvec, datefrom = datefrom)
}


#' FM Taiwan Stock Dispersion.
#'
#' @param symbolvec stock symbol vector, in chr (preferred) or int type.
#' @param datefrom start date formatted "YYYY-MM-DD".
#' @return a tibble contains following variables:
#'   $ HoldingSharesLevel: chr,
#'   $ date: chr,
#'   $ people: int,
#'   $ percent: num,
#'   $ stock_id: chr,
#'   $ unit: num
#' @export
#' @examples
#' tw_FM_StockDispersion(c("2330", "2008"), "2019-01-01")
tw_FM_StockDispersion <- function(symbolvec, datefrom) {
  getTaiwanStockHoldingSharesPer(symbolvec = symbolvec, datefrom = datefrom)
}




#' FM Taiwan Stock Monthly Revenue.
#'
#' @param symbolvec stock symbol vector, in chr (preferred) or int type.
#' @param datefrom start date formatted "YYYY-MM-DD".
#' @return a tibble contains following variables:
#'   $ country: chr,
#'   $ date: chr,
#'   $ revenue: num,
#'   $ revenue_month: int,
#'   $ revenue_year: int,
#'   $ stock_id: chr
#' @export
#' @examples
#' tw_FM_MonthlyRevenue(c("2330", "2008"), "2019-01-01")
tw_FM_MonthlyRevenue <- function(symbolvec, datefrom) {
  getTaiwanStockMonthRevenue(symbolvec = symbolvec, datefrom = datefrom)
}


























#' setup a sqlite db and populate with balance sheet, income statement, monthly revenue, stock price, split and dividend of listed stocks in twse and tpex market.
#'
#' @param dbname a path to locate the sqlite db.
#' @return a list of objects, tables, and fields of
#'   balance sheet, income statement, monthly revenue, stock price, split and dividend tables.
#' @export
#' @examples
#' tw_fmdb()

tw_fmdb <- function(dbname = "./../data/db/fm") {

  if (fs::file_exists("./../data/db/sec")) {

    sec_con <- DBI::dbConnect(drv = RSQLite::SQLite(), dbname = "./../data/db/sec")
    secdatedmax <- DBI::dbReadTable(sec_con, "seclist") %>%
      dplyr::pull(dated) %>% max()
    DBI::dbDisconnect(sec_con)
    rm(sec_con)

    if (lubridate::today() > secdatedmax) tw_secdb()

    sec_con <- DBI::dbConnect(drv = RSQLite::SQLite(), dbname = "./../data/db/sec")

    # sec_symbol in 上市(twse) and 上櫃(tpex) market
    twtp <-
      DBI::dbReadTable(sec_con, "seclist") %>%
      dplyr::filter(
        dated == lubridate::today(),
        no %in% c(2, 4, 5),
        market %in% c("上市", "上櫃"),
        sec_type == "股票") %>%
      dplyr::pull(sec_symbol)

    DBI::dbDisconnect(sec_con)
    rm(sec_con)
  }

  datefrom = "1999-12-31"

  fm_con <- DBI::dbConnect(drv = RSQLite::SQLite(), dbname = dbname)

  for (i in seq_along(twtp)) {

    balance <- tw_FM_BalanceSheet(twtp[i], datefrom) %>%
      dplyr::mutate(update = lubridate::now())
    DBI::dbWriteTable(fm_con, "balance", balance, append = TRUE, temporary = FALSE)
    print(stringr::str_c("symbol: ", twtp[i], "'s balance sheet fetched."))

    income <- tw_FM_IncomeStatement(twtp[i], datefrom) %>%
      dplyr::mutate(update = lubridate::now())
    DBI::dbWriteTable(fm_con, "income", income, append = TRUE, temporary = FALSE)
    print(stringr::str_c("symbol: ", twtp[i], "'s income statement fetched."))

    revenue <- tw_FM_MonthlyRevenue(twtp[i], datefrom) %>%
      dplyr::mutate(update = lubridate::now())
    DBI::dbWriteTable(fm_con, "revenue", revenue, append = TRUE, temporary = FALSE)
    print(stringr::str_c("symbol: ", twtp[i], "'s monthly revenue fetched."))

    price <- tw_FM_StockPrice(twtp[i], datefrom) %>%
      dplyr::mutate(update = lubridate::now())
    DBI::dbWriteTable(fm_con, "price", price, append = TRUE, temporary = FALSE)
    print(stringr::str_c("symbol: ", twtp[i], "'s stock price fetched."))

    dividend <- tw_FM_StockSplitDividend(twtp[i], datefrom) %>%
      dplyr::mutate(update = lubridate::now())
    DBI::dbWriteTable(fm_con, "dividend", dividend, append = TRUE, temporary = FALSE)
    print(stringr::str_c("symbol: ", twtp[i], "'s split dividend fetched."))

  }

  out <- list(
    DBI::dbGetInfo(fm_con),
    DBI::dbListObjects(fm_con),
    DBI::dbListTables(fm_con),
    DBI::dbListFields(fm_con, "balance"),
    DBI::dbListFields(fm_con, "income"),
    DBI::dbListFields(fm_con, "revenue"),
    DBI::dbListFields(fm_con, "price"),
    DBI::dbListFields(fm_con, "dividend")
  )

  out

  DBI::dbDisconnect(fm_con)

}





# lubridate::now() %>% as.numeric() %>% as.POSIXct(origin = "1970-01-01")






# tw_secdb <- function(dbname = "./../data/db/sec") {
#
#   dbexist <- fs::file_exists(path = dbname)
#
#   sec_con <- DBI::dbConnect(drv = RSQLite::SQLite(), dbname = dbname)
#
#   new <- getAllSec() %>% dplyr::mutate(update = lubridate::now() %>% as.numeric())
#
#   if (dbexist) {
#     old <- DBI::dbReadTable(sec_con, "seclist")
#     upd <- dplyr::bind_rows(new, old) %>%
#       arrange(isin_code, desc(update)) %>%
#       mutate(isin_dup = duplicated(isin_code)) %>%
#       filter(isin_dup == FALSE) %>%
#       select(-isin_dup)
#   } else {
#     upd <- new
#   }
#
#   DBI::dbWriteTable(sec_con, "seclist", upd, append = FALSE, temporary = FALSE)
#
#   fs::file_copy(
#     path = dbname,
#     new_path = stringr::str_c(dbname, "_",
#                               lubridate::now() %>% as.character() %>%
#                                 str_replace(pattern = " ", replacement = "_") %>%
#                                 str_replace_all(":", "-")))
#
#   out <- list(
#     # DBI::dbGetInfo(sec_con), # this function is deprecated
#     DBI::dbListObjects(sec_con),
#     DBI::dbListTables(sec_con),
#     DBI::dbListFields(sec_con, "seclist")
#   )
#
#   DBI::dbDisconnect(sec_con)
#
#   out
# }




