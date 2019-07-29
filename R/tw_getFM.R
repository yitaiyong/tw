

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

  out <- resp %>%
    httr::content() %>%
    magrittr::extract2("data") %>%
    tibble::as_tibble() %>%
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


