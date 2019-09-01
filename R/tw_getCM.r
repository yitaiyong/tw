

cm_url <- function(symbol, fs = "bs", yq = "sqr") {
  dplyr::case_when(
    fs == "bs" & yq == "syr" ~ stringr::str_c("https://www.cmoney.tw/finance/f00040.aspx?s=", symbol, "&o=3"),
    fs == "bs" & yq == "sqr" ~ stringr::str_c("https://www.cmoney.tw/finance/f00040.aspx?s=", symbol, "&o=4"),
    fs == "is" & yq == "syr" ~ stringr::str_c("https://www.cmoney.tw/finance/f00041.aspx?s=", symbol, "&o=4"),
    fs == "is" & yq == "ytq" ~ stringr::str_c("https://www.cmoney.tw/finance/f00041.aspx?s=", symbol, "&o=6"),
    fs == "is" & yq == "sqr" ~ stringr::str_c("https://www.cmoney.tw/finance/f00041.aspx?s=", symbol, "&o=5"),
    fs == "cf" & yq == "syr" ~ stringr::str_c("https://www.cmoney.tw/finance/f00042.aspx?s=", symbol, "&o=4"),
    fs == "cf" & yq == "ytq" ~ stringr::str_c("https://www.cmoney.tw/finance/f00042.aspx?s=", symbol, "&o=6"),
    fs == "cf" & yq == "sqr" ~ stringr::str_c("https://www.cmoney.tw/finance/f00042.aspx?s=", symbol, "&o=5"),
    fs == "fr" & yq == "syr" ~ stringr::str_c("https://www.cmoney.tw/finance/f00043.aspx?s=", symbol, "&o=3"),
    fs == "fr" & yq == "sqr" ~ stringr::str_c("https://www.cmoney.tw/finance/f00043.aspx?s=", symbol, "&o=4"),
    TRUE ~ NA_character_)
}


getBSISCF <- function(x, isis = FALSE) {
  # Sys.setlocale(category = "LC_ALL", locale = "cht")
  # Sys.setlocale(category = "LC_ALL", locale = "chs") # fs = is
  # Sys.setlocale(category = "LC_ALL", locale = "usa")
  if (isis) Sys.setlocale(category = "LC_ALL", locale = "chs")
  df <- x %>%
    # stringr::str_conv(encoding = "utf-8") %>%
    xml2::read_html() %>%
    rvest::html_node(xpath = "//*[@id='MainContent']/ul/li/article/div[2]/div/table") %>%
    rvest::html_table()
  if (isis) Sys.setlocale(category = "LC_ALL", locale = "cht")
  names(df)[1] <- "acc_item"
  long <- df %>%
    tidyr::gather(key = "yq", value = "num", -acc_item) %>%
    dplyr::mutate(
      num = num %>%
        stringr::str_trim(side = "both") %>%
        dplyr::if_else(. == "--", NA_character_, .) %>%
        stringr::str_remove_all(pattern = ",") %>%
        as.numeric())
  wide <- long %>%
    tidyr::spread(key = yq, value = num)
  # list(long, wide)
  wide
}

getBS <- purrr::partial(getBSISCF, isis = FALSE)
getIS <- purrr::partial(getBSISCF, isis = TRUE)
getCF <- purrr::partial(getBSISCF, isis = FALSE)

getFR <- function(x) {
  tmp <- x %>%
    # stringr::str_conv(encoding = "utf-8") %>%
    xml2::read_html()
  df_1 <- tmp %>%
    rvest::html_node(xpath = "//*[@id='MainContent']/ul/li[2]/article/div/div/div/table") %>%
    rvest::html_table() # %>% dplyr::mutate(category = "獲利能力")
  Sys.setlocale(category = "LC_ALL", locale = "chs")
  df_2 <- tmp %>%
    rvest::html_node(xpath = "//*[@id='MainContent']/ul/li[3]/article/div/div/div/table") %>%
    rvest::html_table() # %>% dplyr::mutate(category = "經營績效")
  df_3 <- tmp %>%
    rvest::html_node(xpath = "//*[@id='MainContent']/ul/li[4]/article/div/div/div/table") %>%
    rvest::html_table() # %>% dplyr::mutate(category = "經營能力")
  Sys.setlocale(category = "LC_ALL", locale = "cht")
  df_4 <- tmp %>%
    rvest::html_node(xpath = "//*[@id='MainContent']/ul/li[5]/article/div/div/div/table") %>%
    rvest::html_table() # %>% dplyr::mutate(category = "財務結構")
  df_5 <- tmp %>%
    rvest::html_node(xpath = "//*[@id='MainContent']/ul/li[6]/article/div/div/div/table") %>%
    rvest::html_table() # %>% dplyr::mutate(category = "償債能力")
  df <- dplyr::bind_rows(df_1, df_2, df_3, df_4, df_5)
  names(df)[1] <- "acc_item"
  arr_item <- df$acc_item
  long <- df %>%
    tidyr::gather(key = "yq", value = "num", -acc_item) %>%
    dplyr::mutate(
      num = num %>%
        stringr::str_trim(side = "both") %>%
        dplyr::if_else(. == "--", NA_character_, .) %>%
        stringr::str_remove_all(pattern = ",") %>%
        as.numeric())
  wide <- long %>%
    tidyr::spread(key = yq, value = num) %>%
    dplyr::mutate(acc_item = factor(acc_item, levels = arr_item)) %>%
    dplyr::arrange(acc_item) %>%
    dplyr::mutate(acc_item = as.character(acc_item))
  # list(long, wide)
  wide
}


pjs <- webdriver::run_phantomjs()
sess <- webdriver::Session$new(port = pjs$port)


getFS <- function(symbol) {

  tibb <-
    tibble::tibble(
      fs = c("bs", "bs", "is", "is", "is", "cf", "cf", "cf", "fr", "fr"),
      yq = c("syr", "sqr", "syr", "ytq", "sqr", "syr", "ytq", "sqr", "syr", "sqr")) %>%
    dplyr::mutate(
      url = purrr::pmap_chr(
        .l = list(symbol, fs, yq),
        .f = ~ cm_url(symbol = ..1, fs = ..2, yq = ..3)))

  flist <- list(getBS, getBS, getIS, getIS, getIS, getCF, getCF, getCF, getFR, getFR)

  # pjs <- webdriver::run_phantomjs()
  # sess <- webdriver::Session$new(port = pjs$port)

  dlist <- list()

  for (i in seq_along(flist)) {

    sess$go(tibb$url[[i]])
    tmp <- sess$getSource()

    dlist[[i]] <- flist[[i]](tmp) %>%
      dplyr::mutate(fs = tibb$fs[i], yq = tibb$yq[i]) %>%
      tidyr::gather(key = "yrq", value = "num", -c(acc_item, fs, yq))
  }

  out <- dplyr::bind_rows(dlist) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(symbol = as.character(symbol)) %>%
    dplyr::select(symbol, fs, yq, yrq, acc_item, num)

  out
}

getRvn <- function(symbol) {
  url <- stringr::str_c("https://www.cmoney.tw/finance/f00029.aspx?s=", symbol)
  # pjs <- webdriver::run_phantomjs()
  # sess <- webdriver::Session$new(port = pjs$port)
  sess$go(url)
  tmp <- sess$getSource() %>%
    xml2::read_html()
  out <- tmp %>%
    rvest::html_node(xpath = "//*[@id='MainContent']/ul/li[4]/article/div/div/div/table") %>%
    rvest::html_table() %>%
    dplyr::select(1:6) %>%
    purrr::set_names(c("yrm", "rvn", "mom", "yoy", "ytdrvn", "ytdyoy")) %>%
    dplyr::mutate(
      symbol = as.character(symbol),
      yrm = as.character(yrm),
      rvn = rvn %>% stringr::str_remove_all(",") %>% as.numeric(),
      ytdrvn = ytdrvn %>% stringr::str_remove_all(",") %>% as.numeric()) %>%
    dplyr::select(symbol, dplyr::everything())
  out
}



getCMoney <- function(symbolvec) {
  # pjs <- webdriver::run_phantomjs()
  # sess <- webdriver::Session$new(port = pjs$port)
  out = list()
  for (i in seq_along(symbolvec)) {
    out[[i]] <-
      list(FS = getFS(symbol = symbolvec[i]),
           Rvn = getRvn(symbol = symbolvec[i]))
    print(stringr::str_c("symbol ", symbolvec[i], " is done."))
  }
  out
}

#' Taiwan stock info from CMoney.
#'
#' @param symbolvec a numeric symbol vector.
#' @return a list contains financial statements, ratios and monthly revenues.
#' # @export
#' @examples
#' tw_getCMoney(2330)
#' tw_getCMoney(c(2330, 2886, 3008))
tw_getCMoney <- function(symbolvec) {
  getCMoney(symbolvec = symbolvec)
}



