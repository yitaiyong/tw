

`%>%` <- magrittr::`%>%`
`%!in%` <- Negate(`%in%`)


getWebPage <- function(url, encoding = "UTF-8") {
  con <- curl::curl(url)
  out <- con %>%
    readLines() %>%
    stringr::str_conv(encoding = encoding) %>%
    magrittr::extract(8)
  close(con)
  out
}


getTagLoc <- function(text, tag_head, tag_tail = NA) {

  head_df <- text %>%
    stringr::str_locate_all(pattern = tag_head) %>%
    .[[1]] %>% tibble::as_tibble() %>%
    dplyr::mutate(tag = "head",
                  cnt = dplyr::row_number())

  if (is.na(tag_tail)) return(head_df)
  else {
    tail_df <- text %>%
      stringr::str_locate_all(pattern = tag_tail) %>%
      .[[1]] %>% tibble::as_tibble() %>%
      dplyr::mutate(tag = "tail",
                    cnt = dplyr::row_number())
  }

  dplyr::bind_rows(head_df, tail_df) %>%
    dplyr::arrange(start)
}


getTagRange <- function(text, tag_head, tag_tail = NA, cnt_head = 1, cnt_tail = 1) {

  tagloc <- getTagLoc(text = text, tag_head = tag_head, tag_tail = tag_tail)

  txt_start <- tagloc %>%
    dplyr::filter(tag == "head", cnt == cnt_head) %>%
    dplyr::pull(start)

  txt_end <- dplyr::if_else(any(stringr::str_detect(tagloc$tag, "tail")),
                            tagloc %>% dplyr::filter(tag == "tail", cnt == cnt_tail) %>% dplyr::pull(end),
                            stringr::str_length(text))

  c(txt_start, txt_end)
}


getTableTitle <- function(x) {

  tagrange <- getTagRange(text = x, tag_head = "<font", tag_tail = "</font>", cnt_head = 1, cnt_tail = 1)

  out <- x %>%
    stringr::str_sub(tagrange[1], tagrange[2]) %>%
    stringr::str_remove_all(pattern = "<.*?>") %>%
    stringr::str_trim()

  out
}


getDataDate <- function(x) {

  tagrange <- getTagRange(text = x, tag_head = "<center>", tag_tail = "</center>", cnt_head = 1, cnt_tail = 1)

  out <- x %>%
    stringr::str_sub(tagrange[1], tagrange[2]) %>%
    stringr::str_remove_all(pattern = "<.*?>|最近更新日期:") %>%
    stringr::str_trim()

  out
}


getTableText <- function(x) {

  tagrange <- x %>%
    getTagLoc("<tr", "</tr>") %>%
    {c(dplyr::first(.$start), dplyr::last(.$end))}

  out <- x %>%
    stringr::str_sub(tagrange[1], tagrange[2])

  out
}

tidy01 <- function(df) {
  df %>%
    dplyr::slice(-1) %>%
    tidyr::separate(col = var1, into = c("var1_1", "var1_2"), sep = "　") %>%
    dplyr::mutate(var3 = lubridate::ymd(var3)) %>%
    dplyr::select(sort(names(.)))
}


tidy02 <- function(df) {
  df %>%
    dplyr::slice(-1) %>%
    dplyr::mutate(var0 = ifelse(var2 == "", var1, NA_character_)) %>%
    tidyr::fill(var0) %>%
    dplyr::filter(var0 != var1) %>%
    tidyr::separate(col = var1, into = c("var1_1", "var1_2"), sep = "　") %>%
    dplyr::mutate(var3 = lubridate::ymd(var3)) %>%
    dplyr::select(sort(names(.)))
}

tidy03 <- function(df) {
  df %>%
    dplyr::slice(-1) %>%
    dplyr::mutate(var0 = dplyr::if_else(var2 == "", var1, NA_character_)) %>%
    tidyr::fill(var0) %>%
    dplyr::filter(var0 != var1) %>%
    tidyr::separate(col = var1, into = c("var1_1", "var1_2"), sep = "　") %>%
    dplyr::mutate(var3 = lubridate::ymd(var3),
                  var4 = dplyr::if_else(var4 == "無到期日", "9999/12/31", var4) %>%
                    lubridate::ymd()) %>%
    dplyr::select(sort(names(.)))
}

tidy04 <- function(df) {
  df %>%
    dplyr::slice(-1) %>%
    dplyr::mutate(var0 = dplyr::if_else(var2 == "", var1, NA_character_)) %>%
    tidyr::fill(var0) %>%
    dplyr::filter(var0 != var1) %>%
    tidyr::separate(col = var1, into = c("var1_1", "var1_2"), sep = "　") %>%
    dplyr::mutate(var3 = lubridate::ymd(var3)) %>%
    dplyr::select(sort(names(.)))
}

tidy05 <- function(df) {
  df %>%
    dplyr::slice(-1) %>%
    dplyr::mutate(var0 = dplyr::if_else(var2 == "", var1, NA_character_)) %>%
    tidyr::fill(var0) %>%
    dplyr::filter(var0 != var1) %>%
    tidyr::separate(col = var1, into = c("var1_1", "var1_2"), sep = "　") %>%
    dplyr::mutate(var3 = lubridate::ymd(var3)) %>%
    dplyr::select(sort(names(.)))
}

tidy06 <- function(df) {
  df %>%
    dplyr::slice(-1) %>%
    dplyr::mutate(var0 = dplyr::if_else(var2 == "", var1, NA_character_)) %>%
    tidyr::fill(var0) %>%
    dplyr::filter(var0 != var1) %>%
    tidyr::separate(col = var1, into = c("var1_1", "var1_2"), sep = "　") %>%
    dplyr::mutate(var3 = lubridate::ymd(var3)) %>%
    dplyr::select(sort(names(.)))
}

tidy07 <- function(df) {
  df %>%
    dplyr::slice(-1) %>%
    tidyr::separate(col = var1, into = c("var1_1", "var1_2"), sep = "　") %>%
    dplyr::mutate(var3 = lubridate::ymd(var3)) %>%
    dplyr::select(sort(names(.)))
}

tidy08 <- function(df) {
  df %>%
    dplyr::slice(-1) %>%
    dplyr::mutate(var0 = dplyr::if_else(var2 == "", var1, NA_character_)) %>%
    tidyr::fill(var0) %>%
    dplyr::filter(var0 != var1) %>%
    tidyr::separate(col = var1, into = c("var1_1", "var1_2"), sep = "　") %>%
    dplyr::mutate(var3 = lubridate::ymd(var3)) %>%
    dplyr::select(sort(names(.)))
}

tidy09 <- function(df) {
  df %>%
    dplyr::slice(-1) %>%
    tidyr::separate(col = var1, into = c("var1_1", "var1_2"), sep = "　") %>%
    dplyr::mutate(var3 = lubridate::ymd(var3)) %>%
    dplyr::select(sort(names(.)))
}

tidy10 <- function(df) {
  df %>%
    dplyr::slice(-1) %>%
    dplyr::mutate(var3 = lubridate::ymd(var3)) %>%
    dplyr::select(sort(names(.)))
}

tidy11 <- function(df) {
  df %>%
    dplyr::slice(-1) %>%
    tidyr::separate(col = var1, into = c("var1_1", "var1_2"), sep = "　") %>%
    dplyr::mutate(var3 = lubridate::ymd(var3)) %>%
    dplyr::select(sort(names(.)))
}






########################################
# process data within a function
# ======================================



getAllSec <- function() {

  d <- list()

  d$pageno <- 1:11

  d$url <- stringr::str_c("https://isin.twse.com.tw/isin/C_public.jsp?strMode=", d$pageno)

  d$htmtxt <- purrr::map_chr(d$url, ~ getWebPage(.x, "ms950"))

  d$title <- purrr::map_chr(d$htmtxt, ~ getTableTitle(.x))

  d$dated <- purrr::map_chr(d$htmtxt, ~ getDataDate(.x)) %>% lubridate::ymd()

  d$tbltxt <- purrr::map_chr(d$htmtxt, ~ getTableText(.x))

  d$rows <- purrr::map(
    .x = d$tbltxt,
    .f = ~ stringr::str_split(string = .x, pattern = "</tr><tr>") %>%
      magrittr::extract2(1))

  d$coln <- purrr::map_int(
    .x = d$tbltxt,
    .f = ~ .x %>%
      stringr::str_split_fixed(pattern = "</tr><tr>", n = 2) %>%
      magrittr::extract2(1) %>%
      stringr::str_count(pattern = "</td>"))

  d$colv <- purrr::map(d$coln, ~ stringr::str_c("var", 1:(.x)))

  d$data <- purrr::pmap(
    .l = list(d$rows, d$coln, d$colv),
    .f = ~ stringr::str_split_fixed(string = ..1, pattern = "</td>", n = ..2) %>%
      `colnames<-`(..3) %>%
      tibble::as_tibble() %>%
      dplyr::mutate_all(.funs = ~ stringr::str_remove_all(., pattern = "<.*?>")) %>%
      dplyr::mutate_all(.funs = ~ stringr::str_trim(., side = "both")))

  # d$colcn <- purrr::map(
  #   .x = d$data,
  #   .f = ~ dplyr::slice(.x, 1) %>% unlist())

  d$colen <- list(
    names_01 = c("sec_symbol", "sec_name", "isin_code", "start_date", "sector", "cfi_code", "remark"),
    names_02 = c("sec_type", "sec_symbol", "sec_name", "isin_code", "start_date", "market", "sector", "cfi_code", "remark"),
    names_03 = c("sec_type", "sec_symbol", "sec_name", "isin_code", "start_date", "end_date", "intr_rate", "market", "sector", "cfi_code", "remark"),
    names_04 = c("sec_type", "sec_symbol", "sec_name", "isin_code", "start_date", "market", "sector", "cfi_code", "remark"),
    names_05 = c("sec_type", "sec_symbol", "sec_name", "isin_code", "start_date", "market", "sector", "cfi_code", "remark"),
    names_06 = c("sec_type", "sec_symbol", "sec_name", "isin_code", "start_date", "market", "sector", "cfi_code", "remark"),
    names_07 = c("sec_symbol", "sec_name", "isin_code", "start_date", "cfi_code", "remark"),
    names_08 = c("sec_type", "sec_symbol", "sec_name", "isin_code", "start_date", "sector", "cfi_code", "remark"),
    names_09 = c("sec_symbol", "sec_name", "isin_code", "start_date", "cfi_code", "remark"),
    names_10 = c("sec_name", "isin_code", "start_date", "cfi_code", "remark"),
    names_11 = c("sec_symbol", "sec_name", "isin_code", "start_date", "cfi_code", "remark"))

  d$df <- purrr::map2(
    .x = d$data,
    .y = c(tidy01, tidy02, tidy03, tidy04, tidy05, tidy06, tidy07, tidy08, tidy09, tidy10, tidy11),
    .f = ~ .y(.x))

  d$df <- purrr::map2(
    .x = d$df,
    .y = d$colen,
    .f = ~ purrr::set_names(.x, .y))

  d$df <- purrr::map2(
    .x = d$df,
    .y = d$pageno,
    .f = ~ dplyr::mutate(.x, no = .y))

  d$df <- purrr::map2(
    .x = d$df,
    .y = d$title,
    .f = ~ dplyr::mutate(.x, title = .y))

  d$df <- purrr::map2(
    .x = d$df,
    .y = d$dated,
    .f = ~ dplyr::mutate(.x, dated = .y))

  d$df %>%
    dplyr::bind_rows() %>%
    dplyr::select("no", "title", "dated", "sec_type", "sec_symbol", "sec_name", "isin_code", "start_date", "end_date", "intr_rate", "market", "sector", "cfi_code", "remark") %>%
    dplyr::mutate_if(is.character, list(~stringr::str_trim(., side = "both"))) %>%
    dplyr::mutate(title = stringr::str_remove(title, pattern = "，?國際證券辨識號碼一覽表"))
}




#' Taiwan securities list.
#'
#' @param stockonly a boolean value default FALSE for all securities,
#'   if TRUE the output limited to only stocks#'
#' @return a tibble contains following variables: no, title, dated,
#'   sec_type, sec_symbol, sec_name, isin_code, start_date, end_date,
#'   intr_rate, market, sector, cfi_code, remark.
#' @export
#' @examples
#' tw_getSec()
#' tw_getSec(stockonly = TRUE)
tw_getSec <- function(stockonly = FALSE) {
  allsec <- getAllSec()
  function() {
    if (!stockonly) return(allsec)
    else return(
      allsec %>% dplyr::filter(
        no %in% c(2, 4, 5),
        market %in% c("上市", "上櫃", "興櫃"),
        sec_type == "股票"
      )
    )
  }
}()


