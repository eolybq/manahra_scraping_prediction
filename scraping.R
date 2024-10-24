library(rvest)
library(httr)
library(purrr)
library(tibble)
library(dplyr)

base_url <- "http://manahra.cz/"
login_url <- "http://manahra.cz/accounts/login/"

username <- "535432"
password <- "Kikina115"

login_response <- POST(login_url, body = list(username = username, password = password))

if (status_code(login_response) == 200) {
    # Získání cookies pro další požadavky
    session_cookies <- cookies(login_response)
    cookie_vector <- paste(session_cookies$name, session_cookies$value, sep = "=", collapse = "; ")
    
} else {
    cat("Přihlášení se nezdařilo.\n")
}
# Výkazy========
vykazy_url <- "http://manahra.cz/results/company_reports/"
vykazy_page <- GET(vykazy_url, set_cookies(cookie_vector))

page_content <- content(vykazy_page, "text")
page <- read_html(page_content)

links <- page %>% html_nodes("a.ft_xls") %>% html_attr("href")

dir_vykazy <- "Výkazy/"

walk(links, ~ {
    file_name <- paste0(basename(.), ".xlsx")
    file_path <- file.path(dir_vykazy, file_name)
    
    tryCatch({
        # Zkontroluj, zda soubor již existuje
        if (!file.exists(file_path)) {
            
            relative_link <- sub("^/", "", .)
            full_link <- paste0(base_url, relative_link)
            
            download_response <- GET(full_link, set_cookies(cookie_vector))
            writeBin(content(download_response, "raw"), file_path)
            
            cat("Stáhnuto:", file_name, "\n")
        } else {
            cat("Soubor již existuje, přeskočeno:", file_name, "\n")
        }
    }, error = function(e) {
        cat("Chyba při stahování:", file_name, "\n", e$message, "\n")
    })
})


# Ceny=====
load("ceny.RData")
load("ceny_t_1.RData")
load("objem.RData")

scraping <- function() {
    ceny_url <- "http://manahra.cz/exchange/quotations/"
    ceny_page <- GET(ceny_url, set_cookies(cookie_vector))
    
    page_content1 <- content(ceny_page, "text")
    page1 <- read_html(page_content1)

    table <- page1 %>% html_nodes(".list")
    table %>% html_table(fill = TRUE)
}

save_price <- function(burz_kolo) {
    data_tabulka <- scraping()[[1]]
    data_tabulka <- map_df(data_tabulka, ~ gsub("\u00A0", "", .))
    
    transformationIC <- strsplit(data_tabulka[data_tabulka[[1]] == c("IC_all", "IC_trh1","IC_trh2","IC_liche","IC_sude"), "Cena"][[1]], " / ")
    transformationIC <- map_chr(transformationIC, ~ .[2])
    transformationIC <- as.numeric(transformationIC)
    
    data_tabulka <- map_df(data_tabulka, ~ if(any(. == c(data_tabulka[[2]], data_tabulka[[3]], data_tabulka[[4]], data_tabulka[[6]], data_tabulka[[7]]))) {
        as.numeric(.) 
    } else {
        .
    }
    )
    data_tabulka[data_tabulka[[1]] == c("IC_all", "IC_trh1","IC_trh2","IC_liche","IC_sude"), "Cena"] <- transformationIC
    
    ceny[burz_kolo + 7, -1] <- as.list(data_tabulka[["Cena"]])
    save(ceny, file = "ceny.RData")
    
    ceny_t_1[burz_kolo + 8, -1] <- as.list(data_tabulka[["Cena"]])
    save(ceny_t_1, file = "ceny_t_1.RData")
}

save_volume <- function(burz_kolo) {
    data_tabulka <- scraping()[[1]]
    data_tabulka <- map_df(data_tabulka, ~ gsub("\u00A0", "", .))

    data_tabulka <- map_df(data_tabulka, ~ if(any(. == c(data_tabulka[[2]], data_tabulka[[3]], data_tabulka[[4]], data_tabulka[[6]], data_tabulka[[7]]))) {
        as.numeric(.) 
    } else {
        .
    }
    )
    
    volume[burz_kolo + 8, -1] <- as.list(data_tabulka[["Obchodováno"]])
    save(volume, file = "objem.RData")
}
