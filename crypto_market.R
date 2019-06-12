options("scipen"=999)

## Dependencies necessaery for script to execute properly
attach_pre_requisites <- function(){
        required_packages <- c("httr","jsonlite","magrittr","stringr","dplyr", "svMisc", "readr")
        for (packge in required_packages) {
                if (packge %in% rownames(installed.packages())) {
                        require(packge, character.only = TRUE)
                } else {
                        install.packages(packge)
                }
        }
        resp <- GET("https://exrates.me/openapi/v1/public/currency_pairs")
        cont <- content(resp, as = "text")
        currency_pairs <<- cont %>% fromJSON
        
        btc_pairs <- currency_pairs[grep("_btc",currency_pairs$url_symbol),]
        usd_pairs <- currency_pairs[grep("_usd",currency_pairs$url_symbol),]
        eth_pairs <- currency_pairs[grep("_eth",currency_pairs$url_symbol),]
        
        btc_pairs <<- btc_pairs
        usd_pairs <<- usd_pairs
        eth_pairs <<- eth_pairs
        
        valid_pairs <- c()
        count = 1
        for(i in 1:nrow(eth_pairs)){
                coin_symbol <- str_extract(eth_pairs$url_symbol[i], "[:alnum:]{1,9}")
                for(j in 1:nrow(btc_pairs)){
                        if(any(grepl(coin_symbol, btc_pairs$url_symbol)) == TRUE){
                                valid_pairs[count] <- btc_pairs$url_symbol[grep(coin_symbol, btc_pairs$url_symbol)][1]
                                count = count + 1
                        }
                }
        }
        
        count = 1
        valid_pairs1 <- c()
        valid_pairs <- valid_pairs %>% unique
        for(i in 1:length(valid_pairs)){
                coin_symbol <- str_extract(valid_pairs[i], "[:alnum:]{1,9}")
                for(j in 1:nrow(usd_pairs)){
                        if(any(grepl(coin_symbol, usd_pairs$url_symbol)) == TRUE){
                                valid_pairs1[count] <- usd_pairs$url_symbol[grep(coin_symbol, usd_pairs$url_symbol)] %>% unique() %>% .[1]
                                count = count + 1
                        }
                }
        }  
        valid_pairs_usd <- valid_pairs1 %>% unique
        valid_pairs_btc <- paste(str_extract(valid_pairs_usd, "[:alnum:]{1,9}"), "btc", sep = "_")
        valid_pairs_eth <- paste(str_extract(valid_pairs_usd, "[:alnum:]{1,9}"), "eth", sep = "_")
        valid_pairs_usd <<- valid_pairs_usd
        valid_pairs_btc <<- valid_pairs_btc
        valid_pairs_eth <<- valid_pairs_eth
        
        top_buy_order <<- function(pair){
                resp <- GET(paste0("https://exrates.me/openapi/v1/public/orderbook/",pair,"/?order_type=BUY"))
                cont <- content(resp, as = "text")
                option_buy_table <- cont %>% fromJSON %>% .[[1]]
                sprintf("%.8f", option_buy_table$rate[1])
                return(option_buy_table$rate[1] %>% as.numeric)
        }
        
        top_sell_order <<- function(pair){
                resp <- GET(paste0("https://exrates.me/openapi/v1/public/orderbook/",pair,"/?order_type=SELL"))
                cont <- content(resp, as = "text")
                option_sell_table <- cont %>% fromJSON %>% .[[1]]
                sprintf("%.8f", option_sell_table$rate[1])
                return(option_sell_table$rate[1] %>% as.numeric)
        }
        
        x <- list()
        for(i in seq_along(valid_pairs_btc)){
                x[["btc"]][i] <- grep(valid_pairs_btc[i], btc_pairs$url_symbol)
                x[["usd"]][i] <- grep(valid_pairs_usd[i], usd_pairs$url_symbol)
                x[["eth"]][i] <- grep(valid_pairs_eth[i], eth_pairs$url_symbol)
        }
        x <<- x
        
        top_buy_value <<- function(pair = c("btc", "usd", "eth"), all.out = FALSE){
                pair_price <- list()
                if(pair == "btc"){
                        pair = btc_pairs %>% slice(x$btc)
                        for(i in 1:nrow(pair)){
                                
                                pairprice <- top_buy_order(pair$url_symbol[i])
                                temp_list <- list(pp=pairprice[1])
                                pair_price = c(pair_price, temp_list)
                                svMisc::progress(i,nrow(pair))
                                
                        }
                        pair$pair_price <- pair_price %>% unlist
                        
                        if(isTRUE(all.out)){
                                saveas <- paste0("top_buy_btc_", Sys.time(), ".csv") %>% gsub(":","",.)
                                write.csv(pair, saveas, row.names = F)
                                message("\nFile successfully saved as ", saveas," in the current directory")
                        }
                        return(pair)
                } else if (pair == "usd"){
                        pair = usd_pairs %>% slice(x$usd)
                        for(i in 1:nrow(pair)){
                                
                                pairprice <- top_buy_order(pair$url_symbol[i])
                                temp_list <- list(pp=pairprice[1])
                                pair_price = c(pair_price, temp_list)
                                svMisc::progress(i,nrow(pair))
                                
                                
                        }
                        pair$pair_price <- pair_price %>% unlist
                        
                        if(isTRUE(all.out)){
                                saveas <- paste0("top_buy_usd_", Sys.time(), ".csv") %>% gsub(":","",.)
                                write.csv(pair, saveas, row.names = F)
                                message("\nFile successfully saved as ", saveas," in the current directory")
                        }
                        return(pair)
                        
                } else {
                        pair = eth_pairs %>% slice(x$eth)
                        for(i in 1:nrow(pair)){
                                
                                pairprice <- top_buy_order(pair$url_symbol[i])
                                temp_list <- list(pp=pairprice[1])
                                pair_price = c(pair_price, temp_list)
                                svMisc::progress(i,nrow(pair))
                                
                                
                        }
                        pair$pair_price <- pair_price %>% unlist
                        
                        if(isTRUE(all.out)){
                                saveas <- paste0("top_buy_eth_", Sys.time(), ".csv") %>% gsub(":","",.)
                                write.csv(pair, saveas, row.names = F)
                                message("\nFile successfully saved as ", saveas," in the current directory")
                        }
                        return(pair)
                        
                }
                
                
        }
        
        top_sell_value <<- function(pair = c("btc", "usd", "eth"), all.out = FALSE){
                pair_price <- list()
                if(pair == "btc"){
                        pair = btc_pairs %>% slice(x$btc)
                        for(i in 1:nrow(pair)){
                                
                                pairprice <- top_sell_order(pair$url_symbol[i])
                                temp_list <- list(pp=pairprice[1])
                                pair_price = c(pair_price, temp_list)
                                svMisc::progress(i,nrow(pair))
                                
                        }
                        pair$pair_price <- pair_price %>% unlist
                        if(isTRUE(all.out)){
                                saveas <- paste0("top_sell_btc_", Sys.time(), ".csv") %>% gsub(":","",.)
                                write.csv(pair, saveas, row.names = F)
                                message("\nFile successfully saved as ", saveas," in the current directory")
                        }
                        
                        return(pair)
                } else if (pair == "usd"){
                        pair = usd_pairs %>% slice(x$usd)
                        for(i in 1:nrow(pair)){
                                
                                pairprice <- top_sell_order(pair$url_symbol[i])
                                temp_list <- list(pp=pairprice[1])
                                pair_price = c(pair_price, temp_list)
                                svMisc::progress(i,nrow(pair))
                                
                                
                        }
                        pair$pair_price <- pair_price %>% unlist
                        
                        if(isTRUE(all.out)){
                                saveas <- paste0("top_sell_usd_", Sys.time(), ".csv") %>% gsub(":","",.)
                                write.csv(pair, saveas, row.names = F)
                                message("\nFile successfully saved as ", saveas," in the current directory")
                        }
                        return(pair)
                        
                } else {
                        pair = eth_pairs %>% slice(x$eth)
                        for(i in 1:nrow(pair)){
                                
                                pairprice <- top_sell_order(pair$url_symbol[i])
                                temp_list <- list(pp=pairprice[1])
                                pair_price = c(pair_price, temp_list)
                                svMisc::progress(i,nrow(pair))
                                
                                
                        }
                        pair$pair_price <- pair_price %>% unlist
                        
                        if(isTRUE(all.out)){
                                saveas <- paste0("top_sell_eth_", Sys.time(), ".csv") %>% gsub(":","",.)
                                write.csv(pair, saveas, row.names = F)
                                message("\nFile successfully saved as ", saveas," in the current directory")
                        }
                        return(pair)
                }
                
                
        }
        
        final_file <<- function(every.out = FALSE){
                final_df <- data.frame(BTC_top_buy=top_buy_value("btc",all.out = every.out), USD_top_buy=top_buy_value("usd",all.out = every.out), ETH_top_buy=top_buy_value("eth",all.out = every.out),
                                       BTC_top_sell=top_sell_value("btc",all.out = every.out), USD_top_sell=top_sell_value("usd",all.out = every.out), ETH_top_sell=top_sell_value("eth",all.out = every.out),
                                       ETH_BTC_sell=top_sell_order("eth_btc"), BTC_USD_sell=top_sell_order("btc_usd"))
                
                saveas <- paste0("final_file_", Sys.time(), ".csv") %>% gsub(":","",.)
                write.csv(final_df, saveas, row.names = F)
                
                if(isTRUE(every.out)){
                        message("\n\n All files saved successfully in the current directory")
                } else {
                        message("\n\n Final file saved successfully in the current directory")
                }
        }
}

## Dependencies necessary for script to execute properly
attach_pre_requisites()

## This generates the final output
## I added an extra functionality where if you want to see every of the markets in separate csv files
## Just set "every.out = TRUE"
final_file(every.out = TRUE)

## For ETH/BTC top sell value
top_sell_order("eth_btc") 

## For BTC/USD top sell value
top_sell_order("btc_usd")

