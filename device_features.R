require(dplyr)
require(readr)
require(Matrix)

rm(list=ls()); gc()

################################################################################
#
# LOAD RAW DATASET
#
################################################################################
device.events <- read_csv("./rawdata/events.csv", col_types = "icTcc")
app.events <- read_csv("./rawdata/app_events.csv", col_types = "icii")
phone.models <- read_csv("./rawdata/phone_brand_device_model.csv", col_types = "ccc")
train.set <- read_csv("./rawdata/gender_age_train.csv", col_types = "ccic")
test.set <- read_csv("./rawdata/gender_age_test.csv", col_types = "c")

################################################################################
#
# CREATE DEVICE FEATURES DATASET
#
################################################################################
device.events <- device.events %>% 
    distinct(event_id, device_id) %>% 
    select(-timestamp, -longitude, -latitude)

app.events <- app.events %>% 
    select(-is_installed, -is_active) %>% 
    distinct(event_id, app_id) %>% 
    mutate( app_id = paste("app_id:", app_id, sep="") ) %>% 
    rename( feature = app_id )

phone.models <- phone.models %>% 
    mutate( phone_brand = paste("phone_brand:", phone_brand, sep=""),
            device_model = paste("device_model:", device_model, sep="") )

phone.models <- union(
    phone.models %>% select(device_id, phone_brand) %>% rename( feature=phone_brand),
    phone.models %>% select(device_id, device_model) %>% rename( feature=device_model)
)

device.features <- left_join(device.events, app.events) %>%
    select(-event_id) %>% 
    union(phone.models) %>% 
    arrange(device_id)

rm(app.events)
rm(device.events)
rm(phone.models)
gc()

################################################################################
#
# SPREAD
#
################################################################################
require(tidyr)

device.features <- device.features %>% 
    mutate(cnt=1)

x <- device.features %>% spread(feature, cnt, fill=0)

################################################################################
#
# BUILD SPARSE MATRIX
#
################################################################################

device.ids <- device.features %>% select(device_id) %>% distinct() %>% arrange()
feature.ids <- device.features %>% select(feature) %>% distinct() %>% arrange()

tmp.i <- seq(from=1, to=nrow(device.ids))
device.ids <- device.ids %>% bind_cols(as.data.frame(tmp.i)) %>% rename(i=tmp.i)

tmp.j <- seq(from=1, to=nrow(feature.ids))
feature.ids <- feature.ids %>% bind_cols(as.data.frame(tmp.j)) %>% rename(j=tmp.j)

device.features <- device.features %>% 
    left_join(device.ids) %>% 
    left_join(feature.ids)


m <- sparseMatrix(as.integer(device.features$i),
                  as.integer(device.features$j))

rownames(m) <- as.character(device.ids$device_id)
colnames(m) <- as.character(feature.ids$feature)

x <- m[rownames(m) %in% train.set$device_id,]
id <- train.set$device_id[match(train.set$device_id,rownames(x))]
y <- train.set$group[match(train.set$device_id,rownames(x))]

t <- m[rownames(m) %in% test.set$device_id,]

save(x, file="x.m")
save(y, file="y.m")
save(t, file="t.m")

rm(list=ls())
gc()
