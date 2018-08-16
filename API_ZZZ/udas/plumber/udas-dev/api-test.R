
library(httr)
library(RMySQL)
library(jsonlite)

# db connection ----------------------------------------------------------

pool <- pool::dbPool(
  RMySQL::MySQL(),
  host     = '192.168.15.128',
  port     = 3306,
  username = 'api_card',
  password = '123456',
  dbname   = 'udas'
)
con <- pool::poolCheckout(pool)
DBI::dbSendQuery(con, 'set names utf8')
pool::poolReturn(con)
# pool::poolClose(pool)

# global var and func ----------------------------------------------------

year2seq <- function(year_vec) {
  year_vec <- as.integer(year_vec)
  seq(min(year_vec), max(year_vec), 1)
}

GetError <- function(end_point, params_err = NULL, params_ok) {
  temp <- GET(paste0(url_port, end_point))
  stopifnot(status_code(temp) == 400)
  # stopifnot(content(temp)$errorCode == 40001)
  
  if (!is.null(params_err)) {
    temp <- GET(paste0(url_port, end_point), query = params_err)
    stopifnot(status_code(temp) == 400)
    # stopifnot(content(temp)$errorCode == 40002)
  }
  
  temp <- GET(paste0(url_port, end_point), query = params_ok)
  stop_for_status(temp)
  fromJSON(content(temp, 'text', encoding = 'utf-8'))
}

url_port <- 'http://localhost:8000'

area <- tidyr::unnest(tibble::enframe(list(
  '西北' = c('陕西省', '甘肃省', '青海省', '宁夏回族自治区', '新疆维吾尔自治区'), 
  '华东' = c('山东省', '江苏省', '安徽省', '浙江省', '江西省', '福建省', '上海市'), 
  '华北' = c('北京市', '天津市', '河北省', '山西省', '内蒙古自治区'), 
  '华中' = c('河南省', '湖南省', '湖北省'), 
  '华南' = c('广西壮族自治区', '广东省', '海南省'), 
  '西南' = c('西藏自治区', '四川省', '重庆市', '贵州省', '云南省'), 
  '东北' = c('黑龙江省', '吉林省', '辽宁省'), 
  '港澳台' = c('香港特别行政区', '澳门特别行政区', '台湾省')
)))

kl <- c('全部', dbGetQuery(pool, 'select distinct kl from ks_lqb;')$kl)
lqlx <- c('全部', dbGetQuery(pool, 'select distinct lqlx from ks_lqb;')$lqlx)
zymc <- c('全部', dbGetQuery(pool, 'select distinct zymc from ks_lqb;')$zymc)
dwmc <- c('全部', dbGetQuery(pool, 'select distinct dwmc from jzg_jbsjb;')$dwmc)

# selection --------------------------------------------------------------

temp <- GET(paste0(url_port, '/selection/area'))
stop_for_status(temp)
temp <- fromJSON(content(temp, 'text', encoding = 'utf-8'))$area
stopifnot(all(area$value %in% temp))

zczk <- c('全部', '未注册', '注册未通过', '注册', '报到', '未报到')
temp <- GET(paste0(url_port, '/selection/zczk'))
stop_for_status(temp)
temp <- fromJSON(content(temp, 'text', encoding = 'utf-8'))$zczk
stopifnot(all(zczk %in% temp))

year <- year2seq(dbGetQuery(pool, 'select distinct lqnf from ks_lqb;')$lqnf)
temp <- GET(paste0(url_port, '/selection/lqnf'))
stop_for_status(temp)
temp <- fromJSON(content(temp, 'text', encoding = 'utf-8'))$year
stopifnot(all(year == temp))

year <- year2seq(dbGetQuery(pool, 'select distinct xn from bks_zcb;')$xn)
temp <- GET(paste0(url_port, '/selection/xn'))
stop_for_status(temp)
temp <- fromJSON(content(temp, 'text', encoding = 'utf-8'))$year
stopifnot(all(year == temp))

year <- year2seq(dbGetQuery(pool, 'select distinct substr(ydrq, 1, 4) from bks_xjydb;')[, 1])
temp <- GET(paste0(url_port, '/selection/ydrq'))
stop_for_status(temp)
temp <- fromJSON(content(temp, 'text', encoding = 'utf-8'))$year
stopifnot(all(year == temp))

temp <- GET(paste0(url_port, '/selection/kl'))
stop_for_status(temp)
temp <- fromJSON(content(temp, 'text', encoding = 'utf-8'))$kl
stopifnot(all(kl %in% temp))

temp <- GET(paste0(url_port, '/selection/lqlx'))
stop_for_status(temp)
temp <- fromJSON(content(temp, 'text', encoding = 'utf-8'))$lqlx
stopifnot(all(lqlx %in% temp))

temp <- GET(paste0(url_port, '/selection/zymc'))
stop_for_status(temp)
temp <- fromJSON(content(temp, 'text', encoding = 'utf-8'))$zymc
stopifnot(all(zymc %in% temp))

temp <- GET(paste0(url_port, '/selection/dwmc'))
stop_for_status(temp)
temp <- fromJSON(content(temp, 'text', encoding = 'utf-8'))$dwmc
stopifnot(all(dwmc %in% temp, na.rm = TRUE))

# summary ----------------------------------------------------------------

temp <- GET(paste0(url_port, '/summary/peopleCount'))
stop_for_status(temp)
temp <- fromJSON(content(temp, 'text', encoding = 'utf-8'))
stopifnot(all(c('lqnf', 'n', 'scoreAvg') %in% colnames(temp)))

temp <- GetError('/summary/source', list(lqnf = 'test'), list(lqnf = 2017))
stopifnot(all(c('sex', 'nation', 'area', 'zzmm') %in% names(temp)))

temp <- GetError('/summary/peopleMajorCount', list(lqnf = 'test'), list(lqnf = 2017))
yxsmc <- dbGetQuery(
  pool, 'select distinct yxsmc from bks_xjb where substr(rxny, 1, 4) = 2017;'
)$yxsmc
stopifnot(all(yxsmc %in% temp$yxsmc))

# matriculation ----------------------------------------------------------

temp <- GetError('/matriculation/source/structure', 
                 list(lqnf = 'test'), list(lqnf = 2017))
stopifnot(all(c('area', 'sex', 'kl', 'isIn') %in% names(temp)))

temp <- GetError('/matriculation/source/province', 
                 list(lqnf = 'test'), list(lqnf = 2017))
stopifnot(all(temp$area %in% area$value))

temp <- GetError('/matriculation/source/province/detail', 
                 list(lqnf = 'test', area = '甘肃省'), 
                 list(lqnf = 2017, area = '甘肃省'))
stopifnot(all(c('kl', 'sex', 'score') %in% names(temp)))

temp <- GetError('/matriculation/source/major', 
                 list(lqnf = 'test'), list(lqnf = 2017))
stopifnot(all(temp$zymc %in% zymc))

temp <- GetError('/matriculation/source/major/detail', 
                 list(lqnf = 'test', zymc = '物理学类'), 
                 list(lqnf = 2017, zymc = '物理学类'))
stopifnot(all(c('kl', 'sex', 'score') %in% names(temp)))

temp <- GetError(
  '/matriculation/score/summary/absolute', 
  list(lqnf = 'test', kl = '理工', lqlx = '其他统考生', item = '最高分'), 
  list(lqnf = 2017, kl = '理工', lqlx = '其他统考生', item = '最高分')
)
stopifnot(nrow(temp) > 0)
stopifnot(all(temp$area %in% area$value))

temp <- GetError(
  '/matriculation/score/province', 
  list(lqnf = 'test', area = '甘肃省'), 
  list(lqnf = 2017, area = '甘肃省')
)
stopifnot(nrow(temp) > 0)

temp <- GetError(
  '/matriculation/score/major', 
  list(lqnf = 'test', area = '甘肃省'), 
  list(lqnf = 2017, area = '甘肃省')
)
stopifnot(nrow(temp) > 0)

temp <- GetError( '/matriculation/count/summary/trend', NULL, list(area = '甘肃省'))
stopifnot(nrow(temp) > 0)

temp <- GetError('/matriculation/count/summary/top', list(lqnf = 'test'), list(lqnf = 2017))
stopifnot(all(c('thisYearTop', 'yoyMaxTop', 'yoyMinTop', 'areaSum') %in% names(temp)))

temp <- GET(paste0(url_port, '/matriculation/count/province'))
stop_for_status(temp)
temp <- fromJSON(content(temp, 'text', encoding = 'utf-8'))
stopifnot(all(temp$area %in% area$value))

temp <- GetError('/matriculation/count/major', 
                 list(lqnf = 'test', kl = '理工', lqlx = '其他统考生'), 
                 list(lqnf = 2017, kl = '理工', lqlx = '其他统考生'))
stopifnot(all(setdiff(colnames(temp), 'zymc') %in% area$value))
stopifnot(all(temp$zymc %in% zymc))

temp <- GetError('/matriculation/major/summary/structure', 
                 list(lqnf = 'test'), list(lqnf = 2017))
stopifnot(all(c('kl', 'lqlx') %in% names(temp)))
stopifnot(all(setdiff(temp$kl$kl, '未知') %in% kl))
stopifnot(all(setdiff(temp$lqlx$lqlx, '未知') %in% lqlx))

temp <- GetError('/matriculation/major/summary/top', 
                 list(lqnf = 'test'), list(lqnf = 2017))
stopifnot(all(c('thisYearTop', 'yoyMaxTop', 'yoyMinTop') %in% names(temp)))
stopifnot(all(setdiff(temp$thisYearTop$zymc, '未知') %in% zymc))
stopifnot(all(setdiff(temp$yoyMaxTop$zymc, '未知') %in% zymc))
stopifnot(all(setdiff(temp$yoyMinTop$zymc, '未知') %in% zymc))

temp <- GetError(
  '/matriculation/major/summary/trend', 
  NULL, list(area = '甘肃省', kl = '理工', lqlx = '其他统考生', zymc = '物理学类'
))
stopifnot(nrow(temp) > 0)

temp <- GetError('/matriculation/major/score', 
                 list(lqnf = 'test', area = '甘肃省'), 
                 list(lqnf = 2017, area = '甘肃省'))
stopifnot(nrow(temp) > 0)

temp <- GetError('/matriculation/major/count', 
                 list(lqnf = 'test', kl = '理工', lqlx = '其他统考生'), 
                 list(lqnf = 2017, kl = '理工', lqlx = '其他统考生'))
stopifnot(all(setdiff(colnames(temp), 'zymc') %in% area$value))
stopifnot(all(setdiff(temp$zymc, '未知') %in% zymc))

# enroll -----------------------------------------------------------------

temp <- GetError('/enroll/regist/summary/info', list(xn = 'test'), list(xn = 2017))
stopifnot(all(c('total', 'pass', 'prob') %in% names(temp)))
stopifnot(is.numeric(temp$prob) && temp$prob >= 0 && temp$prob <= 1)

temp <- GetError('/enroll/regist/summary/probProvince', list(xn = 'test'), list(xn = 2017))
stopifnot(all(setdiff(temp$area, '其他') %in% area$value))

temp <- GetError('/enroll/regist/summary/probYear', NULL, list(area = '甘肃省'))
stopifnot(nrow(temp) > 0)

temp <- GetError('/enroll/regist/detail', 
                 list(xn = 'test', area = '甘肃省', zczk = '未注册'), 
                 list(xn = 2017, area = '甘肃省', zczk = '未注册'))
stopifnot(nrow(temp) > 0)

# teach ------------------------------------------------------------------

temp <- GetError('/teach/resource/college', NULL, list(dwmc = '物理科学与技术学院'))
stopifnot(nrow(temp) > 0)

# close connection -------------------------------------------------------

dbDisconnect(con)
pool::poolClose(pool)
closeAllConnections()
