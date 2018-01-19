
library(dplyr)
library(stringr)
library(RSQLite)
library(jiebaR)
library(ggplot2)
library(ggthemr)
library(ggrepel)
library(purrr)
library(tidyr)
library(wordcloud)
library(ggraph)
library(ggforce)
library(igraph)
library(tidytext)
library(widyr)
library(text2vec)

# data source ------------------------------------------------------------
drama_name <- '《人民的名义》'
seg <- worker(stop_word = 'dict/stop_words.utf8', 
              user = 'dict/user.dict.utf8', 
              write = 'NOFILE')
tag <- worker('tag', stop_word = 'dict/stop_words.utf8', user = 'dict/user.dict.utf8')
tagToChi <- function(tags) {
  tags[startsWith(tags, 'n')] <- '名词'
  tags[startsWith(tags, 't')] <- '时间词'
  tags[startsWith(tags, 's')] <- '处所词'
  tags[startsWith(tags, 'f')] <- '方位词'
  tags[startsWith(tags, 'v')] <- '动词'
  tags[startsWith(tags, 'a')] <- '形容词'
  tags[startsWith(tags, 'b')] <- '区别词'
  tags[startsWith(tags, 'z')] <- '状态词'
  tags[startsWith(tags, 'r')] <- '代词'
  tags[startsWith(tags, 'm')] <- '数词'
  tags[startsWith(tags, 'q')] <- '量词'
  tags[startsWith(tags, 'd')] <- '副词'
  tags[startsWith(tags, 'p')] <- '介词'
  tags[startsWith(tags, 'c')] <- '连词'
  tags[startsWith(tags, 'u')] <- '助词'
  tags[startsWith(tags, 'e')] <- '叹词'
  tags[startsWith(tags, 'y')] <- '语气词'
  tags[startsWith(tags, 'o')] <- '拟声词'
  tags[startsWith(tags, 'h')] <- '前缀'
  tags[startsWith(tags, 'k')] <- '后缀'
  tags[startsWith(tags, 'x')] <- '其他'
  tags[startsWith(tags, 'w')] <- '其他'
  tags[str_detect(tags, '[a-zA-Z]')] <- '其他'
  tags
}

sentiment <- read.csv('dict/sentiment.csv', na.strings = '', stringsAsFactors = FALSE)
sentiment <- sentiment %>% 
  select(-meaning_No, -meaning_sum, -sentiment2, -strength2, -polarity2) %>% 
  mutate(sentiment0 = case_when(
    sentiment %in% c('PA', 'PE') ~ 'happy', 
    sentiment %in% c('PD', 'PH', 'PG', 'PB', 'PK') ~ 'good',
    sentiment %in% c('NA') ~ 'angry',
    sentiment %in% c('NB', 'NJ', 'NH', 'PF') ~ 'sad',
    sentiment %in% c('NI', 'NC', 'NG') ~ 'fear',
    sentiment %in% c('NE', 'ND', 'NN', 'NK', 'NL') ~ 'disgust',
    sentiment %in% c('PC') ~ 'surprise'
  ), polarity = case_when(
    polarity == 2 ~ -1, 
    polarity == 3 ~ 0, 
    TRUE ~ as.numeric(polarity)
  )) %>% 
  as_tibble()

con <- dbConnect(RSQLite::SQLite(), 'weibo/weibo.sqlite')
df <- dbReadTable(con, 'people_call') %>% 
  as_tibble() %>% 
  mutate(text = str_replace_all(text, '@\\w+[[:punct:][:space:]]', ''))
dbDisconnect(con)

# sentiment --------------------------------------------------------------
ggthemr('flat')
col_user <- c('blue' = '#3498db', 'green' = '#2ecc71', 
              'yellow' = '#f1c40f', 'red' = '#e74c3c')

df %>% 
  select(id, gender, text) %>% 
  mutate(word = map(text, segment, seg), 
         gender = case_when(
           gender == 'f' ~ '女', 
           gender == 'm' ~ '男', 
           TRUE ~ gender
         )) %>% 
  select(id, gender, word) %>% 
  unnest() %>% 
  left_join(sentiment, by = 'word') %>% 
  filter(!is.na(sentiment0)) %>% 
  select(id, gender, word, strength, polarity, sentiment0) %>% 
  mutate(sentiment0 = case_when(
    sentiment0 == 'angry' ~ '怒', 
    sentiment0 == 'disgust' ~ '恶', 
    sentiment0 == 'fear' ~ '惧', 
    sentiment0 == 'good' ~ '好', 
    sentiment0 == 'happy' ~ '乐', 
    sentiment0 == 'sad' ~ '悲', 
    sentiment0 == 'surprise' ~ '惊', 
    TRUE ~ sentiment0
  )) %>% {
    temp <- .
    temp_gender <- group_by(temp, id, gender) %>% 
      summarise(polarity = sum(strength * polarity)) %>% 
      ungroup() %>% 
      group_by(gender) %>% 
      summarise(polarity = mean(polarity))
    
    png_name <- paste0(drama_name, '微博评论情感倾向分布')
    p <- group_by(temp, id) %>% 
      summarise(polarity = sum(strength * polarity)) %>% {
        ggplot(., aes(polarity)) + 
          geom_histogram(aes(y = ..density..), bins = 50, alpha = 0.2, fill = '#2ecc71') + 
          geom_density(alpha = 0.2, size = 1.5) +  
          geom_vline(aes(xintercept = mean(polarity)), 
                     color = col_user['blue'], 
                     linetype = 'dashed', 
                     size = 1) + 
          geom_text(aes(x = mean(polarity) + 15, 
                        y = 0.06, 
                        label = paste0('平均值: ', round(mean(polarity), 2))), 
                    size = 6) + 
          labs(x = NULL, y = NULL, title = png_name) + 
          theme(plot.title = element_text(hjust = 0.5)) + 
          scale_y_continuous(labels = scales::percent)
      }
    ggsave(filename = paste0('fig/text/', png_name, '.png'), plot = p)
    
    png_name <- paste0(drama_name, '微博评论情感倾向分布（性别）')
    p <- group_by(temp, gender, id) %>% 
      summarise(polarity = sum(strength * polarity)) %>% {
        ggplot(., aes(polarity, fill = gender)) + 
          geom_histogram(aes(y = ..density..), 
                         bins = 50, 
                         alpha = 0.2, 
                         position = 'identity') + 
          geom_density(aes(color = gender), alpha = 0.1, size = 1.5) +  
          geom_vline(data = temp_gender, aes(xintercept = polarity, color = gender), 
                     linetype = 'dashed', 
                     size = 1) + 
          geom_text(data = temp_gender, 
                    aes(x = polarity[1] + 15, 
                        y = 0.06, 
                        label = paste0('平均值: ', round(polarity[1], 2))),
                    color = col_user['blue'], 
                    size = 6) + 
          geom_text(data = temp_gender, 
                    aes(x = polarity[2] - 15, 
                        y = 0.06, 
                        label = paste0('平均值: ', round(polarity[2], 2))),
                    color = col_user['green'], 
                    size = 6) + 
          labs(x = NULL, y = NULL, title = png_name) + 
          theme(plot.title = element_text(hjust = 0.5), 
                legend.position = 'bottom', 
                legend.title = element_blank()) + 
          guides(color = guide_legend()) + 
          scale_y_continuous(labels = scales::percent)
      }
    ggsave(filename = paste0('fig/text/', png_name, '.png'), plot = p)
    
    png_name <- paste0(drama_name, '微博评论情感类型占比')
    p <- group_by(temp, sentiment0) %>% 
      summarise(strength = sum(strength)) %>% 
      mutate(strength = prop.table(strength)) %>% {
        .$sentiment0 <- factor(.$sentiment0, 
                               levels = rev(c('好', '恶', '乐', '悲', '惧', '惊', '怒')))
        . <- arrange(., -strength) %>% 
          mutate(text_y = cumsum(strength) - strength/2)
        ggplot(., aes('', strength, fill = sentiment0)) + 
          geom_bar(stat = 'identity', width = 1, color = 'white') + 
          coord_polar('y') + 
          geom_text_repel(aes(label = scales::percent(strength), 
                              y = text_y), 
                    # position = position_stack(vjust = 0.5),  
                    size = 5) + 
          labs(x = NULL, y = NULL, title = png_name) + 
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5),
                axis.text.x = element_blank(),
                legend.title = element_blank(),
                legend.position = 'bottom')
      }
    ggsave(filename = paste0('fig/text/', png_name, '.png'), plot = p)
    
    png_name <- paste0(drama_name, '微博评论情感类型占比（性别）')
    p <- group_by(temp, gender, sentiment0) %>% 
      summarise(strength = sum(strength)) %>% 
      mutate(strength = prop.table(strength)) %>% {
        .$sentiment0 <- factor(.$sentiment0, 
                               levels = rev(c('好', '恶', '乐', '悲', '惧', '惊', '怒')))
        . <- arrange(., -strength) %>% 
          group_by(gender) %>% 
          mutate(text_y = cumsum(strength) - strength/2)
        ggplot(., aes('', strength, fill = sentiment0)) + 
          geom_bar(stat = 'identity', width = 1, color = 'white') + 
          coord_polar('y') + 
          geom_text_repel(aes(label = scales::percent(strength), 
                              y = text_y), 
                    # position = position_stack(vjust = 0.5),  
                    size = 4) + 
          facet_wrap(~ gender) + 
          labs(x = NULL, y = NULL, title = png_name) + 
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5),
                axis.text.x = element_blank(),
                legend.title = element_blank(),
                legend.position = 'bottom')
      }
    ggsave(filename = paste0('fig/text/', png_name, '.png'), plot = p)
  }


# sentiment word ---------------------------------------------------------

df %>% 
  select(id, gender, text) %>% 
  mutate(
    word = map(text, segment, seg), 
    gender = case_when(
      gender == 'f' ~ '女', 
      gender == 'm' ~ '男', 
      TRUE ~ gender
    )
  ) %>% 
  select(id, gender, word) %>% 
  unnest() %>% 
  left_join(sentiment, by = 'word') %>% 
  filter(!is.na(polarity)) %>% 
  select(id, gender, word, strength, polarity, sentiment0) %>%
  group_by(id) %>% 
  mutate(polar_sentence = case_when(
    sum(strength * polarity) > 0 ~ '积极', 
    sum(strength * polarity) == 0 ~ '中性', 
    sum(strength * polarity) < 0 ~ '消极'
  )) %>% 
  mutate(sentiment0 = case_when(
    sentiment0 == 'angry' ~ '怒', 
    sentiment0 == 'disgust' ~ '恶', 
    sentiment0 == 'fear' ~ '惧', 
    sentiment0 == 'good' ~ '好', 
    sentiment0 == 'happy' ~ '乐', 
    sentiment0 == 'sad' ~ '悲', 
    sentiment0 == 'surprise' ~ '惊', 
    TRUE ~ sentiment0
  )) %>% {
    png_name <- paste0(drama_name, '积极-消极微博高频词')
    p <- group_by(., polar_sentence) %>% 
      nest() %>% 
      filter(polar_sentence %in% c('积极', '消极')) %>% 
      mutate(word_count = map(data, ~ top_n(count(.x, word), 10, n))) %>% 
      select(polar_sentence, word_count) %>% 
      unnest() %>% 
      mutate(n = if_else(polar_sentence == '消极', n * -1L, n)) %>% {
        ggplot(., aes(reorder(word, n), n, fill = polar_sentence)) + 
          geom_bar(stat = 'identity', width = 0.6) + 
          geom_text(aes(label = word), check_overlap = TRUE, hjust = -0.3) + 
          labs(x = NULL, y = NULL, title = png_name) + 
          coord_flip(ylim = c(-40, 60)) + 
          theme(plot.title = element_text(hjust = 0.5), 
                axis.text.y = element_blank(), 
                legend.title = element_blank(), 
                legend.position = 'bottom')
      }
    ggsave(filename = paste0('fig/text/', png_name, '.png'), plot = p)
    
    png_name <- paste0(drama_name, '微博各情感类型高频词')
    p <- group_by(., sentiment0) %>% 
      nest() %>% 
      mutate(word_count = map(data, ~ top_n(count(.x, word), 10, n))) %>% 
      select(sentiment0, word_count) %>% 
      unnest() %>% 
      filter(n >= 2) %>% {
        ggplot(., aes(reorder(word, n), n, fill = sentiment0)) + 
          geom_bar(stat = 'identity', width = 0.6) + 
          # geom_text(aes(label = n), check_overlap = TRUE, hjust = -0.3) + 
          labs(x = NULL, y = NULL, title = png_name) + 
          facet_wrap(~ sentiment0, scales = 'free') + 
          coord_flip() + 
          theme(plot.title = element_text(hjust = 0.5), 
                axis.text.y = element_text(size = 7), 
                legend.position = 'none')
      }
    ggsave(filename = paste0('fig/text/', png_name, '.png'), plot = p)
  }
  
  
# word cloud -------------------------------------------------------------

key <- worker('keywords', topn = 100, stop_word = 'dict/stop_words.utf8', 
              user = 'dict/user.dict.utf8',  
              write = 'NOFILE')

user_stop_cloud_words <- paste0(
  c('全文', '视频', '电视剧', '演员', '网页', '连接', 'quot', '热播', '饰演', 
    '链接', '微博', '影视'), 
  collapse = '|'
) %>% 
  paste('[:digit:]', sep = '|')

png_name <- paste0(drama_name, '微博评论关键词词云')
png(paste0('fig/text/', png_name, '.png'), width = 2100, height = 2100)
df$text %>% 
  paste0(collapse = '') %>% 
  str_replace_all(user_stop_cloud_words, '') %>% 
  keywords(key) %>% 
  tibble::enframe() %>% {
    temp <- df %>%
      select(id, text) %>%
      mutate(word = map(text, ~ str_replace_all(.x, user_stop_cloud_words, '') %>%
                          segment(seg))) %>%
      pull(word) %>%
      unlist() %>%
      table() %>%
      tibble::enframe() %>%
      arrange(-value)
    left_join(., temp, by = c('value' = 'name'))
  } %>%
  mutate(value.y = log(as.numeric(value.y), base = 2)) %>% {
    wordcloud(words = .$value, 
              freq = as.numeric(.$value.y), 
              scale = c(13, 0.5), 
              random.order = FALSE, 
              rot.per = 0, 
              colors = brewer.pal(8, 'Greys'))
  }
dev.off()

key <- worker('keywords', topn = 10, 
              stop_word = 'dict/stop_words.utf8', 
              user = 'dict/user.dict.utf8',  
              write = 'NOFILE')
df %>% 
  select(id, gender, text) %>% 
  mutate(word = map(text, ~ str_replace_all(.x, user_stop_cloud_words, '') %>% 
                      keywords(key) %>% 
                      unname())) %>% 
  select(id, gender, word) %>% 
  unnest() %>% 
  group_by(word, gender) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  spread(gender, n, fill = 1) %>% 
  mutate(f_m = f/m, m_f = m/f) %>% {
    
    png_name <- paste0(drama_name, '微博评论区别词(男)')
    png(paste0('fig/text/', png_name, '.png'), width = 2100, height = 2100)
    wordcloud(words = .$word, 
              freq = as.numeric(.$m_f), 
              scale = c(10, 0.5), 
              random.order = FALSE, 
              rot.per = 0, 
              colors = brewer.pal(8, 'Blues'))
    dev.off()
    
    png_name <- paste0(drama_name, '微博评论区别词(女)')
    png(paste0('fig/text/', png_name, '.png'), width = 2100, height = 2100)
    wordcloud(words = .$word, 
              freq = as.numeric(.$f_m), 
              scale = c(10, 0.5), 
              random.order = FALSE, 
              rot.per = 0, 
              colors = brewer.pal(8, 'Reds'))
    dev.off()
  }

# co-occurrence word -----------------------------------------------------
ggthemr_reset()
user_stop_cooc_words <- paste0(
  c('全文', '视频', '电视剧', '演员', '网页', '连接', 'quot', '热播', '饰演', 
    '链接', '微博', '影视', '秒拍', '一个', '没有', '真的', '看到', '一部', 
    '知道', '现在', '里面', '最近', '个月', '博文', '头条', '文章'), 
  collapse = '|'
) %>% 
  paste('[:digit:]', sep = '|')

png_name <- paste0(drama_name, '共现词网络')
df %>% 
  select(id, text) %>% 
  mutate(
    word = map(
      text, ~ str_replace_all(.x, user_stop_cooc_words, '') %>% segment(seg)
    )
  ) %>%
  select(id, word) %>% 
  unnest() %>% 
  filter(nchar(word) >= 2 & word != '人民的名义') %>% 
  pairwise_count(word, id) %>%
  filter(n >= quantile(.$n, 0.999) & n >= 10) %>% 
  graph_from_data_frame() %>% 
  set_vertex_attr(
    'size', 
    value = map_int(V(.)$name, ~ sum(str_detect(df$text, .x)))
  ) %>% 
  set_vertex_attr(
    'col', 
    value = tagToChi(names(tagging(V(.)$name, tag)))
  ) %>% {
    ggraph(., layout = "fr") +
      geom_edge_link(aes(edge_width = n),
                     edge_alpha = 0.05, 
                     edge_colour = 'darkred', 
                     show.legend = FALSE) +
      geom_node_point(aes(size = log(size), 
                          color = col), 
                      show.legend = TRUE) +
      geom_node_text(aes(label = name, size = log(size)), 
                     repel = TRUE, 
                     point.padding = unit(0.2, 'lines'), 
                     show.legend = FALSE) +
      ggtitle(png_name) + 
      theme_void() + 
      theme(plot.title = element_text(hjust = 0.5, vjust = 0.5)) + 
      guides(color = guide_legend(title = '词性')) + 
      scale_edge_width(guide = 'none') + 
      scale_size(guide = 'none')
  } %>% 
  ggsave(filename = paste0('fig/text/', png_name, '.png'), plot = .)


# correlation word -------------------------------------------------------
user_stop_cor_words <- paste0(
  c('全文', '视频', '电视剧', '演员', '网页', '连接', 'quot', '热播', '饰演', 
    '链接', '微博', '影视', '秒拍', '一个', '没有', '真的', '看到', '一部', 
    '知道', '现在', '里面', '最近', '个月', '博文', '头条', '文章'), 
  collapse = '|'
) %>% 
  paste('[:digit:]', sep = '|')


df %>% 
  select(id, text) %>% 
  mutate(
    word = map(
      text, ~ str_replace_all(.x, user_stop_cor_words, '') %>% segment(seg)
    )
  ) %>%
  select(id, word) %>% 
  unnest() %>% 
  group_by(word) %>% 
  filter(n() >= 20 & nchar(word) >= 2) %>% 
  pairwise_cor(word, id, sort = TRUE) %>% {
    
    png_name <- paste0(drama_name, '相关词网络')
    p <- filter(., correlation > 0.3) %>% 
      graph_from_data_frame() %>% 
      set_vertex_attr(
        'size', 
        value = map_int(V(.)$name, ~ sum(str_detect(df$text, .x)))
      ) %>% 
      set_vertex_attr(
        'col', 
        value = tagToChi(names(tagging(V(.)$name, tag)))
      ) %>% {
        ggraph(., layout = "fr") +
          geom_edge_link(aes(edge_width = correlation),
                         edge_alpha = 0.05, 
                         edge_colour = 'darkred', 
                         show.legend = FALSE) +
          geom_node_point(aes(size = log(size), 
                              color = col), 
                          show.legend = TRUE) +
          geom_node_text(aes(label = name, size = log(size)), 
                         repel = TRUE, 
                         point.padding = unit(0.2, 'lines'), 
                         show.legend = FALSE) +
          ggtitle(png_name) + 
          theme_void() + 
          theme(plot.title = element_text(hjust = 0.5, vjust = 0.5)) + 
          guides(color = guide_legend(title = '词性')) + 
          scale_edge_width(guide = 'none') + 
          scale_size(guide = 'none')
      }
    ggsave(filename = paste0('fig/text/', png_name, '.png'), plot = p)
    
    main_character <- c('李达康', '吴刚', '侯亮平', 
                        '陆毅', '高育良', '张志坚', 
                        '祁同伟', '高小琴', '周梅森')
    png_name <- paste0(drama_name, '部分关键词相关词TOP10')
    p <- filter(., item1 %in% main_character) %>% 
      group_by(item1) %>% 
      top_n(10, correlation) %>% {
        ggplot(., aes(reorder(item2, correlation), correlation, fill = item1)) + 
          geom_bar(stat = 'identity', width = 0.6) + 
          # geom_text(aes(label = n), check_overlap = TRUE, hjust = -0.3) + 
          labs(x = NULL, y = NULL, title = png_name) + 
          facet_wrap(~ item1, scales = 'free') + 
          coord_flip() + 
          theme(plot.title = element_text(hjust = 0.5), 
                axis.text.y = element_text(size = 7), 
                legend.position = 'none')
      }
    ggsave(filename = paste0('fig/text/', png_name, '.png'), plot = p)
  }
  


# topic model ------------------------------------------------------------
ggthemr('flat')
user_stop_topic_words <- paste0(
  c('全文', '视频', '电视剧', '演员', '网页', '连接', 'quot', '热播', '饰演', 
    '链接', '微博', '影视', '秒拍', '一个', '没有', '真的', '看到', '一部', 
    '知道', '现在', '里面', '最近', '个月', '博文', '头条', '文章', '人民的名义'), 
  collapse = '|'
) %>% 
  paste('[:digit:]', sep = '|')

it <- map(df$text, ~ str_replace_all(.x, user_stop_topic_words, '') %>% segment(seg)) %>% 
  map(~ .x[str_detect(.x, '\\w\\w+') & !str_detect(.x, '^[[:digit:]a-zA-Z]+$')]) %>% 
  discard(~ identical(character(0), .x)) %>% 
  itoken(progressbar = TRUE)

v <- create_vocabulary(it, ngram = c(1L, 1L)) %>%
  prune_vocabulary(
    doc_count_min = 10
  )
vectorizer <- vocab_vectorizer(v)
dtm <- create_dtm(it, vectorizer, type = 'dgTMatrix')
model_lda <- LDA$new(n_topic = 5, doc_topic_prior = 0.1, topic_word_prior = 0.01)
doc_topic <- model_lda$fit_transform(
  dtm,
  n_iter = 1000,
  convergence_tol = 0.001,
  check_convergence_every_n = 25,
  progressbar = TRUE
)
topic_word <- t(model_lda$topic_word_distribution)

png_name <- paste0(drama_name, '各话题关键词TOP10')
model_lda$get_top_words(n = 10, lambda = 0.3) %>% 
  as_tibble() %>% 
  setNames(as.character(seq_len(ncol(.)))) %>% {
    temp <- . 
    map_df(colnames(temp), ~ {
      topic_word[temp[[as.character(.x)]], as.integer(.x)] %>% 
        tibble::enframe() %>% 
        mutate(topic = .x)
    })
  } %>% 
  mutate(name = reorder(name, value)) %>% {
    ggplot(., aes(reorder(name, value), value, fill = topic)) + 
      geom_bar(stat = 'identity', width = 0.6) + 
      # geom_text(aes(label = word), check_overlap = TRUE, hjust = -0.3) + 
      labs(x = NULL, y = NULL, title = png_name) + 
      facet_wrap(~ topic, scales = 'free') + 
      coord_flip() + 
      theme(plot.title = element_text(hjust = 0.5), 
            axis.text.y = element_text(size = 7), 
            legend.position = 'none')
  } %>% 
  ggsave(filename = paste0('fig/text/', png_name, '.png'), plot = .)

png_name <- paste0(drama_name, '各话题聚类分布')
doc_topic_class <- factor(paste0('话题', unname(apply(doc_topic, 1, which.max))))
doc_topic %>% 
  as_tibble() %>% 
  setNames(paste0('话题', as.character(seq_len(ncol(.))))) %>%
  prcomp(scale. = TRUE) %>% {
    ggbiplot::ggbiplot(., 
             obs.scale = 1, 
             var.scale = 1,
             groups = doc_topic_class, 
             ellipse = TRUE, 
             circle = FALSE) + 
      labs(x = NULL, y = NULL, title = png_name) + 
      theme(plot.title = element_text(hjust = 0.5), 
            legend.title = element_blank(), 
            legend.position = 'bottom')
  } %>% 
  ggsave(filename = paste0('fig/text/', png_name, '.png'), plot = .)

