library(ggplot2)

pay_gap_Europe %>%
  filter(Country == "France") %>%
  ggplot(data = ., aes(x = Year, y = Average)) +
  geom_line(aes(y = "Business"), color = "darkred") +
  geom_line(aes(y = Mining), color = "blue") +
  geom_line(aes(y = Average), color = "black") +
  geom_point()

