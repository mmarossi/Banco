install.packages("dplyr")
library(dplyr)
install.packages("stringr")
library(stringr)
View(client)
client <- mutate(client, sex = ifelse(substr(client$birth_number, 3,3) == 5 | substr(client$birth_number, 3,3) == 6, "F", "M"))
client <- mutate(client, birth_year = as.numeric(substr(client$birth_number, 1,2)))
client <- mutate(client, birth_month = as.numeric(substr(client$birth_number, 3,4)))  
birth_month <- as.numeric(substr(client$birth_number, 3,4))
birth_month1 <- (birth_month %% 50)
client <- mutate(client, birth_month = as.numeric(birth_month1)) 
client <- mutate(client, birth_day = as.numeric(substr(client$birth_number, 5,6)))
#juntando as bases "client" e "disp"
client2 <- full_join(client, disp)
# para juntar client2 e card, temos 1 coluna com o mesmo nome, fazendo a alteracao
names(card)[3] <- "cartoes"
# juntando as bases "client2" e "card"
client3 <- full_join(client2, card)
#quantos clientes temos usando cada tipo de cartao?
client4 <- filter(client3, cartoes != "NA")
y <- table(client4$sex, client4$cartoes)
library(ggplot2)
grafico1 <- barplot(y, col= c("red", "blue"), main = "Tipos de cart?es", beside = TRUE, legend = c("fem", "masc"))
# quantos clientes podem pedir emprestimos
z <- table(client3$type)
grafico2 <- pie(z, main = "Tipo de cliente", col = c("dark blue", "light blue"))
#em qual distrito est?o a maioria dos clientes?
w <- max(table(client3$district_id))
#unindo as tabelas de clientes e emprestimos
client5 <- full_join(client3, loan)
#retirando os clientes que nao possuem emprestimos
client6 <- filter(client5, status != "NA")
# tabela por tipo de emprestimo e por sexo
a <- table(client6$sex, client6$status)
# grafico por sexo, indicando cada tipo de emprestimo
grafico3 <- barplot(a, col= c("red", "blue"), main = "Tipos de emprestimos", beside = TRUE, legend = c("fem", "masc"), 
                    names.arg = c("encerrado", "encerrado s/ pagto", "em execucao", "em atraso"))


