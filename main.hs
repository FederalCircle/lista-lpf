

-- 1. menorDeDois: recebe dois valores e retorna o menor
menorDeDois :: Int -> Int -> Int
menorDeDois x y | x > y = y
                | otherwise = x

-- 2. menorDeTres: recebe três valores e retorna o menor
menorDeTres :: Int -> Int -> Int -> Int
menorDeTres x y z | x < y && x < z = x
                  | y < z = y
                  | otherwise = z

-- 3. fatorial: recebe um numero natural e retorna o seu fatorial
fat :: Int -> Int
fat 0 = 1
fat x = x * fat (x-1)

-- 4. fibonacci: recebe um número inteiro positivo e retorna o n-ésimo elemento da
-- seqüência de Fibonacci (especificar no programa se sua seqüência começa com
-- 0 e 1 ou com 1 e 1)
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib x = fib (x-1) + fib (x-2)

-- 5. elemento: recebe uma lista e um número inteiro positivo para retornar o n-
-- ésimo elemento da lista
-- ex.: (elemento 1 '(3 7 4 2)) ==> 3
getByIndex :: Int -> [Int] -> Int
getByIndex 0 (elm : list) = elm
getByIndex x (elm : list) = getByIndex (x-1) list

-- 6. pertence: recebe uma lista e um elemento qualquer e verifica se o elemento
-- pertence à lista
-- ex.: pertence 1 [3,7,4,2] = False
pertence :: Int -> [Int] -> Bool
pertence _ [] = False
pertence x (elm : list) | x == elm = True
                      | otherwise = pertence x list

-- 7. nro-elementos: recebe uma lista qualquer e retorna o número de elementos na
-- lista
-- obs.: não usar a função length
nroElementos :: [Int] -> Int
nroElementos [] = 0
nroElementos (elm : list) = 1 + nroElementos list

-- 8. maior: recebe uma lista de números e retorna o maior
-- obs.: não usar a função max
maior :: [Int] -> Int
maior [] = 0
maior (elm : list) = _maiorAux elm list

_maiorAux :: Int -> [Int] -> Int
_maiorAux x [] = x
_maiorAux x (elm : list) | x > elm = _maiorAux x list
                         | otherwise = _maiorAux elm list

-- 9. conta-ocorrencias: recebe um elemento e uma lista qualquer, retorna o número
-- de ocorrências do elemento na lista
contaOcorrencias :: Int -> [Int] -> Int
contaOcorrencias _ [] = 0
contaOcorrencias x (elm : list) = (if x == elm then 1 else 0) + contaOcorrencias x list

-- 10. unica-ocorrencia: recebe um elemento e uma lista e verifica se existe uma única
-- ocorrência do elemento na lista
-- ex.:
-- unica-ocorrencia 2 [1,2,3,2] = False
-- unica-ocorrencia 2 [3,1] = False
-- unica-ocorrencia 2 [2] = True
unicaOcorrencia :: Int -> [Int] -> Bool
unicaOcorrencia x list = (contaOcorrencias x list) == 1

-- 11. maiores-que: recebe um número e uma lista de números, retorna uma lista com
-- os números que são maiores que o fornecido
-- ex.:
-- (maiores-que 10 '(4 6 30 3 15 3 10 7)) ==> (30 15)
maioresQue :: Int -> [Int] -> [Int]
maioresQue _ [] = []
maioresQue x (elm : list) | elm > x = elm : maioresQue x list
                          | otherwise = maioresQue x list

-- 12. concatena: recebe duas listas quaisquer e retorna uma terceira lista com os
-- elementos da primeira no início e os elementos da segunda no fim
-- ex.:
-- (concatena '() '()) ==> ()
-- (concatena '(1 2) '(3 4)) ==> (1 2 3 4)
concatena :: [Int] -> [Int] -> [Int]
concatena list [] = list
concatena [] list = list
concatena (elm : list) list2 = elm : concatena list list2

-- 13. remover: recebe um elemento e uma lista e retorna a lista sem a primeira
-- ocorrência do elemento
remover :: Int -> [Int] -> [Int]
remover _ [] = []
remover x (elm : lista) | x == elm = lista
                        | otherwise = elm : remover x lista

-- 14. remover-ultimo: recebe uma lista e remove o último elemento da lista
removerUltimo :: [Int] -> [Int]
removerUltimo [] = []
removerUltimo [elm] = []
removerUltimo (elm : list) = elm : removerUltimo list

-- 15. remover-repetidos: recebe uma lista e retorna outra lista sem repetição de
-- elementos
-- ex.:
-- (remover-repetidos '(7 4 3 5 7 4 4 6 4 1 2)) ==> (7 4 3 5 6 1 2)
removerRepetidos :: [Int] -> [Int]
removerRepetidos [] = []
removerRepetidos (elm : list)
  | contaOcorrencias elm list > 0 = elm : removerRepetidos (remover elm list)
  | otherwise = elm : removerRepetidos list

-- 16. maiores: recebe um numero natural n e uma lista de números, retorna uma lista
-- com os n maiores números sem alterar a ordem entre os elementos
-- ex.:
-- (maiores 4 '(9 3 5 7 8 4 4 7)) ==> (9 7 8 7)
maiores :: Int -> [Int] -> [Int]
maiores _ [] = []
maiores 0 _ = []
maiores n (elm : list)
  | n > length (maioresQue elm list) = elm : maiores (n-1) list
  | otherwise = maiores n list

-- 17. gera-sequencia: recebe um número inteiro n positivo e retorna a lista (1 -1 2
-- -2 3 -3 ... n -n)
geraSequencia :: Int -> [Int]
geraSequencia 1 = [1,-1]
geraSequencia 2 = [1,-1,2,-2]
geraSequencia n = intercala [1,2..n] [-1,-2..n*(-1)]

-- 18. inverte: recebe uma lista e retorna outra, que contém os mesmos elementos da
-- primeira, em ordem invertida
inverte :: [Int] -> [Int]
inverte [] = []
inverte (elm : list) = inverte list ++ [elm]

-- 19. divide: recebe uma lista e um número natural n, retorna um par onde o primeiro
-- elemento é uma lista com os n primeiros números da lista original e o segundo
-- elemento é uma lista com o resto dos elementos da lista original
-- ex.:
-- (divide '(1 2 3 4) 0) ==> (() 1 2 3 4)
-- (divide '(1 2 3 4) 2) ==> ((1 2) 3 4)
divide :: [Int] -> Int -> ([Int], [Int])
divide [] _ = ([],[])
divide list 0 = ([], list)
divide (elm : list) n = ( elm : fst ( divide list (n-1) ), snd ( divide list (n-1) ) )

-- 20. intercala: recebe duas listas e retorna outra lista com os elementos das listas
-- originais intercalados.
-- ex.:
-- (intercala '(1 2 3) '(8 9)) ==> (1 8 2 8 3)
-- (intercala '() '(1 2 6)) ==> (1 2 6)
intercala :: [Int] -> [Int] -> [Int]
intercala [] list = list
intercala list [] = list
intercala (a:as) (b:bs) = a : b : intercala as bs

-- 21. uniao: recebe duas listas que não contenham elementos repetidos e retorna
-- uma nova com todos os elementos das duas listas originais (sem repetição)
-- ex.:
-- (uniao '(3 6 5 7) '(2 9 7 5 1)) ==> (3 6 5 7 2 9 1)
uniao :: [Int] -> [Int] -> [Int]
uniao [] list = list
uniao list [] = list
uniao (a:as) bs
    | contaOcorrencias a bs > 0 = a : uniao as (remover a bs)
    | otherwise = a : uniao as bs

-- 22. interseccao: recebe duas listas sem elementos repetidos e retorna uma lista
-- com os elementos que são comuns às duas
-- ex.:
-- (interseccao '(3 6 5 7) '(9 7 5 1 3)) ==> (3 5 7)
interseccao :: [Int] -> [Int] -> [Int]
interseccao [] list = []
interseccao list [] = []
interseccao (a:as) bs
    | contaOcorrencias a bs > 0 = a : interseccao as bs
    | otherwise = interseccao as (remover a bs)

-- 23. sequencia: recebe dois numeros naturais n e m, e retorna uma lista com n
-- elementos, onde o primeiro é m, o segundo é m+1, etc...
-- ex.:
-- (sequencia 0 2) ==> ( )
-- (sequencia 3 4) ==> (4 5 6)
sequencia :: Int -> Int -> [Int]
sequencia 0 _ = []
sequencia n m = [m..m+n-1]

-- 24. insere-ordenado: recebe uma lista de números em ordem crescente e um
-- número qualquer, retorna uma lista de números em ordem crescente com os
-- elementos da lista inicial mais o número passado.
insereOrdenado :: [Int] -> Int -> [Int]
insereOrdenado [] n = [n]
insereOrdenado (a:as) n
    | n > a = a : insereOrdenado as n
    | otherwise = n : a : as

-- 25. ordenado?: recebe uma lista de números e verifica se eles estão ordenados ou
-- não
isOrdenado :: [Int] -> Bool
isOrdenado [a] = True
isOrdenado (a : as) = a < head as && isOrdenado as

-- 26. ordena: recebe uma lista com números e retorna outra lista com os números
-- ordenados
-- ex.:(ordena '(7 3 5 7 8 4 4)) ==> (3 4 4 5 7 7 8)
ordena :: [Int] -> [Int]
ordena [] = []
ordena (a : as) = ordena smaller ++ (a : ordena greatter)
    where
        smaller = [n | n<-as, n <= a]
        greatter = [n | n<-as, n > a]

-- 27. rodar-esquerda: recebe um número natural, uma lista e retorna uma nova lista
-- onde a posição dos elementos mudou como se eles tivessem sido "rodados"
-- ex.:
-- (rodar-esquerda 0 '(a s d f g)) ==> (a s d f g)
-- (rodar-esquerda 1 '(a s d f g)) ==> (s d f g a)
-- (rodar-esquerda 3 '(a s d f g)) ==> (f g a s d)
-- (rodar-esquerda 4 '(a s d f g)) ==> (g a s d f)
rodarEsquerda :: Int -> [t] -> [t]
rodarEsquerda 0 as = as
rodarEsquerda n (a:as) = rodarEsquerda (n-1) (as ++ [a])

-- 28. rodar-direita: recebe um número natural, uma lista e retorna uma nova lista
-- onde a posição dos elementos mudou como se eles tivessem sido "rodados"
-- ex.:
-- (rodar-direita 0 '(a s d f g)) ==> (a s d f g)
-- (rodar-direita 1 '(a s d f g)) ==> (g a s d f)
-- (rodar-direita 3 '(a s d f g)) ==> (d f g a s)
-- (rodar-direita 4 '(a s d f g)) ==> (s d f g a)
rodarDireita :: Int -> [t] -> [t]
rodarDireita 0 as = as
rodarDireita n as = rodarDireita (n-1) (last as : init as)

-- 29. todas-maiusculas: Recebe uma string qualquer e retorna outra string onde todas
-- as letras são maiúsculas. Pode ser útil saber os seguintes códigos ASCII:
-- a=97, z=122, A=65, Z=90, 0=48, 9=57, espaço=32.
-- ex.: todas-maiusculas "abc 123" = "ABC 123"
todasMaiusculas :: [Char] -> [Char]
todasMaiusculas [] = []
todasMaiusculas (a:as) = upperCase a : todasMaiusculas as
    where

upperCase :: Char -> Char
upperCase c
    | c >= 'a' && c <= 'z' = (toEnum(fromEnum c - 32)::Char)
    | otherwise = c

-- 30. primeiras-maiusculas: recebe uma string qualquer e retorna outra string onde
-- somente as iniciais são maíusculas
-- ex.:
-- (primeiras-maiusculas "FuLaNo bElTrAnO silva") ==>
-- "Fulano Beltrano Silva"
primeirasMaiusculas :: [Char] -> [Char]
primeirasMaiusculas [] = []
primeirasMaiusculas string = _aux 0 string
    where
        -- Quando deep é 0, deixa a primeira letra maiúscula, quando não, minúscula
        _aux _ [] = []
        _aux 0 (c : string) = upperCase c : _aux 1 string
        _aux depth (c : string)
            | c == ' ' = c : _aux 0 string
            | depth > 0 = lowerCase c : _aux (depth+1) string

lowerCase :: Char -> Char
lowerCase c
    | c >= 'A' && c <= 'Z' = (toEnum(fromEnum c + 32)::Char)
    | otherwise = c

-- 31. seleciona: recebe uma lista qualquer e uma lista de posições, retorna uma lista
-- com os elementos da primeira que estavam nas posições indicadas
-- ex.:
-- (seleciona '(a b c d e f) '(0 3 2 3)) ==> (a d c d)
seleciona :: [t] -> [Int] -> [t]
seleciona [] _ = []
seleciona _ [] = []
seleciona as bs = map (\ b -> as !! b ) bs
-- 32. palindrome?: recebe uma string e verifica se ela é uma palíndrome ou nao
-- ex.:
-- (palindrome? "ana") ==> #t
-- (palindrome? "abbccbba") ==> #t
-- (palindrome? "abbdbbaa") ==> #f


-- 33. primo?: verifica se um número é primo ou não


-- 34. soma-digitos: recebe um número natural e retorna a soma de seus dígitos
-- ex.:
-- (soma-digitos 328464584658) ==> 63


-- 35. bolha: recebe uma lista de números e retorna a lista ordenada, pelo método da
-- bolha (bolha burra)


-- 36. compactar: recebe uma lista de números e transforma todas as repetições em
-- sub-listas de dois elementos: sendo o primeiro elemento o número de
-- repetições encontradas e o segundo elemento é o número que repete na lista
-- original. Os números que não repetem na lista original não devem ser alterados.
-- ex.:(compactar '(2 2 2 3 4 4 2 9 5 2 4 5 5 5)) ==> ((3 2) 3 (2 4) 2 9 5 2 4 (3 5))
-- Em Haskell, como não é possível implementar listas heterogêneas, a função
-- deve retornar uma lista de listas. Ex.:
-- compactar [2,2,2,3,4,4,2,9,5,2,4,5,5,5] = [[3,2],[3],[2,4],[2],[9],[5],[2],[4],[3,5]]


-- 37. Faça um programa que dada uma lista, retorne uma tupla lista-lista (de inteiros)
-- onde a lista da esquerda contém os números impares e a lista da direita os
-- números pares
-- ex:
-- func :: [Int] -> ([Int],[Int])
-- [1,2,3,4,5,6,7] => ([1,3,5,7],[2,4,6])


-- 38. Dizemos que um quadrado perfeito é um numero cuja raiz quadrada é um
-- número inteiro. Sabemos o que a raiz quadrada é um cálculo lento quando
-- comparado à operações como adição ou multiplicação. Implemente uma função
-- que verifica se um número é um quadrado perfeito sem usar uma função que
-- calcula raiz quadrada.


-- 39. Faça um programa que encontra a representação de um número natural numa
-- base b qualquer (1 < b < 37). Exemplo:
-- (muda_base 17 2) ==> "10001"(muda_base 26 16) ==> "1A"


-- 40. O conjunto de todos os subconjuntos de um segundo conjunto é denominado
-- conjuntos das partes desse segundo conjunto. Faça um programa que encontra
-- o conjunto das partes de uma lista. Exemplo:
-- partes [2,3,2,31] = [[],[2],[3],[31],[2,2],[2,3],[2,31],[3,31],[2,2,3],[2,2,31],
-- [2,3,31],[2,2,3,31]]
