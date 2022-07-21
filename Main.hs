import Data.List
-- Trabalho final da disciplina PLP 2021.3--
-- --------------------------------------------------------
-- Aluno: Kalil Saldanha Kaliffe
-- 1) Aplicação parcial de funções. Considere as duas versões
-- abaixo da função "potencia"/"pot":

--a)
potencia :: (Int,Float) -> Float
potencia (exp, base) =  
   if exp == 0 
   then 1.0 
   else base * potencia (exp-1,base)


pot :: Int -> Float -> Float
pot exp base =  
   if exp == 0 
   then 1.0 
   else base * pot (exp-1) base
-- Explique a diferença entre as duas DECLARAÇÕES DE TIPO das funções
-- Qual a consequência dessas definições em termos de seu uso?
-- A função pot espera um tipo Int e então um tipo Float na sua declaração ou seja, espera 2 argumentos, então pode ser usada assim: pot 3 2.5 ; enquanto a função potencia espera 1 argumento (Int,Float) que contem um Int e um Float

-- explique o significado, e forma de uso, de: 
pot5 = pot 5
-- é possível fazer o mesmo com a função "potencia" ?
-- Como a função é dividia em 2 argumentos o primeiro pode ser aplicado com um valor default
-- Não já que não é possivel dividir o Int e o Float uma vez que são parte do mesmo argumento, a tupla. 

-- Crie um exemplo de aplicação parcial de função envolvendo
-- uma função aplicFuncLst, que receba como parâmetros uma 
-- função f e uma lista, e aplique f a cada elemento da lista.
-- Na sequência, crie  um exemplo de aplicação parcial
--A função aplicFuncLst vai ser a multLista que multiplica cada um dos itens da lista por f

multLista f [] = [] 
multLista f (x:xs) = do 
                        let j = x
                        let x = j*f
                        x:ys
                        where
                        ys = multLista f xs
                   
-- 2) Desenvolva uma função que calcule o valor do enésimo elemento 
-- da sequência de Fibonacci     n   0  1  2  3  4  5  6  7
--                           fin n [ 0, 1, 1, 2, 3, 5, 8, 13... ]
-- a) usando if
-- b) usando pattern matching
-- Teste as funções, calculando o fibonacci de 30, 32, etc.
-- Houve problema de desempenho? Crie uma segunda função,
-- fibonacci, que solucione esse problema. Uma soleção seria
-- criar uma "lista infinita" da sequencia de Fibonacci?

--Versão lenta:
fib1 :: Int -> Int
fib1 0 = 0
fib1 1 = 1
fib1 n =  fib1 (n-1) + fib1 (n-2)

--Versão Fib mais rapida, cria uma lista infinita 
fib2 n j = n:fib2 j (n+j)


-- 3) Considere as funções abaixo apresentadas em aula  

-- função que testa se x eh primo
-- COMPLETAR: ESPECIFICAR AQUI O TIPO DA FUNÇÃO

ehPrimo :: Int -> Bool
--ehPrimo x = null [x' | x' <- [2..x-1], x `rem` x' == 0]
--ehPrimo estava contabilizando 1 como primo, versão correta abaixo:
ehPrimo x = if x > 1 then null [x' | x' <- [2..x-1], x `rem` x' == 0] else False

-- função que cria uma lista "infinita" com valores não menores que n
-- ESPECIFICAR AQUI O TIPO DA FUNÇÃO
from :: Int -> [Int]
from n = n : from (n+1)

-- função que acha o primeiro primo na lista
-- ESPECIFICAR AQUI O TIPO DA FUNÇÃO
primPrimo :: [Int] -> Int
primPrimo (n : ns) =
    if ehPrimo n then n else primPrimo ns

-- Desenvolva funções similares para: 
-- a) criar uma lista infinita de pares a partir de certo valor n (par);
pares :: Int -> [Int]
pares n = [n, n+2..]

-- b) criar a função que forneça o primeiro valor dessa lista que seja
-- múltiplo de 25
-- OBS: especificar os tipos das funções
--
mult25 :: Int -> Bool
mult25 x = if (x `rem` 25 == 0) then True else False

primMult25 :: [Int] -> Int  
primMult25 (n : ns) =
    if mult25 n then n else primMult25 ns

-- Cite (usando um comentário no próprio código) o conceito de 
-- Haskell que permite a criação de uma lista infinita sem produzir
-- um ciclo infinito de chamadas. Explique esse conceito

-- O que permite isso em Haskell é a lazy evaluation já que, ela só avalia uma parte finita da lista infinita, uma vez que não precisa avaliar a lista infinita toda o que precisaria de um tempo infinito


-- 4) Considere a função qsort abaixo, que usa como pivô a
-- cabeça da lista (sugestão: teste a mesma no GHC)

qsort :: [Int] -> [Int]
qsort [] = []
qsort (n : ns) =
   qsort [i | i <- ns, i < n]
   ++ [n]
   ++ qsort [i | i <- ns, i >= n]


-- Abaixo exploramos o conceito de função genérica
-- ao invés de usarmos o operador "<" usamos uma
-- função genérica precede  

genericSort :: (t -> t -> Bool) -> [t] -> [t]
genericSort precede =
   let
      sort [] = []
      sort (n : ns) =
         sort [i | i <- ns, i `precede` n]
            ++ [n]
            ++ sort [i | i <- ns, not (i `precede` n)]
   in sort

-- assim, para produzirmos uma função para ordenar inteiros
-- criamos uma instância da função genérica. Nesse caso,
-- o operador "<" tomará o lugar do "precede"

ordenaInt :: [Int] -> [Int]
ordenaInt = genericSort (<)    --  funciona?
-- Sim já que `precede` pode ser substituido < e então por ser executado por exemplo ordenaInt [6,2,4,7,3,3,1]
-- 4.a)Crie outra instância de genericSort, ordenaRac para ordenar
-- uma lista de números racionais, isto é, aqueles que são escritos
-- em forma de fração. Por exemplo, a partir da lista
-- [ [1,2], [1,3], [1,5], [9,10], [1,10] ]
-- será retornada a lista ordenada abaixo (de formna ascendente)
-- [[1,10],[1,5],[1,3],[1,2],[9,10]]
-- OBS:  [1,10]  represente 1 décimo (1/10), etc.
--

-- defina a função "menorRac", que define se o primeiro precede
-- (é menor que) o segundo 

-- OBS: a função genericSort teria que ser modificada para trabalhar com as tuplas, então optei por usar a função menorRac como um comparador para o sortBy que recebe um comparador

menorRac (a1, b1) (a2, b2)
          | (a1 * b2) > (b1 * a2) = GT
          | (a1 * b2)  == (a1 * b2) = EQ
          | otherwise = LT          

ordenaRac = sortBy menorRac

-- 4.b) Agora considere uma lista que guarde as seguintes 
--  informações de n alunos: matrícula (Int), nome (String), 
--  e CRG (Float). Ex:
--  [[3,"Leôncio", 9.3],[9,"Paulina", 9.8],[7,"Bernardo", 8.9] ...]
--
--  - defina a função "precedeAl", que define qual aluno vem
--  antes de acordo com sua matrícula (ord. ascendente). 
--  -- defina a função ordenaAl como instância de "genericSort"
--  para produzir a lista ordenada pela matrícula


-- O ordenaAl pela matricula ainda não precisa de precedeAl já que a matricula é o primeiro item da tupla

ordenaAl = genericSort (<)            

--  - defina a função "precedeAl2", que define qual aluno vem
--  antes de acordo com seu CRG (ord. ascendente). 
--  Crie a função ordenaAl2 como instância de "genericSort"
--  para produzir a lista ordenada pelo CRG

precedeAl2 (a1, b1, c1) (a2, b2, c2)
          | c1 > c2 = GT
          | c1 == c2 = EQ
          | otherwise = LT

ordenaAl2 = sortBy precedeAl2 

main = do
    putStr "Pot:"
    print $ pot5 5

    putStr "AParcial multLista:"
    print $ multLista 2 [2,3,4,5,6]

    putStr "Fib1:"
    print $ fib1 30
    
    putStr "Fib2:"
    --take 31 para dar um break na lista infinita e fazer dela uma lista finita  
    print $ take 31 (fib2 0 1)

    putStr "ehPrimo:"
    print $ ehPrimo 3

    putStr "from:"
    --take 100 para dar um break na lista infinita e fazer dela uma lista finita
    print $ take 100 (from 7)

    putStr "primPrimo:"
    print $ primPrimo [1,4,6,10,3,4,5,10]

    putStr "pares:"
    --take 30 para dar um break na lista infinita e fazer dela uma lista finita
    print $ take 30 (pares 4)

    putStr "primMult25:"
    print $ primMult25 [1, 24, 70, 50]
    
    putStr "genericSort:"
    print $ ordenaInt [3,1,4,5]
    
    putStr "ordenaRac:"
    print $ ordenaRac [(1,2),(1,3),(1,5),(9,10),(1,10)]

    putStr "ordenaAl:"
    print $ ordenaAl [(3,"Leôncio", 9.3),(9,"Paulina", 9.8),(7,"Bernardo", 8.9),(10,"Renato",9.9),(2,"Mauricio",7.7)]

    putStr "ordenaAl2:"
    print $ ordenaAl2 [(3,"Leôncio", 9.3),(9,"Paulina", 9.8),(7,"Bernardo", 8.9),(10,"Renato",7.5),(2,"Mauricio",9.9)]

