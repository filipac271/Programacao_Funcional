module Aula7 where

f l = takeWhile (\x -> odd x)  --odd ve se o numero é impar

g l = dropWhile (\x -> odd x) -- larga os elementos até a condição ser falsa, i.e. fica com os elementos a partir do qual uma das condições deu falso

h l = span (\x -> odd x) -- (takeWhile, dropWhile)

k l = filter (\x -> even x) -- dá todos os elementos que sao pares

maisNorte lcoordenadas = maximum (map (\(x,y) -> y ) lcoordenadas)  -- queremos o ponto mais a norte então queremos o maximo das segundas corrdenadas 
                                                                    -- o map vai bucar o segundo valor de todos os pontos e depois o maximum faz o maximo

--zipWith (+) [1..10] [2..] vai somar os elementos das duas listas [1+2,2+3,3+4.....]

--zipWith (\x c -> (c,x)) [1..5] ['a','b'..] vai dar uma lista de pares do tipo [('a',1), ('b',2).....]

--zipWith (\x c -> chr (ord c +x)) [1..] ['a'..'e'] vai dar uma lista em que os caracteres avançam x, o a passa a b porque a ord de a fica ord de a +1

-- any odd [2,4,6] verifica se há algum numero que é impar

-- all odd [2,4,6] verifica se são todos impares, o que não são então vai dar false
