
vendas :: Int -> Int
vendas 0 = 36
vendas 1 = 22
vendas 2 = 53
vendas 3 = 40
vendas 4 = 22

checkVendas :: Int -> Int -> Int
checkVendas s n
  | n == 0 && vendas 0 == s = 1
  | n == 0 && vendas 0 /= s = 0
  | n /= 0 && vendas n == s = 1 + checkVendas s (n-1)
  | n /= 0 && vendas n /= s = 0 + checkVendas s (n-1)

maxi :: Int -> Int -> Int
maxi n m 
  | n >= m = n
  | otherwise = m

totalVendas :: Int -> Int
totalVendas n 
  | n == 0 = vendas 0
  | otherwise = totalVendas (n-1) + vendas n

maxVendas :: Int -> Int
maxVendas n 
  | n == 0 = vendas 0
  | otherwise = max (maxVendas (n-1)) (vendas n)
    
-- String = [Char]
-- Estruturas de dados recursivas

myLostList :: [Int]
myLostList = [1,2,3,4,5,6,7,8]

sumList as
  | as == [] = 0
  | otherwise = (head as) + sumList(tail as)
  
-- Doubles the element of a list
double :: [Int] -> [Int]
double ak
  | ak == [] = []
  | otherwise = head ak *2 : double(tail ak)
  
membership :: [Int] -> Int -> Bool
membership ab v
  | tail ab == [] && head ab == v = True
  | tail ab == [] && head ab /= v = False
  | otherwise = membership([head ab]) v || membership(tail ab) v 

digits :: String -> String
digits x
  | x == [] = []
  | head x >= '0' && head x <= '9' = digits (tail x)
  | otherwise = head x : digits(tail x)

sumPairs :: [Int] -> [Int] -> [Int]
sumPairs a b
  | length a /= length b = error "Diferent list size"
  | a == [] || b == [] = []
  | otherwise = head a + head b : sumPairs (tail a) (tail b)

maiorLista :: [Int] -> Int
maiorLista [] = minBound :: Int
maiorLista [x] = x

fat :: Int -> Int
fat x
  | x == 0 = 1
  | otherwise = fat (x-1) * x

fib :: Int -> Int
fib x
  | x == 0 = 0
  | x == 1 = 1
  | otherwise = fib(x-1) + fib(x-2) 

listFib :: Int -> Int -> [Int]
listFib n x 
  | n == 0 = []
  | mod (fib (x)) 2 == 0 = fib(x):listFib (n-1) (x+1)
  | otherwise = listFib(n) (x+1) 

