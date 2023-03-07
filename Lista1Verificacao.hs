import Lista1

ex1 =  [ehTriangulo 10 20 30  == False, ehTriangulo 5 3 3 == True, 
        ehTriangulo 5 3 4 == True]

ex2 =  [tipoTriangulo 5 5 5 == "equilatero", tipoTriangulo 5 3 3 == "isosceles", 
        tipoTriangulo 5 3 4 == "escaleno"]
        
ex3 =  [triangulo 5 5 5 == "equilatero", triangulo 5 3 3 == "isosceles", 
        triangulo 5 3 4 == "escaleno", triangulo 15 6 5 == "nao eh um triangulo"]
        
ex4 = [somaPares 8 == 20, somaPares 5 == 6]

ex5 = [somaPot2m 6 4 == 186, somaPot2m 3 3 == 45]

ex6 = [primo 37 == True, primo 9 == False]

ex7 = [abs (pi - seriePI 100) < 0.1, abs (pi - seriePI 10000) < 0.001] 
        
  
ex = zip [1..] $ map (foldr (&&) True) [ex1, ex2, ex3, ex4, ex5, ex6, ex7]
  
verifica [] = putStrLn ""
verifica ((n, t): nts) = do putStrLn ("Exercicio " ++ show n ++
                                        if t then " Ok" else " Falhou")
                            verifica nts                                        
      
main = verifica ex 

 