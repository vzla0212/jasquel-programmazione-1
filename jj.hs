module Main where
import List
import Char
data Figura = Redonda | Blanca | Negra | Corchea
type Nota = (Int, Figura)
type Compas = [Nota]
data Armadura = C | G | D | A
type Tiempo = (Int, Int)
type Musica = (Armadura, Tiempo, [Compas])

instance Show Figura where
  show Redonda = "Redonda"
  show Blanca = "Blanca"
  show Corchea = "Corchea"
  show Negra = "Negra"

instance Show Armadura where
  show A = "A"
  show C = "C"
  show G = "G"
  show D = "D"

compases a b = compasesp a b 0 [] []

{-
a lista de notas
b tiempo
c contador que lleva el tiempo de cada compas
d lista donde se almacena el compas parcial
e lista donde se almacenan los compases
-}
--FIRMA
compasesp [] b c d e = return (reverse(reverse(d):e))
compasesp a b c d e = if c < (fst b)*2
                     then  compasesp (tail a) b (c+dtiempo(head a)) ((head a):d) e 
                     else  compasesp a        b 0                      []         ((reverse d):e)
{-
a compases
b los sostenido afectados por la armadura
c lista de compases
-}
--FIRMA
foo1 a b c = reverse $ foldl (\acc x -> (do {l<-(foo a b x []); l:acc})) [] c


{-
a compases
b los sostenidos afectados por la armadura
c compas sobre el cual estamos operando
e compas salida

Ante las modificaciones propuestas, en la cual se agregaba un constructor para manejar los becuadros, preferimos usar la
solución que nosotros planteamos, la cual es explicada en el siguiente parrafo:

Los becuadros van a ser representados por el entero midi más un entero razonable (100) que no entre en conflicto con el 
conjunto de notas planteada en el enunciado que va desde 60 a 77; al ejecutar la operación de adición producirá enteros
en el rango de 160 a 177 y este rango es disjunto al rango de notas, lo cual no ocasionara ningún tipo de mezcla entre
ellas.

Para lograr una mayor comprensión de lo anterior planteado, se exhiben los sguientes puntos:

0) al restar 100 al entero midi que representa al becuadro se obtiene la nota sin ningún tipo de alteración.
1) al restar 99 al entero midi que representa al becuadro se obtiene la nota, esta vez con la alteración de sostenido.
-}
--FIRMA
foo a b [] e = return $ reverse e            
foo a b c e = if elem (fst(head c)) a
                 then  foo a b (tail c) ((fst(head c)-1,(snd (head c))):e)
                 else  if elem (fst(head c)) b
                       then foo ((fst(head c)):a) b (tail c) ((head c):e)
                       else if ((fst(head c) > 100) && (elem (fst(head c)-99)  a))
                            then foo (delete ((fst(head c))-99) a) b (tail c) ((head c):e)
                            else foo a b (tail c) ((head c):e

{-
a string de la entrada que representa el tiempo

Devuelve un Tiempo
-}
tiempo :: String -> Tiempo
tiempo a = (digitToInt (head a ) , 4)

{
n Figura

Devuelve el tiempo de una Figura
}
dtiempo :: Nota -> Int
dtiempo n= 
        case snd n of
                Redonda -> 8
                Blanca -> 4
                Negra -> 2
                Corchea -> 1

{
a Lista de Strings de notas

Devuelve una lista de Notas
}
notas :: [String]->[Nota]
notas f = map nota f

{
x String de una nota

Devuelve una Nota
}
nota :: String -> Nota
nota x = (midi x, case digitToInt $ last x of{1->Redonda;2->Blanca;4->Negra;8->Corchea}) 

{}
midi :: String -> Int
midi x = midis x 0


midis :: Num a => [Char] -> a -> a
midis [] i =  i
midis x i =
        case toUpper(head x) of
                'C'-> midis (tail x) (60+i)
                'D'-> midis (tail x) (62+i)
                'E'-> midis (tail x) (64+i)
                'F'-> midis (tail x) (65+i)
                'G'-> midis (tail x) (67+i)
                'A'-> midis (tail x) (69+i)
                'B'-> midis (tail x) (71+i)
                '%'-> midis (tail x) (100+i)
                '#'-> midis (tail x) (1+i)
                '\''-> midis (tail x) (12+i)
                otherwise -> midis (tail x) i

armadura :: String -> Armadura
armadura l = 
        case l of
                 "A"-> A
                 "C"-> C
                 "D"-> D
                 "G"-> G

partirEntrada :: FilePath -> IO (String, (String, [String]))
partirEntrada b = do{ x <- readFile b;
                      return (head(words x), (head(tail(words x)), tail(tail(words x))))}

main = do
        x <- partirEntrada "g"
        u <- compases (notas(snd (snd x))) (tiempo(fst(snd x)))
        print $ (foo1 [78,73,66,61] [61,63,66,68,70,73,75,78] u)
        --print $ foo [78,73,66,61] [61,63,66,68,70,73,75,78] [(68,Negra),(68,Negra)]
       -- print y
        --case snd(nota(head(snd(snd(x))))) of
                --Negra -> print "negra"

--        print $ fst (nota(head(snd(snd(x)))))
--        print $ nota(head(snd(snd(x))))
--        print $ tiempo(fst(snd x))
         --print (fst y);
         --print $ fst (snd y));
         --print $ snd (snd y) }
