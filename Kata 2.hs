module Library where
import PdePreludat

type CriterioPropina = Number -> Number

cuantoPagar :: Number -> CriterioPropina -> Number
cuantoPagar precio criterioPropina = precio + criterioPropina precio

criterioPropinaTioCarlos :: String -> CriterioPropina
criterioPropinaTioCarlos "Carlos" precio = 2 * criterioPropinaRecomendada precio 
criterioPropinaTioCarlos _ precio = 0.5 * criterioPropinaRecomendada precio 

criterioPropinaRecomendada ::CriterioPropina
criterioPropinaRecomendada = (*0.1)

criterioPropinaMrPink :: CriterioPropina
criterioPropinaMrPink _ = 0

criterioPropinaConservadora :: CriterioPropina
criterioPropinaConservadora precio 
    | esImpar precio = 25
    | otherwise = 20

esImpar :: Number -> Bool
esImpar = (odd . truncate)

criterioPropinaPayasos :: String -> CriterioPropina
criterioPropinaPayasos "azul" precio = criterioPropinaRecomendada precio
criterioPropinaPayasos "rojo" precio = 100
criterioPropinaPayasos "negra" precio = 0
criterioPropinaPayasos _ precio = 0

criterioRojo :: CriterioPropina 
criterioRojo _ precio = 100

criterioPersonasRebuscadas :: CriterioPropina 
criterioPersonasRebuscadas cuenta | validador cuenta = criterioPropinaRecomendada cuenta * 3
                                  | validador cuenta = criterioPropinaConservadora

validador :: Number -> CriterioPropina
validador cuenta | cuenta <= 500
                 | cuenta > 500 && cuenta < 1000
                 |otherwise > 1000

criterioSatisfaccionmozo :: Number -> Bool
criterioSatisfaccionMozo | criterioPropina > criterioPropina * (0.15) 
criterioSatisfaccionMozo | otherwise criterioPropina < criterioPropina * (0.15) 