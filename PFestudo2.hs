module PFestudo2 where

---------Ficha 3

data Hora = H Int Int
       deriving Show

type Etapa = (Hora,Hora)
type Viagem = [Etapa]



etapaboa :: Etapa -> Bool
etapaboa (H h m, H h1 m1)
      | h1 > h = True
      | h == h1 && m1 > m = True
      | otherwise = False


viagemboa :: Viagem -> Bool
viagemboa [ ] = False
viagemboa ((H h1 m1, H h2 m2):(H h3 m3, H h4 m4):xs) 
    | etapaboa (H h1 m1, H h2 m2) || etapaboa (H h3 m3, H h4 m4) == False = False
    | h3 > h2 = True
    | h3 == h2 && m3 > m2 = True
    | otherwise = False

---- supomos que a viagem é valida

tempodeViagem :: Viagem -> Int
tempodeViagem [ ] = 0
tempodeViagem ((H h1 m1, H h2 m2):xs) = ((horaparaminuto (H h2 m2)) - (horaparaminuto (H h1 m1))) + tempodeViagem (xs)


horaparaminuto :: Hora -> Int
horaparaminuto (H h1 m1) = (h1*60) + m1

tempodeEspera :: Viagem -> Int
tempodeEspera ((H h1 m1, H h2 m2):(H h3 m3, H h4 m4):xs) = ((horaparaminuto (H h3 m3)) - (horaparaminuto (H h2 m2))) + tempodeEspera ((H h3 m3, H h4 m4):xs)
tempodeEspera _ = 0


esperaeviagem :: Viagem -> Int
esperaeviagem [ ] = 0
esperaeviagem v = tempodeEspera v + tempodeViagem v


data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String
              deriving Show

type Nome = String
type Agenda = [(Nome, [Contacto])]


acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail n e [ ] = [(n, [Email e])]
acrescEmail n mail ((n1, (e)):nx)
            | n == n1 = (n1, Email mail : e ): nx
            | otherwise = acrescEmail n mail nx

verEmails :: Nome -> Agenda -> Maybe [String]
verEmails n [ ] = Nothing
verEmails n ((n1, (Email e)): resto2)
         | n == n1 = Just e
         | otherwise = verEmails n resto2
