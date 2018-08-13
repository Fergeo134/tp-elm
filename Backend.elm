module Backend exposing(..)
import Models exposing(Movie, Preferences)

completaAca = identity

-- **************
-- Requerimiento: filtrar películas por su título a medida que se escribe en el buscador;
-- Finalizado ok!
-- **************

filtrarPeliculasPorPalabrasClave : String -> List Movie -> List Movie
filtrarPeliculasPorPalabrasClave palabras = List.filter (peliculaTienePalabrasClave palabras)

-- esta función la dejamos casi lista, pero tiene un pequeño bug. ¡Corregilo!
--
-- Además tiene dos problemas, que también deberías corregir:
--
-- * distingue mayúsculas de minúsculas, pero debería encontrar a "Lion King" aunque escriba "kINg"
-- * busca una coincidencia exacta, pero si escribís "Avengers Ultron" debería encontrar a "Avengers: Age Of Ultron"
--

peliculaTienePalabrasClave: String -> Movie -> Bool
peliculaTienePalabrasClave palabras pelicula = List.all (peliculaTienePalabraClave pelicula) (String.words palabras)

peliculaTienePalabraClave: Movie -> String -> Bool
peliculaTienePalabraClave pelicula palabra = String.contains (String.toUpper palabra) (String.toUpper pelicula.title)

-- **************
-- Requerimiento: visualizar las películas según el género elegido en un selector;
-- Finalizado ok! ver donde tipea los generos en el select
-- **************

filtrarPeliculasPorGenero : String -> List Movie -> List Movie
filtrarPeliculasPorGenero genero = List.filter (esPeliculaDelGenero genero)

esPeliculaDelGenero : String -> Movie -> Bool
esPeliculaDelGenero genero pelicula = List.any ((==) genero) pelicula.genre

-- **************
-- Requerimiento: filtrar las películas que sean aptas para menores de edad,
--                usando un checkbox;
-- Finalizado ok!
-- **************

filtrarPeliculasPorMenoresDeEdad : Bool -> List Movie -> List Movie
filtrarPeliculasPorMenoresDeEdad mostrarSoloMenores = List.filter (esPeliculaParaMenores mostrarSoloMenores)

esPeliculaParaMenores : Bool -> Movie -> Bool
esPeliculaParaMenores mostrarSoloMenores pelicula = (mostrarSoloMenores == pelicula.forKids)

-- **************
-- Requerimiento: ordenar las películas por su rating;
-- Finalizado ok! de menor a mayor
-- **************

ordenarPeliculasPorRating : List Movie -> List Movie
ordenarPeliculasPorRating = List.sortBy .rating

-- **************
-- Requerimiento: dar like a una película
-- **************

darLikeAPelicula : Int -> List Movie -> List Movie
darLikeAPelicula id = List.map (sumarUnLike (List.filter id))

sumarUnLike : List Movie -> List Movie
sumarUnLike = List.map (pelicula.likes + 1)
-- **************
-- Requerimiento: cargar preferencias a través de un popup modal,
--                calcular índice de coincidencia de cada película y
--                mostrarlo junto a la misma;
-- **************

calcularPorcentajeDeCoincidencia : Preferences -> List Movie -> List Movie
calcularPorcentajeDeCoincidencia preferencias = completaAca