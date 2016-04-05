import Signal
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Window


draw_line ax ay bx by =
 traced (dashed black) <| path [(toFloat ax,toFloat ay),(toFloat bx,toFloat by)]


hor_lines cellsize rows cols = List.foldl (\y acc-> (draw_line 0 y (cols*cellsize) y) :: acc) []  (List.map (\x->x*cellsize) [0..rows])

ver_lines cellsize rows cols = List.foldl (\x acc -> (draw_line x 0 x (rows*cellsize)) :: acc) []  (List.map (\x->x*cellsize) [0..cols])

draw_grid cell_sz rows cols=
                        (hor_lines cell_sz rows cols) ++ (ver_lines cell_sz rows cols)
                      

draw_circle radius (x,y)=  circle (toFloat radius) |> filled red |>  move (toFloat x,toFloat y)

render m n cell_sz coll_sz = color grey <|collage coll_sz coll_sz  <|  (++) (draw_grid cell_sz m n)  <| [draw_circle (cell_sz//4) (cell_sz//2,cell_sz*m - cell_sz//2)]


main =
       render 10 10 50 1200