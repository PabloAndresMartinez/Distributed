{-# LANGUAGE FlexibleContexts #-}

-- | Pretty-printing of low-level quantum circuits. The majority of this source code comes from Quipper 0.8, module Quipper.Printing.
-- A few functions were modified or added (render_wire, render_gate, render_gates) so that the circuit would be colored depending on comments.

module Distributer.ColorPrinting (preview_withColor) where

-- import other Quipper stuff
import Quipper.Utils.Auxiliary
import Quipper.Internal.Circuit
import Quipper.Internal.Generic
import Quipper.Internal.Monad
import Quipper.Internal.QData

-- import other stuff
import Prelude
import Text.Printf
import Data.Char(isSpace)
import Data.List
import Data.Maybe
import Control.Monad(when)
import Graphics.EasyRender
import System.IO
import System.Process
import System.Directory
import System.Environment
import System.Info

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.IntMap as IntMap
import qualified Data.List as List

-- ======================================================================
-- * Auxiliary functions

-- | Determine whether a named gate is self-inverse. The kind of a
-- gate is uniquely determined by its name, and the number of input
-- wires and generalized controls.
-- 
-- For now, we only recognize "X", "Y", "Z", "H", "not", "swap", and
-- "W" as self-inverse; it is not currently possible for user code to
-- extend this list.
self_inverse :: String -> [Wire] -> [Wire] -> Bool
self_inverse "X" [q] [] = True
self_inverse "Y" [q] [] = True
self_inverse "Z" [q] [] = True
self_inverse "H" [q] [] = True
self_inverse "not" [q] [] = True
self_inverse "swap" [q1,q2] [] = True
self_inverse "W" [q1,q2] [] = True
self_inverse _ _ _ = False

-- ----------------------------------------------------------------------
-- * Graphical representation of circuits

-- | The color white.
white :: Color
white = Color_Gray 1.0

-- | The color black.
black :: Color
black = Color_Gray 0.0

-- | The color palette up to 16 different values, after that color is white by default.
palette :: Int -> Color
palette 0  = Color_RGB 1.0 0.0 0.0     -- Red
palette 1  = Color_RGB 1.0 0.84 0.0    -- Yellow
palette 2  = Color_RGB 0.0 0.0  1.0    -- Blue
palette 3  = Color_RGB 0.20 0.80 0.20  -- Lime
palette 4  = Color_RGB 0.71 0.71 0.71  -- Grey 
palette 5  = Color_RGB 1.0 0.50 0.31   -- Orange
palette 6  = Color_RGB 1.0 0.0 1.0     -- Magenta
palette 7  = Color_RGB 0.50 0.0 0.0    -- Maroon
palette 8  = Color_RGB 0.0 0.50 0.0    -- Navy
palette 9  = Color_RGB 0.63 0.32 0.18  -- Brown
palette 10 = Color_RGB 0.5 0 0.5       -- Purple
palette 11 = Color_RGB 0.0 1.0 1.0     -- Cyan
palette 12 = Color_RGB 0.13 0.55 0.13  -- Green
palette 13 = Color_RGB 1.0 0.8 0.58    -- Pink
palette 14 = Color_RGB 0.0 0.5 0.5     -- Teal  
palette 15 = Color_RGB 0.5 0.5 0.0     -- Olive   
palette _  = white

type Dashed = Bool

-- | A data type that holds all the customizable parameters.
data FormatStyle = FormatStyle {
  -- | The RenderFormat to use.
  renderformat :: RenderFormat,
  -- | The color of the background.
  backgroundcolor :: Color,
  -- | The color of the foreground (e.g. wires and gates).
  foregroundcolor :: Color,
  -- | The map (dictionary) of colors for each wire and pattern.
  colorMap :: Map Wire (Color, Dashed),
  -- | Line width.
  linewidth :: Double,
  -- | Gap for double line representing classical bit.
  coffs :: Double,
  -- | Radius of dots for \"controlled\" gates.
  dotradius :: Double,
  -- | Radius of oplus for \"not\" gate.
  oplusradius :: Double,
  -- | Horizontal column width.
  xoff :: Double,
  -- | Difference between width of box and width of label.
  gatepad :: Double,
  -- | Height of labelled box.
  gateheight :: Double,
  -- | Width and height of \"cross\" for swap gate.
  crossradius :: Double,
  -- | Vertical shift for text labels.
  stringbase :: Double,
  -- | Width of \"bar\" bar.
  barwidth :: Double, 
  -- | Height of \"bar\" bar.
  barheight :: Double,
  -- | Width of \"D\" symbol.
  dwidth :: Double,
  -- | Height of \"D\" symbol.
  dheight :: Double,
  -- | Maximal width of a gate label.
  maxgatelabelwidth :: Double,
  -- | Maximal width of a wire label.
  maxlabelwidth :: Double,
  -- | Maximal width of a wire number.
  maxnumberwidth :: Double,
  -- | Font to use for labels on gates.
  gatefont :: Font,
  -- | Font to use for comments.
  commentfont :: Font,
  -- | Color to use for comments.
  commentcolor :: Color,
  -- | Font to use for labels.
  labelfont :: Font,
  -- | Color to use for labels.
  labelcolor :: Color,
  -- | Font to use for numbers.
  numberfont :: Font,
  -- | Color to use for numbers.
  numbercolor :: Color,
  -- | Whether to label each subroutine call with shape parameters
  subroutineshape :: Bool
} deriving Show

-- | Function that updates 'colorMap'
updWireColor :: Wire -> (Color, Dashed) -> FormatStyle -> FormatStyle
updWireColor w cd fs = fs {
  colorMap = Map.insert w cd $ colorMap fs
}

-- | Function that retrieves the color of the wire, given by 'colorMap fs'.
-- If the wire is not found, the default color is used: white.
getColor :: FormatStyle -> Wire -> (Color, Dashed)
getColor fs w = if Map.member w (colorMap fs) then (colorMap fs) Map.! w else (white, False)


-- | A RenderFormat consisting of some default parameters, 
-- along with the given RenderFormat.
defaultStyle :: RenderFormat -> FormatStyle
defaultStyle rf = FormatStyle {
  renderformat = rf,
  backgroundcolor = white,
  foregroundcolor = black,
  colorMap = Map.empty,
  linewidth = 0.02, 
  coffs = 0.03,
  dotradius  = 0.15,
  oplusradius = 0.25,
  xoff = 1.5,
  gatepad = 0.3, 
  gateheight  = 0.8,
  crossradius = 0.2,
  stringbase = 0.25,
  barwidth = 0.1,
  barheight = 0.5,
  dwidth = 0.3,
  dheight = 0.4,
  maxgatelabelwidth = 1.1,
  maxlabelwidth = 0.7,
  maxnumberwidth = 0.7,
  gatefont = Font TimesRoman 0.5,
  commentfont = Font TimesRoman 0.3,
  commentcolor = black,
  labelfont = Font TimesRoman 0.3,
  labelcolor = black,
  numberfont = Font Helvetica 0.5,
  numbercolor = Color_RGB 0 0.7 0,
  subroutineshape = True
}

-- | The default PDF Style.
pdf :: FormatStyle
pdf = defaultStyle Format_PDF

-- | The default EPS Style.
eps :: FormatStyle
eps = defaultStyle (Format_EPS 1)

-- | The default PS Style.
ps :: FormatStyle
ps = defaultStyle (Format_PS)

-- ----------------------------------------------------------------------
-- ** General-purpose PostScript functions

-- | Escape special characters in a string literal.
ps_escape :: String -> String
ps_escape [] = []
ps_escape ('\\' : t) = '\\' : '\\' : ps_escape t
ps_escape ('('  : t) = '\\' : '('  : ps_escape t
ps_escape (')'  : t) = '\\' : ')'  : ps_escape t
ps_escape (h : t)    = h : ps_escape t

-- ----------------------------------------------------------------------
-- ** String formatting

-- | Convert a 'BoxId' to the string in the format \"/name/, shape /x/\".
string_of_boxid :: BoxId -> String
string_of_boxid (BoxId name shape) = name ++ ", shape " ++ shape

-- ----------------------------------------------------------------------
-- ** Functions for dealing with x-coordinates

-- | Pre-processing: figure out the /x/-column of each gate. Returns
-- (/n/,/xgs/) where /xgs/ is a list of ('Gate', 'X') pairs, and
-- /n/ is the rightmost /x/-coordinate of the circuit. Here we start
-- from /x0/ and use constant step /xoff/ taken from the 'FormatStyle'.
assign_x_coordinates :: FormatStyle -> [Gate] -> X -> (X, [(Gate, X)])
assign_x_coordinates fs gs x0 =
  let ((x,ws), xgs) = mapAccumL (\ (x, ws) g ->
        -- count the wires attached to the gate. If there is precisely
        -- one (unary gate), merge it with adjacent unary gates. Do
        -- not merge comments.
        let merge = case (g, wirelist_of_gate g) of
              (Comment _ _ _, _) -> Nothing
              (_, [w]) -> Just w
              (_, _) -> Nothing
        in
        case merge of
          Just w ->
            if not (w `elem` ws) then
              ((x, w:ws), (g, x))
            else
              ((x + (xoff fs), [w]), (g, x + (xoff fs)))
          _ ->
            if ws == [] then
              ((x + (xoff fs), []), (g, x))
            else
              ((x + 2.0 * (xoff fs), []), (g, x + (xoff fs)))
        ) (x0, []) gs
  in
   if ws == [] then
     (x, xgs)
   else
     (x + (xoff fs), xgs)

-- | A 'Xarity' is a map from wire id's to pairs of a wiretype and a
-- starting /x/-coordinate.
type Xarity = Map Wire (Wiretype, X)

-- | Figure out how a gate at coordinate /x/ affects the current 'Xarity'.
-- Return a pair (/term/, /new/), where /term/ is the 'Xarity' of wires
-- terminated by this gate, and /new/ is the outgoing 'Xarity' of this
-- gate.
-- In particular, the 'teleport' gate terminates its inputs and initializes 
-- them from /x/ onwards.
update_xarity :: Xarity -> Gate -> X -> (Xarity, Xarity)
update_xarity xarity (QGate "teleport" _ [w] _ _ _) x = (xarity, Map.insert w (Qbit,x) xarity)
update_xarity xarity gate x =
  let (win, wout) = gate_arity gate
      safe_lookup xarity w = 
        case Map.lookup w xarity of 
          Just x -> x
          Nothing -> (Qbit, x) -- error ("update_xarity: the wire " ++ show w ++ " does not exist. In the gate:\n" ++ ascii_render_gate gate)
      (win', wout') = (win \\ wout, wout \\ win)
      -- extract terminating wires from xarity
      xarity_term = foldl (\xar (w,_) -> Map.insert w (xarity `safe_lookup` w) xar) Map.empty win' 
      -- extract continuing wires from xarity
      xarity_cont = foldl (\xar (w,_) -> Map.delete w xar) xarity win'
      -- add new wires to xarity_cont
      xarity_new = foldl (\xar (w,t) -> Map.insert w (t,x) xar) xarity_cont wout'
  in
   (xarity_term, xarity_new)

-- ----------------------------------------------------------------------
-- ** Low-level drawing functions

-- | @'render_line' x0 y0 x1 y1@: Draw a line from (/x0/, /y0/)
-- to (/x1/, /y1/). In case of a zero-length line, draw nothing.
render_line :: X -> Y -> X -> Y -> Draw ()
render_line x0 y0 x1 y1 | x0 == x1 && y0 == y1 = return ()
render_line x0 y0 x1 y1 = draw_subroutine alt $ do
  moveto x0 y0
  lineto x1 y1
  stroke
  where
    alt = [custom_ps $ printf "%f %f %f %f line\n" x0 y0 x1 y1]

-- | @'render_wire' (c, dashed) x0 y0 x1 y1@: Draw a line from (/x0/, /y0/)
-- to (/x1/, /y1/) with background color /c/, with pattern given by /dashed/.
render_wire :: (Color, Dashed) -> X -> Y -> X -> Y -> Draw ()
render_wire _           x0 y0 x1 y1 | x0 == x1 && y0 == y1 = return ()
render_wire (c, dashed) x0 y0 x1 y1 = draw_subroutine alt $ do
  if dashed 
    then foldr (\(x,w) d -> background x w >> d) (fill c) (dashes x0 x1)
    else rectangle x0 (y0-0.5) (x1-x0) 1 >> fill c
  moveto x0 y0
  lineto x1 y1
  stroke
  where
    background x w = moveto x bot >> lineto (x+w) bot >> lineto (x+w+0.25) top >> lineto (x+0.25) top >> lineto x bot
    dashes a b = let a' = a+dashWidth+dashSep in if a' < b
      then (a, dashWidth) : dashes a' b
      else (a, b-a) : []
    dashWidth = 1.75; dashSep = 0.25; rombShift = 0.25; bot = y0-0.5; top=y0+0.5
    alt = [custom_ps $ printf "%f %f %f %f line\n" x0 y0 x1 y1]

-- | @'render_dot' x y@: Draw a filled control dot at (/x/,/y/).
render_dot :: FormatStyle -> X -> Y -> Draw ()
render_dot fs x y = draw_subroutine alt $ do
  arc x y (dotradius fs) 0 360
  fill (foregroundcolor fs)
  where
    alt = [custom_ps $ printf "%f %f dot\n" x y]

-- | @'render_circle' x y@: Draw an empty control dot at
-- (/x/,/y/).
render_circle :: FormatStyle -> X -> Y -> Draw ()
render_circle fs x y = draw_subroutine alt $ do
  arc x y (dotradius fs) 0 360
  fillstroke (backgroundcolor fs)
  where
    alt = [custom_ps $ printf "%f %f circ\n" x y]

-- | @'render_not' x y@: Draw a \"not\" gate at (/x/,/y/).
render_not :: FormatStyle -> X -> Y -> Draw ()
render_not fs x y = draw_subroutine alt $ do
  arc x y (oplusradius fs) 0 360
  fillstroke (backgroundcolor fs)
  render_line (x-(oplusradius fs)) y (x+(oplusradius fs)) y
  render_line x (y-(oplusradius fs)) x (y+(oplusradius fs))
  where
    alt = [custom_ps $ printf "%f %f oplus\n" x y]

-- | @'render_swap' x y@: Draw a cross (swap gate component) at
--  (/x/,/y/).
render_swap :: FormatStyle -> X -> Y -> Draw ()
render_swap fs x y = draw_subroutine alt $ do
  render_line (x-(crossradius fs)) (y-(crossradius fs)) (x+(crossradius fs)) (y+(crossradius fs))
  render_line (x-(crossradius fs)) (y+(crossradius fs)) (x+(crossradius fs)) (y-(crossradius fs))
  where  
    alt = [custom_ps $ printf "%f %f cross\n" x y]

-- | @'render_bar' x y@: Draw an init/term bar at (/x/,/y/).
render_bar :: FormatStyle -> X -> Y -> Draw ()
render_bar fs x y = draw_subroutine alt $ do
  rectangle (x - (barwidth fs)/2) (y - (barheight fs)/2) (barwidth fs) (barheight fs)
  fill (foregroundcolor fs)
  where
    alt = [custom_ps $ printf "%f %f bar\n" x y]

-- | @'render_bar' x y@: Draw a dterm bar at (/x/,/y/).
render_dbar :: FormatStyle -> X -> Y -> Draw ()
render_dbar fs x y = draw_subroutine alt $ do
  block $ do
    translate (x+(barwidth fs)/2) y
    scale (dwidth fs) (dheight fs)
    moveto (-1) (-0.5)
    arc_append (-0.5) 0 0.5 (-90) 90
    lineto (-1) 0.5
    closepath
    fill (foregroundcolor fs)
  where
    alt = [custom_ps $ printf "%f %f dbar\n" x y]

-- | @'render_init' name x y@: Draw an \"init\" gate at
-- (/x/,/y/), with state /name/.
render_init :: FormatStyle -> String -> X -> Y -> Draw ()
render_init fs name x y = draw_subroutine alt $ do
  render_bar fs x y
  textbox align_right (gatefont fs) (foregroundcolor fs) (x-(xoff fs)/2+(gatepad fs)/2) y (x-(gatepad fs)/2) y (stringbase fs) name
  where
    alt = [custom_ps $ printf "(%s) %f %f init\n" (ps_escape name) x y]

-- | @'render_term' name x y@: Draw a \"term\" gate at
-- (/x/,/y/), with state /name/.
render_term :: FormatStyle -> String -> X -> Y -> Draw ()
render_term fs name x y = draw_subroutine alt $ do
  render_bar fs x y
  textbox align_left (gatefont fs) (foregroundcolor fs) (x+(gatepad fs)/2) y (x+(xoff fs)/2-(gatepad fs)/2) y (stringbase fs) name
  where
    alt = [custom_ps $ printf "(%s) %f %f term\n" (ps_escape name) x y]

-- | @'render_dterm' name x y@: Draw a \"dterm\" gate at
-- (/x/,/y/), with state /name/.
render_dterm :: FormatStyle -> String -> X -> Y -> Draw ()
render_dterm fs name x y = draw_subroutine alt $ do
  render_dbar fs x y
  textbox align_left (gatefont fs) (foregroundcolor fs) (x+(gatepad fs)/2) y (x+(xoff fs)/2-(gatepad fs)/2) y (stringbase fs) name
  where
    alt = [custom_ps $ printf "(%s) %f %f dterm\n" (ps_escape name) x y]

-- | @'render_namedgate' name inv x y@: draw a named box centered at
-- (/x/,/y/). If /inv/ = 'True', append an \"inverse\" symbol to the
-- end of the name.
render_namedgate :: FormatStyle -> String -> InverseFlag -> X -> Y -> Draw ()
render_namedgate fs name inv x y = draw_subroutine alt $ do
  rectangle (x-gatewidth/2) (y-(gateheight fs)/2) gatewidth (gateheight fs)
  fillstroke (backgroundcolor fs)
  textbox align_center (gatefont fs) (foregroundcolor fs) (x-labelwidth/2) y (x+labelwidth/2) y (stringbase fs) name'
  where
    alt = [custom_ps $ printf "(%s) %f %f gate\n" (ps_escape name') x y]
    name' = name ++ optional inv "*"
    w = text_width (gatefont fs) name'
    labelwidth = min w (maxgatelabelwidth fs)
    gatewidth = labelwidth + (gatepad fs)
            
-- | @'render_gphasegate' name x y@: draw a global phase gate
-- centered at (/x/,/y/).
render_gphasegate :: FormatStyle -> String -> X -> Y -> Draw ()
render_gphasegate fs name x y = draw_subroutine alt $ do
  render_circgate fs name x (y-0.5)
  where
    alt = [custom_ps $ printf "(%s) %f %f gphase\n" (ps_escape name) x y]

-- | @'render_circgate' name x y@: draw a named oval centered at
-- (/x/,/y/).
render_circgate :: FormatStyle -> String -> X -> Y -> Draw ()
render_circgate fs name x y = draw_subroutine alt $ do
  oval x y (0.5*gatewidth) (0.4*(gateheight fs))
  fillstroke (backgroundcolor fs)
  textbox align_center (gatefont fs) (foregroundcolor fs) (x-labelwidth/2) y (x+labelwidth/2) y (stringbase fs) name
  where
    alt = [custom_ps $ printf "(%s) %f %f circgate\n" (ps_escape name) x y]
    w = text_width (gatefont fs) name
    labelwidth = min w (maxgatelabelwidth fs)
    gatewidth = labelwidth + (gatepad fs)
    
-- | @'render_blankgate' name x y@: draw an empty box centered
-- at (/x/,/y/), big enough to hold /name/.
render_blankgate :: FormatStyle -> String -> X -> Y -> Draw ()
render_blankgate fs name x y = draw_subroutine alt $ do
  rectangle (x-gatewidth/2) (y-(gateheight fs)/2) gatewidth (gateheight fs)
  fillstroke (backgroundcolor fs)
  where
    alt = [custom_ps $ printf "(%s) %f %f box\n" (ps_escape name) x y]
    w = text_width (gatefont fs) name
    labelwidth = min w (maxgatelabelwidth fs)
    gatewidth = labelwidth + (gatepad fs)

-- | @'render_comment' center s x y m@: draw the given string
-- vertically, with the top of the string near the given
-- /y/-coordinate. If /center/=='True', center it at the
-- /x/-coordinate, else move it just to the left of the
-- /x/-coordinate. /m/ is the maximum height allowed for the comment.
render_comment :: FormatStyle -> Bool -> String -> X -> Y -> Y -> Draw ()
render_comment fs center s x y maxh = draw_subroutine alt $ do
  textbox align_right (commentfont fs) (commentcolor fs) x (y-maxh) x (y+0.4) b s
  where
    alt = [custom_ps $ printf "(%s) %f %f %f %f comment\n" (ps_escape s) x y maxh yshift]
    b = if center then 0.15 else -0.25
    yshift = -b * nominalsize (commentfont fs)

-- | @'render_label' center s x y@: draw the given label just above
-- the given point. If /center/=='True', center it at the
-- /x/-coordinate, else move it just to the right of the
-- /x/-coordinate.
render_label :: FormatStyle -> Bool -> String -> X -> Y -> Draw ()
render_label fs True s x y = draw_subroutine alt $ do
  textbox align_center (labelfont fs) (labelcolor fs) (x-(maxlabelwidth fs)) y' (x+(maxlabelwidth fs)) y' (-0.5) s
  where
    alt = [custom_ps $ printf "(%s) %f %f clabel\n" (ps_escape s) x y']
    y' = y + 0.5 * (coffs fs)
render_label fs False s x y = draw_subroutine alt $ do
  textbox align_left (labelfont fs) (labelcolor fs) x y' (x+(maxlabelwidth fs)) y' (-0.5) s
  where
    alt = [custom_ps $ printf "(%s) %f %f rlabel\n" (ps_escape s) x y']
    y' = y + 0.5 * (coffs fs)
    
-- | Render the number at the given point (/x/,/y/). If the boolean
-- argument is 'True', put the number to the right of /x/, else to the left. 
render_number :: FormatStyle -> Int -> Bool -> X -> Y -> Draw ()
render_number fs i True x y = draw_subroutine alt $ do
  textbox align_left (numberfont fs) (numbercolor fs) (x+0.2) y (x+0.2+(maxnumberwidth fs)) y (stringbase fs) (show i)
  where
    alt = [custom_ps $ printf "(%s) %f %f rnumber\n" (ps_escape (show i)) x y]
render_number fs i False x y = draw_subroutine alt $ do
  textbox align_right (numberfont fs) (numbercolor fs) (x-0.2-(maxnumberwidth fs)) y (x-0.2) y (stringbase fs) (show i)
  where
    alt = [custom_ps $ printf "(%s) %f %f lnumber\n" (ps_escape (show i)) x y]

-- ----------------------------------------------------------------------
-- ** Higher-level rendering functions

-- | Render a horizontal wire from /x/-coordinates /oldx/ to /x/,
-- using /t/ as the type and figuring out the /y/-coordinate from /ys/
-- and /w/. Append to the given string. If the parameters are invalid
-- (/w/ not in /ys/), throw an error.
render_typeas :: FormatStyle -> Map Wire Y -> X -> X -> Wire -> Wiretype -> Draw ()
render_typeas fs ys oldx x w t =
  let y = ys Map.! w in
  case t of
    Qbit -> do
      render_wire (getColor fs w) oldx y x y
    Cbit -> do
      render_line oldx (y + (coffs fs)) x (y + (coffs fs))
      render_line oldx (y - (coffs fs)) x (y - (coffs fs))

-- | Render a bunch of horizontal wires from their respective starting
-- 'Xarity' to /x/.
render_xarity :: FormatStyle -> Map Wire Y -> Xarity -> X -> Draw ()
render_xarity fs ys xarity x = do
  sequence_ [ render_typeas fs ys oldx x w t | (w,(t,oldx)) <- Map.toList xarity ]

-- | Format a floating point number in concise form, with limited
-- accuracy.
dshow :: Double -> String
dshow dbl = 
  if abs dbl < 0.01 
  then
    printf "%.1e" dbl
  else
    (reverse . strip . reverse) (printf "%.3f" dbl)
      where
        strip [] = []
        strip ('.' : t) = t
        strip ('0' : t) = strip t
        strip t = t
        
-- | @'render_controlwire' /x/ /ys/ /ws/ /c/@: 
-- Render the line connecting all the box components and all the
-- control dots of some gate. 
-- 
-- Parameters: /x/ is the current /x/-coordinate, /ys/ is an indexed
-- array of /y/-coordinates, /ws/ is the set of wires for boxes, and
-- /c/ is a list of controls.
render_controlwire :: X -> Map Wire Y -> [Wire] -> Controls -> Draw ()
render_controlwire x ys ws c =
  case ws of
    [] -> return ()
    w:ws -> render_line x y0 x y1      
      where
        ymap w = ys Map.! w
        y = ymap w
        cy = map (\(Signed w _) -> ymap w) c
        yy = map (\w -> ymap w) ws
        y0 = foldr min y (cy ++ yy)
        y1 = foldr max y (cy ++ yy)

-- | @'render_controlwire_float' /x/ /ys/ /y/ /c/@: Render the line
-- connecting all control dots of the given controls, as well as a
-- floating \"global phase\" gate located just below (/x/, /y/). 
-- 
-- Parameters: /x/ is the current /x/-coordinate, /ys/ is an indexed
-- array of /y/-coordinates, /y/ is the /y/-coordinate of the wire
-- where the floating gate is attached, and /c/ is a list of controls.
render_controlwire_float :: X -> Map Wire Y -> Y -> Controls -> Draw ()
render_controlwire_float x ys y c = render_line x y0 x y1 
  where
    y' = y - 0.5
    cy = map (\(Signed w _) -> ys Map.! w) c
    y0 = minimum (y':cy)
    y1 = maximum (y':cy)

-- | @'render_controldots' /x/ /ys/ /c/@: Render the control dots
-- for the given controls.
render_controldots :: FormatStyle -> X -> Map Wire Y -> Controls -> Draw ()
render_controldots fs x ys c = do
  sequence_ [ renderdot x | x <- c ]
  where
    renderdot (Signed w True) = render_dot fs x (ys Map.! w)
    renderdot (Signed w False) = render_circle fs x (ys Map.! w)

-- | @'render_multi_gate' /x/ /ys/ /name/ /inv/ /wires/@: Render the
-- boxes for an /n/-ary gate of the given /name/, potentially
-- /inv/erted, at the given list of /wires/. The first two arguments
-- are the current /x/-coordinate and an indexed array of
-- /y/-coordinates.
render_multi_gate :: FormatStyle -> X -> Map Wire Y -> String -> InverseFlag -> [Wire] -> Draw ()
render_multi_gate fs x ys name inv [w] = 
  render_namedgate fs name inv x (ys Map.! w)
render_multi_gate fs x ys name inv ws =
  sequence_ [ render_namedgate fs (name ++ " " ++ show i) inv x (ys Map.! a) | (a,i) <- zip ws [1..] ]

-- | @'render_multi_named_ctrl' /x/ /ys/ /wires/ /names/@: Render
-- the boxes for multiple generalized controls at the given /wires/,
-- using the given /names/. We take special care of the fact that
-- generalized controls may be used non-linearly. 
render_multi_named_ctrl :: FormatStyle -> X -> Map Wire Y -> [Wire] -> [String] -> Draw ()
render_multi_named_ctrl fs x ys ws names =
  sequence_ [ render_circgate fs name x (ys Map.! a) | (a,name) <- IntMap.toList map ]
  where
    -- Combine the labels for w if w has multiple occurrences.
    map = IntMap.fromListWith (\x y -> y ++ "," ++ x) (zip ws names)

-- | @'render_multi_genctrl' /x/ /ys/ /wires/@: Render the boxes for
-- multiple (numbered) generalized controls at the given /wires/.
render_multi_genctrl :: FormatStyle -> X -> Map Wire Y -> [Wire] -> Draw ()
render_multi_genctrl fs x ys ws = render_multi_named_ctrl fs x ys ws names
  where
    names = map show [1..]
            
-- | Number a list of wires in increasing order, at the given
-- /x/-coordinate. If the boolean argument is 'True', put the numbers
-- to the right of /x/, else to the left.
render_ordering :: FormatStyle -> X -> Map Wire Y -> Bool -> [Wire] -> Draw ()
render_ordering fs x ys b ws =
  sequence_ [ render_number fs i b x (ys Map.! w) | (w,i) <- numbering ]
  where
    numbering = zip ws [1..]

-- | Render gate /g/ at /x/-coordinate /x/ and /y/-coordinates as
-- given by /ys/, which is a map from wires to
-- /y/-coordinates. Returns a pair (/s/,/t/) of draw actions for
-- background and foreground, respectively.
render_gate :: FormatStyle -> Gate -> X -> Map Wire Y -> Y -> (Draw (), Draw ())
render_gate fs g x ys maxh =
  let ymap w = ys Map.! w 
  in
  case g of
    -- Certain named gates are recognized for custom rendering.
    QGate "CZ" _ [w] [] c ncf -> (s2, t2 >> t3)
      where
        y = ymap w
        s2 = render_controlwire x ys [w] c
        t2 = render_controldots fs x ys c
        t3 = (render_dot fs x y)
    QGate "not" _ [w] [] c ncf -> (s2, t2 >> t3)
      where
        y = ymap w
        s2 = render_controlwire x ys [w] c
        t2 = render_controldots fs x ys c
        t3 = (render_not fs x y)
    QGate "multinot" _ ws [] c ncf -> (s2, t2 >> t3)
      where
        s2 = render_controlwire x ys ws c
        t2 = render_controldots fs x ys c
        t3 = sequence_ (map (\w -> (render_not fs x (ymap w))) ws)
    QGate "swap" _ [w1,w2] [] c ncf -> (s2, t2 >> t3)
      where
        y1 = ymap w1
        y2 = ymap w2
        s2 = render_controlwire x ys [w1,w2] c
        t2 = render_controldots fs x ys c
        t3 = (render_swap fs x y1) >> (render_swap fs x y2)
    QGate "trace" _ _ _ _ _ -> (return (), return ())
    QGate name inv ws1 ws2 c ncf -> (s2, t2 >> t3 >> t4)
      where
       s2 = render_controlwire x ys (ws1 ++ ws2) c
       t2 = render_multi_gate fs x ys name inv' ws1
       t3 = render_controldots fs x ys c
       t4 = render_multi_genctrl fs x ys ws2
       inv' = inv && not (self_inverse name ws1 ws2)
    QRot name inv theta ws1 ws2 c ncf -> (s2, t2 >> t3 >> t4)
      where
       s2 = render_controlwire x ys (ws1 ++ ws2) c
       t2 = render_multi_gate fs x ys name' inv ws1
       t3 = render_controldots fs x ys c
       t4 = render_multi_genctrl fs x ys ws2
       name' = substitute name '%' (dshow theta)
    GPhase t ws c ncf -> (s2, t2 >> t3)
      where
        y = case (ws, c) of
          ([], []) -> maximum (0.0 : Map.elems ys)
          ([], c)  -> minimum [ ymap w | Signed w b <- c ]
          (ws, c)  -> minimum [ ymap w | w <- ws ]
        s2 = render_controlwire_float x ys y c
        t2 = render_controldots fs x ys c
        t3 = (render_gphasegate fs (dshow t) x y)
    CNot w c ncf -> (s2, t2 >> t3)
      where
        y = ymap w
        s2 = render_controlwire x ys [w] c
        t2 = render_controldots fs x ys c
        t3 = (render_not fs x y)
    CGate "if" w [a,b,c] ncf -> (s2, t1 >> t3)  -- special case
      where
       y = ymap w
       s2 = render_controlwire x ys [w,a,b,c] []
       t1 = render_multi_named_ctrl fs x ys [a,b,c] ["if", "then", "else"]
       t3 = render_namedgate fs ">" False x y
    CGateInv "if" w [a,b,c] ncf -> (s2, t1 >> t3)  -- special case
      where
       y = ymap w
       s2 = render_controlwire x ys [w,a,b,c] []
       t1 = render_multi_named_ctrl fs x ys [a,b,c] ["if", "then", "else"]
       t3 = render_namedgate fs "<" False x y
    CGate name w c ncf -> (s2, t2 >> t3)
      where
       y = ymap w
       s2 = render_controlwire x ys (w:c) []
       t2 = render_multi_named_ctrl fs x ys c [ "  " | a <- c ]
       t3 = render_namedgate fs name False x y
    CGateInv name w c ncf -> (s2, t2 >> t3)
      where
       y = ymap w
       s2 = render_controlwire x ys (w:c) []
       t2 = render_multi_named_ctrl fs x ys c [ "  " | a <- c ]
       t3 = render_namedgate fs name True x y
    CSwap w1 w2 c ncf -> (s2, t2 >> t3)
      where
        y1 = ymap w1
        y2 = ymap w2
        s2 = render_controlwire x ys [w1,w2] c
        t2 = render_controldots fs x ys c
        t3 = (render_swap fs x y1) >> (render_swap fs x y2)
    QPrep w ncf -> (return (), t3)
      where
        y = ymap w
        t3 = (render_namedgate fs "prep" False x y)
    QUnprep w ncf -> (return (), t3)
      where
        y = ymap w
        t3 = (render_namedgate fs "unprep" False x y)
    QInit b w ncf -> (return (), t3)
      where
        y = ymap w
        t3 = (render_init fs (if b then "1" else "0") x y)
    CInit b w ncf -> (return (), t3)
      where
        y = ymap w
        t3 = (render_init fs (if b then "1" else "0") x y)
    QTerm b w ncf -> (return (), t3)
      where
        y = ymap w
        t3 = (render_term fs (if b then "1" else "0") x y)
    CTerm b w ncf -> (return (), t3)
      where
        y = ymap w
        t3 = (render_term fs (if b then "1" else "0") x y)
    QMeas w -> (return (), t3)
      where
        y = ymap w
        t3 = (render_namedgate fs "meas" False x y)
    QDiscard w -> (return (), t3)
      where
        y = ymap w
        t3 = (render_bar fs x y)
    CDiscard w -> (return (), t3)
      where
        y = ymap w
        t3 = (render_bar fs x y)
    DTerm b w -> (return (), t3)
      where
        y = ymap w
        t3 = (render_dterm fs (if b then "1" else "0") x y)
    Subroutine boxid inv ws1 a1 ws2 a2 c ncf scf rep -> (s2, t2 >> t3)
      where
       ws = union ws1 ws2
       s2 = render_controlwire x ys ws c
       t2 = render_multi_gate fs x ys label inv ws
       t3 = render_controldots fs x ys c
       show_rep = if rep == RepeatFlag 1 then "" else "(x" ++ show rep ++ ")"
       BoxId name shape = boxid
       label = name ++ show_rep ++ if (subroutineshape fs) then (", shape " ++ shape) else ""
    Comment s inv ws -> (return (), t1 >> t2)
      where
        t1 = render_comment fs (null ws) s' x (ymap 0) maxh
        t2 = sequence_ [render_label fs (null s) l x (ymap w) | (w,l) <- ws]
        s' = s ++ optional inv "*"

-- | Render the gates in the circuit. The parameters are: /xarity/:
-- the 'Xarity' of the currently pending wires. /xgs/: the list of
-- gates, paired with pre-computed /x/-coordinates. /ys/: a map from
-- wires to pre-computed /y/-coordinates. /x/: the right-most
-- /x/-coordinate where the final wires will be drawn to. /maxh/: the
-- maximal height of comments.
render_gates :: FormatStyle -> Xarity -> [(Gate, X)] -> Map Wire Y -> X -> Y -> (Draw (), Draw ())
render_gates fs xarity xgs ys x maxh =
  case xgs of
    [] ->
      let s2 = render_xarity fs ys xarity x
      in (s2, return ())
    (g,newx):gls -> case g of
      Comment "QPU_allocation" False ws -> 
        let fs' = foldr (\(w,l) -> updWireColor w ((\[c,d] -> (palette $ read c, d=="ebit")) $ words l)) fs ws in
        let newComment = (Comment "QPU_allocation" True ws, newx) in -- I use the comment's flag to indicate colors have been updated
        render_gates fs' xarity (newComment:gls) ys x maxh
      _ ->
        let (xarity_term, xarity_new) = update_xarity xarity g newx in
        let s1 = render_xarity fs ys xarity_term newx in
        let (s2, t2) = render_gate fs g newx ys maxh in
        let (sx, tx) = render_gates fs xarity_new gls ys x maxh in
        (s1 >> sx, s2 >> t2 >> tx)

-- | @'page_of_ocircuit' name ocirc@: Render the circuit /ocirc/ on a
-- single page.
-- 
-- The rendering takes place in the following user coordinate system:
-- 
-- \[image coord.png]
page_of_ocircuit :: FormatStyle -> Maybe BoxId -> OCircuit -> Document ()
page_of_ocircuit fs boxid ocirc = do
  newpage bboxx bboxy $ do
    when (isJust boxid) $ do
      comment ("drawing commands for " ++ string_of_boxid (fromJust boxid))
    
    -- set up the user coordinate system
    scale sc sc
    translate ((xoff fs) + 1) 1
    
    -- drawing commands
    setlinewidth (linewidth fs)
    when (isJust boxid) $ do
      textbox align_left (gatefont fs) (foregroundcolor fs) (-(xoff fs)) (raw_height-0.25) raw_width (raw_height-0.25) (stringbase fs) ("Subroutine " ++ string_of_boxid (fromJust boxid) ++ ":")
    rendered_wires
    rendered_gates
    render_ordering fs (-(xoff fs)) ys False w_in
    render_ordering fs raw_width ys True w_out
  where
    -- unit scale: distance, in points, between wires
    sc = 10
    
    -- decompose OCircuit
    OCircuit (w_in, circ, w_out) = ocirc
    (a1,gs,a2,_) = circ
    
    -- figure out y-coordinates and height
    ws = wirelist_of_circuit circ
    raw_height = fromIntegral $ length ws
    ys = Map.fromList (zip (reverse ws) [0.0 ..])
    maxh = raw_height + 0.3
    bboxy = sc * (raw_height + 1)
    
    -- figure out x-coordinates and width
    (raw_width,xgs) = assign_x_coordinates fs gs 0.0
    bboxx = sc * (raw_width + (xoff fs) + 2.0)
    
    xa1 = IntMap.map (\t -> (t, -(xoff fs))) a1
    (rendered_wires, rendered_gates) = render_gates fs (Map.fromList (IntMap.assocs xa1)) xgs ys raw_width maxh

-- | Render a low-level boxed quantum circuit as a graphical
-- 'Document'. If there are subroutines, each of them is placed on a
-- separate page.
render_bcircuit :: FormatStyle -> BCircuit -> Document ()
render_bcircuit fs (circ, namespace) = do
  page_of_ocircuit fs Nothing (OCircuit ([], circ, []))
  sequence_ [ page_of_ocircuit fs (Just boxid) ocirc | (boxid, TypedSubroutine ocirc _ _ _) <- Map.toList namespace]

-- | Render a low-level dynamic quantum circuit as a graphical
-- 'Document'. If there are subroutines, each of them is placed on a
-- separate page.  If the circuit uses dynamic lifting, an error is
-- produced.
render_dbcircuit :: FormatStyle -> ErrMsg -> DBCircuit a -> Document ()
render_dbcircuit fs e dbcirc = render_bcircuit fs bcirc where
  (bcirc, _) = bcircuit_of_static_dbcircuit errmsg dbcirc
  errmsg x = e ("operation not permitted during graphical rendering: " ++ x)


-- ----------------------------------------------------------------------
-- * Interface to external programs

-- | @'system_pdf_viewer' zoom pdffile@: Call a system-specific PDF
-- viewer on /pdffile/ file. The /zoom/ argument is out of 100 and may
-- or may not be ignored by the viewer.
system_pdf_viewer :: Double -> String -> IO ()
system_pdf_viewer zoom pdffile = do
  envList <- getEnvironment
  if (List.elem ("OS", "Windows_NT") envList)
  then do
    rawSystem "acroread.bat" [pdffile]
  else if (os == "darwin")
  then do
    rawSystem "open" [pdffile]
    rawSystem "sleep" ["1"] -- required or the file may be deleted too soon
  else do
    rawSystem "acroread" ["/a", "zoom=100", pdffile]
  return ()

-- ----------------------------------------------------------------------
-- * Previewing

-- | Display a document directly in Acrobat Reader. This may not be
-- portable. It requires the external program \"acroread\" to be
-- installed.
preview_document :: Document a -> IO a
preview_document = preview_document_custom custom

-- | Display a document directly in Acrobat Reader. This may not be
-- portable. It requires the external program \"acroread\" to be
-- installed.
preview_document_custom :: Custom -> Document a -> IO a
preview_document_custom custom doc = do
  tmpdir <- getTemporaryDirectory
  (pdffile, fd) <- openTempFile tmpdir "Quipper.pdf"
  a <- render_custom_file fd Format_PDF custom doc
  hClose fd
  system_pdf_viewer 100 pdffile
  removeFile pdffile
  return a

-- | Display the circuit directly in Acrobat Reader. This may not be
-- portable. It requires the external program \"acroread\" to be
-- installed.
preview_bcircuit :: BCircuit -> IO ()
preview_bcircuit bcirc =
  preview_document doc
  where
    doc = render_bcircuit pdf bcirc

-- | Display a low-level dynamic quantum circuit directly in Acrobat
-- Reader. This may not be portable. It requires the external program
-- \"acroread\" to be installed. If the circuit uses dynamic lifting,
-- an error is produced.
preview_dbcircuit :: ErrMsg -> DBCircuit a -> IO ()
preview_dbcircuit e dbcirc = preview_bcircuit bcirc where
  (bcirc, _) = bcircuit_of_static_dbcircuit errmsg dbcirc
  errmsg x = e ("operation not permitted for PDF preview: " ++ x)

-- ----------------------------------------------------------------------
-- * Printing to multiple formats

-- | Available output formats.

data Format = 
  EPS         -- ^ Encapsulated PostScript graphics.
  | PDF       -- ^ Portable Document Format. One circuit per page.
  | PS        -- ^ PostScript. One circuit per page.
  | ASCII     -- ^ A textual representation of circuits.
  | Preview   -- ^ Don't print anything, but preview directly on screen (requires the external program /acroread/).
  | GateCount -- ^ Print statistics on gate counts.
  | CustomStyle FormatStyle
  deriving Show
    
-- | A mapping from lower-case strings (to be used, e.g., with command
-- line options) to available formats.
format_enum :: [(String, Format)]
format_enum = [
  ("eps", EPS),
  ("pdf", PDF),
  ("ps", PS),
  ("postscript", PS),
  ("ascii", ASCII),
  ("preview", Preview),
  ("gatecount", GateCount)
  ]
                    
-- | Print a low-level quantum circuit directly to the IO monad, using
-- the specified format.
print_dbcircuit :: Format -> ErrMsg -> DBCircuit a -> IO ()
print_dbcircuit Preview = preview_dbcircuit

-- | Print a document to the requested format, which must be one of
-- 'PS', 'PDF', 'EPS', or 'Preview'.
print_of_document :: Format -> Document a -> IO a
print_of_document = print_of_document_custom custom

-- | Like 'print_of_document', but also takes a 'Custom' data
-- structure.
print_of_document_custom :: Custom -> Format -> Document a -> IO a
print_of_document_custom custom Preview doc = preview_document_custom custom doc

-- ======================================================================
-- * Generic printing

-- | Like 'print_unary', but also takes a stub error message.
print_errmsg :: (QCData qa) => ErrMsg -> Format -> (qa -> Circ b) -> qa -> IO ()
print_errmsg e format f shape = print_dbcircuit format e dbcircuit
  where 
    (in_bind, dbcircuit) = encapsulate_dynamic f shape

-- | Print a circuit generating function to the specified format; this
-- requires a shape parameter.
print_unary :: (QCData qa) => Format -> (qa -> Circ b) -> qa -> IO ()
print_unary = print_errmsg errmsg
  where 
    errmsg x = "print_unary: " ++ x

-- | Print a circuit generating function to the specified
-- format. Unlike 'print_unary', this can be applied to a
-- circuit-generating function in curried form with /n/ arguments, for
-- any /n >= 0/. It then requires /n/ shape parameters.
-- 
-- The type of this heavily overloaded function is difficult to
-- read. In more readable form, it has all of the following types:
-- 
-- > print_generic :: Format -> Circ qa -> IO ()
-- > print_generic :: (QCData qa) => Format -> (qa -> Circ qb) -> a -> IO ()
-- > print_generic :: (QCData qa, QCData qb) => Format -> (qa -> qb -> Circ qc) -> a -> b -> IO ()
-- 
-- and so forth.
 
print_generic :: (QCData qa, QCurry qfun qa b, Curry fun qa (IO())) => Format -> qfun -> fun
print_generic format f = g where
  f1 = quncurry f
  g1 = print_errmsg errmsg format f1
  g = mcurry g1
  errmsg x = "print_generic: " ++ x

-- | Like 'print_generic', but only works at simple types, and
-- therefore requires no shape parameters.
print_simple :: (QCData qa, QCurry qfun qa b, Curry fun qa (IO()), QCData_Simple qa) => Format -> qfun -> IO ()
print_simple format f = print_errmsg errmsg format f1 fs_shape where
  f1 = quncurry f
  errmsg x = "print_simple: " ++ x

preview_withColor :: (QCData qa, QCurry qfun qa b, Curry fun qa (IO())) => qfun -> fun
preview_withColor = print_generic Preview