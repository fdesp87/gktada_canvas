------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2018, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------
-- Modified by JLF (fdesp87@gmail.com) to separate the various concepts.    --
-- See gtkada.canvas_view                                                   --
------------------------------------------------------------------------------

package body Gtkada.Canvas_Link_Pkg.Best_Route_Pkg is

   ---------------------------------------
   --  Quadrant_Position_Of_Point_To  --
   ---------------------------------------

   function Quadrant_Position_Of_Point_To
     (Point_From, Point_To : Item_Point) return Quadrant is
   begin
      if Point_From.X < Point_To.X then
         if Point_From.Y < Point_To.Y then
            return Sud_East;
         else
            return North_East;
         end if;
      else
         if Point_From.Y < Point_To.Y then
            return Sud_West;
         else
            return North_West;
         end if;
      end if;
   end Quadrant_Position_Of_Point_To;

   ---------------------------
   --  Get_Best_Entry_Side  --
   ---------------------------

   function Best_Entry_Side (Start_Point    : Item_Point;
                             Exit_Side      : Side_Attachment;
                             End_Point      : Item_Point;
                             Next_End_Point : Item_Point)
                             return Side_Attachment is
      Q_End_Point : constant Quadrant :=
        Quadrant_Position_Of_Point_To (Point_From => Start_Point,
                                       Point_To   => End_Point);
      Q_Next_End_Point : constant Quadrant :=
        Quadrant_Position_Of_Point_To (Point_From => End_Point,
                                       Point_To   => Next_End_Point);
   begin
      case Exit_Side is
         when Top =>
            case Q_End_Point is
               when North_West =>
                  case Q_Next_End_Point is
                     when North_West =>
                        return Bottom;
                     when North_East =>
                        return Bottom;
                     when Sud_East =>
                        return Top;
                     when Sud_West =>
                        return Top;
                  end case;
               when North_East =>
                  case Q_Next_End_Point is
                     when North_West =>
                        return Bottom;
                     when North_East =>
                        return Bottom;
                     when Sud_East =>
                        return Top;
                     when Sud_West =>
                        return Top;
                  end case;
               when Sud_East =>
                  case Q_Next_End_Point is
                     when North_West =>
                        return Left;
                     when North_East =>
                        return Left;
                     when Sud_East =>
                        return Top;
                     when Sud_West =>
                        return Top;
                  end case;
               when Sud_West =>
                  case Q_Next_End_Point is
                     when North_West =>
                        return Right;
                     when North_East =>
                        return Right;
                     when Sud_East =>
                        return Top;
                     when Sud_West =>
                        return Top;
                  end case;
            end case;
         when Bottom =>
            case Q_End_Point is
               when North_West =>
                  case Q_Next_End_Point is
                     when North_West =>
                        return Bottom;
                     when North_East =>
                        return Bottom;
                     when Sud_East =>
                        return Left;
                     when Sud_West =>
                        return Right;
                  end case;
               when North_East =>
                  case Q_Next_End_Point is
                     when North_West =>
                        return Bottom;
                     when North_East =>
                        return Bottom;
                     when Sud_East =>
                        return Right;
                     when Sud_West =>
                        return Left;
                  end case;
               when Sud_East =>
                  case Q_Next_End_Point is
                     when North_West =>
                        return Bottom;
                     when North_East =>
                        return Bottom;
                     when Sud_East =>
                        return Top;
                     when Sud_West =>
                        return Top;
                  end case;
               when Sud_West =>
                  case Q_Next_End_Point is
                     when North_West =>
                        return Right;
                     when North_East =>
                        return Bottom;
                     when Sud_East =>
                        return Top;
                     when Sud_West =>
                        return Top;
                  end case;
            end case;
         when Left   =>
            case Q_End_Point is
               when North_West =>
                  case Q_Next_End_Point is
                     when North_West =>
                        return Bottom;
                     when North_East =>
                        return Bottom;
                     when Sud_East =>
                        return Top;
                     when Sud_West =>
                        return Right;
                  end case;
               when North_East =>
                  case Q_Next_End_Point is
                     when North_West =>
                        return Bottom;
                     when North_East =>
                        return Bottom;
                     when Sud_East =>
                        return Left;
                     when Sud_West =>
                        return Top;
                  end case;
               when Sud_East =>
                  case Q_Next_End_Point is
                     when North_West =>
                        return Bottom;
                     when North_East =>
                        return Bottom;
                     when Sud_East =>
                        return Left;
                     when Sud_West =>
                        return Top;
                  end case;
               when Sud_West =>
                  case Q_Next_End_Point is
                     when North_West =>
                        return Left;
                     when North_East =>
                        return Right;
                     when Sud_East =>
                        return Top;
                     when Sud_West =>
                        return Top;
                  end case;
            end case;
         when Right  =>
            case Q_End_Point is
               when North_West =>
                  case Q_Next_End_Point is
                     when North_West =>
                        return Right;
                     when North_East =>
                        return Bottom;
                     when Sud_East =>
                        return Top;
                     when Sud_West =>
                        return Right;
                  end case;
               when North_East =>
                  case Q_Next_End_Point is
                     when North_West =>
                        return Bottom;
                     when North_East =>
                        return Bottom;
                     when Sud_East =>
                        return Right;
                     when Sud_West =>
                        return Top;
                  end case;
               when Sud_East =>
                  case Q_Next_End_Point is
                     when North_West =>
                        return Bottom;
                     when North_East =>
                        return Left;
                     when Sud_East =>
                        return Top;
                     when Sud_West =>
                        return Top;
                  end case;
               when Sud_West =>
                  case Q_Next_End_Point is
                     when North_West =>
                        return Right;
                     when North_East =>
                        return Bottom;
                     when Sud_East =>
                        return Top;
                     when Sud_West =>
                        return Right;
                  end case;
            end case;
         when others =>
            raise Program_Error with "case not considered";
      end case;
   end Best_Entry_Side;

end Gtkada.Canvas_Link_Pkg.Best_Route_Pkg;
