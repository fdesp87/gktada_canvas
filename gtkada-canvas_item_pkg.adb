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

with Gtkada.Canvas_View_Pkg.Objects;

package body Gtkada.Canvas_Item_Pkg is

   --------------
   -- Position --
   --------------

   overriding function Position
     (Self : not null access Canvas_Item_Record) return Gtkada.Style_Pkg.Point
   is
   begin
      return Self.Position;
   end Position;

   --------------
   -- Contains --
   --------------

   overriding function Contains
     (Self    : not null access Canvas_Item_Record;
      Point   : Item_Point;
      Context : Draw_Context) return Boolean
   is
      pragma Unreferenced (Context);
      Box : constant Item_Rectangle :=
        Canvas_Item_Record'Class (Self.all).Bounding_Box;
   begin
      --  ??? This does not take into account the line width
      return Point_In_Rect (Box, Point);
   end Contains;

   -----------------------
   -- Link_Anchor_Point --
   -----------------------

   overriding function Link_Anchor_Point
     (Self   : not null access Canvas_Item_Record;
      Anchor : Anchor_Attachment)
      return Item_Point
   is
   begin
      return Canvas_View_Pkg.Objects.Link_Anchor_Point (Self, Anchor);
   end Link_Anchor_Point;

   ---------------
   -- Clip_Line --
   ---------------

   overriding function Clip_Line
     (Self   : not null access Canvas_Item_Record;
      P1, P2 : Item_Point) return Item_Point
   is
      Box    : constant Item_Rectangle  :=
        Canvas_Item_Record'Class (Self.all).Bounding_Box;
      Deltax : constant Item_Coordinate := P2.X - P1.X;
      Deltay : constant Item_Coordinate := P2.Y - P1.Y;
      Offset : constant Item_Coordinate := P1.X * P2.Y - P2.X * P1.Y;
      Result : Item_Point;

   begin
      if Deltay /= 0.0 then
         if Deltay < 0.0 then
            Result.Y := Box.Y;
         else
            Result.Y := Box.Y + Box.Height;
         end if;

         Result.X := (Deltax * Result.Y + Offset) / Deltay;
         if Box.X <= Result.X and then Result.X <= Box.X + Box.Width then
            return Result;
         end if;
      end if;

      if Deltax /= 0.0 then
         if Deltax < 0.0 then
            Result.X := Box.X;
         else
            Result.X := Box.X + Box.Width;
         end if;

         Result.Y := (Deltay * Result.X - Offset) / Deltax;

         if Box.Y <= Result.Y and then Result.Y <= Box.Y + Box.Height then
            return Result;
         end if;
      end if;

      return (Box.X, Box.Y);
   end Clip_Line;

   -----------------
   -- Edit_Widget --
   -----------------

   function Edit_Widget
     (Self  : not null access Canvas_Item_Record;
      View  : not null access Canvas_View_Record'Class)
      return Gtk.Widget.Gtk_Widget
   is
      pragma Unreferenced (Self, View);
   begin
      --  for some reason, we need to define this function even though the
      --  one defined for the interface is already defined as "null".
      --  Otherwise, dispatching from Start_Inline_Editing does not work...
      return null;
   end Edit_Widget;

   ----------------------
   -- Draw_As_Selected --
   ----------------------

   overriding procedure Draw_As_Selected
     (Self            : not null access Canvas_Item_Record;
      Context         : Draw_Context) is
   begin
      if Context.View /= null then
         Canvas_Item (Self).Draw_Outline
           (Style   => Context.View.The_Selection_Style,
            Context => Context);
      end if;
      Canvas_Item_Record'Class (Self.all).Draw (Context);
   end Draw_As_Selected;

   ------------------
   -- Draw_Outline --
   ------------------

   procedure Draw_Outline
     (Self    : not null access Canvas_Item_Record;
      Style   : Gtkada.Style_Pkg.Drawing_Style;
      Context : Draw_Context)
   is
      Box : constant Item_Rectangle :=
        Canvas_Item_Record'Class (Self.all).Bounding_Box;
      Margin_Pixels : constant View_Coordinate := 2.0;
      Margin        : Model_Coordinate;
   begin
      if Context.View /= null then
         Margin := Margin_Pixels / Context.View.The_Scale;
         Style.Draw_Rect
           (Context.Cr,
            (Box.X - Margin, Box.Y - Margin),
            Box.Width + 2.0 * Margin,
            Box.Height + 2.0 * Margin);
      end if;
   end Draw_Outline;

   ------------------------------
   -- Set_Visibility_Threshold --
   ------------------------------

   procedure Set_Visibility_Threshold
     (Self      : not null access Canvas_Item_Record;
      Threshold : Gdouble)
   is
   begin
      Self.Visibility_Threshold := Threshold;
   end Set_Visibility_Threshold;

   ------------------------------
   -- Get_Visibility_Threshold --
   ------------------------------

   function Get_Visibility_Threshold
     (Self : not null access Canvas_Item_Record) return Gdouble is
   begin
      return Self.Visibility_Threshold;
   end Get_Visibility_Threshold;

   ------------------
   -- Set_Position --
   ------------------

   procedure Set_Position
     (Self  : not null access Canvas_Item_Record;
      Pos   : Gtkada.Style_Pkg.Point) is
   begin
      Self.Position := Pos;
   end Set_Position;

end Gtkada.Canvas_Item_Pkg;
