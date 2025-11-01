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

with Ada.Unchecked_Deallocation;
with System.Storage_Elements;            use System.Storage_Elements;
with Gtkada.Bindings;                    use Gtkada.Bindings;
with Cairo;                              use Cairo;

with Gtkada.Canvas_View_Pkg;             use Gtkada.Canvas_View_Pkg;
with Gtkada.Canvas_Model_Pkg;            use Gtkada.Canvas_Model_Pkg;

package body Gtkada.Canvas_Abstract_Item_Pkg is

   Debug_Show_Bounding_Boxes : constant Boolean := False;
   --  Set to True to visualize the bounding boxes of items

   -----------------------------
   -- GValue_To_Abstract_Item --
   -----------------------------

   function GValue_To_Abstract_Item (Value : GValue) return Abstract_Item is
      S : constant System.Address := Get_Address (Value);
      pragma Warnings (Off, "possible aliasing problem*");
      function Unchecked_Convert is new Ada.Unchecked_Conversion
        (System.Address, Abstract_Item);
      pragma Warnings (On, "possible aliasing problem*");
   begin
      return Unchecked_Convert (S);
   end GValue_To_Abstract_Item;

   ----------
   -- Hash --
   ----------

   function Hash (Key : Abstract_Item) return Ada.Containers.Hash_Type is
   begin
      if Key = null then
         return 0;
      else
         return Ada.Containers.Hash_Type
           (To_Integer (Key.all'Address)
            mod Integer_Address (Ada.Containers.Hash_Type'Last));
      end if;
   end Hash;

   --------------------------
   -- Size_Above_Threshold --
   --------------------------
   --  internal
   function Size_Above_Threshold
     (Self : not null access Abstract_Item_Record'Class;
      View : access Canvas_View_Record'Class) return Boolean;
   --  Whether the item's size is above the visibility threshold, i.e. whether
   --  the item is visible.

   function Size_Above_Threshold
     (Self : not null access Abstract_Item_Record'Class;
      View : access Canvas_View_Record'Class) return Boolean
   is
      R   : View_Rectangle;
      Threshold : constant Gdouble := Self.Get_Visibility_Threshold;
   begin
      if Threshold = Gdouble'Last then
         --  Always hidden
         return False;
      elsif Threshold > 0.0 and then View /= null then
         R := View.Model_To_View (Self.Model_Bounding_Box);
         if R.Width < Threshold or else R.Height < Threshold then
            return False;
         end if;
      end if;
      return True;
   end Size_Above_Threshold;

   -----------------------------
   -- Translate_And_Draw_Item --
   -----------------------------

   procedure Translate_And_Draw_Item
     (Self          : not null access Abstract_Item_Record'Class;
      Context       : Draw_Context;
      As_Outline    : Boolean := False;
      Outline_Style : Drawing_Style := No_Drawing_Style)
   is
      Pos : Gtkada.Style_Pkg.Point;
   begin
      if not Size_Above_Threshold (Self, Context.View) then
         return;
      end if;

      Save (Context.Cr);
      Pos := Self.Position;
      Translate (Context.Cr, Pos.X, Pos.Y);

      if As_Outline then
         Self.Draw_Outline (Outline_Style, Context);
      elsif Context.View /= null
        and then Context.View.Model /= null
        and then Context.View.Model.Is_Selected (Self)
      then
         Self.Draw_As_Selected (Context);
      else
         Self.Draw (Context);
      end if;

      if Debug_Show_Bounding_Boxes then
         declare
            Box : constant Item_Rectangle := Self.Bounding_Box;
         begin
            Gtk_New (Stroke => (1.0, 0.0, 0.0, 0.8),
                     Dashes => (2.0, 2.0))
              .Draw_Rect (Context.Cr, (Box.X, Box.Y), Box.Width, Box.Height);
         end;
      end if;

      Restore (Context.Cr);

   exception
      when E : others =>
         Restore (Context.Cr);
         Process_Exception (E);
   end Translate_And_Draw_Item;

   -----------------------
   -- Get_Toplevel_Item --
   -----------------------

   function Get_Toplevel_Item
     (Self : not null access Abstract_Item_Record'Class)
      return Abstract_Item
   is
      Result : Abstract_Item := Abstract_Item (Self);
      P      : Abstract_Item;
   begin
      loop
         P := Parent (Result);
         exit when P = null;
         Result := P;
      end loop;
      return Result;
   end Get_Toplevel_Item;

   ------------------------
   -- Model_Bounding_Box --
   ------------------------

   function Model_Bounding_Box
     (Self     : not null access Abstract_Item_Record'Class)
      return Model_Rectangle
   is
   begin
      return Self.Item_To_Model (Self.Bounding_Box);
   end Model_Bounding_Box;

   ----------
   -- Show --
   ----------

   procedure Show (Self : not null access Abstract_Item_Record'Class) is
   begin
      Self.Set_Visibility_Threshold (Threshold => 0.0);
   end Show;

   ----------
   -- Hide --
   ----------

   procedure Hide (Self : not null access Abstract_Item_Record'Class) is
   begin
      Self.Set_Visibility_Threshold (Threshold => Gdouble'Last);
   end Hide;

   ----------------------
   -- Destroy_And_Free --
   ----------------------

   procedure Destroy_And_Free
     (Self     : in out Abstract_Item;
      In_Model : not null access Canvas_Model_Record'Class)
   is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Abstract_Item_Record'Class, Abstract_Item);
   begin
      if Self /= null then
         Item_Destroyed (In_Model, Self);
         Destroy (Self, In_Model);
         Unchecked_Free (Self);
      end if;
   end Destroy_And_Free;

   -------------------
   -- Item_To_Model --
   -------------------

   function Item_To_Model
     (Item   : not null access Abstract_Item_Record'Class;
      Rect   : Item_Rectangle) return Model_Rectangle
   is
      Parent : Abstract_Item := Abstract_Item (Item);
      Pos    : Item_Point;
      Result : Model_Rectangle := (Rect.X, Rect.Y, Rect.Width, Rect.Height);

   begin
      while Parent /= null loop
         --  ??? Should take item rotation into account when we implement it.

         Pos := Position (Parent);
         Result.X := Result.X + Pos.X;
         Result.Y := Result.Y + Pos.Y;

         Parent := Parent.Parent;
      end loop;
      return Result;
   end Item_To_Model;

   -------------------
   -- Item_To_Model --
   -------------------

   function Item_To_Model
     (Item   : not null access Abstract_Item_Record'Class;
      P      : Item_Point) return Model_Point
   is
      R : constant Model_Rectangle :=
        Item.Item_To_Model ((P.X, P.Y, 0.0, 0.0));
   begin
      return (R.X, R.Y);
   end Item_To_Model;

   -------------------
   -- Model_To_Item --
   -------------------

   function Model_To_Item
     (Item   : not null access Abstract_Item_Record'Class;
      P      : Model_Point) return Item_Point
   is
      Rect   : constant Item_Rectangle :=
        Model_To_Item (Item, (P.X, P.Y, 1.0, 1.0));
   begin
      return (Rect.X, Rect.Y);
   end Model_To_Item;

   -------------------
   -- Model_To_Item --
   -------------------

   function Model_To_Item
     (Item   : not null access Abstract_Item_Record'Class;
      P      : Model_Rectangle) return Item_Rectangle
   is
      Parent : Abstract_Item := Abstract_Item (Item);
      Result : Item_Rectangle := (P.X, P.Y, P.Width, P.Height);
      Pos    : Item_Point;
   begin
      while Parent /= null loop
         Pos := Parent.Position;
         Result.X := Result.X - Pos.X;
         Result.Y := Result.Y - Pos.Y;
         Parent := Parent.Parent;
      end loop;
      return Result;
   end Model_To_Item;
end Gtkada.Canvas_Abstract_Item_Pkg;
