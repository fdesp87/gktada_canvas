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

package body Gtkada.Canvas_List_Model_Pkg is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out List_Canvas_Model) is
   begin
      Self := new List_Canvas_Model_Record;
      Canvas_Model_Pkg.Initialize (Self);
   end Gtk_New;

   ---------
   -- Add --
   ---------

   procedure Add
     (Self : not null access List_Canvas_Model_Record;
      Item : not null access Abstract_Item_Record'Class)
   is
   begin
      Self.Items.Append (Abstract_Item (Item));
   end Add;

   -----------
   -- Clear --
   -----------

   procedure Clear
     (Self : not null access List_Canvas_Model_Record)
   is
      use Items_Lists;
      Items : constant Items_Lists.List := Self.Items;  --  a copy of the list
      C     : Items_Lists.Cursor := Items.First;
   begin
      --  More efficient to clear the list first, so that 'Remove' finds no
      --  related link (since we are going to free them anyway).
      Self.Items.Clear;

      while Has_Element (C) loop
         Canvas_Model_Record'Class (Self.all).Remove (Element (C));
         Next (C);
      end loop;
      Canvas_Model_Record'Class (Self.all).Refresh_Layout;
   end Clear;

   ------------
   -- Remove --
   ------------

   overriding procedure Remove
     (Self : not null access List_Canvas_Model_Record;
      Item : not null access Abstract_Item_Record'Class)
   is
      To_Remove : Item_Sets.Set;
   begin
      Self.Include_Related_Items (Item, To_Remove);
      Remove (Self, To_Remove);
   end Remove;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Self : not null access List_Canvas_Model_Record;
      Set  : Item_Sets.Set)
   is
      use Item_Sets, Items_Lists;
      C2   : Item_Sets.Cursor := Set.First;
      It   : Abstract_Item;
      C    : Items_Lists.Cursor;
   begin
      --  First pass: remove the items from the list of items. This means
      --  that when we later destroy the items (and thus in the case of
      --  container_item we remove the link to their children), we will not
      --  see these items again.

      while Has_Element (C2) loop
         It := Element (C2);
         Next (C2);
         Self.Remove_From_Selection (It);

         C := Self.Items.Find (It);
         if Has_Element (C) then
            Self.Items.Delete (C);
         end if;
      end loop;

      --  Now destroy the items

      C2 := Set.First;
      while Has_Element (C2) loop
         It := Element (C2);
         Next (C2);
         Destroy_And_Free (It, In_Model => Self);
      end loop;
   end Remove;

   -------------------
   -- For_Each_Item --
   -------------------

   overriding procedure For_Each_Item
     (Self     : not null access List_Canvas_Model_Record;
      Callback : not null access procedure
        (Item : not null access Abstract_Item_Record'Class);
      Selected_Only : Boolean := False;
      Filter        : Item_Kind_Filter := Kind_Any;
      In_Area       : Model_Rectangle := No_Rectangle)
   is
      use Items_Lists;
      C    : Items_Lists.Cursor := Self.Items.First;
      Item : Abstract_Item;
   begin
      while Has_Element (C) loop
         Item := Element (C);

         --  ??? Might not work when the Callback removes the item, which in
         --  turn removes a link which might happen to be the next element
         --  we were pointing to.
         Next (C);

         if (Filter = Kind_Any
             or else (Filter = Kind_Item and then not Item.Is_Link)
             or else (Filter = Kind_Link and then Item.Is_Link))
           and then
             (not Selected_Only
              or else List_Canvas_Model (Self).Is_Selected (Item))
           and then
             (In_Area = No_Rectangle
              or else Intersects (In_Area, Item.Model_Bounding_Box))
         then
            Callback (Item);
         end if;
      end loop;
   end For_Each_Item;

   ----------------
   -- Raise_Item --
   ----------------

   overriding procedure Raise_Item
     (Self : not null access List_Canvas_Model_Record;
      Item : not null access Abstract_Item_Record'Class)
   is
      use Items_Lists;
      C : Items_Lists.Cursor := Self.Items.First;
   begin
      while Has_Element (C) loop
         if Element (C) = Abstract_Item (Item) then
            if C /= Self.Items.Last then
               Self.Items.Delete (C);
               Self.Items.Append (Abstract_Item (Item));
               List_Canvas_Model_Record'Class (Self.all).Layout_Changed;
            end if;

            return;
         end if;
         Next (C);
      end loop;
   end Raise_Item;

   ----------------
   -- Lower_Item --
   ----------------

   procedure Lower_Item
     (Self : not null access List_Canvas_Model_Record;
      Item : not null access Abstract_Item_Record'Class)
   is
      use Items_Lists;
      C : Items_Lists.Cursor := Self.Items.First;
   begin
      if not Has_Element (C)
        or else Element (C) = Abstract_Item (Item)
      then
         return;   --  nothing to do
      end if;

      Next (C);

      while Has_Element (C) loop
         if Element (C) = Abstract_Item (Item) then
            Self.Items.Delete (C);
            Self.Items.Prepend (Abstract_Item (Item));
            List_Canvas_Model_Record'Class (Self.all).Layout_Changed;
            return;
         end if;
         Next (C);
      end loop;
   end Lower_Item;

   ----------------------
   -- Toplevel_Item_At --
   ----------------------

   overriding function Toplevel_Item_At
     (Self    : not null access List_Canvas_Model_Record;
      Point   : Model_Point;
      Context : Draw_Context) return Abstract_Item
   is
      use Items_Lists;
      C : Items_Lists.Cursor := Self.Items.Last;
      Item : Abstract_Item;
   begin
      while Has_Element (C) loop
         Item := Element (C);
         if Item.Contains (Model_To_Item (Item, Point), Context) then
            return Item;
         end if;
         Previous (C);
      end loop;
      return null;
   end Toplevel_Item_At;

end Gtkada.Canvas_List_Model_Pkg;
