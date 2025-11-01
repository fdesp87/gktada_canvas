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

with Interfaces.C.Strings;               use Interfaces.C.Strings;
with Gtk.Handlers;                       use Gtk.Handlers;
with Gtkada.Handlers;                    use Gtkada.Handlers;
with Gtkada.Style_Pkg;                       use Gtkada.Style_Pkg;

with Gtkada.Canvas_Link_Pkg;             use Gtkada.Canvas_Link_Pkg;
with Gtkada.Canvas_View_Pkg;             use Gtkada.Canvas_View_Pkg;

package body Gtkada.Canvas_Model_Pkg is

   Model_Signals : constant Interfaces.C.Strings.chars_ptr_array :=
     (1 => New_String (String (Signal_Item_Contents_Changed)),
      2 => New_String (String (Signal_Layout_Changed)),
      3 => New_String (String (Signal_Selection_Changed)),
      4 => New_String (String (Signal_Item_Destroyed)));

   Model_Class_Record : Glib.Object.Ada_GObject_Class :=
     Glib.Object.Uninitialized_Class;

   --------------------
   -- Model_Get_Type --
   --------------------

   function Model_Get_Type return Glib.GType is
   begin
      Glib.Object.Initialize_Class_Record
        (Ancestor     => GType_Object,
         Signals      => Model_Signals,
         Class_Record => Model_Class_Record,
         Type_Name    => "GtkadaCanvasModel",
         Parameters   => (1 => (1 => GType_Pointer), --  item_content_changed
                          2 => (1 => GType_None),  --  layout_changed
                          3 => (1 => GType_Pointer),
                          4 => (1 => GType_Pointer)));  --  item_destroyed
      return Model_Class_Record.The_Type;
   end Model_Get_Type;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : not null access Canvas_Model_Record'Class) is
   begin
      if not Self.Is_Created then
         G_New (Self, Model_Get_Type);
      end if;

      --  ??? When destroyed, should unreferenced Self.Layout
   end Initialize;

   ------------------------
   -- Get_Selected_Items --
   ------------------------

   function Get_Selected_Items
     (Self : not null access Canvas_Model_Record) return Item_Sets.Set is
   begin
      return Self.Selection;
   end Get_Selected_Items;

   -------------------
   -- For_Each_Link --
   -------------------

   procedure For_Each_Link
     (Self       : not null access Canvas_Model_Record;
      Callback   : not null access procedure
        (Item : not null access Abstract_Item_Record'Class);
      From_Or_To : Item_Sets.Set)
   is
      function Matches
        (It : not null access Abstract_Item_Record'Class) return Boolean;
      pragma Inline (Matches);
      --  Whether the corresponding link should be returned

      function Matches
        (It : not null access Abstract_Item_Record'Class) return Boolean
      is
         L : Canvas_Link;
      begin
         --  A custom link ? We don't know how to check its ends, so always
         --  return it.
         if It.all not in Canvas_Link_Record'Class then
            return True;
         end if;

         L := Canvas_Link (It);

         return From_Or_To.Contains (L.Get_From.Get_Toplevel_Item)
           or else From_Or_To.Contains (L.Get_To.Get_Toplevel_Item)
           or else (L.Get_From.Is_Link and then Matches (L.Get_From))
           or else (L.Get_To.Is_Link and then Matches (L.Get_To));
      end Matches;

      procedure Local (It : not null access Abstract_Item_Record'Class);
      procedure Local (It : not null access Abstract_Item_Record'Class) is
      begin
         if Matches (It) then
            Callback (It);
         end if;
      end Local;

   begin
      Canvas_Model_Record'Class (Self.all).For_Each_Item
        (Local'Access, Filter => Kind_Link);
   end For_Each_Link;

   ---------------------------
   -- Include_Related_Items --
   ---------------------------

   procedure Include_Related_Items
     (Self : not null access Canvas_Model_Record'Class;
      Item : not null access Abstract_Item_Record'Class;
      Set  : in out Item_Sets.Set)
   is
      procedure Internal (C : not null access Abstract_Item_Record'Class);
      procedure Internal (C : not null access Abstract_Item_Record'Class) is
      begin
         if C.all in Canvas_Link_Record'Class
           and then (Canvas_Link (C).Get_From = Abstract_Item (Item)
                     or else Canvas_Link (C).Get_To = Abstract_Item (Item))
         then
            Include_Related_Items (Self, C, Set);
         end if;
      end Internal;

   begin
      Self.For_Each_Item (Internal'Access, Filter => Kind_Link);

      --  Removing the container items will call their destroy, and therefore
      --  remove all links to their children.

      Set.Include (Abstract_Item (Item));
   end Include_Related_Items;

   ----------
   -- From --
   ----------

   procedure From
     (Self : not null access Canvas_Model_Record'Class;
      Item : not null access Abstract_Item_Record'Class;
      Set  : in out Item_Sets.Set)
   is
      procedure Internal (C : not null access Abstract_Item_Record'Class);
      procedure Internal (C : not null access Abstract_Item_Record'Class) is
      begin
         if C.all in Canvas_Link_Record'Class
           and then Canvas_Link (C).Get_From = Abstract_Item (Item)
         then
            declare
               Cur : constant Abstract_Item := Canvas_Link (C).Get_To;
            begin
               if not Set.Contains (Abstract_Item (C)) then
                  Set.Include (Abstract_Item (C));
                  if not Set.Contains (Cur) then
                     Set.Include (Cur);
                     To (Self, Cur, Set);
                     From (Self, Cur, Set);
                  end if;
               end if;
            end;
         end if;
      end Internal;
   begin
      Self.For_Each_Item (Internal'Access, Filter => Kind_Link);
   end From;

   --------
   -- To --
   --------

   procedure To
     (Self : not null access Canvas_Model_Record'Class;
      Item : not null access Abstract_Item_Record'Class;
      Set  : in out Item_Sets.Set)
   is
      procedure Internal (C : not null access Abstract_Item_Record'Class);
      procedure Internal (C : not null access Abstract_Item_Record'Class) is
      begin
         if C.all in Canvas_Link_Record'Class
           and then Canvas_Link (C).Get_To = Abstract_Item (Item)
         then
            declare
               Cur : constant Abstract_Item := Canvas_Link (C).Get_From;
            begin
               if not Set.Contains (Abstract_Item (C)) then
                  Set.Include (Abstract_Item (C));
                  if not Set.Contains (Cur) then
                     Set.Include (Cur);
                     To (Self, Cur, Set);
                     From (Self, Cur, Set);
                  end if;
               end if;
            end;
         end if;
      end Internal;
   begin
      Self.For_Each_Item (Internal'Access, Filter => Kind_Link);
   end To;

   ------------------
   -- Bounding_Box --
   ------------------

   function Bounding_Box
     (Self   : not null access Canvas_Model_Record;
      Margin : Model_Coordinate := 0.0)
      return Model_Rectangle
   is
      Result : Model_Rectangle;
      Is_First : Boolean := True;

      procedure Do_Item (Item : not null access Abstract_Item_Record'Class);
      procedure Do_Item (Item : not null access Abstract_Item_Record'Class) is
         Box : constant Model_Rectangle := Item.Model_Bounding_Box;
      begin
         if Is_First then
            Is_First := False;
            Result := Box;
         else
            Union (Result, Box);
         end if;
      end Do_Item;
   begin
      Canvas_Model_Record'Class (Self.all).For_Each_Item
        (Do_Item'Access);

      if Is_First then
         return No_Rectangle;
      else
         Result.X := Result.X - Margin;
         Result.Y := Result.Y - Margin;
         Result.Width := Result.Width + 2.0 * Margin;
         Result.Height := Result.Height + 2.0 * Margin;
         return Result;
      end if;
   end Bounding_Box;

   --------------------
   -- Refresh_Layout --
   --------------------

   procedure Refresh_Layout
     (Self        : not null access Canvas_Model_Record;
      Send_Signal : Boolean := True)
   is
      Context : constant Draw_Context :=
        (Cr => <>, Layout => Self.Layout, View => null);

      procedure Do_Container_Layout
        (Item : not null access Abstract_Item_Record'Class);
      procedure Do_Container_Layout
        (Item : not null access Abstract_Item_Record'Class) is
      begin
         Item.Refresh_Layout (Context);
      end Do_Container_Layout;

   begin
      Canvas_Model_Record'Class (Self.all).For_Each_Item
        (Do_Container_Layout'Access, Filter => Kind_Item);
      Refresh_Link_Layout (Self);

      if Send_Signal then
         Canvas_Model_Record'Class (Self.all).Layout_Changed;
      end if;
   end Refresh_Layout;

   -----------------------
   -- Toplevel_Items_At --
   -----------------------

   function Toplevel_Item_At
     (Self    : not null access Canvas_Model_Record;
      Point   : Model_Point;
      Context : Draw_Context) return Abstract_Item
   is
      Found : Abstract_Item;

      procedure Check_Item
        (Item : not null access Abstract_Item_Record'Class);
      procedure Check_Item
        (Item : not null access Abstract_Item_Record'Class) is
      begin
         --  topmost items always occur later in the list.
         if Item.Contains (Model_To_Item (Item, Point), Context) then
            Found := Abstract_Item (Item);
         end if;
      end Check_Item;

   begin
      Canvas_Model_Record'Class (Self.all).For_Each_Item (Check_Item'Access);
      return Found;
   end Toplevel_Item_At;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Self : not null access Canvas_Model_Record;
      Set  : Item_Sets.Set)
   is
      use Item_Sets;
      C  : Item_Sets.Cursor := Set.First;
   begin
      while Has_Element (C) loop
         Canvas_Model_Record'Class (Self.all).Remove (Element (C));
         Next (C);
      end loop;
   end Remove;

   ------------------------
   -- Set_Selection_Mode --
   ------------------------

   procedure Set_Selection_Mode
     (Self : not null access Canvas_Model_Record;
      Mode : Selection_Mode)
   is
   begin
      if Mode /= Self.Mode then
         Self.Mode := Mode;
         Canvas_Model_Record'Class (Self.all).Clear_Selection;
      end if;
   end Set_Selection_Mode;

   ---------------------
   -- Clear_Selection --
   ---------------------

   procedure Clear_Selection (Self : not null access Canvas_Model_Record) is
   begin
      if not Self.Selection.Is_Empty then
         Self.Selection.Clear;
         Self.Selection_Changed;
      end if;
   end Clear_Selection;

   ----------------------
   -- Add_To_Selection --
   ----------------------

   procedure Add_To_Selection
     (Self : not null access Canvas_Model_Record;
      Item : not null access Abstract_Item_Record'Class) is
   begin
      case Self.Mode is
         when Selection_None =>
            null;

         when Selection_Single =>
            if Self.Is_Selectable (Item) then
               Canvas_Model_Record'Class (Self.all).Clear_Selection;
               Self.Selection.Include (Abstract_Item (Item));
               Self.Selection_Changed (Item);
            end if;

         when Selection_Multiple =>
            if Self.Is_Selectable (Item) then
               Self.Selection.Include (Abstract_Item (Item));
               Self.Selection_Changed (Item);
            end if;
      end case;
   end Add_To_Selection;

   ---------------------------
   -- Remove_From_Selection --
   ---------------------------

   procedure Remove_From_Selection
     (Self : not null access Canvas_Model_Record;
      Item : not null access Abstract_Item_Record'Class) is
   begin
      if Canvas_Model_Record'Class (Self.all).Is_Selected (Item) then
         Self.Selection.Delete (Abstract_Item (Item));
         Self.Selection_Changed (Item);
      end if;
   end Remove_From_Selection;

   -----------------
   -- Is_Selected --
   -----------------

   function Is_Selected
     (Self : not null access Canvas_Model_Record;
      Item : not null access Abstract_Item_Record'Class)
      return Boolean
   is
   begin
      return Self.Selection.Contains (Abstract_Item (Item));
   end Is_Selected;

   -----------------------
   -- Selection_Changed --
   -----------------------

   procedure Selection_Changed
     (Self : not null access Canvas_Model_Record'Class;
      Item : access Abstract_Item_Record'Class := null) is
   begin
      Abstract_Item_Emit
        (Self, Signal_Selection_Changed & ASCII.NUL,
         Abstract_Item (Item));
   end Selection_Changed;

   --------------------------
   -- On_Selection_Changed --
   --------------------------

   function On_Selection_Changed
     (Self : not null access Canvas_Model_Record'Class;
      Call : not null access procedure
        (Self : not null access GObject_Record'Class;
         Item : Abstract_Item);
      Slot : access GObject_Record'Class := null)
      return Gtk.Handlers.Handler_Id
   is
   begin
      if Slot = null then
         return Object_Callback.Connect
           (Self,
            Signal_Selection_Changed,
            Abstract_Item_Marshallers.To_Marshaller (Call));
      else
         return Object_Callback.Object_Connect
           (Self,
            Signal_Selection_Changed,
            Abstract_Item_Marshallers.To_Marshaller (Call),
            Slot);
      end if;
   end On_Selection_Changed;

   --------------------
   -- Layout_Changed --
   --------------------

   procedure Layout_Changed
     (Self : not null access Canvas_Model_Record'Class) is
   begin
      Object_Callback.Emit_By_Name (Self, Signal_Layout_Changed);
   end Layout_Changed;

   -----------------------
   -- On_Layout_Changed --
   -----------------------

   function On_Layout_Changed
     (Self : not null access Canvas_Model_Record'Class;
      Call : not null access procedure
        (Self : not null access GObject_Record'Class);
      Slot : access GObject_Record'Class := null)
      return Gtk.Handlers.Handler_Id
   is
   begin
      if Slot = null then
         return Object_Callback.Connect
            (Self,
             Signal_Layout_Changed,
             Object_Callback.To_Marshaller (Call));
      else
         return Object_Callback.Object_Connect
            (Self,
             Signal_Layout_Changed,
             Object_Callback.To_Marshaller (Call),
             Slot);
      end if;
   end On_Layout_Changed;

   -----------------------------
   -- Item_Contents_Changed --
   -----------------------------

   procedure Item_Contents_Changed
     (Self   : not null access Canvas_Model_Record'Class;
      Item : not null access Abstract_Item_Record'Class)
   is
   begin
      Abstract_Item_Emit
        (Self, Signal_Item_Contents_Changed & ASCII.NUL,
         Abstract_Item (Item));
   end Item_Contents_Changed;

   --------------------------------
   -- On_Item_Contents_Changed --
   --------------------------------

   function On_Item_Contents_Changed
     (Self : not null access Canvas_Model_Record'Class;
      Call : not null access procedure
        (Self : access GObject_Record'Class; Item : Abstract_Item);
      Slot : access GObject_Record'Class := null)
      return Gtk.Handlers.Handler_Id
   is
      Id : Handler_Id;
   begin
      if Slot = null then
         Id := Object_Callback.Connect
           (Self, Signal_Item_Contents_Changed,
            Abstract_Item_Marshallers.To_Marshaller (Call));
      else
         Id := Object_Callback.Object_Connect
           (Self, Signal_Item_Contents_Changed,
            Abstract_Item_Marshallers.To_Marshaller (Call), Slot);
      end if;
      return Id;
   end On_Item_Contents_Changed;

   -----------------------
   -- On_Item_Destroyed --
   -----------------------

   function On_Item_Destroyed
     (Self : not null access Canvas_Model_Record'Class;
      Call : not null access procedure
        (Self : access GObject_Record'Class;
         Item : Abstract_Item);
      Slot : access GObject_Record'Class := null)
      return Gtk.Handlers.Handler_Id
   is
      Id : Handler_Id;
   begin
      if Slot = null then
         Id := Object_Callback.Connect
           (Self, Signal_Item_Destroyed,
            Abstract_Item_Marshallers.To_Marshaller (Call));
      else
         Id := Object_Callback.Object_Connect
           (Self, Signal_Item_Destroyed,
            Abstract_Item_Marshallers.To_Marshaller (Call), Slot);
      end if;
      return Id;
   end On_Item_Destroyed;

   --------------------
   -- Item_Destroyed --
   --------------------

   procedure Item_Destroyed
     (Self : not null access Canvas_Model_Record'Class;
      Item : not null access Abstract_Item_Record'Class)
   is
   begin
      Abstract_Item_Emit
        (Self, Signal_Item_Destroyed & ASCII.NUL, Abstract_Item (Item));
   end Item_Destroyed;

   ----------------
   -- The_Layout --
   ----------------

   function The_Layout
     (Self : not null access Canvas_Model_Record'Class)
      return Pango.Layout.Pango_Layout is
   begin
      return Self.Layout;
   end The_Layout;

   ----------------
   -- Set_Layout --
   ----------------

   procedure Set_Layout
     (Self   : not null access Canvas_Model_Record'Class;
      Layout : Pango.Layout.Pango_Layout) is
   begin
      Self.Layout := Layout;
   end Set_Layout;

   -------------------------
   -- Refresh_Link_Layout --
   -------------------------

   procedure Refresh_Link_Layout
     (Model : not null access Canvas_Model_Record'Class;
      Items : Item_Drag_Infos.Map := Item_Drag_Infos.Empty_Map)
   is
      S : Item_Sets.Set;
      Context : constant Draw_Context :=
        (Cr => <>, Layout => Model.Layout, View => null);

      procedure Reset_Link_Layout
        (It : not null access Abstract_Item_Record'Class);
      --  Invalid the current layout for the link

      procedure Do_Link_Layout
        (It : not null access Abstract_Item_Record'Class);
      --  Recompute the layout for the link (and first to any link it is
      --  linked to).

      procedure Reset_Link_Layout
        (It : not null access Abstract_Item_Record'Class) is
      begin
         Unchecked_Free_Points (Canvas_Link (It));
      end Reset_Link_Layout;

      procedure Do_Link_Layout
        (It : not null access Abstract_Item_Record'Class)
      is
         Link : constant Canvas_Link := Canvas_Link (It);
      begin
         if Link.Get_Points = null then
            Link.Refresh_Layout (Context);
         end if;
      end Do_Link_Layout;

      use Item_Drag_Infos;
      C : Item_Drag_Infos.Cursor;

   begin
      --  To properly do the layout of links, we must first compute the
      --  layout of any item they are linked to, including other links.
      --  So we do the following:
      --     reset all previous layout computation
      --     when we layout a link, we first layout its end

      if Items.Is_Empty then
         Model.For_Each_Item
           (Reset_Link_Layout'Access, Filter => Kind_Link);
         Model.For_Each_Item (Do_Link_Layout'Access, Filter => Kind_Link);

      else
         C := Items.First;
         while Has_Element (C) loop
            S.Include (Element (C).Item);  --  toplevel items
            Next (C);
         end loop;

         Model.For_Each_Link (Reset_Link_Layout'Access, From_Or_To => S);
         Model.For_Each_Link (Do_Link_Layout'Access, From_Or_To => S);
      end if;
   end Refresh_Link_Layout;

end Gtkada.Canvas_Model_Pkg;
