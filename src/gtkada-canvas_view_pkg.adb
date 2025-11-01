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

with Ada.Unchecked_Conversion;
with Interfaces.C.Strings;               use Interfaces.C.Strings;
with System;
with Cairo;                              use Cairo;
with Cairo.Png;
with Cairo.Surface;
with Cairo.SVG;
with Glib.Properties.Creation;           use Glib.Properties.Creation;
with Glib.Values;                        use Glib.Values;
with Gdk;                                use Gdk;
with Gdk.Cairo;
with Cairo.PDF;
with Gdk.Window_Attr;                    use Gdk.Window_Attr;
with Gdk.Window;                         use Gdk.Window;
with Gtk.Accel_Group;                    use Gtk.Accel_Group;
with Gtk.Enums;                          use Gtk.Enums;
with Gtk.Handlers;                       use Gtk.Handlers;
with Gtk.Scrollable;
with Gtk.Style_Context;                  use Gtk.Style_Context;
with Gtkada.Bindings;                    use Gtkada.Bindings;
with Gtkada.Handlers;                    use Gtkada.Handlers;

with Gtkada.Canvas_Link_Pkg;             use Gtkada.Canvas_Link_Pkg;
with Gtkada.Canvas_View_Pkg.Views;       use Gtkada.Canvas_View_Pkg.Views;

pragma Warnings (Off, "call to obsolescent procedure ""Set_Background""");
--  Deprecated in Gtk+ 3.24

package body Gtkada.Canvas_View_Pkg is

   View_Signals : constant Interfaces.C.Strings.chars_ptr_array :=
     (1 => New_String (String (Signal_Viewport_Changed)),
      2 => New_String (String (Signal_Item_Event)),
      3 => New_String (String (Signal_Inline_Editing_Started)),
      4 => New_String (String (Signal_Inline_Editing_Finished)));

   View_Class_Record : aliased Glib.Object.Ada_GObject_Class :=
     Glib.Object.Uninitialized_Class;

   H_Adj_Property    : constant Property_Id := 1;
   V_Adj_Property    : constant Property_Id := 2;
   H_Scroll_Property : constant Property_Id := 3;
   V_Scroll_Property : constant Property_Id := 4;
   --  The properties for the View

   Mouse_Move_Before_Drag : constant Gdouble := 4.0 * 4.0;
   --  Minimal amount the mouse should move before we start dragging (this is
   --  the square).

   ----------------------------------------------------------------------------
   --                      Forward Declarations                              --
   ----------------------------------------------------------------------------

   function On_View_Draw
     (View : System.Address; Cr : Cairo_Context) return Gboolean;
   pragma Convention (C, On_View_Draw);
   --  default handler for "draw" on views.

   procedure On_Size_Allocate (View : System.Address; Alloc : Gtk_Allocation);
   pragma Convention (C, On_Size_Allocate);
   --  default handler for "size_allocate" on views.

   procedure Move_Inline_Edit_Widget
     (Self : not null access Canvas_View_Record'Class);
   --  Move the inline editing widget, if one exists

   function GValue_To_EDA (Value : GValue) return Event_Details_Access;
   function EDA_To_Address is new Ada.Unchecked_Conversion
     (Event_Details_Access, System.Address);
   package EDA_Marshallers is new Object_Return_Callback.Marshallers
     .Generic_Marshaller (Event_Details_Access, GValue_To_EDA);
   function EDA_Emit
     is new EDA_Marshallers.Emit_By_Name_Generic (EDA_To_Address);
   --  support for the "item_contents_changed" signal

   procedure View_Class_Init (Self : GObject_Class);
   pragma Convention (C, View_Class_Init);
   --  Initialize the class record, in particular adding interfaces, for
   --  the view class.

   procedure On_View_Destroy (Self : access Gtk_Widget_Record'Class);
   --  Called when the view is destroyed

   procedure Refresh
     (Self : not null access Canvas_View_Record'Class;
      Cr   : Cairo.Cairo_Context;
      Area : Model_Rectangle := No_Rectangle);
   --  Redraw the canvas (clear area, set transformation matrix and call Draw)

   procedure On_Layout_Changed_For_View
     (View : not null access GObject_Record'Class);

   procedure On_Item_Contents_Changed_For_View
     (View   : access GObject_Record'Class;
      Item : Abstract_Item);

   procedure On_Item_Destroyed_For_View
     (View   : access GObject_Record'Class;
      Item : Abstract_Item);

   procedure On_Selection_Changed_For_View
     (View : not null access GObject_Record'Class;
      Item : Abstract_Item);
   --  Handles the model events for the view.

   procedure On_Adj_Value_Changed
     (View : access Glib.Object.GObject_Record'Class);
   --  Called when one of the scrollbars has changed value.

   procedure View_Set_Property
     (Object        : access Glib.Object.GObject_Record'Class;
      Prop_Id       : Property_Id;
      Value         : Glib.Values.GValue;
      Property_Spec : Param_Spec);

   procedure View_Get_Property
     (Object        : access Glib.Object.GObject_Record'Class;
      Prop_Id       : Property_Id;
      Value         : out Glib.Values.GValue;
      Property_Spec : Param_Spec);
   --  Handlers for gtk+ properties

   procedure Compute_Item
     (Self    : not null access Canvas_View_Record'Class;
      Details : in out Canvas_Event_Details);
   --  Compute the item that was clicked on, from the coordinates stored in
   --  Details.

   function On_Button_Event
     (View  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Button) return Boolean;

   function On_Key_Event
     (View  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Key) return Boolean;

   function On_Motion_Notify_Event
     (View  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Motion) return Boolean;

   function On_Scroll_Event
     (View  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Scroll) return Boolean;
   --  Low-level handling of mouse events.

   procedure On_View_Realize (Widget : System.Address);
   pragma Convention (C, On_View_Realize);
   --  Called when the view is realized

   procedure Cancel_Drag (Self : not null access Canvas_View_Record'Class);
   --  Cancel any drag currently in place.

   -------------------------
   -- Refresh_Link_Layout --
   -------------------------

   procedure Refresh_Link_Layout
     (Model : not null access Canvas_Model_Record'Class;
      Items : Item_Drag_Infos.Map := Item_Drag_Infos.Empty_Map)
   is
      S : Item_Sets.Set;
      Context : constant Draw_Context :=
        (Cr => <>, Layout => Model.The_Layout, View => null);

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
         --  Unchecked_Free (Canvas_Link_Record'Class (It.all).Points);
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

   ------------------------------------
   -- Copy_Selected_To_Dragged_Items --
   ------------------------------------

   procedure Copy_Selected_To_Dragged_Items
     (Self  : not null access Canvas_View_Record'Class;
      Force : access Abstract_Item_Record'Class)
   is
      use Item_Sets;
      Selected_Set : constant Item_Sets.Set := Self.Model.Get_Selected_Items;
      C    : Item_Sets.Cursor := Selected_Set.First;
      Item : Abstract_Item;
      P    : Gtkada.Style_Pkg.Point;
   begin
      Self.Dragged_Items.Clear;

      while Has_Element (C) loop
         Item := Element (C).Get_Toplevel_Item;
         if not Item.Is_Link then
            P := Item.Position;
            Self.Dragged_Items.Include
              (Item, (Item => Item, Pos  => (X => P.X, Y => P.Y)));
         end if;

         Next (C);
      end loop;

      if Force /= null and then not Force.Is_Link then
         Item := Force.Get_Toplevel_Item;
         P := Item.Position;
         Self.Dragged_Items.Include
           (Item, (Item => Item, Pos  => (X => P.X, Y => P.Y)));
      end if;
   end Copy_Selected_To_Dragged_Items;

   ---------------------------
   -- Set_Adjustment_Values --
   ---------------------------

   procedure Set_Adjustment_Values
     (Self : not null access Canvas_View_Record'Class)
   is
      Box   : Model_Rectangle;
      Area  : constant Model_Rectangle := Self.Get_Visible_Area;
      Min, Max : Gdouble;
   begin
      if Self.Model = null or else Area.Width <= 1.0 then
         --  Not allocated yet
         return;
      end if;

      --  We want a small margin around the minimal box for the model, since it
      --  looks better.

      Box := Self.Model.Bounding_Box (View_Margin / Self.Scale);

      --  We set the adjustments to include the model area, but also at least
      --  the current visible area (if we don't, then part of the display will
      --  not be properly refreshed).

      if Self.Hadj /= null then
         Min := Gdouble'Min (Area.X, Box.X);
         Max := Gdouble'Max (Area.X + Area.Width, Box.X + Box.Width);
         Self.Hadj.Configure
           (Value          => Area.X,
            Lower          => Min,
            Upper          => Max,
            Step_Increment => 5.0,
            Page_Increment => 100.0,
            Page_Size      => Area.Width);
      end if;

      if Self.Vadj /= null then
         Min := Gdouble'Min (Area.Y, Box.Y);
         Max := Gdouble'Max (Area.Y + Area.Height, Box.Y + Box.Height);
         Self.Vadj.Configure
           (Value          => Area.Y,
            Lower          => Min,
            Upper          => Max,
            Step_Increment => 5.0,
            Page_Increment => 100.0,
            Page_Size      => Area.Height);
      end if;

      Self.Viewport_Changed;
   end Set_Adjustment_Values;

   ------------------
   -- On_View_Draw --
   ------------------

   function On_View_Draw
     (View : System.Address; Cr : Cairo_Context) return Gboolean
   is
      Self : constant Canvas_View := Canvas_View (Glib.Object.Convert (View));
      X1, Y1, X2, Y2 : Gdouble;
   begin
      Clip_Extents (Cr, X1, Y1, X2, Y2);

      if X2 < X1 or else Y2 < Y1 then
         Refresh (Self, Cr);
      else
         Refresh (Self, Cr, Self.View_To_Model ((X1, Y1, X2 - X1, Y2 - Y1)));
      end if;

      --  We might have an inline widget, which we need to draw.
      if Self.Inline_Edit.Item /= null then
         if Inherited_Draw
           (View_Class_Record,
            Widget => Self,
            Cr     => Cr)
         then
            return 1;
         else
            return 0;
         end if;
      else
         return 1;
      end if;

   exception
      when E : others =>
         Process_Exception (E);
         return 0;
   end On_View_Draw;

   ----------------------
   -- On_Size_Allocate --
   ----------------------

   procedure On_Size_Allocate
     (View : System.Address; Alloc : Gtk_Allocation)
   is
      Self : constant Canvas_View := Canvas_View (Glib.Object.Convert (View));
      SAlloc : Gtk_Allocation := Alloc;
   begin
      --  For some reason, when we maximize the toplevel window in testgtk, or
      --  at least enlarge it horizontally, we are starting to see an alloc
      --  with X < 0 (likely related to the GtkPaned). The drawing area then
      --  moves the GdkWindow, which would introduce an extra ofset in the
      --  display (and influence the clipping done automatically by gtk+
      --  before it emits "draw"). So we prevent the automatic offseting done
      --  by GtkDrawingArea.

      SAlloc.X := 0;
      SAlloc.Y := 0;
      Self.Set_Allocation (SAlloc);
      Set_Adjustment_Values (Self);

      if Self.Get_Realized then
         if Self.Get_Has_Window then
            Move_Resize
              (Self.Get_Window, Alloc.X, Alloc.Y, Alloc.Width, Alloc.Height);
         end if;

         --  send_configure event ?
      end if;

      --  Are we in the middle of inline-editing ?
      Move_Inline_Edit_Widget (Self);

      if Self.Scale_To_Fit_Requested /= 0.0 then
         Self.Scale_To_Fit
           (Rect      => Self.Scale_To_Fit_Area,
            Max_Scale => Self.Scale_To_Fit_Requested);
      end if;
   end On_Size_Allocate;

   -----------------------------
   -- Move_Inline_Edit_Widget --
   -----------------------------

   procedure Move_Inline_Edit_Widget
      (Self : not null access Canvas_View_Record'Class)
   is
      Edit   : Gtk_Widget;
      Box    : View_Rectangle;
      SAlloc : Gtk_Allocation;
      WMin, WNat, HMin, HNat : Gint;
   begin
      if Self.Inline_Edit.Item /= null then
         Edit := Self.Get_Child;
         Box := Self.Model_To_View (Self.Inline_Edit.Item.Model_Bounding_Box);

         --  SAlloc is relative to the view, so we should not add Alloc.{X,Y}
         SAlloc.X := Gint (Box.X);
         SAlloc.Y := Gint (Box.Y);

         Edit.Get_Preferred_Height (HMin, HNat);
         SAlloc.Height := Gint'Max (HMin, Gint (Box.Height));

         Edit.Get_Preferred_Width_For_Height (SAlloc.Height, WMin, WNat);
         SAlloc.Width := Gint'Max (WMin, Gint (Box.Width));

         Edit.Size_Allocate (SAlloc);
      end if;
   end Move_Inline_Edit_Widget;


   -------------------
   -- GValue_To_EDA --
   -------------------

   function GValue_To_EDA (Value : GValue) return Event_Details_Access is
      S : constant System.Address := Get_Address (Value);
      pragma Warnings (Off, "possible aliasing problem*");
      function Unchecked_Convert is new Ada.Unchecked_Conversion
        (System.Address, Event_Details_Access);
      pragma Warnings (On, "possible aliasing problem*");
   begin
      return Unchecked_Convert (S);
   end GValue_To_EDA;

   ---------------------
   -- View_Class_Init --
   ---------------------

   procedure View_Class_Init (Self : GObject_Class) is
   begin
      Set_Properties_Handlers
        (Self, View_Set_Property'Access, View_Get_Property'Access);

      Override_Property (Self, H_Adj_Property, "hadjustment");
      Override_Property (Self, V_Adj_Property, "vadjustment");
      Override_Property (Self, H_Scroll_Property, "hscroll-policy");
      Override_Property (Self, V_Scroll_Property, "vscroll-policy");

      Set_Default_Draw_Handler (Self, On_View_Draw'Access);
      Set_Default_Size_Allocate_Handler (Self, On_Size_Allocate'Access);
      Set_Default_Realize_Handler (Self, On_View_Realize'Access);
   end View_Class_Init;

   ---------------------
   -- On_View_Destroy --
   ---------------------

   procedure On_View_Destroy (Self : access Gtk_Widget_Record'Class) is
      S : constant Canvas_View := Canvas_View (Self);
   begin
      Cancel_Continuous_Scrolling (S);
      Terminate_Animation (S);

      if S.Model /= null then
         Unref (S.Model);
         S.Model := null;
      end if;

      if S.Layout /= null then
         Unref (S.Layout);
         S.Layout := null;
      end if;
   end On_View_Destroy;

   -------------
   -- Refresh --
   -------------

   procedure Refresh
     (Self : not null access Canvas_View_Record'Class;
      Cr   : Cairo.Cairo_Context;
      Area : Model_Rectangle := No_Rectangle)
   is
      A : Model_Rectangle;
      C : Draw_Context;
   begin
      if Area = No_Rectangle then
         A := Self.Get_Visible_Area;
      else
         A := Area;
      end if;

      --  GDK already clears the exposed area to the background color, so
      --  we do not need to clear ourselves.

      C := (Cr => Cr, Layout => Self.Layout, View => Canvas_View (Self));

      Save (Cr);
      Self.Set_Transform (Cr);
      Self.Draw_Internal (C, A);
      Restore (Cr);
   end Refresh;

   --------------------------------
   -- On_Layout_Changed_For_View --
   --------------------------------

   procedure On_Layout_Changed_For_View
     (View : not null access GObject_Record'Class)
   is
      Self  : constant Canvas_View := Canvas_View (View);
      Alloc : Gtk_Allocation;
   begin
      Self.Get_Allocation (Alloc);

      --  On_Adjustments_Set will be called anyway when Size_Allocate is called
      --  so no need to call it now if the size is unknown yet.

      if Alloc.Width > 1 then
         Set_Adjustment_Values (Self);
         Self.Queue_Draw;
      end if;

      Move_Inline_Edit_Widget (Self);
   end On_Layout_Changed_For_View;

   -----------------------------------------
   -- On_Item_Contents_Changed_For_View --
   -----------------------------------------

   procedure On_Item_Contents_Changed_For_View
     (View : access GObject_Record'Class;
      Item : Abstract_Item)
   is
      pragma Unreferenced (Item);
      Self : constant Canvas_View := Canvas_View (View);

      --  ??? Ideally we should only redraw the minimal area
--        Rect : constant View_Rectangle :=
--          Self.Model_To_View (Item.Model_Bounding_Box);
   begin
      Self.Queue_Draw;
--        Self.Queue_Draw_Area
--          (X      => Gint (Rect.X),
--           Y      => Gint (Rect.Y),
--           Width  => Gint (Rect.Width),
--           Height => Gint (Rect.Height));
   end On_Item_Contents_Changed_For_View;

   --------------------------------
   -- On_Item_Destroyed_For_View --
   --------------------------------

   procedure On_Item_Destroyed_For_View
     (View   : access GObject_Record'Class;
      Item : Abstract_Item)
   is
      Self : constant Canvas_View := Canvas_View (View);
   begin
      if Self.Last_Button_Press.Item = Item then
         Self.Last_Button_Press.Item := null;
      end if;

      if Self.Last_Button_Press.Toplevel_Item = Item then
         Self.Last_Button_Press.Toplevel_Item := null;
      end if;

      if Self.Dragged_Items.Contains (Item) then
         Self.Dragged_Items.Delete (Item);
      end if;

      if Self.Inline_Edit.Item = Item then
         Cancel_Inline_Editing (Self);
      end if;

      Terminate_Animation_For_Item (Self, Item);
   end On_Item_Destroyed_For_View;

   -----------------------------------
   -- On_Selection_Changed_For_View --
   -----------------------------------

   procedure On_Selection_Changed_For_View
     (View : not null access GObject_Record'Class;
      Item : Abstract_Item)
   is
      pragma Unreferenced (Item);
      Self  : constant Canvas_View := Canvas_View (View);
   begin
      Self.Queue_Draw;
   end On_Selection_Changed_For_View;

   --------------------------
   -- On_Adj_Value_Changed --
   --------------------------

   procedure On_Adj_Value_Changed
     (View : access Glib.Object.GObject_Record'Class)
   is
      Self : constant Canvas_View := Canvas_View (View);
      Pos  : constant Model_Point :=
        (X => Self.Hadj.Get_Value,
         Y => Self.Vadj.Get_Value);
   begin
      if Pos /= Self.Topleft then
         Self.Topleft := Pos;
         Self.Viewport_Changed;
         Queue_Draw (Self);
      end if;
   end On_Adj_Value_Changed;

   -----------------------
   -- View_Set_Property --
   -----------------------

   procedure View_Set_Property
     (Object        : access Glib.Object.GObject_Record'Class;
      Prop_Id       : Property_Id;
      Value         : Glib.Values.GValue;
      Property_Spec : Param_Spec)
   is
      pragma Unreferenced (Property_Spec);
      Self : constant Canvas_View := Canvas_View (Object);
   begin
      case Prop_Id is
         when H_Adj_Property =>
            Self.Hadj := Gtk_Adjustment (Get_Object (Value));
            if Self.Hadj /= null then
               Set_Adjustment_Values (Self);
               Self.Hadj.On_Value_Changed (On_Adj_Value_Changed'Access, Self);
               Self.Queue_Draw;
            end if;

         when V_Adj_Property =>
            Self.Vadj := Gtk_Adjustment (Get_Object (Value));
            if Self.Vadj /= null then
               Set_Adjustment_Values (Self);
               Self.Vadj.On_Value_Changed (On_Adj_Value_Changed'Access, Self);
               Self.Queue_Draw;
            end if;

         when H_Scroll_Property =>
            null;

         when V_Scroll_Property =>
            null;

         when others =>
            null;
      end case;
   end View_Set_Property;

   -----------------------
   -- View_Get_Property --
   -----------------------

   procedure View_Get_Property
     (Object        : access Glib.Object.GObject_Record'Class;
      Prop_Id       : Property_Id;
      Value         : out Glib.Values.GValue;
      Property_Spec : Param_Spec)
   is
      pragma Unreferenced (Property_Spec);
      Self : constant Canvas_View := Canvas_View (Object);
   begin
      case Prop_Id is
         when H_Adj_Property =>
            Set_Object (Value, Self.Hadj);

         when V_Adj_Property =>
            Set_Object (Value, Self.Vadj);

         when H_Scroll_Property =>
            Set_Enum (Value, Gtk_Policy_Type'Pos (Policy_Automatic));

         when V_Scroll_Property =>
            Set_Enum (Value, Gtk_Policy_Type'Pos (Policy_Automatic));

         when others =>
            null;
      end case;
   end View_Get_Property;

   --------------------------
   -- Size_Above_Threshold --
   --------------------------

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

   ------------------
   -- Compute_Item --
   ------------------

   procedure Compute_Item
     (Self    : not null access Canvas_View_Record'Class;
      Details : in out Canvas_Event_Details)
   is
      Context : Draw_Context;
   begin
      Context := (Cr     => Gdk.Cairo.Create (Self.Get_Window),
                  View   => Canvas_View (Self),
                  Layout => null);

      Details.Toplevel_Item := Self.Model.Toplevel_Item_At
        (Details.M_Point, Context => Context);

      if Details.Toplevel_Item = null then
         Details.Item := null;
      else
         Details.T_Point := Details.Toplevel_Item.Model_To_Item
           (Details.M_Point);
         Details.Item := Details.Toplevel_Item.Inner_Most_Item
           (Details.M_Point, Context);

         if Details.Item /= null then
            Details.I_Point := Details.Item.Model_To_Item (Details.M_Point);
         end if;
      end if;

      Cairo.Destroy (Context.Cr);
   end Compute_Item;

   ---------------------
   -- On_Button_Event --
   ---------------------

   function On_Button_Event
     (View  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Button)
      return Boolean
   is
      Self    : constant Canvas_View := Canvas_View (View);
      Details : aliased Canvas_Event_Details;
   begin
      Cancel_Continuous_Scrolling (Self);
      Free_Smart_Guides (Self);

      if Self.Model /= null then
         Self.Set_Details (Details, Event);

         if Details.Event_Type = Key_Press then
            return False;
         elsif Details.Event_Type = Button_Press then
            Cancel_Inline_Editing (Self);
            Self.Grab_Focus;
         end if;

         if Event.The_Type = Gdk.Event.Button_Release
           and then Self.In_Drag
         then
            --  Validate the position of items and recompute all links,
            --  not just the ones that moved.
            Self.Model.Refresh_Layout;
         end if;

         if Self.Item_Event (Details'Unchecked_Access) then
            Cancel_Drag (Self);

            if Details.Event_Type = Button_Press then
               Self.Last_Button_Press := Details;
            end if;

            return True;
         end if;
      end if;

      Cancel_Drag (Self);
      return False;
   end On_Button_Event;

   ------------------
   -- On_Key_Event --
   ------------------

   function On_Key_Event
     (View  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Key) return Boolean
   is
      Self    : constant Canvas_View := Canvas_View (View);
      Details : aliased Canvas_Event_Details;
      Box     : Model_Rectangle;
      IBox    : Item_Rectangle;
   begin
      --  Do not cancel drag, since pressing shift is used to disable snapping
      --  in this context.

      if Self.Model /= null then
         Details.Event_Type        := Key_Press;
         Details.State             := Event.State and Get_Default_Mod_Mask;
         Details.Allow_Snapping    := True;
         Details.Allowed_Drag_Area := No_Drag_Allowed;
         Details.Key               := Event.Keyval;

         if not Self.Model.Get_Selected_Items.Is_Empty then
            Details.Item := Item_Sets.Element (Self.Model.Get_Selected_Items.First);

            if Details.Item /= null then
               IBox := Details.Item.Bounding_Box;
               Details.I_Point := (IBox.X + IBox.Width / 2.0,
                                   IBox.Y + IBox.Height / 2.0);

               Box := Details.Item.Model_Bounding_Box;
               Details.M_Point := (Box.X + Box.Width / 2.0,
                                   Box.Y + Box.Height / 2.0);

               Details.Toplevel_Item := Details.Item.Get_Toplevel_Item;

               IBox := Details.Toplevel_Item.Bounding_Box;
               Details.T_Point := (IBox.X + IBox.Width / 2.0,
                                   IBox.Y + IBox.Height / 2.0);
            end if;
         end if;

         return Self.Item_Event (Details'Unchecked_Access);
      end if;

      return False;
   end On_Key_Event;

   ----------------------------
   -- On_Motion_Notify_Event --
   ----------------------------

   function On_Motion_Notify_Event
     (View  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Motion) return Boolean
   is
      Self    : constant Canvas_View := Canvas_View (View);
      Details : aliased Canvas_Event_Details;
      Dx, Dy  : Gdouble;
      Dummy   : Boolean;
   begin
      if Self.Model /= null
        and then Self.Last_Button_Press.Allowed_Drag_Area /= No_Drag_Allowed
        and then Self.Get_Child = null   --  no inline editing
      then
         if not Self.In_Drag then
            Dx := Event.X_Root - Self.Last_Button_Press.Root_Point.X;
            Dy := Event.Y_Root - Self.Last_Button_Press.Root_Point.Y;

            if Dx * Dx + Dy * Dy >= Mouse_Move_Before_Drag then
               Self.In_Drag := True;
               Details := Self.Last_Button_Press;
               Details.Event_Type := Start_Drag;
               Dummy := Self.Item_Event (Details'Unchecked_Access);

               Self.Topleft_At_Drag_Start := Self.Topleft;
               Self.Grab_Add;

               --  ??? Should add all selected items
               if Details.Toplevel_Item /= null
                 and then not Details.Toplevel_Item.Is_Link
               then
                  Copy_Selected_To_Dragged_Items
                    (Self, Force => Details.Toplevel_Item);
                  Prepare_Smart_Guides (Self);
               end if;
            end if;
         end if;

         --  Whether we were already in a drag or just started

         if Self.In_Drag then
            Details            := Self.Last_Button_Press;
            Details.Event_Type := In_Drag;
            Details.State      := Event.State and Get_Default_Mod_Mask;
            Details.Root_Point := (Event.X_Root, Event.Y_Root);
            Details.M_Point    :=
              Self.Window_To_Model ((X => Event.X, Y => Event.Y));
            Dummy := Self.Item_Event (Details'Unchecked_Access);
         end if;
      end if;
      return False;
   end On_Motion_Notify_Event;

   ---------------------
   -- On_Scroll_Event --
   ---------------------

   function On_Scroll_Event
     (View  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Scroll) return Boolean
   is
      Self    : constant Canvas_View := Canvas_View (View);
      Details : aliased Canvas_Event_Details;
      Button  : Guint;
   begin
      if Self.Model /= null then
         case Event.Direction is
            when Scroll_Up | Scroll_Left =>
               Button := 5;
            when Scroll_Down | Scroll_Right =>
               Button := 6;
            when Scroll_Smooth =>
               if Event.Delta_Y > 0.0 then
                  Button := 6;
               else
                  Button := 5;
               end if;
         end case;

         Details :=
           (Event_Type => Scroll,
            Button     => Button,
            Key        => 0,
            State      => Event.State and Get_Default_Mod_Mask,
            Root_Point => (Event.X_Root, Event.Y_Root),
            M_Point    => Self.Window_To_Model ((X => Event.X, Y => Event.Y)),
            T_Point    => No_Item_Point,
            I_Point    => No_Item_Point,
            Item       => null,
            Toplevel_Item => null,
            Allow_Snapping    => True,
            Allowed_Drag_Area => No_Drag_Allowed);
         Compute_Item (Self, Details);
         return Self.Item_Event (Details'Unchecked_Access);
      end if;
      return False;
   end On_Scroll_Event;

   ---------------------
   -- On_View_Realize --
   ---------------------

   procedure On_View_Realize (Widget : System.Address) is
      W          : constant Gtk_Widget :=
        Gtk_Widget (Get_User_Data_Or_Null (Widget));
      Allocation : Gtk_Allocation;
      Window     : Gdk_Window;
      Attr       : Gdk.Window_Attr.Gdk_Window_Attr;
      Mask       : Gdk_Window_Attributes_Type;

   begin
      if not W.Get_Has_Window then
         Inherited_Realize (View_Class_Record, W);
      else
         W.Set_Realized (True);
         W.Get_Allocation (Allocation);

         Gdk_New
           (Attr,
            Window_Type => Gdk.Window.Window_Child,
            X           => Allocation.X,
            Y           => Allocation.Y,
            Width       => Allocation.Width,
            Height      => Allocation.Height,
            Wclass      => Gdk.Window.Input_Output,
            Visual      => W.Get_Visual,
            Event_Mask  => W.Get_Events or Exposure_Mask);
         Mask := Wa_X or Wa_Y or Wa_Visual;

         Gdk_New (Window, W.Get_Parent_Window, Attr, Mask);
         Register_Window (W, Window);
         W.Set_Window (Window);
         Get_Style_Context (W).Set_Background (Window);

         --  See also handler for size_allocate, which moves the window to its
         --  proper location.
      end if;
   end On_View_Realize;

   -----------------
   -- Cancel_Drag --
   -----------------

   procedure Cancel_Drag (Self : not null access Canvas_View_Record'Class) is
   begin
      Self.In_Drag := False;
      Self.Dragged_Items.Clear;
      Self.Last_Button_Press.Allowed_Drag_Area := No_Drag_Allowed;
      Self.Grab_Remove;
   end Cancel_Drag;

   -------------------
   -- Build_Context --
   -------------------

   function Build_Context
     (Self : not null access Canvas_View_Record'Class)
      return Draw_Context
   is
   begin
      return (Cr => <>, Layout => Self.Layout, View => Canvas_View (Self));
   end Build_Context;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Self  : out Canvas_View;
      Model : access Canvas_Model_Record'Class := null) is
   begin
      Self := new Canvas_View_Record;
      Initialize (Self, Model);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self  : not null access Canvas_View_Record'Class;
      Model : access Canvas_Model_Record'Class := null)
   is
   begin
      G_New (Self, View_Get_Type);

      Self.Layout := Self.Create_Pango_Layout;
      Self.Set_Has_Window (True);

      Self.Add_Events
        (Scroll_Mask or Smooth_Scroll_Mask or Touch_Mask
         or Button_Press_Mask or Button_Release_Mask
         or Button1_Motion_Mask
         or Button2_Motion_Mask
         or Button3_Motion_Mask
         --  or Pointer_Motion_Mask or Pointer_Motion_Hint_Mask
        );

      Self.On_Destroy (On_View_Destroy'Access);
      Self.On_Button_Press_Event (On_Button_Event'Access);
      Self.On_Button_Release_Event (On_Button_Event'Access);
      Self.On_Motion_Notify_Event (On_Motion_Notify_Event'Access);
      Self.On_Key_Press_Event (On_Key_Event'Access);
      Self.On_Scroll_Event (On_Scroll_Event'Access);

      Self.Set_Can_Focus (True);

      Self.Set_Model (Model);
   end Initialize;

   ---------------
   -- Set_Model --
   ---------------

   procedure Set_Model
      (Self  : not null access Canvas_View_Record'Class;
       Model : access Canvas_Model_Record'Class)
   is
   begin
      if Self.Model = Canvas_Model (Model) then
         return;
      end if;

      if Self.Model /= null then
         Disconnect (Self.Model, Self.Id_Layout_Changed);
         Disconnect (Self.Model, Self.Id_Item_Contents_Changed);
         Disconnect (Self.Model, Self.Id_Selection_Changed);
         Disconnect (Self.Model, Self.Id_Item_Destroyed);
         Unref (Self.Model);
      end if;

      Self.Model := Canvas_Model (Model);

      if Self.Model /= null then
         Ref (Self.Model);
         Self.Id_Layout_Changed := Model.On_Layout_Changed
            (On_Layout_Changed_For_View'Access, Self);
         Self.Id_Selection_Changed := Model.On_Selection_Changed
           (On_Selection_Changed_For_View'Access, Self);
         Self.Id_Item_Contents_Changed := Model.On_Item_Contents_Changed
            (On_Item_Contents_Changed_For_View'Access, Self);
         Self.Id_Item_Destroyed :=
           Model.On_Item_Destroyed (On_Item_Destroyed_For_View'Access, Self);
      end if;

      if Self.Model /= null and then Self.Model.The_Layout = null then
         Self.Model.Set_Layout (Self.Layout);  --  needed for layout
         Ref (Self.Model.The_Layout);
         Self.Model.Refresh_Layout;
      else
         Set_Adjustment_Values (Self);
         Self.Queue_Draw;
      end if;

      Self.Viewport_Changed;
   end Set_Model;

   -----------
   -- Model --
   -----------

   function Model
     (Self  : not null access Canvas_View_Record'Class)
      return Canvas_Model
   is
   begin
      return Self.Model;
   end Model;

   -------------------
   -- View_Get_Type --
   -------------------

   function View_Get_Type return Glib.GType is
      Info : GInterface_Info_Access;
   begin
      if Glib.Object.Initialize_Class_Record
        (Ancestor     => Gtk.Bin.Get_Type,
         Signals      => View_Signals,
         Class_Record => View_Class_Record'Access,
         Type_Name    => "GtkadaCanvasView",
         Parameters   => (1 => (1 => GType_None),
                          2 => (1 => GType_Pointer),
                          3 => (1 => GType_Pointer),
                          4 => (1 => GType_Pointer)),
         Returns      => (1 => GType_None,
                          2 => GType_Boolean),
         Class_Init   => View_Class_Init'Access)
      then
         Info := new GInterface_Info'
           (Interface_Init     => null,
            Interface_Finalize => null,
            Interface_Data     => System.Null_Address);
         Glib.Object.Add_Interface
           (View_Class_Record,
            Iface => Gtk.Scrollable.Get_Type,
            Info  => Info);
      end if;

      return View_Class_Record.The_Type;
   end View_Get_Type;

   -------------------
   -- Set_Grid_Size --
   -------------------

   procedure Set_Grid_Size
     (Self : not null access Canvas_View_Record'Class;
      Size : Model_Coordinate := 30.0)
   is
   begin
      Self.Grid_Size := Size;
   end Set_Grid_Size;

   --------------
   -- Set_Snap --
   --------------

   procedure Set_Snap
     (Self           : not null access Canvas_View_Record'Class;
      Snap_To_Grid   : Boolean := True;
      Snap_To_Guides : Boolean := False;
      Snap_Margin    : Model_Coordinate := 5.0;
      Guides_Style   : Gtkada.Style_Pkg.Drawing_Style := Default_Guide_Style)
   is
   begin
      Self.Snap.Grid         := Snap_To_Grid;
      Self.Snap.Smart_Guides := Snap_To_Guides;
      Self.Snap.Margin       := Snap_Margin;
      Self.Snap.Style        := Guides_Style;
   end Set_Snap;

   -------------------
   -- Draw_Internal --
   -------------------

   procedure Draw_Internal
     (Self    : not null access Canvas_View_Record;
      Context : Draw_Context;
      Area    : Model_Rectangle)
   is
      S  : Item_Sets.Set;

      procedure Draw_Item
        (Item : not null access Abstract_Item_Record'Class);
      procedure Draw_Item
        (Item : not null access Abstract_Item_Record'Class) is
      begin
         --  If the item is not displayed explicitly afterwards.
         if not Self.In_Drag
           or else not S.Contains (Abstract_Item (Item))
         then
            Translate_And_Draw_Item (Item, Context);
         end if;
      end Draw_Item;

      procedure Add_To_Set (Item : not null access Abstract_Item_Record'Class);
      procedure Add_To_Set
        (Item : not null access Abstract_Item_Record'Class) is
      begin
         S.Include (Abstract_Item (Item));
      end Add_To_Set;

      use Item_Drag_Infos, Item_Sets;
      C  : Item_Drag_Infos.Cursor;
      C2 : Item_Sets.Cursor;
   begin
      if Self.Model /= null then
         --  We must always draw the selected items and their links explicitly
         --  (since the model might not have been updated yet if we are during
         --  an automatic scrolling for instance, using a RTree).

         if Self.In_Drag then
            C := Self.Dragged_Items.First;
            while Has_Element (C) loop
               S.Include (Element (C).Item);  --  toplevel items
               Next (C);
            end loop;
            Self.Model.For_Each_Link (Add_To_Set'Access, From_Or_To => S);
         end if;

         --  Draw the active smart guides if needed
         if Self.In_Drag
           and then Self.Last_Button_Press.Allow_Snapping
           and then Self.Snap.Smart_Guides
           and then not Self.Dragged_Items.Is_Empty
         then
            Draw_Visible_Smart_Guides
              (Self, Context, Element (Self.Dragged_Items.First).Item);
         end if;

         Self.Model.For_Each_Item
           (Draw_Item'Access, In_Area => Area, Filter => Kind_Link);
         Self.Model.For_Each_Item
           (Draw_Item'Access, In_Area => Area, Filter => Kind_Item);

         if Self.In_Drag then
            C2 := S.First;
            while Has_Element (C2) loop
               Translate_And_Draw_Item (Element (C2), Context);
               Next (C2);
            end loop;
         end if;
      end if;
   end Draw_Internal;

   ----------------------
   -- Get_Visible_Area --
   ----------------------

   function Get_Visible_Area
     (Self : not null access Canvas_View_Record)
      return Model_Rectangle
   is
   begin
      return Self.View_To_Model
        ((0.0,
         0.0,
         Gdouble (Self.Get_Allocated_Width),
         Gdouble (Self.Get_Allocated_Height)));
   end Get_Visible_Area;

   -------------------
   -- Set_Transform --
   -------------------

   procedure Set_Transform
     (Self   : not null access Canvas_View_Record;
      Cr     : Cairo.Cairo_Context;
      Item : access Abstract_Item_Record'Class := null)
   is
      Model_P : Model_Point;
      P       : View_Point;
   begin
      if Item /= null then
         Model_P := Item.Item_To_Model ((0.0, 0.0));
      else
         Model_P := (0.0, 0.0);
      end if;

      P := Self.Model_To_View (Model_P);
      Translate (Cr, P.X, P.Y);
      Scale (Cr, Self.Scale, Self.Scale);
   end Set_Transform;

   -------------------
   -- View_To_Model --
   -------------------

   function View_To_Model
     (Self   : not null access Canvas_View_Record;
      Rect   : View_Rectangle) return Model_Rectangle
   is
   begin
      return (X      => Rect.X / Self.Scale + Self.Topleft.X,
              Y      => Rect.Y / Self.Scale + Self.Topleft.Y,
              Width  => Rect.Width / Self.Scale,
              Height => Rect.Height / Self.Scale);
   end View_To_Model;

   -------------------
   -- View_To_Model --
   -------------------

   function View_To_Model
     (Self   : not null access Canvas_View_Record;
      P      : View_Point) return Model_Point
   is
   begin
      return (X      => P.X / Self.Scale + Self.Topleft.X,
              Y      => P.Y / Self.Scale + Self.Topleft.Y);
   end View_To_Model;

   -------------------
   -- Model_To_View --
   -------------------

   function Model_To_View
     (Self   : not null access Canvas_View_Record;
      Rect   : Model_Rectangle) return View_Rectangle
   is
   begin
      return (X      => (Rect.X - Self.Topleft.X) * Self.Scale,
              Y      => (Rect.Y - Self.Topleft.Y) * Self.Scale,
              Width  => Rect.Width * Self.Scale,
              Height => Rect.Height * Self.Scale);
   end Model_To_View;

   -------------------
   -- Model_To_View --
   -------------------

   function Model_To_View
     (Self   : not null access Canvas_View_Record;
      P      : Model_Point) return View_Point
   is
   begin
      return (X => (P.X - Self.Topleft.X) * Self.Scale,
              Y => (P.Y - Self.Topleft.Y) * Self.Scale);
   end Model_To_View;

   ---------------------
   -- Model_To_Window --
   ---------------------

   function Model_To_Window
     (Self   : not null access Canvas_View_Record;
      Rect   : Model_Rectangle) return Window_Rectangle
   is
      View  : constant View_Rectangle := Self.Model_To_View (Rect);
      Alloc : Gtk_Allocation;
   begin
      Self.Get_Allocation (Alloc);
      return (X    => Window_Coordinate (View.X) + Window_Coordinate (Alloc.X),
              Y    => Window_Coordinate (View.Y) + Window_Coordinate (Alloc.Y),
              Width  => Window_Coordinate (View.Width),
              Height => Window_Coordinate (View.Height));
   end Model_To_Window;

   ---------------------
   -- Window_To_Model --
   ---------------------

   function Window_To_Model
     (Self   : not null access Canvas_View_Record;
      P      : Window_Point) return Model_Point
   is
      Alloc : Gtk_Allocation;
   begin
      Self.Get_Allocation (Alloc);
      return Self.View_To_Model
        ((X      => View_Coordinate (P.X) - View_Coordinate (Alloc.X),
          Y      => View_Coordinate (P.Y) - View_Coordinate (Alloc.Y)));
   end Window_To_Model;

   ---------------------
   -- Window_To_Model --
   ---------------------

   function Window_To_Model
     (Self   : not null access Canvas_View_Record;
      Rect   : Window_Rectangle) return Model_Rectangle
   is
      Alloc : Gtk_Allocation;
   begin
      Self.Get_Allocation (Alloc);
      return Self.View_To_Model
        ((X      => View_Coordinate (Rect.X) - View_Coordinate (Alloc.X),
          Y      => View_Coordinate (Rect.Y) - View_Coordinate (Alloc.Y),
          Width  => View_Coordinate (Rect.Width),
          Height => View_Coordinate (Rect.Height)));
   end Window_To_Model;

   -------------------------
   -- Set_Selection_Style --
   -------------------------

   procedure Set_Selection_Style
     (Self  : not null access Canvas_View_Record;
      Style : Gtkada.Style_Pkg.Drawing_Style) is
   begin
      Self.Selection_Style := Style;
   end Set_Selection_Style;

   -------------------------
   -- Get_Selection_Style --
   -------------------------

   function Get_Selection_Style
     (Self  : not null access Canvas_View_Record)
      return Gtkada.Style_Pkg.Drawing_Style is
   begin
      return Self.Selection_Style;
   end Get_Selection_Style;

   ---------------
   -- Set_Scale --
   ---------------

   procedure Set_Scale
     (Self     : not null access Canvas_View_Record;
      Scale    : Gdouble := 1.0;
      Preserve : Model_Point := No_Point)
   is
      Box : Model_Rectangle;
      Old_Scale : constant Gdouble := Self.Scale;
      P   : Model_Point;
   begin
      if Preserve /= No_Point then
         P := Preserve;
      else
         Box := Self.Get_Visible_Area;
         P := (Box.X + Box.Width / 2.0, Box.Y + Box.Height / 2.0);
      end if;

      Self.Scale := Scale;
      Self.Topleft :=
        (P.X - (P.X - Self.Topleft.X) * Old_Scale / Scale,
         P.Y - (P.Y - Self.Topleft.Y) * Old_Scale / Scale);

      Self.Scale_To_Fit_Requested := 0.0;
      Self.Set_Adjustment_Values;
      Self.Queue_Draw;
   end Set_Scale;

   -----------------
   -- Set_Topleft --
   -----------------

   procedure Set_Topleft
     (Self         : not null access Canvas_View_Record;
      Topleft      : Model_Point) is
   begin
      Self.Topleft := Topleft;
      Self.Queue_Draw;
   end Set_Topleft;

   ---------------
   -- Center_On --
   ---------------

   procedure Center_On
     (Self         : not null access Canvas_View_Record;
      Center_On    : Model_Point;
      X_Pos, Y_Pos : Gdouble := 0.5;
      Duration     : Standard.Duration := 0.0)
   is
      Area : constant Model_Rectangle := Self.Get_Visible_Area;
      Pos  : constant Model_Point :=
        (Center_On.X - Area.Width * X_Pos,
         Center_On.Y - Area.Height * Y_Pos);
   begin
      Self.Scale_To_Fit_Requested := 0.0;

      if Duration = 0.0 then
         Self.Topleft := Pos;
         Self.Set_Adjustment_Values;
         Self.Queue_Draw;
      else
         Animate_Scroll (Self, Pos, Duration => Duration).Start (Self);
      end if;
   end Center_On;

   ----------------------
   -- Scroll_Into_View --
   ----------------------

   procedure Scroll_Into_View
     (Self     : not null access Canvas_View_Record;
      Item     : not null access Abstract_Item_Record'Class;
      Duration : Standard.Duration := 0.0)
   is
   begin
      Scroll_Into_View (Self, Item.Model_Bounding_Box, Duration);
   end Scroll_Into_View;

   ----------------------
   -- Scroll_Into_View --
   ----------------------

   procedure Scroll_Into_View
     (Self     : not null access Canvas_View_Record;
      Rect     : Model_Rectangle;
      Duration : Standard.Duration := 0.0)
   is
      Box  : Model_Rectangle := Rect;
      Area : Model_Rectangle := Self.Get_Visible_Area;
      Modified : Boolean := False;
      Margin   : constant Model_Coordinate := View_Margin * Self.Scale;
   begin
      Box.X := Box.X - Margin;
      Box.Y := Box.Y - Margin;
      Box.Width := Box.Width + Margin * 2.0;
      Box.Height := Box.Height + Margin * 2.0;

      if Box.X < Area.X then
         Area.X := Box.X;
         Modified := True;
      elsif Box.X + Box.Width > Area.X + Area.Width then
         Area.X := Gdouble'Min (Box.X, Box.X + Box.Width - Area.Width);
         Modified := True;
      end if;

      if Box.Y < Area.Y then
         Area.Y := Box.Y;
         Modified := True;
      elsif Box.Y + Box.Height > Area.Y + Area.Height then
         Area.Y := Gdouble'Min (Box.Y, Box.Y + Box.Height - Area.Height);
         Modified := True;
      end if;

      if Modified then
         Self.Center_On
           ((Area.X, Area.Y), X_Pos => 0.0, Y_Pos => 0.0,
            Duration => Duration);
      end if;
   end Scroll_Into_View;

   ---------------
   -- Get_Scale --
   ---------------

   function Get_Scale
     (Self : not null access Canvas_View_Record) return Gdouble is
   begin
      return Self.Scale;
   end Get_Scale;

   ------------------
   -- Scale_To_Fit --
   ------------------

   procedure Scale_To_Fit
     (Self      : not null access Canvas_View_Record;
      Rect      : Model_Rectangle := No_Rectangle;
      Min_Scale : Gdouble := 1.0 / 4.0;
      Max_Scale : Gdouble := 4.0;
      Duration  : Standard.Duration := 0.0)
   is
      Box     : Model_Rectangle;
      W, H, S : Gdouble;
      Alloc   : Gtk_Allocation;
      TL      : Model_Point;
      Wmin, Hmin : Gdouble;
   begin
      Self.Get_Allocation (Alloc);
      if Alloc.Width <= 1 then
         Self.Scale_To_Fit_Requested := Max_Scale;
         Self.Scale_To_Fit_Area := Rect;

      elsif Self.Model /= null then
         Self.Scale_To_Fit_Requested := 0.0;

         if Rect = No_Rectangle then
            Box := Self.Model.Bounding_Box;
         else
            Box := Rect;
         end if;

         if Box.Width /= 0.0 and then Box.Height /= 0.0 then
            W := Gdouble (Alloc.Width);
            H := Gdouble (Alloc.Height);

            --  The "-1.0" below compensates for rounding errors, since
            --  otherwise we are still seeing the scrollbar along the axis
            --  used to compute the scale.
            Wmin := (W - 2.0 * View_Margin - 1.0) / Box.Width;
            Hmin := (H - 2.0 * View_Margin - 1.0) / Box.Height;
            Wmin := Gdouble'Min (Wmin, Hmin);
            S := Gdouble'Min (Max_Scale, Wmin);
            S := Gdouble'Max (Min_Scale, S);
            TL :=
              (X => Box.X - (W / S - Box.Width) / 2.0,
               Y => Box.Y - (H / S - Box.Height) / 2.0);

            if Duration = 0.0 then
               Self.Scale := S;
               Self.Topleft := TL;
               Self.Set_Adjustment_Values;
               Self.Queue_Draw;

            else
               Animate_Scale (Self, S, Duration => Duration).Start (Self);
               Animate_Scroll (Self, TL, Duration).Start (Self);
            end if;
         end if;
      end if;
   end Scale_To_Fit;

   -------------------
   -- Avoid_Overlap --
   -------------------

   procedure Avoid_Overlap
     (Self     : not null access Canvas_View_Record'Class;
      Avoid    : Boolean;
      Duration : Standard.Duration := 0.2)
   is
   begin
      Self.Avoid_Overlap := Avoid;
      Self.Avoid_Overlap_Duration := Duration;
   end Avoid_Overlap;

   ------------
   -- Export --
   ------------

   function Export
     (Self              : not null access Canvas_View_Record;
      Filename          : String;
      Page              : Page_Format;
      Format            : Export_Format := Export_PDF;
      Visible_Area_Only : Boolean := True)
      return Boolean
   is
      W : constant Gdouble := Page.Width_In_Inches * 72.0;  --  in points
      H : constant Gdouble := Page.Height_In_Inches * 72.0;
      Surf : Cairo_Surface;
      Context : Draw_Context;
      Old_Scale : constant Gdouble := Self.Scale;
      Topleft   : constant Model_Point := Self.Topleft;
      Box       : Model_Rectangle;
      Status    : Cairo_Status;
   begin
      case Format is
         when Export_PDF =>
            Surf := Cairo.PDF.Create
              (Filename         => Filename,
               Width_In_Points  => W,
               Height_In_Points => H);
         when Export_SVG =>
            Surf := Cairo.SVG.Create (Filename, W, H);
         when Export_PNG =>
            Surf := Create_Similar_Surface
              (Get_Window (Self),
               Cairo.Cairo_Content_Color_Alpha, Gint (W), Gint (H));
      end case;

      Context := (Cr     => Create (Surf),
                  Layout => Self.Layout,
                  View   => Canvas_View (Self));

      if Visible_Area_Only then
         Box := Self.Get_Visible_Area;

         Self.Scale := Self.Scale *
           Gdouble'Min
             (Gdouble'Min
                (W / Gdouble (Self.Get_Allocated_Width),
                 H / Gdouble (Self.Get_Allocated_Height)),
              1.0);
         Canvas_View_Record'Class (Self.all).Set_Transform (Context.Cr);

         --  Need to clip, otherwise the items partially visible on the screen
         --  will be fully visible in the print

         Rectangle (Context.Cr, Box.X, Box.Y, Box.Width, Box.Height);
         Clip (Context.Cr);

         Canvas_View_Record'Class (Self.all).Draw_Internal (Context, Box);

      else
         Box := Self.Model.Bounding_Box;
         Self.Scale := Gdouble'Min
           (1.0,
            Gdouble'Min (W / (Box.Width + 20.0), H / (Box.Height + 20.0)));
         Self.Topleft := (Box.X - 10.0 * Self.Scale,
                          Box.Y - 10.0 * Self.Scale);
         Canvas_View_Record'Class (Self.all).Set_Transform (Context.Cr);
         Canvas_View_Record'Class (Self.all).Draw_Internal (Context, Box);
      end if;

      Destroy (Context.Cr);

      if Format = Export_PNG then
         Status := Cairo.Png.Write_To_Png (Surf, Filename);
      else
         Status := Cairo.Surface.Status (Surf);
      end if;

      Surface_Destroy (Surf);
      Self.Scale := Old_Scale;
      Self.Topleft := Topleft;

      return Status = Cairo_Status_Success;
   end Export;

   ------------------------
   -- Initialize_Details --
   ------------------------

   procedure Initialize_Details
      (Self    : not null access Canvas_View_Record'Class;
       Details : out Canvas_Event_Details) is
      pragma Unreferenced (Self);
   begin
      Details :=
        (Event_Type => Custom,
         Button     => 1,
         Key        => 0,
         State      => 0,
         Root_Point => (0.0, 0.0),
         M_Point    => (0.0, 0.0),
         T_Point    => No_Item_Point,
         I_Point    => No_Item_Point,
         Item       => null,
         Toplevel_Item     => null,
         Allow_Snapping    => True,
         Allowed_Drag_Area => No_Drag_Allowed);
   end Initialize_Details;

   -----------------
   -- Set_Details --
   -----------------

   procedure Set_Details
     (Self    : not null access Canvas_View_Record'Class;
      Details : out Canvas_Event_Details;
      Event   : Gdk.Event.Gdk_Event_Button)
   is
   begin
      Details :=
        (Event_Type => Button_Press,
         Button     => Event.Button,
         Key        => 0,
         State      => Event.State and Get_Default_Mod_Mask,
         Root_Point => (Event.X_Root, Event.Y_Root),
         M_Point    => Self.Window_To_Model ((X => Event.X, Y => Event.Y)),
         T_Point    => No_Item_Point,
         I_Point    => No_Item_Point,
         Item       => null,
         Toplevel_Item     => null,
         Allow_Snapping    => True,
         Allowed_Drag_Area => No_Drag_Allowed);

      case Event.The_Type is
         when Gdk.Event.Button_Press =>
            Compute_Item (Self, Details);

         when Gdk_2button_Press =>
            Details.Event_Type := Double_Click;
            Compute_Item (Self, Details);

         when Gdk.Event.Button_Release =>
            if Self.In_Drag then
               Details.Event_Type := End_Drag;
               Details.Toplevel_Item :=
                 Self.Last_Button_Press.Toplevel_Item;
            else
               Details.Event_Type := Button_Release;

               --  The previous button press even might have deleted the item,
               --  in which case On_Item_Destroyed_For_View has reset the
               --  Last_Buttton_Press field
               if Details.M_Point = Self.Last_Button_Press.M_Point
                 and then Details.Toplevel_Item /= null
               then
                  --  Do not spend time recomputing
                  Details.Toplevel_Item :=
                    Self.Last_Button_Press.Toplevel_Item;
                  Details.Item := Self.Last_Button_Press.Item;
               else
                  Compute_Item (Self, Details);
               end if;
            end if;

         when others =>
            --  invalid
            Details.Event_Type := Key_Press;
      end case;
   end Set_Details;

   ----------------------
   -- Viewport_Changed --
   ----------------------

   procedure Viewport_Changed
     (Self   : not null access Canvas_View_Record'Class)
   is
   begin
      Object_Callback.Emit_By_Name (Self, Signal_Viewport_Changed);
   end Viewport_Changed;

   -------------------------
   -- On_Viewport_Changed --
   -------------------------

   function On_Viewport_Changed
     (Self : not null access Canvas_View_Record'Class;
      Call : not null access procedure
        (Self : not null access GObject_Record'Class);
      Slot : access GObject_Record'Class := null)
      return Gtk.Handlers.Handler_Id
   is
   begin
      if Slot = null then
         return Object_Callback.Connect
           (Self, Signal_Viewport_Changed,
            Object_Callback.To_Marshaller (Call));
      else
         return Object_Callback.Object_Connect
           (Self, Signal_Viewport_Changed,
            Object_Callback.To_Marshaller (Call), Slot);
      end if;
   end On_Viewport_Changed;

   ----------------
   -- Item_Event --
   ----------------

   function Item_Event
     (Self    : not null access Canvas_View_Record'Class;
      Details : Event_Details_Access)
      return Boolean
   is
   begin
      return EDA_Emit
        (Self, Signal_Item_Event & ASCII.NUL, Details);
   end Item_Event;

   -------------------
   -- On_Item_Event --
   -------------------

   procedure On_Item_Event
     (Self : not null access Canvas_View_Record'Class;
      Call : not null access function
        (Self    : not null access GObject_Record'Class;
         Details : Event_Details_Access)
        return Boolean;
      Slot : access GObject_Record'Class := null)
   is
   begin
      if Slot = null then
         Object_Return_Callback.Connect
           (Self, Signal_Item_Event,
            EDA_Marshallers.To_Marshaller (Call));
      else
         Object_Return_Callback.Object_Connect
           (Self, Signal_Item_Event,
            EDA_Marshallers.To_Marshaller (Call), Slot);
      end if;
   end On_Item_Event;

   ----------------------------
   -- Inline_Editing_Started --
   ----------------------------

   procedure Inline_Editing_Started
     (Self   : not null access Canvas_View_Record'Class;
      Item : not null access Abstract_Item_Record'Class) is
   begin
      Abstract_Item_Emit
        (Self, Signal_Inline_Editing_Started & ASCII.NUL,
         Abstract_Item (Item));
   end Inline_Editing_Started;

   -------------------------------
   -- On_Inline_Editing_Started --
   -------------------------------

   function On_Inline_Editing_Started
     (Self : not null access Canvas_View_Record'Class;
      Call : not null access procedure
        (Self : access GObject_Record'Class; Item : Abstract_Item);
      Slot : access GObject_Record'Class := null)
      return Gtk.Handlers.Handler_Id is
   begin
      if Slot = null then
         return Object_Callback.Connect
           (Self, Signal_Inline_Editing_Started,
            Abstract_Item_Marshallers.To_Marshaller (Call));
      else
         return Object_Callback.Object_Connect
           (Self, Signal_Inline_Editing_Started,
            Abstract_Item_Marshallers.To_Marshaller (Call), Slot);
      end if;
   end On_Inline_Editing_Started;

   -----------------------------
   -- Inline_Editing_Finished --
   -----------------------------

   procedure Inline_Editing_Finished
     (Self  : not null access Canvas_View_Record'Class;
      Item : not null access Abstract_Item_Record'Class) is
   begin
      Abstract_Item_Emit
        (Self, Signal_Inline_Editing_Finished & ASCII.NUL,
         Abstract_Item (Item));
   end Inline_Editing_Finished;

   --------------------------------
   -- On_Inline_Editing_Finished --
   --------------------------------

   function On_Inline_Editing_Finished
     (Self : not null access Canvas_View_Record'Class;
      Call : not null access procedure
        (Self : access GObject_Record'Class; Item : Abstract_Item);
      Slot : access GObject_Record'Class := null)
      return Gtk.Handlers.Handler_Id is
   begin
      if Slot = null then
         return Object_Callback.Connect
           (Self, Signal_Inline_Editing_Finished,
            Abstract_Item_Marshallers.To_Marshaller (Call));
      else
         return Object_Callback.Object_Connect
           (Self, Signal_Inline_Editing_Finished,
            Abstract_Item_Marshallers.To_Marshaller (Call), Slot);
      end if;
   end On_Inline_Editing_Finished;

   -------------------------
   -- The_Selection_Style --
   -------------------------

   function The_Selection_Style (Self : not null access Canvas_View_Record'Class)
                                 return Gtkada.Style_Pkg.Drawing_Style is
   begin
      return Self.Selection_Style;
   end The_Selection_Style;

   ---------------
   -- The_Scale --
   ---------------

   function The_Scale (Self : not null access Canvas_View_Record'Class)
                       return Gdouble is
   begin
      return Self.Scale;
   end The_Scale;

   --------------------------
   -- The_Inline_Edit_Item --
   --------------------------

   function The_Inline_Edit_Item (Self : not null access Canvas_View_Record'Class)
                                  return Abstract_Item is
   begin
      return Self.Inline_Edit.Item;
   end The_Inline_Edit_Item;

end Gtkada.Canvas_View_Pkg;
