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

with Cairo;                  use Cairo;
with Cairo.Pattern;          use Cairo.Pattern;
with Cairo.Matrix;           use Cairo.Matrix;
with Gdk.RGBA;               use Gdk.RGBA;

package body Gtkada.Canvas_Container_Item_Pkg is

   Fixed_Size : constant Size := (Unit_Pixels, Gdouble'First);
   --  When this is set for the max size of an item, it indicates that the min
   --  size is in fact a hard-coded size that the widget must respect.

   ----------------------
   -- Set_Child_Layout --
   ----------------------

   procedure Set_Child_Layout
     (Self   : not null access Container_Item_Record'Class;
      Layout : Child_Layout_Strategy)
   is
   begin
      Self.Layout := Layout;
   end Set_Child_Layout;

   ---------------
   -- Add_Child --
   ---------------

   procedure Add_Child
     (Self     : not null access Container_Item_Record'Class;
      Child    : not null access Container_Item_Record'Class;
      Align    : Alignment_Style := Align_Start;
      Pack_End : Boolean := False;
      Margin   : Margins := No_Margins;
      Float    : Boolean := False;
      Overflow : Overflow_Style := Overflow_Prevent) is
   begin
      Child.Margin   := Margin;
      Child.Parent   := Container_Item (Self);
      Child.Float    := Float;
      Child.Overflow := Overflow;
      Child.Align    := Align;
      Child.Pack_End := Pack_End;
      Self.Children.Append (Abstract_Item (Child));
   end Add_Child;

   -----------
   -- Clear --
   -----------

   procedure Clear
     (Self     : not null access Container_Item_Record;
      In_Model : not null access Canvas_Model_Record'Class)
   is
      To_Remove : Item_Sets.Set;

      procedure Do_Child (C : not null access Container_Item_Record'Class);
      procedure Do_Child (C : not null access Container_Item_Record'Class) is
      begin
         --  Remove the children and their links
         In_Model.Include_Related_Items (C, To_Remove);
      end Do_Child;

   begin
      if not Self.Children.Is_Empty then
         Container_Item_Record'Class (Self.all).For_Each_Child
            (Do_Child'Access, Recursive => False);
         Self.Children.Clear;
         Remove (In_Model, To_Remove);
         In_Model.Refresh_Layout;
      end if;
   end Clear;

   ---------------------
   -- Set_Width_Range --
   ---------------------

   procedure Set_Width_Range
     (Self     : not null access Container_Item_Record;
      Min, Max : Size := Auto_Size) is
   begin
      Self.Min_Width := Min;
      Self.Max_Width := Max;
   end Set_Width_Range;

   ----------------------
   -- Set_Height_Range --
   ----------------------

   procedure Set_Height_Range
     (Self     : not null access Container_Item_Record;
      Min, Max : Size := Auto_Size) is
   begin
      Self.Min_Height := Min;
      Self.Max_Height := Max;
   end Set_Height_Range;

   --------------
   -- Set_Size --
   --------------

   procedure Set_Size
      (Self : not null access Container_Item_Record;
       Width, Height : Size := Auto_Size) is
   begin
      if Width = Auto_Size or else Width = Fit_Size then
         Self.Min_Width := (Unit_Pixels, 1.0);
         Self.Max_Width := Width;
      else
         Self.Min_Width := Width;
         Self.Max_Width := Fixed_Size;
      end if;

      if Height = Auto_Size or else Height = Fit_Size then
         Self.Min_Height := (Unit_Pixels, 1.0);
         Self.Max_Height := Height;
      else
         Self.Min_Height := Height;
         Self.Max_Height := Fixed_Size;
      end if;
   end Set_Size;

   ------------------
   -- Size_Request --
   ------------------

   procedure Size_Request
     (Self    : not null access Container_Item_Record;
      Context : Draw_Context)
   is
      use Items_Lists;
      C     : Items_Lists.Cursor := Self.Children.First;
      Child : Container_Item;
      Tmp, Tmp2 : Model_Coordinate;
   begin
      Self.Width  := 0.0;
      Self.Height := 0.0;

      Tmp := 0.0; --  Current coordinate (X or Y) for the child

      while Has_Element (C) loop
         Child := Container_Item (Element (C));

         Child.Computed_Position := No_Position;
         Child.Size_Request (Context);

         case Self.Layout is
            when Vertical_Stack =>
               case Child.Overflow is
                  when Overflow_Prevent =>
                     Self.Width := Model_Coordinate'Max
                       (Child.Width + Child.Margin.Left + Child.Margin.Right,
                        Self.Width);
                  when Overflow_Hide =>
                     null;
               end case;

               if Child.Position.Y /= Gdouble'First then
                  Tmp2 := Child.Position.Y + Child.Height;
               else
                  Tmp2 := Tmp + Child.Height;
               end if;

               Tmp2 := Tmp2 + Child.Margin.Top + Child.Margin.Bottom;
               Self.Height := Model_Coordinate'Max (Self.Height, Tmp2);

               if not Child.Float then
                  --  The lowest point so far
                  Tmp := Self.Height;
               end if;

            when Horizontal_Stack =>
               case Child.Overflow is
                  when Overflow_Prevent =>
                     Self.Height := Model_Coordinate'Max
                        (Child.Height + Child.Margin.Top
                         + Child.Margin.Bottom,
                         Self.Height);
                  when Overflow_Hide =>
                     null;
               end case;

               if Child.Position.X /= Gdouble'First then
                  Tmp2 := Child.Position.X + Child.Width;
               else
                  Tmp2 := Tmp + Child.Width;
               end if;

               Tmp2 := Tmp2 + Child.Margin.Left + Child.Margin.Right;
               Self.Width := Model_Coordinate'Max (Self.Width, Tmp2);

               if not Child.Float then
                  Tmp := Self.Width;
               end if;
         end case;

         Next (C);
      end loop;

      --  The previous computation was for the standalone / ideal size for
      --  the children and Self.
      --  But we now need to take the size constraints into account. When
      --  they are given in pixels, we apply them immediately. When they are
      --  given in units relative to the parent's size, we can only apply
      --  them in Size_Allocate.

      if Self.Min_Width.Unit = Unit_Pixels then
         if Self.Max_Width = Fixed_Size then
            Self.Width := Self.Min_Width.Length;
         else
            Self.Width := Model_Coordinate'Max
               (Self.Width, Self.Min_Width.Length);
         end if;
      end if;

      if Self.Max_Width.Unit = Unit_Pixels
         and then Self.Max_Width /= Fixed_Size
      then
         Self.Width := Model_Coordinate'Min
            (Self.Width, Self.Max_Width.Length);
      end if;

      if Self.Min_Height.Unit = Unit_Pixels then
         if Self.Max_Height = Fixed_Size then
            Self.Height := Self.Min_Height.Length;
         else
            Self.Height := Model_Coordinate'Max
               (Self.Height, Self.Min_Height.Length);
         end if;
      end if;

      if Self.Max_Height.Unit = Unit_Pixels
         and then Self.Max_Height /= Fixed_Size
      then
         Self.Height := Model_Coordinate'Min
            (Self.Height, Self.Max_Height.Length);
      end if;
   end Size_Request;

   ----------------------
   -- Set_Size_Request --
   ----------------------

   procedure Set_Size_Request
     (Self    : not null access Container_Item_Record;
      Width, Height : Gdouble := -1.0)
   is
   begin
      if Width >= 0.0 then
         Self.Width := Width;
      end if;

      if Height >= 0.0 then
         Self.Height := Height;
      end if;
   end Set_Size_Request;

   -------------------
   -- Size_Allocate --
   -------------------

   procedure Size_Allocate
     (Self  : not null access Container_Item_Record)
   is
      use Items_Lists;
      C       : Items_Lists.Cursor := Self.Children.First;
      Child   : Container_Item;
      Tmp     : Model_Coordinate := 0.0;
      Tmp_End : Model_Coordinate;
   begin
      case Self.Layout is
         when Vertical_Stack   => Tmp_End := Self.Height;
         when Horizontal_Stack => Tmp_End := Self.Width;
      end case;

      while Has_Element (C) loop
         Child := Container_Item (Element (C));

         --  Apply size constraints when they are proportional to the
         --  parent's size.

         case Child.Min_Width.Unit is
            when Unit_Auto | Unit_Fit => null; --  Only relevant for Max_Width
            when Unit_Pixels => null; --  Taken care of in Size_Request
            when Unit_Percent =>
               if Child.Max_Width = Fixed_Size then
                  Child.Width := Child.Min_Width.Value * Self.Width;
               else
                  Child.Width := Model_Coordinate'Max
                     (Child.Width, Child.Min_Width.Value * Self.Width);
               end if;
         end case;

         case Child.Max_Width.Unit is
            when Unit_Pixels  => null;  --  Taken care of in Size_Request
            when Unit_Percent =>        --  Not Fixed_Size
               Child.Width := Model_Coordinate'Min
                  (Child.Width, Child.Max_Width.Value * Self.Width);
            when Unit_Fit     =>        --  Use full parent size
               if Self.Layout = Vertical_Stack then
                  Child.Width :=
                     Self.Width - Child.Margin.Left - Child.Margin.Right;
               end if;
            when Unit_Auto    =>  --  Use size computed in Size_Request
               null;
         end case;

         case Child.Min_Height.Unit is
            when Unit_Auto | Unit_Fit => null; --  Only relevant for Max_Width
            when Unit_Pixels => null; --  Taken care of in Size_Request
            when Unit_Percent =>
               if Child.Max_Height = Fixed_Size then
                  Child.Height := Child.Min_Height.Value * Self.Height;
               else
                  Child.Height := Model_Coordinate'Max
                     (Child.Height, Child.Min_Height.Value * Self.Height);
               end if;
         end case;

         case Child.Max_Height.Unit is
            when Unit_Pixels  => null;  --  taken care of in Size_Request
            when Unit_Percent =>        --  not Fixed_Size
               Child.Height := Model_Coordinate'Min
                  (Child.Height, Child.Max_Height.Value * Self.Height);
            when Unit_Fit    => --  Use full parent size
               if Self.Layout = Horizontal_Stack then
                  Child.Height :=
                     Self.Height - Child.Margin.Top - Child.Margin.Bottom;
               end if;
            when Unit_Auto    =>  --  Use size computed in Size_Request
               null;
         end case;

         --  Now compute the position for the child

         case Self.Layout is
            when Vertical_Stack =>
               if Child.Position.Y /= Gdouble'First then
                  Child.Computed_Position.Y :=
                    Child.Position.Y + Child.Margin.Top
                    - Child.Height * Child.Anchor_Y;
               else
                  if Child.Pack_End then
                     Child.Computed_Position.Y :=
                        Tmp_End - Child.Height - Child.Margin.Bottom;
                  else
                     Child.Computed_Position.Y := Tmp + Child.Margin.Top;
                  end if;
               end if;

               if Child.Position.X /= Gdouble'First then
                  Child.Computed_Position.X :=
                    Child.Position.X + Child.Margin.Left
                    - Child.Width * Child.Anchor_X;
               else
                  case Child.Align is
                     when Align_Start =>
                        Child.Computed_Position.X := Child.Margin.Left;

                     when Align_Center =>
                        Child.Computed_Position.X :=
                           (Self.Width - Child.Width) / 2.0;

                     when Align_End =>
                        Child.Computed_Position.X :=
                          Self.Width - Child.Width - Child.Margin.Right;
                  end case;
               end if;

               Child.Size_Allocate;

               if not Child.Float then
                  if Child.Pack_End then
                     Tmp_End := Child.Computed_Position.Y - Child.Margin.Top;
                  else
                     Tmp := Child.Computed_Position.Y
                       + Child.Height + Child.Margin.Bottom;
                  end if;
               end if;

            when Horizontal_Stack =>
               if Child.Position.X /= Gdouble'First then
                  Child.Computed_Position.X :=
                    Child.Position.X + Child.Margin.Left
                    - Child.Width * Child.Anchor_X;
               else
                  if Child.Pack_End then
                     Child.Computed_Position.X :=
                        Tmp_End - Child.Width - Child.Margin.Right;
                  else
                     Child.Computed_Position.X := Tmp + Child.Margin.Left;
                  end if;
               end if;

               if Child.Position.Y /= Gdouble'First then
                  Child.Computed_Position.Y :=
                    Child.Position.Y + Child.Margin.Top
                    - Child.Height * Child.Anchor_Y;
               else
                  case Child.Align is
                     when Align_Start =>
                        Child.Computed_Position.Y := Child.Margin.Top;

                     when Align_Center =>
                        Child.Computed_Position.Y :=
                            (Self.Height - Child.Height) / 2.0;

                     when Align_End =>
                        Child.Computed_Position.Y :=
                          Self.Height - Child.Height - Child.Margin.Bottom;
                  end case;
               end if;

               Child.Size_Allocate;

               if not Child.Float then
                  if Child.Pack_End then
                     Tmp_End := Child.Computed_Position.X - Child.Margin.Left;
                  else
                     Tmp := Child.Computed_Position.X
                       + Child.Width + Child.Margin.Right;
                  end if;
               end if;
         end case;

         Next (C);
      end loop;
   end Size_Allocate;

   --------------------
   -- For_Each_Child --
   --------------------

   procedure For_Each_Child
     (Self     : not null access Container_Item_Record'Class;
      Callback : not null access procedure
        (Child : not null access Container_Item_Record'Class);
      Recursive : Boolean := False)
   is
      use Items_Lists;
      C : Items_Lists.Cursor := Self.Children.First;
   begin
      while Has_Element (C) loop
         if Recursive then
            Container_Item (Element (C)).For_Each_Child (Callback, Recursive);
         end if;
         Callback (Container_Item (Element (C)));
         Next (C);
      end loop;
   end For_Each_Child;

   -------------------
   -- Draw_Children --
   -------------------

   procedure Draw_Children
     (Self    : not null access Container_Item_Record'Class;
      Context : Draw_Context)
   is
      use Items_Lists;
      C     : Items_Lists.Cursor := Self.Children.First;
   begin
      while Has_Element (C) loop
         Translate_And_Draw_Item (Element (C), Context);
         Next (C);
      end loop;
   end Draw_Children;

   ---------------
   -- Set_Style --
   ---------------

   procedure Set_Style
     (Self  : not null access Container_Item_Record;
      Style : Drawing_Style) is
   begin
      Self.Style := Style;
   end Set_Style;

   ---------------
   -- Get_Style --
   ---------------

   function Get_Style
     (Self : not null access Container_Item_Record) return Drawing_Style is
   begin
      return Self.Style;
   end Get_Style;

   --------------------
   -- Refresh_Layout --
   --------------------

   procedure Refresh_Layout
     (Self    : not null access Container_Item_Record;
      Context : Draw_Context) is
   begin
      Self.Computed_Position := Self.Position;
      Container_Item_Record'Class (Self.all).Size_Request (Context);
      Container_Item_Record'Class (Self.all).Size_Allocate;

      Self.Computed_Position.X :=
        Self.Computed_Position.X - (Self.Width * Self.Anchor_X);
      Self.Computed_Position.Y :=
        Self.Computed_Position.Y - (Self.Height * Self.Anchor_Y);
   end Refresh_Layout;

   ------------------
   -- Set_Position --
   ------------------

   overriding procedure Set_Position
     (Self     : not null access Container_Item_Record;
      Pos      : Gtkada.Style_Pkg.Point)
   is
   begin
      Container_Item_Record'Class (Self.all).Set_Position (Pos, 0.0, 0.0);
   end Set_Position;

   ------------------
   -- Set_Position --
   ------------------

   procedure Set_Position
     (Self     : not null access Container_Item_Record;
      Pos      : Gtkada.Style_Pkg.Point := (Gdouble'First, Gdouble'First);
      Anchor_X : Percent;
      Anchor_Y : Percent)
   is
   begin
      Self.Computed_Position := Pos;
      Self.Anchor_X := Anchor_X;
      Self.Anchor_Y := Anchor_Y;
      Canvas_Item_Record (Self.all).Set_Position (Pos);  --  inherited
   end Set_Position;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy
     (Self     : not null access Container_Item_Record;
      In_Model : not null access Canvas_Model_Record'Class) is
   begin
      Self.Clear (In_Model);
      Canvas_Item_Record (Self.all).Destroy (In_Model);  --  inherited
   end Destroy;

   --------------
   -- Position --
   --------------

   overriding function Position
     (Self : not null access Container_Item_Record) return Gtkada.Style_Pkg.Point
   is
   begin
      return Self.Computed_Position;
   end Position;

   ------------
   -- Parent --
   ------------

   overriding function Parent
     (Self : not null access Container_Item_Record)
      return Abstract_Item
   is
   begin
      return Abstract_Item (Self.Parent);
   end Parent;

   ------------------
   -- Bounding_Box --
   ------------------

   function Bounding_Box
     (Self : not null access Container_Item_Record)
      return Item_Rectangle is
   begin
      --  assumes size_request has been called already
      return (0.0,
              0.0,
              Self.Width,
              Self.Height);
   end Bounding_Box;

   ---------------------
   -- Inner_Most_Item --
   ---------------------

   overriding function Inner_Most_Item
     (Self     : not null access Container_Item_Record;
      At_Point : Model_Point;
      Context  : Draw_Context) return Abstract_Item
   is
      use Items_Lists;
      C     : Items_Lists.Cursor := Self.Children.First;
      P     : Item_Point;
      Child : Container_Item;
   begin
      while Has_Element (C) loop
         Child := Container_Item (Element (C));
         P := Child.Model_To_Item  (At_Point);

         if Child.Contains (P, Context) then
            return Child.Inner_Most_Item (At_Point, Context);
         end if;

         Next (C);
      end loop;
      return Abstract_Item (Self);
   end Inner_Most_Item;

   ------------------
   -- Is_Invisible --
   ------------------

   overriding function Is_Invisible
     (Self : not null access Container_Item_Record)
      return Boolean
   is
   begin
      return Self.Style.Get_Stroke = Null_RGBA
        and then Self.Style.Get_Fill = Null_Pattern;
   end Is_Invisible;

   ---------------
   -- Get_Width --
   ---------------

   function Get_Width (Self : not null access Container_Item_Record)
                       return Model_Coordinate is
   begin
      return Self.Width;
   end Get_Width;

   ----------------
   -- Get_Height --
   ----------------

   function Get_Height (Self : not null access Container_Item_Record)
      return Model_Coordinate is
   begin
      return Self.Height;
   end Get_Height;

   ---------------------------
   -- Get_Computed_Position --
   ---------------------------

   function Get_Computed_Position (Self : not null access Container_Item_Record)
      return Gtkada.Style_Pkg.Point is
   begin
      return Self.Computed_Position;
   end Get_Computed_Position;

   -----------------------------
   -- Store_Computed_Position --
   -----------------------------

   procedure Store_Computed_Position (Self : not null access Container_Item_Record;
                                      CP   : Gtkada.Style_Pkg.Point) is
   begin
      if CP.X /= Gdouble'First then
         Self.Computed_Position.X := CP.X;
      end if;
      if CP.Y /= Gdouble'First then
         Self.Computed_Position.Y := CP.Y;
      end if;
   end Store_Computed_Position;

   -------------------------
   -- Resize_Fill_Pattern --
   -------------------------

   procedure Resize_Fill_Pattern
     (Self : not null access Container_Item_Record'Class)
   is
      Matrix  : aliased Cairo_Matrix;
      Fill    : constant Cairo_Pattern := Self.Style.Get_Fill;
   begin
      if Fill /= Null_Pattern then
         case Get_Type (Fill) is
            when Cairo_Pattern_Type_Linear
               | Cairo_Pattern_Type_Radial =>

               Init_Scale (Matrix'Access, 1.0 / Self.Width, 1.0 / Self.Height);
               Set_Matrix (Fill, Matrix'Access);

            when others =>
               null;
         end case;
      end if;
   end Resize_Fill_Pattern;

   ------------------
   -- The_Children --
   ------------------

   function The_Children (Self : not null access Container_Item_Record)
                          return Items_Lists.List is
   begin
      return Self.Children;
   end The_Children;
end Gtkada.Canvas_Container_Item_Pkg;
