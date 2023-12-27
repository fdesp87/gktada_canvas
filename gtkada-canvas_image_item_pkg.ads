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

with Gtkada.Style_Pkg;
with Gdk.Pixbuf;                          use Gdk.Pixbuf;
with GNAT.Strings;                        use GNAT.Strings;

with Gtkada.Canvas_Defs;                  use Gtkada.Canvas_Defs;
with Gtkada.Canvas_Container_Item_Pkg;    use Gtkada.Canvas_Container_Item_Pkg;
with Gtkada.Canvas_View_Pkg;              use Gtkada.Canvas_View_Pkg;
with Gtkada.Canvas_Model_Pkg;             use Gtkada.Canvas_Model_Pkg;

package Gtkada.Canvas_Image_Item_Pkg is

   type Image_Item_Record is new Container_Item_Record with private;
   type Image_Item is access all Image_Item_Record'Class;
   --  An item that shows an image.
   --  The style is used to draw a rectangle around the image

   function Gtk_New_Image
     (Style  : Gtkada.Style_Pkg.Drawing_Style;
      Image  : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class;
      Allow_Rescale : Boolean := True;
      Width, Height : Model_Coordinate := Fit_Size_As_Double)
      return Image_Item;

   procedure Initialize_Image
     (Self   : not null access Image_Item_Record'Class;
      Style  : Gtkada.Style_Pkg.Drawing_Style;
      Image  : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class;
      Allow_Rescale : Boolean := True;
      Width, Height : Model_Coordinate := Fit_Size_As_Double);
   --  Create a new image item.
   --  By default, the size is computed from the image, but if self is
   --  actually allocated a different size, the image will be rescaled as
   --  appropriate. You can disable this behavior by setting Allow_Rescale to
   --  False.

   function Gtk_New_Image
     (Style  : Gtkada.Style_Pkg.Drawing_Style;
      Icon_Name : String;
      Allow_Rescale : Boolean := True;
      Width, Height : Model_Coordinate := Fit_Size_As_Double)
      return Image_Item;

   procedure Initialize_Image
     (Self   : not null access Image_Item_Record'Class;
      Style  : Gtkada.Style_Pkg.Drawing_Style;
      Icon_Name : String;
      Allow_Rescale : Boolean := True;
      Width, Height : Model_Coordinate := Fit_Size_As_Double);
   --  Same as buffer, but the image is created from one of the files given
   --  by the Gtk.Icon_Theme. This will often result in better (more sharp)
   --  rendering.
   --  You should in general specify the size you want to use, since the
   --  icon_name itself does not provide this information.

   overriding procedure Draw
     (Self    : not null access Image_Item_Record;
      Context : Draw_Context);

   overriding procedure Destroy
     (Self     : not null access Image_Item_Record;
      In_Model : not null access Canvas_Model_Record'Class);

   overriding procedure Size_Request
     (Self    : not null access Image_Item_Record;
      Context : Draw_Context);

private

   type Image_Item_Record is new Container_Item_Record with record
      Image         : Gdk.Pixbuf.Gdk_Pixbuf;
      Icon_Name     : GNAT.Strings.String_Access;
      Allow_Rescale : Boolean := True;
   end record;

end Gtkada.Canvas_Image_Item_Pkg;
