------------------------------------------------------------------------------
--                                                                          --
--                         REFLEX COMPILER COMPONENTS                       --
--                                                                          --
--          Copyright (C) 1992-2011, Free Software Foundation, Inc.         --
--                                                                          --
-- Reflex is free software; you can redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware Foundation; either version 3, or (at your option) any later version --
-- Reflex is distributed in the hope that it will be useful, but WITH-      --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License distributed with Reflex; see file COPYING3. If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- Reflex is originally developed  by the Artics team at Grenoble.          --
--                                                                          --
------------------------------------------------------------------------------

package body Reflex.Guids is
   
   
    ---------------------
    -- GUID GENERATION --
    ---------------------

    -- GUID Generation system
    Current_GUID_ID : Integer := 1;
    Packages_GUID   : Package_GUID_List.List_id := Package_GUID_List.New_list;


    ------------------
    -- Get_One_GUID --
    ------------------

    function Get_One_GUID
      return String
    is
        GUID : String := "00000000-0000-0000-0000-000000000000";
    begin
        -- Generate a new unique GUID
        GUID((9 - Trim(Current_GUID_ID'Img, both)'Length) .. 8) := Trim(Current_GUID_ID'Img, both);
        Current_GUID_ID := Current_GUID_ID + 1;

        return GUID;
    end Get_One_GUID;


    -----------------------------
    -- Get_Current_Folder_GUID --
    -----------------------------

    function Get_Current_Folder_GUID(Imposed_Nbr : Integer := -1)
                                    return String
    is
        GUID : String := "00000000-0000-0000-0000-000000000000";
        The_Id : Integer := Imposed_Nbr;
    begin
        -- This GUID is renewed only when Get_One_GUID is called
        -- Imposed_Nbr is used when you need to get a specific
        -- GUID. It forces to use the given number instead of
        -- "Current_GUID_ID".

        if The_Id = -1 then
            The_Id := Current_GUID_ID;
        end if;

        GUID(GUID'First + 9 .. Trim(The_Id'Img, both)'Length + 9)
          := Trim(The_Id'Img, both);
        return GUID;
    end Get_Current_Folder_GUID;


    -----------------------
    -- Have_Package_GUID --
    -----------------------

    function Have_Package_GUID
      (Node_Pack : Node_id)
     return Boolean is
        it : Package_GUID_List.List_Iterator := Package_GUID_List.New_Iterator(Packages_GUID);
    begin
        while not Package_GUID_List.Is_End(it) loop
            if Package_GUID_List.current_item(it).The_Package = Node_Pack
              and then Package_GUID_List.current_item(it).GUID /= -1 then
                return True;
            end if;
            Package_GUID_List.Next(it);
        end loop;

        return false;
    end Have_Package_GUID;


    ----------------------
    -- Get_Package_GUID --
    ----------------------

    function Get_Package_GUID(Node_Pack : Node_Id)
                             return String is
        it : Package_GUID_List.List_Iterator := Package_GUID_List.New_Iterator(Packages_GUID);
        new_Pack : Codesys_Package_Id;
    begin
        -- The aim of this function is to retrieve an already generated UID
        -- for the given package.
        -- By default, if the package doesn't have an attributed ID, a new
        -- one is created and associated with the package.
        while not Package_GUID_List.Is_End(it) loop
            if Package_GUID_List.current_item(it).The_Package = Node_Pack
              and then Package_GUID_List.current_item(it).GUID /= -1 then
                return Get_Current_Folder_GUID(Package_GUID_List.current_item(it).GUID);
            end if;
            Package_GUID_List.Next(it);
        end loop;

        -- Package is not associated with a guid, get a new one and save it
        new_Pack := (Node_Pack, Current_GUID_ID);

        Package_GUID_List.Append(new_Pack, Packages_GUID);

        return Get_Current_Folder_GUID;
    end Get_Package_GUID;

   
end Reflex.Guids;
