with Ada.Task_Identification; use Ada.Task_Identification;
with Ada.Containers.Hashed_Maps;  use Ada.Containers;

package Vehicle_Task_Type is

   task type Vehicle_Task is
      entry Identify (Set_Vehicle_No : Positive; Local_Task_Id : out Task_Id);
   end Vehicle_Task;

   -- hash map is the map operation type model in ada as same as in python, can have union, difference...
   -- very helpful for me to do stage D, which need uplode one table every time quickly
   function ID_Hashed (id : Positive) return Hash_Type is (Hash_Type (id));

    package My_Hash is new Ada.Containers.Hashed_Maps (Key_Type => Positive,
                                                         Element_Type => Boolean,
                                                         Hash => ID_Hashed,
                                                         Equivalent_Keys => "=");
    use My_Hash;

   -- Reference from: https://en.wikibooks.org/wiki/Ada_Programming/Containers

   subtype Live_table is My_Hash.Map;
   -- Live_table is the subtype for the tasks to build own live table easily

end Vehicle_Task_Type;
