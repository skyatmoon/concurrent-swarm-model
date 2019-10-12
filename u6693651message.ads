-- Suggestions for packages which might be useful:

with Ada.Real_Time;         use Ada.Real_Time;
with Vectors_3D;            use Vectors_3D;
with Vehicle_Task_Type;     use Vehicle_Task_Type;

package Vehicle_Message_Type is

   -- Replace this record definition by what your vehicles need to communicate.

   type Inter_Vehicle_Messages is
      record

         Message_Time : Time;
         -- how new the message is
         Energy_Position : Vector_3D;
         -- the position for the energy ball now
         Live_Death_Table  : Live_table;
         -- the live table every car should uplode it together to know it should to die or not

      end record;

end Vehicle_Message_Type;
