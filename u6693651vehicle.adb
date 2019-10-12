-- Suggestions for packages which might be useful:

with Exceptions;                 use Exceptions;
with Vehicle_Interface;          use Vehicle_Interface;
with Ada.Real_Time;              use Ada.Real_Time;
with Swarm_Size;                 use Swarm_Size;
with Vectors_3D;                 use Vectors_3D;
with Vehicle_Message_Type;       use Vehicle_Message_Type;
with Swarm_Structures;           use Swarm_Structures;
with Swarm_Structures_Base;      use Swarm_Structures_Base;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Numerics.Discrete_Random;

package body Vehicle_Task_Type is

   task body Vehicle_Task is
      -------------------------------------------------------------------
      -- Private Variables define --
      -------------------------------------------------------------------

      Vehicle_No : Positive;
      -- for stage D, need a Vehicle No as an ID in Hash map
      Send_Message, My_Message : Inter_Vehicle_Messages;
      -- two messages, one for task own private information, one for sending
      Orbital_radius :  Long_Float := 0.01;
      -- moving radius
      Random_only_one : Boolean := True;
      -- only need to compute once thr random number
      N1, N2 : Float;
      -- two random number for the angle
      Count : Integer := 0;
      -- count how many times the Vehicle in Celestial moving
      D_Num : constant Positive := Target_No_of_Elements;
      -- car know how many should alive in stage D
      Stage_D : Boolean;
      -- Do stage D or not
      Live_set : Live_table;
      -- Live_set is the table every car have, each uplode it every time to find it should goto die or not
      My_energy : Vehicle_Charges;
      -- be the flag car or not, if not it be the current charge, else 0.1
      Scout_bee : Boolean := False;
      -- Flag car or not
      Total : Integer := 0;
      -- the total number ships can get from their own live table

      -- You will want to take the pragma out, once you use the "Vehicle_No"

      -------------------------------------------------------------------
      -- Useful functions define --
      -------------------------------------------------------------------

      function Live_or_Death (No : Positive) return Boolean is
         Sum : Integer := 0;      -- how many in the table
         Dead : Boolean := False; -- return alive or not
      begin
         for i in Live_set.Iterate loop
            if Element (i) and then Key (i) < No  then
               Sum := Sum + 1;
            end if;
         end loop;
         -- count how many in hash map
         Sum := Sum + 1;
         -- count how many cars already alive
         if Sum > D_Num then
            Dead := True;
         end if;
         -- no space for it, die
         return Dead;
      end Live_or_Death;
      -- count the number in the car's live set, not too mach, alive
      function Total_Num return Integer is
         Sum : Integer := 0;      -- how many in the table
      begin
         for i in Live_set.Iterate loop
            if Element (i) and then Key (i) < 999 then
               Sum := Sum + 1;
            end if;
         end loop;
         -- count how many in hash map
         Sum := Sum + 1;
         -- count how many cars already alive
         return Sum;
      end Total_Num;
      -- count the number in the car's live set return how many are there

   begin

      -- You need to react to this call and provide your task_id.Current_Charge
      -- You can e.g. employ the assigned vehicle number (Vehicle_No)
      -- in communications with other vehicles.

      accept Identify (Set_Vehicle_No : Positive; Local_Task_Id : out Task_Id) do
         Vehicle_No     := Set_Vehicle_No;
         Local_Task_Id  := Current_Task;
      end Identify;

      -- Replace the rest of this task with your own code.
      -- Maybe synchronizing on an external event clock like "Wait_For_Next_Physics_Update",
      -- yet you can synchronize on e.g. the real-time clock as well.
      -- Without control this vehicle will go for its natural swarming instinct.

      select

         Flight_Termination.Stop;

      then abort

         Outer_task_loop : loop

            Wait_For_Next_Physics_Update;

            -- Your vehicle should respond to the world here: sense, listen, talk, act?
            declare
               -- using declare to let the task can work as a whole part, avoid terminat by other tasks information
               type Rand_Range is range 0 .. 360;
               package Rand_Int is new Ada.Numerics.Discrete_Random (Rand_Range);
               seed : Rand_Int.Generator;
               Num : Rand_Range;
               -- for a random angle for the ball
               type Rand_Range2 is range 0 .. 360;
               package Rand_Int2 is new Ada.Numerics.Discrete_Random (Rand_Range2);
               seed2 : Rand_Int2.Generator;
               Num2 : Rand_Range2;
               -- for another random angle for the ball
               type Rand_Range3 is range 0 .. 20;
               package Rand_Int3 is new Ada.Numerics.Discrete_Random (Rand_Range3);
               seed3 : Rand_Int3.Generator;
               Num3 : Rand_Range3;
               -- for another random angle for the ball
               X : Long_Float;
               Y : Long_Float;
               Z : Long_Float;
               -- X, Y, Z Position for the ball
               Celestial_matrix : constant array (0 .. 15) of Vector_3D :=
                 (
                    (0.7 * Orbital_radius, 0.7 * Orbital_radius, 0.0), (0.0, Orbital_radius, 0.0), (-0.7 * Orbital_radius, 0.7 * Orbital_radius, 0.0), (-Orbital_radius, 0.0, 0.0), (-0.7 * Orbital_radius, -0.7 * Orbital_radius, 0.0), (0.0, -Orbital_radius, 0.0), (0.7 * Orbital_radius, -0.7 * Orbital_radius, 0.0), ((0.1 + Orbital_radius), 0.0, 0.0), (0.7 * Orbital_radius, 0.0, 0.7 * Orbital_radius),
                  (0.0, 0.0, Orbital_radius), (-0.7 *  Orbital_radius, 0.0,  0.7 * Orbital_radius), ((-0.1 - Orbital_radius), 0.0, 0.0), (-0.7 * Orbital_radius, 0.0, -0.7 * Orbital_radius), (0.0, 0.0, -Orbital_radius), (0.7 * Orbital_radius, 0.0, -0.7 * Orbital_radius), (Orbital_radius, 0.0, 0.0)
                 );
               -- 0.7 using as sqrt(2)/2 to avoid too many computing happend in concurrent systems to avoid problems

               speed_matrix : constant array (0 .. 2) of Long_Float := (0.5, 0.7, 0.9);
               -- compute three different types ships: energy keeper--keep system going on using half energy than others
               -- commen bees: using 70% energy than others,  busy bees: use most of energy as fast as it can to
               -- keep message update
               plus_matrix : constant array (0 .. 2) of Long_Float := (0.0, 0.05, 0.1);
               -- let ships can change their radius depended on the total number they know, avoid such as only two
               -- ships but R = 0.5
               waiting_position : Positions;
               -- waiting point on the circle
               R_plus : Long_Float;
               --
            begin

               if Random_only_one then

                  Random_only_one := False;
                  Rand_Int.Reset (seed);
                  Num := Rand_Int.Random (seed);
                  N1 := Float (Num);
                  -- compute random angle 1
                  Rand_Int2.Reset (seed2);
                  Num2 := Rand_Int2.Random (seed2);
                  N2 := Float (Num2);
                  -- compute random angle 2
                  My_Message.Energy_Position := (0.0, 0.0, 0.0);
                  My_Message.Message_Time := Clock;
                  Send_Message := My_Message;
                  Count := Integer (Vehicle_No mod 16);
                  -- Initialization procedures to avoid information gaps caused by the collapse of the program in the initial transfer
               end if;
               Rand_Int3.Reset (seed3);
               Num3 := Rand_Int3.Random (seed3);
               -- compute random 3 for Scout_bee or not
               if Num3 = 1 then
                  Scout_bee := True;
               end if;
               -- only 5% of car can be the Scout_bee

               -------------------------------------------------------------------
               -- Do Stage_D or not --
               -------------------------------------------------------------------
               Stage_D := True;
               -- By talking with WILL about stage D, I choose to use Hash Map to do it
               -------------------------------------------------------------------
               -- Message send & get part --
               -------------------------------------------------------------------

               if not Live_set.Contains (Vehicle_No) then
                  Live_set.Insert (Vehicle_No, True);
               end if;
               -- add self to live

               if Energy_Globes_Around'Length > 0 then
                  -- find the energy ball first concurrent programe !!! more important more forward !!!
                  My_Message.Message_Time := Clock;
                  My_Message.Energy_Position := Vector_3D (Energy_Globes_Around (1).Position);
                  -- only 1 enery ball around, use it
                  if Energy_Globes_Around'Length > 1 then
                     for i of Energy_Globes_Around loop
                        if abs (i.Position - Position) < abs (My_Message.Energy_Position - Position) then
                           My_Message.Energy_Position := i.Position;
                        end if;
                     end loop;
                  end if;
                  -- more than one enery ball around, find the nearest one

                  Send_Message.Message_Time := My_Message.Message_Time;
                  Send_Message.Energy_Position := My_Message.Energy_Position;
                  if not Send_Message.Live_Death_Table.Contains (Vehicle_No) then
                     Send_Message.Live_Death_Table.Insert (Vehicle_No, True);
                  end if;
                  -- update Send_Message with My_Message and Live set just use,
                  -- instead of time and position now  to avoid Delayed affect the accuracy
                  My_Message := Send_Message;
                  Send (Send_Message);
                  -- send the newest message
               end if;

               if not Send_Message.Live_Death_Table.Contains (Vehicle_No) then
                  Send_Message.Live_Death_Table.Insert (Vehicle_No, True);
               end if;
               -- greed: no me, let me alive
               Send (Send_Message);
               ----Send the initial Send_Message as well if the vehicle cannot locate any energy globe(Energt_Globes_Around'Length=0)

               while Messages_Waiting loop
                  -- waiting for the new message
                  Receive (Send_Message);
                  -- update live set == down loade the Send_Message.Live_Death_Table
                  for i in Send_Message.Live_Death_Table.Iterate loop
                     if Live_set.Contains (Key (i)) then
                        Live_set.Replace (Key (i), Element (i));
                     else
                        Live_set.Insert (Key (i), Element (i));
                     end if;
                  end loop;
               end loop;

               if My_Message.Message_Time < Send_Message.Message_Time then
                  -- new enough message should update
                  My_Message := Send_Message;
                  -- update message
               end if;

               -- Live_set.Difference (My_Message.Live_Death_Table);
               -- avoid same Vehicle in the live table

               if Current_Charge < 0.025 then
                  Send_Message.Live_Death_Table.Replace (Vehicle_No, False);
                  Send (Send_Message);
                  -- no energy, go die, let the live table new to avoid dead too muchx-special/nautilus-clipboard
                  exit Outer_task_loop;
               end if;

               if Live_or_Death (Vehicle_No) and then Stage_D then
                  -- find it should die or not
                  Send_Message.Live_Death_Table.Replace (Vehicle_No, False);
                  Send (Send_Message);
                  -- before go die, tell others that you are die
                  Set_Throttle (0.0); -- free
                  -- Go die
                  Flight_Termination.Stop;
                  exit Outer_task_loop;
               end if;
               -------------------------------------------------------------------
               -- Total number compute part --
               -------------------------------------------------------------------
               Total := Total_Num;
               -------------------------------------------------------------------
               -- Energy charge part --
               -------------------------------------------------------------------

               if not Scout_bee then
                  My_energy := Current_Charge;
               else
                  My_energy := 0.1;
               end if;
               -- should be a flag car, My energy should be < 0.5

               if My_energy <= 0.5 or else Scout_bee then

                  Set_Throttle (1.0 - Long_Float (Current_Charge));
                  -- As the energy decreases, it becomes more and more hungry.
                  Set_Destination (My_Message.Energy_Position);
                  -- Greed, always go to the newest one
                  My_energy := Current_Charge;
                  -- no metter the Scout bee can find new message or not, it time to go back to circle
               end if;

               -------------------------------------------------------------------
               -- How to moving part --
               -------------------------------------------------------------------
               if Total > 250 then
                  R_plus := plus_matrix (2);
               elsif Total < 100 then
                  R_plus := plus_matrix (0);
               else
                  R_plus := plus_matrix (1);
               end if;
               -- compute how many distance should change because of the total num change radius
               Orbital_radius := 0.3 * Long_Float (Current_Charge) + R_plus;
               X := Long_Float (Float (Orbital_radius) * Sin (N1) * Cos (N2));
               Y := Long_Float (Float (Orbital_radius) * Sin (N2) * Cos (N1));
               Z := Long_Float (Float (Orbital_radius) * Cos (N1));
               -- compute Orbital_radius and position on a ball

               if My_energy <= 1.0 and then My_energy > 0.5 then
                  Scout_bee := False;
                  Set_Throttle (Long_Float (Current_Charge));

                  if Count < 16 then
                     waiting_position := My_Message.Energy_Position +  Celestial_matrix (Count mod 16);
                     Set_Destination (waiting_position);
                     Set_Throttle (speed_matrix (Vehicle_No mod 3) * Long_Float (Current_Charge));
                     if abs (Position - waiting_position) < abs (Position - My_Message.Energy_Position) then
                        Count := Count + 1;
                        -- Vehicle the next stp location lis larger than the new message energy globe,
                        -- it will go to the new one-> for the The outer spherical shell to go to an other
                     end if;
                  else -- my Orbital model is compute by 16 different points, counter 1~16 means running 1 time around

                     Count := 0;
                     -- more than 16, should restart a new round
                     Set_Throttle (1.0 - Long_Float (Current_Charge));
                     -- the more energy it have, the lower speed it have
                     Set_Destination (My_Message.Energy_Position + (X, Y, Z));
                     -- Distribute the ship as evenly as possible on the sphere
                  end if;

               end if;

            end;
         end loop Outer_task_loop;

      end select;

   exception
      when E : others => Show_Exception (E);

   end Vehicle_Task;

end Vehicle_Task_Type;
