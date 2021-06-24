-----------------------------------------------------------------------
--                       Reflex Library                              --
--                                                                   --
--              Copyright (C) 2012-2014, Artics                      --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

package body Rx.Library.Analogs.Utils is
   
   --------------------
   -- Epsilon_Equal --
   --------------------
   
   function Epsilon_Equal
     (V1 : Float;
      V2 : Float) return Boolean is
   begin
      return (abs (V2-V1)) < Epsilon;
   end Epsilon_Equal;
   
   ----------------
   -- Saturation --
   ----------------

   function Saturation
     (Value : Float;
      Low   : Float;
      High  : Float) return Float
   is
      Val : Float;
   begin
      if Value < Low then
	 Val := Low;
      elsif Value > High then
	 Val := High;
      else
	 Val := Value;
      end if;
      return Val;
   end Saturation;

   ----------------------
   -- Saturation_Value --
   ----------------------

   procedure Saturate_Value
     (Value : in out Float;
      Low   : in Float;
      High  : in Float) is
      
      Val : Float;
   begin
      if Value < Low then
	 Val := Low;
      elsif Value > High then
	 Val := High;
      else
	 Val := Value;
      end if;
      Value := Val;
   end Saturate_Value;
   
   ---------------
   -- Dead_Band --
   ---------------
   
   procedure Dead_Band
     (In_Value       : in Float;
      Out_Value      : out Float;
      Dead_Band_High : in Float;
      Dead_Band_Low  : in Float) is
   begin
      if In_Value > Dead_Band_Low and In_Value < Dead_Band_High then
	 Out_Value := 0.0;
      else
	 Out_Value := In_Value;
      end if;
   end Dead_Band;
   
   ----------
   -- Gain --
   ----------
   
   procedure Gain 
     (Value : in Float;
      G     : in Float;
      Q     : out Float) is
   begin
      Q := Value * G;
   end Gain;
   
   ------------
   -- Max_Of --
   ------------

   function Max_Of
     (Val1 : Float;
      Val2 : Float) return Float
   is
      Val : Float;
   begin
      if Val1 > Val2 then
	 Val := Val1;
      else
	 Val := Val2;
      end if;
      return Val;
   end Max_Of;

  -------------
   -- Min_Of --
   ------------

   function Min_Of
     (Val1 : Float;
      Val2  : Float) return Float
   is
      Val : Float;
   begin
      if Val1 < Val2 then
	 Val := Val1;
      else
	 Val := Val2;
      end if;
      return Val;
   end Min_Of;

   ---------
   -- Min --
   ---------

   procedure Min_Limitation
     (Value : in out Float;
      Mini  : Float) is
   begin
      if Value < Mini then
	 Value := Mini;
      end if;
   end Min_Limitation;

   ---------
   -- Max --
   ---------

   procedure Max_Limitation
     (Value : in out Float;
      Maxi  : Float) is
   begin
      if Value > Maxi then
	 Value := Maxi;
      end if;
   end Max_Limitation;

   --------------------
   -- Pid_Controller --
   --------------------

   procedure Pid_Controller
     (Init           : in out Boolean;
      Setpoint       : in Float;
      Meas           : in Float;
      Dead_Band_Low  : in Float;
      Dead_Band_High : in Float;
      Kp             : in Float;
      Ki             : in Float;
      Kd             : in Float;
      Ki_Limit_Low   : in Float;
      Ki_Limit_High  : in Float;
      Period         : in Duration;
      last_Clock     : in out Duration;
      Ki_Out_1       : in out Float;
      Err_Db         : out Float;
      Cmd            : in out Float) 
   is

      Err    : Float;
      Ki_Out : Float;
      Kp_Out : Float;
      Kd_Out : Float;
      
      Dead_Band_Out : Float;
      Dt     : Duration;
      
      Err1   : Float;
      Clock  : Duration;   
      --
      -- -- PID_Controller --
      -- --------------------
      --
      -- The controller is a PID controller with an integral limitation. The
      -- PID follows the controller fonction :
      -- cons_p := err * kp + err * Ki + err * Kd
      -- where :
      --   err = is the error between the measure at instant i and the PID
      --         command at instant i-1, to which a Dead Band is applied.
      --   Ki is the value of the integral with a low limitation Ki_Limit_Low
      --         and a high limitation Ki_Limit_High
      --
      -- The initialization of the PID controller consists to set the PID
      -- command with the measure of the active power P_Meas
      --
      -- -- Interface --
      -- ---------------
      --
      -- Init           is a boolean to initialize the PID
      -- Meas           is the measure of the active Power at instant i
      -- Dead_Band_Low  is the error limitation low
      -- Dead_Band_High is the error limitation high
      -- Kp             is the proportional coeficient
      -- Ki             is the integral coeficient
      -- Kd             is the derivate coeficient
      -- Ki_Limit_Low   is the low integral limitation
      -- Ki_Limit_High  is high the integral limitation
      -- Cmd            is the PID computed command at instant i
      --
      -- -- Algorithm --
      -- ---------------

   begin
      -- PID controller Initialization
      Clock := Duration (0.0);
      
      if Init then
         Cmd  := Meas;
         Init := False;
         Last_Clock := Clock; -- Rx.Run_Time.Rxclock.Clock;
         Err1 := 0.0;
         Ki_Out_1 := 0.0;
      end if;

      Dt := Clock; -- Rx.Run_Time.Rxclock.Clock - Last_Clock;
      
      if Dt >= Period then
         Last_Clock := Clock; -- Rx.Run_Time.Rxclock.Clock;
         
         -- Error deviation
         
         Err := Setpoint - Meas;
         Err_Db := Err;
         
         -- Lib.Saturate_Value (Err_Db, Dead_Band_Low, Dead_Band_High);
         
         Dead_Band
           (In_Value       => Err,
            Out_Value      => Dead_Band_Out,
            Dead_Band_High => Dead_Band_Low,
            Dead_Band_Low  => Dead_Band_High);
         
         Err := Dead_Band_Out;
         
         -- Pid Controller
         
         Kp_Out := Err * Kp;
         Ki_Out := Ki * Err * Float (Dt) + Ki_Out_1;
         Kd_Out := Err * Kd;

         Ki_Out_1 := Ki_Out;

         -- Integral Limitation
         
         Saturate_Value (Ki_Out, Ki_Limit_Low,  Ki_Limit_High);
         
         -- PID controller command
         Cmd :=Kp_Out + Ki_Out + Kd_Out;
         Saturate_Value (Cmd, -1.0,  1.0);
      end if;
   end Pid_Controller;
   
   ---------------------
   -- Ramp_Limitation --
   ---------------------
   
   procedure Ramp_Limitation
     (Init       : in Boolean;
      In_P       : in Float;
      Setpoint   : in Float;
      Dt         : in Float;
      Limit_Rise : in Float;
      Limit_Fall : in Float;
      Out_P_Old  : in out Float;
      Out_P      : out Float) is

      Rate    : Float;
      Err     : Float;
      Err_Abs : Float;
      Rise    : Float;
      Fall    : Float;
      
      --    
      -- -- Ramp_Limitation --
      -- ---------------------
      -- 
      -- The Setpoint is either the computed Setpoint by Frequency or Curtail, 
      -- but limited by the TSO maximun allowed ramp and WTGs maximun allowed
      -- ramp. The Ramp limitation is computed by the module 
      -- Ramp_Rate_Calculation which choose the miminum of TSO and WTGs 
      -- limitation. 
      -- At the init of regul, the setpoint with the P measured.
      --
      -- -- Interface --
      -- ---------------
      --
      -- Init       => True when the regulation starts
      -- In_P       => The active power measured (Kw)
      -- Setpoint   => Stepoint to reach
      -- Dt         => The period of the regulation
      -- Limit_Rise => The maximum Ramp allowed (Kw/s)
      -- Limit_Fall => The minimun Ramp allowed (Kw/s)
      -- Out_P_Old  => The value of Active Setpoint at the previous instant (Kw)
      -- Out_P      => The current Setpoint (Kw) computed for current instant
      
   begin
      Err := Setpoint - In_P;
      
      Err_Abs := abs (Err);
      
      Rate := Err;
      
      Rise := Limit_Rise * Dt;
      Fall := Limit_Fall * Dt;
      
      if Err > 0.0 and Err_Abs > Rise then
	 Out_P := In_P + Rise;
	 
      elsif Err < 0.0 and Err_Abs > Fall then
	 Out_P := In_P - Fall;
	 
      else
         Out_P := In_P + Err;
      end if;
   end Ramp_Limitation;
   
end Rx.Library.Analogs.Utils;
