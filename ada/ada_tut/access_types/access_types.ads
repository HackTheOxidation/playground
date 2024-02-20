with Dates; use Dates;

package Access_Types is
        type Date_Acc is access Date;
        type String_Acc is access String;

        D : Date_Acc := new Date'(30, November, 2_011);

        Msg : String_Acc := new String'("Hello");

        Buffer : String_Acc := new String (1 .. 10);

        Today : Date := D.all;

        J : Integer := D.Day;
end Access_Types;
