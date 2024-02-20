with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

procedure Open_File is
  File : File_Type;
begin
  begin
    Open(File, In_File, "input.txt");
  exception
    when E : Name_Error =>
      Put("Cannot open input file : ");
      Put_Line(Exception_Message(E));
      raise;
  end;
end Open_File;
