package Test_Discriminants is
        type Point (X, Y : Natural := 0) is record
                null;
        end record;

        P2 : Point (1, 2);
        P3 : Point := (1, 2);

end Test_Discriminants;
