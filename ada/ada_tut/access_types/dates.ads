package Dates is
        type Months is
               (January, February, March, April, May, June, July, August,
                September, October, November, December);

        type Date is record
                Day   : Integer range 1 .. 31;
                Month : Months;
                Year  : Integer;
        end record;
end Dates;
