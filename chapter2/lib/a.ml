let fib n = 
    let rec fib_tr p pp i = 
        if (i = 1) then pp
        else fib_tr pp (p + pp) (i-1) in
    fib_tr 0 1 n;;
