-module(pi_checker).
-export([pi/1,pi/4]).
-export([check/2]).

%if given no parameters start with N = 1 and previous guess at value as pi=0
pi(Dec) -> pi(Dec, 1,0,1).

pi(Dec, _, Pi, 0.00) ->	
	io_lib:format("~w", [check(Pi,Dec)]); %Print out the result, to specified no of decimal places

pi(Dec, N, Old, _) ->
		DecCheck = ((Dec*2)+1), %Create a number of decimal places to check to for changes. This won't be sufficient for larger numbers (>8)
	    Latest =  Old + (4/N - 4/(N+2)),
		Difference = check(Latest,DecCheck) - check(Old,DecCheck), %Check the difference between last iteration and this
		pi(Dec, N+4,Latest,Difference).

check(F, N) ->
    Prec = math:pow(10, N),
    trunc(F*Prec)/Prec.