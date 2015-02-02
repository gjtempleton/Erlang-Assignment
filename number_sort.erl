-module(number_sort).
-export([main/1,unique/2,sort/1,processing/1]).

main(FileName) ->
    io:format("~nLoading File : ~p~n", [FileName]),
    {ok, File} = file:read_file(FileName), %Read in the text file
    Content = unicode:characters_to_list(File), %Convert the input to chars
    TokenList = string:tokens(string:to_lower(Content), " .,;:!?/>'<{}$%^&()-=+_[]*#\\\n\r\"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"), %Strip out punctuation and letters
    processing(TokenList).

processing(L) ->                                  % Take in the unsorted list
    Seen = [],                            
    io:format("~nUnsorted List : "),  %Print out the unsorted list from the file to the user
    T = countAndPrint(L), %Count and print out the input numbers
    io:format("~nTotal : ~p~n", [T]),
    UniqueList = unique(L,Seen),            % Creates unique list of items (ie dump out repeats)
    SortedList = sort(UniqueList),          % Sort the list of uniques
    io:format("~nSorted List : "),			% Print out the sorted list of uniques
    U = countAndPrint(SortedList),
    io:format("~nUnique : ~p~n", [U]).		%Print out the count of uniques


unique([H|T],Seen) ->                       % Accepts list of numbers and list of so far seen
    case lists:member(H, Seen) of           % checks to see if H is in the list of those so far seen
        true -> N_Seen = Seen;              % if true, ignore it
        false -> N_Seen = Seen ++ [H]       % if false, head added to the unique seen list
    end,
    unique(T,N_Seen);                       % calls uniques with Tail and the seen list, so carry on working through

%Pass back the list of uniques once all input been worked through
unique([],Seen) -> Seen.                    % Stop Function


sort([Pivot|T]) ->                          %Quicksort taking a List as a parameter
   	sort([ X || X <- T, X < Pivot]) ++ 	    % list of all elements in T which are less than Pivot
    [Pivot] ++							    % pivot 
    sort([ X || X <- T, X >= Pivot]);

sort([]) -> [].                             %Quicksort stop function

%If just passed a list, start counting with the count=0
countAndPrint(L) ->
    countAndPrint(L,0).
	
countAndPrint([H|T],N) ->                           %counts items in list using tail recursion
    io:fwrite("~p", [H]),  %Print out the head element
    io:fwrite("  "), 
    countAndPrint(T, N+1); %Carry on working on the tail
	
%If passed an empty list, ie all elements been printed and counted pass back the count
countAndPrint([],N)-> N.

