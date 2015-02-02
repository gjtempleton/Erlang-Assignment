-module(word_counter).
-export([mainWithSingleQuotes/1]).
-export([mainRemovingSingleQuotes/1]).
-export([unique/2]).
-export([sort/1]).
-export([processing/1]).
-export([wordCount/3]).

%Does not remove single quote characters on read in, means contractions are counted, however also results in
%Words at start of sentences being counted (unable to find way to remove these as well as preserving 
%contractions such as "'tis")
mainWithSingleQuotes(FileName) ->
    io:format("~nLoading File : ~p~n", [FileName]),
    {ok, File} = file:read_file(FileName),
    Content = unicode:characters_to_list(File),
    TokenList = string:tokens(string:to_lower(Content), " .,;:!?~/><{}£$%^&()@-=+_[]*#\\\n\r\"0123456789"),
	F = fun ("'") -> false ; (_) -> true end, %fun to match strings of single quote mark
	NewTokenList = lists:filter(F, TokenList), %Remove words which are just a single quote
    processing(NewTokenList).

%Does remove single quote characters on opening file, unfortunately means that contractions such as "don't" are
%not read in properly and therefore not counted
mainRemovingSingleQuotes(FileName) ->
	io:format("~nLoading File : ~p~n", [FileName]),
    {ok, File} = file:read_file(FileName),
    Content = unicode:characters_to_list(File),
    TokenList = string:tokens(string:to_lower(Content), " '.,;:!?~/><{}£$%^&()@-=+_[]*#\\\n\r\"0123456789"),
    processing(TokenList).

processing(TokenList) ->                                  % Takes in the unsorted list from main
    % Scan through the tokens and find a list of unique words
    UniqueList = unique(TokenList,[]),            % Creates Uniquelist of Items
    io:format("~nSorted List : ~n"),
    SortedList = sort(UniqueList),          % Sorts UniqueList into SortedList
    
    io:format("~nWriting to unique_words.txt~n"),
	io:format("~nHere are the most common words:~n"),
    {ok, F} = file:open("unique_words.txt", [write]), %Open output file to write to
    register(my_output_file, F),
    U = wordCounter(SortedList,TokenList,0),
    io:format("~nThere are ~p unique words in the input file~n", [U]).

wordCounter([H|T],TokenList,N) ->
	wordCount(H, TokenList, 0),
    wordCounter(T,TokenList,N+1);

wordCounter([], _, N) -> N.

% Word count takes the unique word, and searches the original list
% counts occurrences of that word
wordCount(Word,[H|T],N) ->
    case Word == H of           % checks to see if H is in Seen List
        true -> wordCount(Word, T, N+1);              % if seen before, N_Seen = Seen List
        false -> wordCount(Word, T, N)       % if not seen before head added to the Seen List.
    end;
    
wordCount(Word,[],N) -> 
	case (N>=20) of
		true -> io:fwrite("~p   \t:  ~p ~n", [N,Word]), %Write the number of times seen and string seen out to the screen,
				io:format(whereis(my_output_file), "~p   \t: ~p ~n", [N,Word]); %Write the number of times seen and string seen out to the results file
		false -> io:format(whereis(my_output_file), "~p   \t: ~p ~n", [N,Word]) %Write the number of times seen and string seen out to the results file
	end.
    
    

unique([H|T],Seen) ->                       % Accepts List of numbers and Seen List
    case lists:member(H, Seen) of           % checks to see if H is in Seen List
        true -> N_Seen = Seen;              % if true, N_Seen = Seen List
        false -> N_Seen = Seen ++ [H]       % if false, head appends Seen List.
    end,
    unique(T,N_Seen);                       % Recursion down the Tail and Seen List.

unique([],Seen) -> Seen.                    % Stop function when the Tail is empty

sort([Pivot|T]) ->                          %Quicksort taking a List as a parameter
	case lists:member([], T) of
		true -> NewT = lists:subtract([], T),
			sort([ X || X <- NewT, X < Pivot]) ++ 
			[Pivot] ++
			sort([ X || X <- NewT, X >= Pivot]);	
		false -> 
			sort([ X || X <- T, X < Pivot]) ++ 
			[Pivot] ++
			sort([ X || X <- T, X >= Pivot])
			end;

sort([]) -> [].                             %Stop function when the list to be sorted is empty, ie the list has already been sorted on the pivots