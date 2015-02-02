-module(concurrent_char_count).
-export([main/2,count/3,collector/3]).

%N = number of processes wanted
main(FileName, N)->
io:format("~n Loading File : ~p~n", [FileName]),
   {ok, Binary} = file:read_file(FileName),
   List=binary_to_list(Binary), %Convert binary read into a list
   Length=round(length(List)/N), %Calculate length of lists to pass to each process
   Ls=string:to_lower(List), %Lower case everything and shove it into a list
   Segments=split(Ls,Length), %Split the input text into a list of the specified threads size
   io:fwrite(" Loaded and Split x ~p Segments~n", [N]),
   CollectorID = spawn(?MODULE, collector, [[],1, N]), %Spawn the collector with the specified no of processes
   io:format("~n Spawned the Collector with \t Pid : ~p~n",[CollectorID]),
   splitter(Segments,1,CollectorID). %Call the splitter with the Collector process ID
   
%
%If passed an empty list to split returns an empty list
%
split([],_)->[];

%
%When passed a populated list and a number corresponding to the length it splits the list into
%A list consisting of the passed text chunked to the specified size
%
split(List,Length)->
   S1=string:substr(List,1,Length),
   case length(List) > Length of 
      true->S2=string:substr(List,Length+1,length(List));
      false->S2=[] %If the size of the list is now smaller than the chunk size the remainder string is empty
   end,
   [S1]++split(S2,Length). %Add on the processing of the remainder of the text

%
%
%
collector(ResultsList,Joins, N) ->
  receive
    {start, _, CountedList} -> 
    case ResultsList == []  of 
      true ->  Shuffle = CountedList; %If ResultsList is empty set it to the countedlist
      false -> Shuffle = joinLists(CountedList, ResultsList) %if it's not empty join the counted and results lists
    end,
    case Joins == N of %If the number of joins performed is equal to the no of threads spawned (ie all results have been collected and joined)
		%Print out result of the join of the process results
      true -> io:fwrite("~n Results = ~p ~n~p Processes completed and joined~n",[Shuffle,Joins]),
      collector(Shuffle,Joins, N);
      false -> collector(Shuffle,Joins+1, N) %If number of joins != N, add results from next process
    end;
    Msg -> %Print out error message if one is passed back
      io:format("~p Collector received unexpected message ~n", [Msg]),
      collector(ResultsList,Joins, N)
  end.

%
%If both lists are empty return an empty list
%
joinLists([],[])->[];

%
%If the counted list is empty but results list is not just return the results list (shouldn't ever be needed)
%
joinLists([],R)->R;

%
%If both lists are populated add the counts from both tuples together through the lists
%
joinLists([H1 |T1],[H2|T2])->
   {_C,N}=H1,
   {C1,N1}=H2,
   [{C1,N+N1}]++joinLists(T1,T2). %Call the function on the tails of both lists

%
%
%
splitter([H|T],N,CollectorID) ->
  spawner(H,N,CollectorID), %Call spawner with the collector id and chunk of text
  splitter(T,N+1,CollectorID); %Call splitter again with the tail of the list

%
% Stop Function when list is empty
%
splitter([], N,_) -> io:fwrite(" Spawned the ~p Processes~n",[N-1]). %Tell the user how many processes have been spawned

%
%Takes the chunk of words to be processed and the number identifying the process,
%then creates the process and tells it to start on processing the chunk
%
spawner(WordList,N,CollectorID) ->
  SeenList = [],
  Letters=[$a,$b,$c,$d,$e,$f,$g,$h,$i,$j,$k,$l,$m,$n,$o,$p,$q,$r,$s,$t,$u,$v,$w,$x,$y,$z],
  Processes = [spawn_link(fun() -> 
          process(WordList, CollectorID,N,Letters) end) ],
  [ Process ! {start, CollectorID, SeenList,N} || Process <- Processes].

%
%Is passed the segment of the file to be processed and passes it to the specified process id
%Then runs the process with the letters list
%
process(Segment, ProcessID,N,Letters) ->
  receive
    {start, ProcessID, CountedList,N} -> run_process(Segment, ProcessID, CountedList,N,Letters);
    Msg -> %Error catching message
      io:format("~p received unexpected message ~p~n", [Segment, Msg]),
      process(Segment, ProcessID,N,Letters)
  end.

%
%
%
run_process(Segment, ProcessID, CountedList, N,[H|T])  ->
    Num=count(H,Segment,0),
    CountedList2=CountedList++[{[H],Num}],
    run_process(Segment, ProcessID, CountedList2,N,T);

%
% Run Process - Stop Function, if the list of letters still to be counted is empty, so shut process down
%
run_process(_, ProcessID, CountedList,N,[]) ->
  ProcessID ! {start, N, CountedList}.

%
%  Counts occurrences of character in list - based on provided code
%If the list is empty then just return the count
%
count(_, [],N)->N;

count(Ch, [H|T],N) ->
   case Ch==H of
   true-> count(Ch,T,N+1); %Increment the count and move onto next character
   false -> count(Ch,T,N) %Doesn't match character provided so move on to next in list
end.

