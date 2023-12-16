-module(list_printer).
-export([print_list/1]).

print_list([]) ->
    ok;  % Base case for an empty list
print_list([Head | Tail]) ->
    io:format("~p~n", [Head]),  % Print the head of the list
    print_list(Tail).  % Recursively call print_list with the tail of the list