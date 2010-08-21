-module(swarm).
-compile(export_all).
-record(agent,{pos={random:uniform() * 100000,
                    random:uniform() * 100000,
                    random:uniform() * 100000},
               velocity={random:uniform() * 10,
                         random:uniform() * 10,
                         random:uniform() * 5},
               vitality=random:uniform(),
               avoid={0,10},
               comfort={10,100},
               attract={100,1000}}).

distance(A) when is_list(A) ->
    lists:map( fun(X) -> distance(X,A) end,
         A).

distance(A,B) when is_record(A,agent) andalso is_record(B,agent) ->
    distance(A#agent.pos,B#agent.pos);
distance(A,B) when is_record(A,agent) andalso is_list(B) ->
    lists:map(fun (X) -> distance(A,X) end, B);
distance(A,B) when is_tuple(A) andalso is_tuple(B) ->
    math:sqrt(lists:sum(lists:map( fun (X) -> math:pow(element(2,X)-element(1,X), 2) end,lists:zip(tuple_to_list(A),tuple_to_list(B))))).

create_agent() ->
    #agent{}.

create_agents(N) ->
    [#agent{} || _ <- lists:seq(1,N)].

gravity(A) when is_list(A) ->
    lists:map( fun(X) -> gravity(X,A) end,
         A).
gravity(A,A) when is_record(A,agent) ->
    0;
gravity(A,B) when is_record(A,agent) andalso is_record(B,agent) ->
    1 / math:pow(distance(A,B),2);
gravity(A,B) when is_record(A,agent) andalso is_list(B) ->
    lists:map(fun (X) -> gravity(A,X) end, B).

in_range({Min, Max}, Distance)
  when Min < Distance andalso Distance =< Max ->
    true;
in_range(_, _) ->
    false.

normal_vector(Agent,Bgent) ->
    Distance=distance(Agent,Bgent),
    {(element(1,Bgent#agent.pos) - element(1,Agent#agent.pos)) / Distance,
     (element(2,Bgent#agent.pos) - element(2,Agent#agent.pos)) / Distance,
     (element(3,Bgent#agent.pos) - element(3,Agent#agent.pos)) / Distance}.

dump_positions(A) ->
    lists:foreach(fun (X) ->
                      io:format("~p ~p ~p~n", [element(1,X#agent.pos), element(2,X#agent.pos), element(3,X#agent.pos)]) end, A).

test() ->
    test(100).

test(N) ->
    SWARM=create_agents(N),
    [ HEAD | TAIL ] = SWARM,
    [ HEAD1 | _ ] = TAIL,
    %io:format("Swarm: ~p~n",[SWARM]),
    %io:format("Distances: ~p~n",[swarm:distance(SWARM)]),
    %io:format("Distances: ~p~n",[swarm:distance(HEAD,TAIL)]),
    %io:format("Gravity: ~p~n",[swarm:gravity(HEAD,HEAD1)]),
    %io:format("Gravity: ~p~n",[swarm:gravity(HEAD,TAIL)]),
    %io:format("Gravity: ~p~n",[swarm:gravity(SWARM)]),
    %swarm:dump_positions(SWARM),
    %io:format("inrange 1,5,2: ~p~n",[swarm:in_range({1,5},2)]),
    %io:format("inrange 4,5,2: ~p~n",[swarm:in_range({4,5},2)]),
    %io:format("inrange 4,5,7: ~p~n",[swarm:in_range({4,5},7)]),
    %io:format("inrange 4,5,5: ~p~n",[swarm:in_range({4,5},5)]),
    %io:format("inrange 4,5,4: ~p~n",[swarm:in_range({4,5},4)]),
    io:format("normal Vector:  ~p~n",[swarm:normal_vector(HEAD,HEAD1)]).
