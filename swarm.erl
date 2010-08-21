-module(swarm).
-compile(export_all).
-record(agent,{pos={random:uniform() * 1000,
                    random:uniform() * 1000,
                    random:uniform() * 1000},
               velocity={0,0,0},
               avoid={0,4},
               comfort={4,20},
               attract={20,300}}).

create_agent() ->
    #agent{}.

create_agents(N) ->
    [#agent{} || _ <- lists:seq(1,N)].

distance(A,B) when is_record(A,agent) andalso is_record(B,agent) ->
    distance(A#agent.pos,B#agent.pos);
distance(A,B) when is_tuple(A) andalso is_tuple(B) ->
    math:sqrt(lists:sum(lists:map( fun (X) -> math:pow(element(2,X)-element(1,X), 2) end,lists:zip(tuple_to_list(A),tuple_to_list(B))))).

gravity(A,B) when is_record(A,agent) andalso is_record(B,agent) ->
    1 / math:pow(distance(A,B),2).

in_range({Min, Max}, Distance)
  when Min < Distance andalso Distance =< Max ->
    true;
in_range(_, _) ->
    false.

normal_vector(Agent,Bgent) ->
    Distance=distance(Agent,Bgent),
    {X1,Y1,Z1}=Agent#agent.pos,
    {X2,Y2,Z2}=Bgent#agent.pos,
    {(X2 - X1) / Distance,
     (Y2 - Y1) / Distance,
     (Z2 - Z1) / Distance}.

gravity_vector(Agent,Bgent,Valid,Const) when Valid =:= true ->
    Force = gravity(Agent,Bgent)*Const,
    {X,Y,Z}=normal_vector(Agent,Bgent),
    {X*Force,
     Y*Force,
     Z*Force};
gravity_vector(_,_,_,_) ->
    {0, 0, 0}.

which (A,{0, 0, 0}) ->
    A;
which ({0, 0, 0}, B) ->
    B;
which (_,_) ->
    {0, 0, 0}.

move(Swarm) ->
    lists:map(fun (Agent) ->
                      move(Agent,Swarm) end, Swarm).

move(Agent, Swarm) when is_record(Agent,agent) andalso is_list(Swarm)->
    {X,Y,Z} = Agent#agent.pos,
    {Xd,Yd,Zd}=lists:foldl(fun ({X1,Y1,Z1},{X2,Y2,Z2}) -> {X1+X2,Y1+Y2,Z1+Z2} end,
                {0,0,0},
                lists:map(fun (Bgent) ->
                                  move(Agent,Bgent) end, Swarm)),
    #agent{pos={X + Xd,Y + Yd,Z + Zd},
           velocity={Xd,Yd,Zd},
           avoid=Agent#agent.avoid,
           comfort=Agent#agent.comfort,
           attract=Agent#agent.attract};
move(Agent, Agent) ->
    {0, 0, 0};
move(Agent, Bgent) when is_record(Agent,agent) andalso is_record(Bgent,agent)->
    Distance = distance(Agent,Bgent),
    which(gravity_vector(Agent,Bgent,in_range(Agent#agent.avoid,Distance),-1),
          gravity_vector(Agent,Bgent,in_range(Agent#agent.attract,Distance),1)).

dump_positions(Swarm) ->
    lists:foreach(fun (Agent) ->
                          {X,Y,Z} = Agent#agent.pos,
                      io:format("~p ~p ~p~n", [X/100,Y/100,Z/100]) end, Swarm),
    io:format("done~n").

test() ->
    test(30).

run(Swarm) ->
    run(move(Swarm),100).

run(_,0) ->
    ok;
run(Swarm,I) ->
    dump_positions(Swarm),
    run(move(Swarm),I-1).

test(N) ->
    Swarm=create_agents(N),
    run(Swarm).
    %[ HEAD | TAIL ] = Swarm,
    %[ HEAD1 | _ ] = TAIL,
    %io:format("Swarm: ~p~n",[Swarm]),
    %io:format("ASDF~n"),
    %io:format("Distances: ~p~n",[swarm:distance(HEAD,HEAD1)]),
    %io:format("Gravity: ~p~n",[swarm:gravity(HEAD,HEAD1)]),
    %swarm:dump_positions(Swarm),
    %io:format("inrange 1,5,2: ~p~n",[swarm:in_range({1,5},2)]),
    %io:format("inrange 4,5,2: ~p~n",[swarm:in_range({4,5},2)]),
    %io:format("inrange 4,5,7: ~p~n",[swarm:in_range({4,5},7)]),
    %io:format("inrange 4,5,5: ~p~n",[swarm:in_range({4,5},5)]),
    %io:format("inrange 4,5,4: ~p~n",[swarm:in_range({4,5},4)]),
    %io:format("normal Vector:  ~p~n",[swarm:move(Swarm)]).
