-module(swarm).
-compile(export_all).
-define(REPEL_FORCE,-10).
-define(ATTRACT_FORCE,1000).
-define(ITERS_PER_FRAME,1).
-record(agent,{pos={random:uniform() * 500,
                    random:uniform() * 500,
                    random:uniform() * 500},
               avoid={0,((random:uniform()-0.5)*10)+10},
               attract={18,((random:uniform()-0.5)*100)+300}}).


create_agent() ->
    #agent{}.

create_agents(N) ->
    [#agent{} || _ <- lists:seq(1,N)].

distance(A,B) when is_record(A,agent) andalso is_record(B,agent) ->
    distance(A#agent.pos,B#agent.pos);
%distance(A,B) ->
%    math:sqrt(lists:sum(
%                lists:map(
%                  fun ({X,Y}) -> math:pow(Y-X, 2) end,
%                  lists:zip(tuple_to_list(A),
%                            tuple_to_list(B))))).
distance({X1,Y1,Z1},{X2,Y2,Z2}) ->
    math:sqrt(math:pow((X2-X1),2)+math:pow((Y2-Y1),2)+math:pow((Z2-Z1),2)).

gravity(A,B) ->
    gravity(distance(A,B)).

gravity(D) when D == 0 ->
    0;
gravity(D) ->
    1 / math:pow(D,2).

in_range({Min, Max}, Distance)
  when Min < Distance andalso Distance =< Max ->
    true;
in_range(_, _) ->
    false.

normal_vector(Agent,Agent) ->
    {0,0,0};
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

which (A, {0, 0, 0}) ->
    A;
which ({0, 0, 0}, B) ->
    B;
which (_,_) ->
    {0, 0, 0}.

new_pos(D,P,_) when D == 0 ->
    P;
new_pos(D, {X,Y,Z}, {Xd,Yd,Zd}) ->
    {X + Xd/D, Y + Yd/D, Z + Zd/D}.

len(Vector) ->
    distance({0,0,0},Vector).

add_vectors({X1,Y1,Z1},{X2,Y2,Z2}) ->
    {X1+X2,Y1+Y2,Z1+Z2}.

move(Swarm) ->
    lists:map(fun (Agent) ->
                      move(Agent,Swarm) end, Swarm).

move(Agent, Swarm) when is_record(Agent,agent) andalso is_list(Swarm)->
    Vector=lists:foldl(fun swarm:add_vectors/2,
                       {0,0,0},
                       lists:map(fun (Bgent) ->
                                         move(Agent,Bgent) end, Swarm)),
    #agent{pos=new_pos(len(Vector),Agent#agent.pos, Vector),
           avoid=Agent#agent.avoid,
           attract=Agent#agent.attract};
move(Agent, Agent) ->
    {0, 0, 0};
move(Agent, Bgent) when is_record(Agent,agent) andalso is_record(Bgent,agent)->
    Distance = distance(Agent,Bgent),
    which(gravity_vector(Agent,Bgent,in_range(Agent#agent.avoid,Distance),?REPEL_FORCE),
          gravity_vector(Agent,Bgent,in_range(Agent#agent.attract,Distance),?ATTRACT_FORCE)).

dump_positions(Swarm) ->
    lists:foreach(fun (Agent) ->
                          {X,Y,Z} = Agent#agent.pos,
                          io:format("~p ~p ~p~n", [X/100,Y/100,Z/100]) end, Swarm),
    io:format("done~n").

infinite(Swarm) ->
    dump_positions(Swarm),
    infinite(run(move(Swarm),?ITERS_PER_FRAME)).

run(Swarm) ->
    run(move(Swarm),100).

run(Swarm,0) ->
    Swarm;
run(Swarm,I) ->
    dump_positions(Swarm),
    run(move(Swarm),I-1).

test() ->
    test(30).
test(N) ->
    Swarm=create_agents(N),
    run(Swarm,4).
    %[ HEAD | TAIL ] = Swarm,
    %[ HEAD1 | _ ] = TAIL,
    %io:format("Swarm: ~p~n",[Swarm]),
    %io:format("Distances: ~p~n",[swarm:distance(HEAD,HEAD1)]),
    %io:format("Gravity: ~p~n",[swarm:gravity(HEAD,HEAD1)]),
    %swarm:dump_positions(Swarm),
    %io:format("inrange 1,5,2: ~p~n",[swarm:in_range({1,5},2)]),
    %io:format("inrange 4,5,2: ~p~n",[swarm:in_range({4,5},2)]),
    %io:format("inrange 4,5,7: ~p~n",[swarm:in_range({4,5},7)]),
    %io:format("inrange 4,5,5: ~p~n",[swarm:in_range({4,5},5)]),
    %io:format("inrange 4,5,4: ~p~n",[swarm:in_range({4,5},4)]),
    %io:format("normal Vector:  ~p~n",[swarm:move(Swarm)]).

main(_) ->
    Swarm=create_agents(100),
    run(Swarm,5000).
    %infinite(Swarm).
