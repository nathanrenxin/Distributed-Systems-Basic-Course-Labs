-module(test_body).
-export([start/2]).
-export([recv/1]).
-export([send/1]).
    
start(Host, Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    {ok, Server} = gen_tcp:connect(Host, Port, Opt),
    spawn(fun() -> recv(Server) end),
    register(server, spawn(fun() -> recv_server(Server) end)).
    
recv_server(Server)->
    receive 
        {send, Msg} -> 
            io:format("Sending Http Requests: ~p~n", [Msg]),
            gen_tcp:send(Server, Msg),
            recv_server(Server);
        {response, Recv} -> 
            case Recv of
                {ok, Response} ->
                    io:format("Received Http response: ~p~n", [Response]),
                    ok;
                {error, Error} ->
                    io:format("test_body: error: ~w~n", [Error])
            end,
            gen_tcp:close(Server)
        end.
    
recv(Server)->
    Recv = gen_tcp:recv(Server, 0),
    server ! {response, Recv}.
    
send(Msg) ->
    server ! {send, Msg}.