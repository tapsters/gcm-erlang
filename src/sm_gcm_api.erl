-module(sm_gcm_api).
-export([push/3]).

-define(BASEURL, "https://gcm-http.googleapis.com/gcm/send").

-type header()  :: {string(), string()}.
-type headers() :: [header(),...].
-type regids()  :: [binary(),...].
-type message() :: [tuple(),...].
-type result()  :: {number(), non_neg_integer(), non_neg_integer(), non_neg_integer(), [any()]}.

-spec push(regids(),message(),string()) -> {'error',any()} | {'noreply','unknown'} | {'ok',result()}.
push(RegIds, {struct, MessageProps}, Key) ->
    Request = lists:flatten(sm:json_enc({struct, [{"registration_ids", {array, RegIds}}|MessageProps]})),
    ApiKey = string:concat("key=", Key),

    try httpc:request(post, {?BASEURL, [{"Authorization", ApiKey}], "application/json", Request}, [], []) of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            Json = sm:json_dec(Body),
            %%error_logger:info_msg("Result was: ~p~n", [Json]),
            {ok, result_from(Json)};
        {ok, {{_, 400, _}, _, Body}} ->
            error_logger:error_msg("Error in request. Reason was: Bad Request - ~p~n", [Body]),
            {error, Body};
        {ok, {{_, 401, _}, _, _}} ->
            error_logger:error_msg("Error in request. Reason was: authorization error~n", []),
            {error, auth_error};
        {ok, {{_, Code, _}, Headers, _}} when Code >= 500 andalso Code =< 599 ->
            RetryTime = retry_after_from(Headers),
            error_logger:error_msg("Error in request. Reason was: retry. Will retry in: ~p~n", [RetryTime]),
            {error, {retry, RetryTime}};
        {ok, {{_StatusLine, _, _}, _, _Body}} ->
            error_logger:error_msg("Error in request. Reason was: timeout~n", []),
            {error, timeout};
        {error, Reason} ->
            error_logger:error_msg("Error in request. Reason was: ~p~n", [Reason]),
            {error, Reason};
        OtherError ->
            error_logger:error_msg("Error in request. Reason was: ~p~n", [OtherError]),
            {noreply, unknown}
    catch
        Exception ->
            error_logger:error_msg("Error in request. Exception ~p while calling URL: ~p~n", [Exception, ?BASEURL]),
            {error, Exception}
    end.

-spec result_from([{struct,list()}]) -> result().
result_from({struct, Props}) ->
    {
      proplists:get_value("multicast_id", Props),
      proplists:get_value("success", Props),
      proplists:get_value("failure", Props),
      proplists:get_value("canonical_ids", Props),
      proplists:get_value("results", Props)
    }.

-spec retry_after_from(headers()) -> 'no_retry' | non_neg_integer().
retry_after_from(Headers) ->
    case proplists:get_value("retry-after", Headers) of
        undefined ->
            no_retry;
        RetryTime ->
            case string:to_integer(RetryTime) of
                {Time, _} when is_integer(Time) ->
                    Time;
                {error, no_integer} ->
                    sm:parse_http_date(RetryTime) - sm:now()
            end
    end.
