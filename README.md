smoothie-gcm
============

This is fork of [gcm-erlang](https://github.com/pdincau/gcm-erlang) optimized to use with [smoothie](https://github.com/tapsters/smoothie).

This software provides an Erlang client for [`Google Cloud Messaging`](http://developer.android.com/google/gcm/index.html "Google Cloud Messaging for Android").

### Features

Using `smoothie-gcm` you can:

* Start several `gen_servers` representing different `GCM applications` defined by different `GCM API keys`
* Send notification messages to Android mobile devices registered to your specific application and registered to `GCM` using a specific `registration id`

So far `smoothie-gcm` does only provide support for JSON messages since GCM does not allow to send multicast messages using plain text.

### Setting Up

You can use gcm_app as a dependency in your rebar.config:

```Erlang
{deps, [
    {gcm, ".*", {git, "https://github.com/tapsters/smoothie-gcm.git", {tag, "master"}}}
]}.
```

### Starting GCM API Worker (one for each GCM application)

While `smoothie-gcm` is running you can start several supervised gen_servers, one for each GCM application. Every gen_server is defined by an atom used internally for registration and by a `GCM API key`.

```Erlang
1> sm_gcm:start(foo, "myapikey").
{ok,<0.60.0>}
2> sm_gcm:start(bar, "myotherapikey").
{ok,<0.65.0>}
```

You can stop a `gen_server` representing a GCM Application using:

```Erlang
3> sm_gcm:stop(foo).
```

### Sending Messages

At any time you can send a GCM message to one or more mobile devices by calling:

```Erlang
4> sm_gcm:push(RegisteredName, RegIds, Message).
```

or by calling:

```Erlang
5> sm_gcm:sync_push(RegisteredName, RegIds, Message).
```

Where `RegistereName` is the atom used during registration, `RegIds` is a list (max 1000 elements) of Registration Ids specified as Erlang binaries (e.g., `<<"APA91bHun4MxP5egoKMwt2KZFBaFUH-1RYqx...">>`) and `Message` is an Erlang term representing the data you want to send to the device.

The JSON message is built [yaws-json2](https://github.com/tapsters/yaws-json2) in the module `sm_gcm.erl` and in the end will have the following form:

```JavaScript
{
  "registration_ids" : ["APA91bHun4MxP5egoKMwt2KZFBaFUH-1RYqx..."],
  "data" : {
    "message" : "a message"
  },
  "time_to_live" : 3600,
  "collapse_key" : "your_update"
}
```

You can send this message using:

```Erlang
6> sm_gcm:push(RegisteredName, RegIds, [{<<"data">>, [
6>     {<<"message">>, <<"a message">>}
6> ]}, {<<"time_to_live">>,3600}, {<<"collapse_key">>,<<"your_update">>}]).
```

or simply:

```Erlang
7> sm_gcm:push(RegisteredName, RegIds, [{<<"data">>, [
7>     {<<"message">>, <<"a message">>}
7> ]}]).
```

`smoothie-gcm` will push the message for you to `Google Cloud Messaging` servers and will parse the JSON provided as result.

In order to understand errors see: [Interpreting an error response](http://developer.android.com/google/gcm/gcm.html#response).

### Note

Some of the concepts I used for building this Erlang application are based on this [`blog post`](http://tiliman.wordpress.com/2013/01/02/google-cloud-messaging-with-erlang/) and on this [`Erlang application for APN`](https://github.com/extend/ex_apns).
