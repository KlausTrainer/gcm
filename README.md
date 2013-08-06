# GCM

An Erlang/OTP application for Google Cloud Messaging (GCM).

## Usage

### HTTP Registration API

In order to register or unregister a device, simply send an HTTP `PUT`
or `DELETE` request respectively, with the registration id as part of
the URI as below.

* Register: `PUT /registrations/:registration_id`

* Unregister: `DELETE /registrations/:registration_id`

### Erlang Push API

You will probably just need the `gcm_sender:push/2` function, which
takes a list of registration ids (binaries) and a message (binary).  For
the case that you would like to send a broadcast message to all
registered devices, there's also the `gcm_sender:push_broadcast/1`
function.
