# GCM

An Erlang/OTP application for Google Cloud Messaging (GCM).

## Usage

In order to register or unregister a device, simply send an HTTP `PUT`
or `DELETE` request respectively, with the registration id as part of
the URI as below.

* Register: `PUT /registrations/:registration_id`

* Unregister: `DELETE /registrations/:registration_id`
