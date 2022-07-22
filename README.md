# Time2

## Allow sending time zone through wire

We introduce this package because we want to transfer time zone back and forth between client and server.

Previously, in order to send time zone through the wire, we have to keep a separate copy of the data and only convert it
to `Time.Zone` at the very last step. We feel it would be convenient for us if we put that copy in a package.

## Observe daylight saving time

This package also tries to observes daylight saving time, especially the `toParts` and `fromParts` functions.
